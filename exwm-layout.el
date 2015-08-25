;;; exwm-layout.el --- Layout Module for EXWM  -*- lexical-binding: t -*-

;; Copyright (C) 2015 Chris Feng

;; Author: Chris Feng <chris.w.feng@gmail.com>
;; Keywords: unix

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This module is responsible for keeping X client window properly displayed.

;;; Code:

(defun exwm-layout--show-at (id geometry)
  (unless (equal geometry exwm--dimensions)
    (if geometry
        (with-slots ((x-relative x) (y-relative y) width height) (car geometry)
          (with-slots ((x-absolute x) (y-absolute y)) (cdr geometry)
            (xcb:+request exwm--connection (make-instance 'xcb:MapWindow :window id))
            (xcb:+request exwm--connection
                (make-instance 'xcb:icccm:set-WM_STATE
                               :window id :state xcb:icccm:WM_STATE:NormalState
                               :icon xcb:Window:None))
            ;; (message "reparent %S to %S %S" id (frame-parameter frame 'exwm-window-id)
            ;;          (xcb:+request-checked+request-check exwm--connection
            ;;     (make-instance 'xcb:ReparentWindow
            ;;                    :window id
            ;;                    :parent (frame-parameter frame 'exwm-window-id)
            ;;                    :x 0 :y 0)))
            (xcb:+request exwm--connection
                (make-instance 'xcb:ConfigureWindow
                               :window id
                               :value-mask (logior xcb:ConfigWindow:X
                                                   xcb:ConfigWindow:Y
                                                   xcb:ConfigWindow:Width
                                                   xcb:ConfigWindow:Height
                                                   xcb:ConfigWindow:StackMode)
                               :x x-relative :y y-relative
                               :width width :height height
                               ;; In order to put non-floating window at bottom
                               :stack-mode xcb:StackMode:Below))
            ;; (xcb:+request exwm--connection
            ;;     (make-instance 'xcb:ConfigureWindow
            ;;                    :window id
            ;;                    :value-mask (logior xcb:ConfigWindow:X
            ;;                                        xcb:ConfigWindow:Y
            ;;                                        xcb:ConfigWindow:Width
            ;;                                        xcb:ConfigWindow:Height
            ;;                                        xcb:ConfigWindow:StackMode)
            ;;                    :x 0 :y 0 :width width :height height
            ;;                    ;; In order to put non-floating window at bottom
            ;;                    :stack-mode xcb:StackMode:Below))
            (xcb:+request exwm--connection
                (make-instance 'xcb:SendEvent
                               :propagate 0 :destination id
                               :event-mask xcb:EventMask:StructureNotify
                               :event (xcb:marshal
                                       (make-instance 'xcb:ConfigureNotify
                                                      :event id :window id
                                                      :above-sibling xcb:Window:None
                                                      :x x-absolute :y y-absolute
                                                      :width width :height height
                                                      :border-width 0
                                                      :override-redirect 0)
                                       exwm--connection)))))
      (xcb:+request exwm--connection
          (make-instance 'xcb:ChangeWindowAttributes
                         :window id :value-mask xcb:CW:EventMask
                         :event-mask xcb:EventMask:NoEvent))
      (xcb:+request exwm--connection (make-instance 'xcb:UnmapWindow :window id))
      (xcb:+request exwm--connection
          (make-instance 'xcb:ChangeWindowAttributes
                         :window id :value-mask xcb:CW:EventMask
                         :event-mask exwm--client-event-mask))
      (xcb:+request exwm--connection
          (make-instance 'xcb:icccm:set-WM_STATE
                         :window id
                         :state xcb:icccm:WM_STATE:IconicState
                         :icon xcb:Window:None)))
    (setq exwm--dimensions geometry)))

(defun exwm-layout--show (id window)
  "Show window ID exactly fit in the Emacs window WINDOW."
  (let* ((buffer (exwm--id->buffer id))
         (frame (and window (window-frame window)))
         geometry)
    (cl-assert buffer)
    (with-current-buffer buffer
      (when window
        (let* ((edges-rel-frame (or exwm--floating-edges
                                    (window-inside-pixel-edges window)))
               (x-offset (frame-parameter frame 'left))
               (y-offset (frame-parameter frame 'top))
               (x-rel-frame (elt edges-rel-frame 0))
               (y-rel-frame (elt edges-rel-frame 1))
               (x-absolute (+ x-rel-frame x-offset))
               (y-absolute (+ y-rel-frame y-offset))
               (width (- (elt edges-rel-frame 2) (elt edges-rel-frame 0)))
               (height (- (elt edges-rel-frame 3) (elt edges-rel-frame 1))))
          (setq geometry (cons (make-instance 'xcb:RECTANGLE :x x-rel-frame :y y-rel-frame :width width :height height)
                               (make-instance 'xcb:RECTANGLE :x x-absolute :y y-absolute :width width :height height)))))
      (exwm-layout--show-at id geometry))))

(defun exwm-layout-set-fullscreen (&optional id)
  "Make window ID fullscreen."
  (interactive)
  (with-current-buffer (if id (exwm--id->buffer id) (window-buffer))
    (when exwm--fullscreen
      (user-error "Already in full-screen mode."))
    ;; Set the floating frame fullscreen first when the client is floating
    (when exwm--floating-frame
      (let* ((outer-id (frame-parameter exwm--floating-frame 'exwm-outer-id))
             (geometry (xcb:+request-unchecked+reply exwm--connection
                           (make-instance 'xcb:GetGeometry
                                          :drawable outer-id))))
        (setq exwm--floating-frame-geometry
              (vector (slot-value geometry 'x) (slot-value geometry 'y)
                      (slot-value geometry 'width)
                      (slot-value geometry 'height)))
        (xcb:+request exwm--connection
            (make-instance 'xcb:ConfigureWindow
                           :window outer-id
                           :value-mask (logior xcb:ConfigWindow:X
                                               xcb:ConfigWindow:Y
                                               xcb:ConfigWindow:Width
                                               xcb:ConfigWindow:Height)
                           :x 0 :y 0
                           :width (frame-pixel-width exwm-workspace--current)
                           :height (frame-pixel-height
                                    exwm-workspace--current))))
      (xcb:flush exwm--connection))
    (xcb:+request exwm--connection
        (make-instance 'xcb:ConfigureWindow
                       :window exwm--id
                       :value-mask (logior xcb:ConfigWindow:X
                                           xcb:ConfigWindow:Y
                                           xcb:ConfigWindow:Width
                                           xcb:ConfigWindow:Height)
                       :x 0 :y 0
                       :width (frame-pixel-width exwm-workspace--current)
                       :height (frame-pixel-height exwm-workspace--current)))
    (xcb:+request exwm--connection
        (make-instance 'xcb:ewmh:set-_NET_WM_STATE
                       :window exwm--id
                       :data (vector xcb:Atom:_NET_WM_STATE_FULLSCREEN)))
    (xcb:flush exwm--connection)
    (setq exwm--fullscreen t)
    (exwm-input-release-keyboard)))

(defun exwm-layout-unset-fullscreen (&optional id)
  "Restore window from fullscreen state."
  (interactive)
  (with-current-buffer (if id (exwm--id->buffer id) (window-buffer))
    (unless exwm--fullscreen
      (user-error "Not in full-screen mode."))
    ;; Restore the floating frame if the client is floating
    (when exwm--floating-frame
      (xcb:+request exwm--connection
          (make-instance 'xcb:ConfigureWindow
                         :window (frame-parameter exwm--floating-frame
                                                  'exwm-outer-id)
                         :value-mask (logior xcb:ConfigWindow:X
                                             xcb:ConfigWindow:Y
                                             xcb:ConfigWindow:Width
                                             xcb:ConfigWindow:Height)
                         :x (elt exwm--floating-frame-geometry 0)
                         :y (elt exwm--floating-frame-geometry 1)
                         :width (elt exwm--floating-frame-geometry 2)
                         :height (elt exwm--floating-frame-geometry 3))))
    (cl-assert exwm--fullscreen)
    (setq exwm--fullscreen nil)
    (exwm-layout--show exwm--id nil)
    (xcb:+request exwm--connection
        (make-instance 'xcb:ewmh:set-_NET_WM_STATE :window exwm--id :data []))
    (xcb:flush exwm--connection)
    (exwm-input-grab-keyboard)))

(defun exwm-layout--fullscreen-geometry (frame)
  "Return the monitor geometry for frame FRAME."
  (or (frame-parameter frame 'exwm-geometry)
      (xcb:+request-unchecked+reply exwm--connection
          (make-instance 'xcb:GetGeometry
                         :drawable exwm--root))
      (make-instance 'xcb:RECTANGLE :x 0 :y 0
                     :width (x-display-width) :height (x-display-height))))

;; This function is superficially similar to `exwm-layout-set-fullscreen', but
;; they do very different things: `exwm-layout--set-frame-fullscreen' resizes a
;; frame to the actual monitor size, `exwm-layout-set-fullscreen' resizes an X
;; window to the frame size.
(defun exwm-layout--set-frame-fullscreen (frame &optional keep-x keep-y)
  "Make frame FRAME fullscreen, with regard to its XRandR output if applicable.\

With KEEP-X non-nil, keep x position and width; with KEEP-Y
non-nil, keep y position and height."
  (let* ((screen-geometry (exwm-layout--fullscreen-geometry frame))
         (id (frame-parameter frame 'exwm-outer-id))
         (window-geometry
          (xcb:+request-unchecked+reply exwm--connection
              (make-instance 'xcb:GetGeometry
                             :drawable id)))
         (x (slot-value screen-geometry 'x))
         (y (slot-value screen-geometry 'y))
         (width (slot-value screen-geometry 'width))
         (height (slot-value screen-geometry 'height)))
    (when keep-x
      (setq x (slot-value window-geometry 'x))
      (setq width (slot-value window-geometry 'width)))
    (when keep-y
      (setq y (slot-value window-geometry 'y))
      (setq height (slot-value window-geometry 'height)))
    (xcb:+request exwm--connection
        (make-instance 'xcb:ConfigureWindow
                       :window id
                       :value-mask (logior xcb:ConfigWindow:X
                                           xcb:ConfigWindow:Y
                                           xcb:ConfigWindow:Width
                                           xcb:ConfigWindow:Height)
                       :x x :y y
                       :width width
                       :height height))
    (xcb:+request exwm--connection
        (make-instance 'xcb:SendEvent
                       :propagate 0 :destination id
                       :event-mask xcb:EventMask:StructureNotify
                       :event (xcb:marshal
                               (make-instance 'xcb:ConfigureNotify
                                              :event id :window id
                                              :above-sibling xcb:Window:None
                                              :x x :y y
                                              :width width :height height
                                              :border-width 0
                                              :override-redirect 0)
                               exwm--connection)))
    (xcb:flush exwm--connection)))

(defun exwm-layout--placeholder-window ()
  (let ((placeholder (get-buffer "*scratch*")))
    (unless placeholder
      (setq placeholder (get-buffer-create "*scratch*"))
      (set-buffer-major-mode placeholder))
    placeholder))

(defun exwm-layout--refresh-window (window)
  (cond
   ((not (eq major-mode 'exwm-mode))
    nil)
   ;; Floating frames
   (exwm--floating-frame
    (exwm--log "Refresh floating window #x%x" exwm--id)
    (exwm-layout--show exwm--id window))
   ((and exwm--current-frame
	 (frame-visible-p exwm--current-frame)
	 (eq window (get-buffer-window (current-buffer) 'visible)))
    (exwm--log "Refreshing in-frame window %S" window)
    (exwm-layout--show exwm--id window))
   (t
    (exwm--log "Not refreshing anything for window %S" window)
    (set-window-buffer window (exwm-layout--placeholder-window)))))

(defun exwm-layout--pre-redisplay (window)
  (exwm-layout--refresh-window window))

(defun exwm-layout--init ()
  "Initialize layout module."
  (push 'exwm-layout--pre-redisplay pre-redisplay-functions))



(provide 'exwm-layout)

;;; exwm-layout.el ends here
