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

(defun exwm-layout--show (id window)
  "Show window ID exactly fit in the Emacs window WINDOW."
  (let* ((buffer (exwm--id->buffer id))
         (frame (and window (window-frame window))))
    (cl-assert buffer)
    (with-current-buffer buffer
      (unless (eq frame exwm--current-frame)
        (exwm--log "Show #x%x in %s" id window)
        (when window
          (xcb:+request exwm--connection (make-instance 'xcb:MapWindow :window id))
          (xcb:+request exwm--connection
              (make-instance 'xcb:icccm:set-WM_STATE
                             :window id :state xcb:icccm:WM_STATE:NormalState
                             :icon xcb:Window:None))
          (let* ((edges-rel-frame (or (and buffer
                                           (with-current-buffer buffer exwm--floating-edges))
                                      (window-inside-pixel-edges window)))
                 (x-offset (frame-parameter frame 'left))
                 (y-offset (frame-parameter frame 'top))
                 (x-rel-frame (elt edges-rel-frame 0))
                 (y-rel-frame (elt edges-rel-frame 1))
                 (x-absolute (+ x-rel-frame x-offset))
                 (y-absolute (+ y-rel-frame y-offset))
                 (width (- (elt edges-rel-frame 2) (elt edges-rel-frame 0)))
                 (height (- (elt edges-rel-frame 3) (elt edges-rel-frame 1))))
            (when (and buffer (with-current-buffer buffer exwm--fullscreen))
              (setq x-rel-frame 0
                    y-rel-frame 0
                    width (frame-pixel-width frame)
                    height (frame-pixel-height frame)))
            (exwm--log "Show #x%x in %s at %s" id window edges-rel-frame)
            (when buffer
              (with-current-buffer buffer
                (xcb:+request exwm--connection
                    (make-instance 'xcb:MapWindow
                                   :window (frame-parameter frame 'exwm-inner-id)))
                (xcb:flush exwm--connection)
                (message "reparent %S to %S %S" id (frame-parameter frame 'exwm-inner-id)
                         (xcb:+request-checked+request-check exwm--connection
                    (make-instance 'xcb:ReparentWindow
                                   :window id
                                   :parent (frame-parameter frame 'exwm-inner-id)
                                   :x 0 :y 0)))
                (xcb:flush exwm--connection)
                (xcb:+request exwm--connection
                    (make-instance 'xcb:ConfigureWindow
                                   :window (frame-parameter frame 'exwm-inner-id)
                                   :value-mask (logior xcb:ConfigWindow:X
                                                       xcb:ConfigWindow:Y
                                                       xcb:ConfigWindow:Width
                                                       xcb:ConfigWindow:Height
                                                       xcb:ConfigWindow:StackMode)
                                   :x x-rel-frame :y y-rel-frame :width width :height height
                                   ;; In order to put non-floating window at bottom
                                   :stack-mode xcb:StackMode:Above))
                (xcb:+request exwm--connection
                    (make-instance 'xcb:MapWindow
                                   :window (frame-parameter frame 'exwm-inner-id)))))
            (xcb:+request exwm--connection
                (make-instance 'xcb:ConfigureWindow
                               :window id
                               :value-mask (logior xcb:ConfigWindow:X
                                                   xcb:ConfigWindow:Y
                                                   xcb:ConfigWindow:Width
                                                   xcb:ConfigWindow:Height
                                                   xcb:ConfigWindow:StackMode)
                               :x 0 :y 0 :width width :height height
                               ;; In order to put non-floating window at bottom
                               :stack-mode xcb:StackMode:Above))
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
        (when exwm--current-frame
          (exwm--log "Hide #x%x" id)
          (xcb:+request exwm--connection (make-instance 'xcb:UnmapWindow :window (frame-parameter exwm--current-frame 'exwm-inner-id)))
          (when (not window)
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
                               :icon xcb:Window:None))))
        (setq exwm--current-frame frame))))
  (xcb:flush exwm--connection))

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
  (let* ((screen-geometry (exwm-layout--fullscreen-geometry))
         (window-geometry
          (xcb:+request-unchecked+reply exwm--connection
              (make-instance 'xcb:GetGeometry
                             :drawable (frame-parameter frame 'outer-id))))
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

(defun exwm-layout--refresh (&optional frame)
  "Refresh layout."
  (unless frame (setq frame (selected-frame)))
  (let ((placeholder (get-buffer "*scratch*"))
        windows)
    (if (not (exwm-workspace-index frame))
        (if (frame-parameter frame 'exwm-window-id)
            ;; Refresh a floating frame
            (progn
              (when (eq major-mode 'exwm-mode)
                (let ((window (frame-first-window frame)))
                  (with-current-buffer (window-buffer window)
                    (exwm--log "Refresh floating window #x%x" exwm--id)
                    (exwm-layout--show exwm--id window)))))
          ;; Other frames (e.g. terminal/graphical frame of emacsclient)
          ;; We shall bury all `exwm-mode' buffers in this case
          (unless placeholder ;create the *scratch* buffer if it's killed
            (setq placeholder (get-buffer-create "*scratch*"))
            (set-buffer-major-mode placeholder))
          (setq windows (window-list frame 0)) ;exclude minibuffer
          (dolist (window windows)
            (with-current-buffer (window-buffer window)
              (when (eq major-mode 'exwm-mode)
                (set-window-buffer window placeholder)))))
      ;; Refresh the whole workspace
      ;; Workspaces other than the active one can also be refreshed (RandR)
      ;;(exwm--log "Refresh workspace %s" frame)
      (unless placeholder  ;create the *scratch* buffer if it's killed
        (setq placeholder (get-buffer-create "*scratch*"))
        (set-buffer-major-mode placeholder))
      (dolist (pair exwm--id-buffer-alist)
        (when (cdr pair)
          (with-current-buffer (cdr pair)
            ;; Exclude windows on invisible workspaces and floating frames
            (when (and exwm--frame
                       (frame-visible-p exwm--frame)
                       (not exwm--floating-frame))
              (setq windows (get-buffer-window-list (current-buffer) 0 'visible))
              (exwm-layout--show exwm--id (car windows))
              (dolist (i (cdr windows))
                (set-window-buffer i placeholder))))))
      ;; Make sure windows floating / on other workspaces are excluded
      (dolist (window (window-list frame 0))
        (with-current-buffer (window-buffer window)
          (when (and (eq major-mode 'exwm-mode)
                     (or exwm--floating-frame
                         (not exwm--frame)
                         (not (frame-visible-p exwm--frame))))
            (set-window-buffer window placeholder)))))))

(defun exwm-layout--on-minibuffer-setup ()
  "Refresh layout when minibuffer grows."
  (run-with-idle-timer 0.01 nil         ;FIXME
                       (lambda ()
                         (unless (and (eq major-mode 'exwm-mode)
                                         exwm--floating-frame))
                           (exwm-layout--refresh))))

(defun exwm-layout--on-window-size-change (frame)
  (run-with-idle-timer 1.0 nil
                       `(lambda ()
                          (dolist (pair exwm--id-buffer-alist)
                            (when (cdr pair)
                              (with-current-buffer (cdr pair)
                                (when (eq exwm--current-frame ,frame)
                                  (setq exwm--current-frame nil)))))
                          (exwm-layout--refresh)
                          (exwm-layout--refresh ,frame)
                          (dolist (f exwm-workspace--list)
                            (exwm-layout--refresh f))
                          (run-with-idle-timer .01 nil
                                               (lambda ()
                                                 (dolist (f exwm-workspace--list)
                                                   (exwm-layout--refresh f))
                                                  (exwm-layout--refresh)
                                                  (exwm-layout--refresh ,frame))))))

(defun exwm-layout--on-focus-out ()
  (exwm-layout--refresh)
  (dolist (f exwm-workspace--list)
    (exwm-layout--refresh f))
  (run-with-idle-timer 1.0 nil
                       `(lambda ()
                          (dolist (f exwm-workspace--list)
                            (exwm-layout--refresh f))
                          (exwm-layout--refresh))))

(defun exwm-layout--on-window-text-change ()
  (exwm-layout--refresh))

(defun exwm-layout--on-echo-area-clear ()
  (exwm-layout--refresh)
  (dolist (f exwm-workspace--list)
    (exwm-layout--refresh f))
  (run-with-idle-timer .01 nil
                       `(lambda ()
                          (dolist (f exwm-workspace--list)
                            (exwm-layout--refresh f))
                          (exwm-layout--refresh))))

(defun exwm-layout--init ()
  "Initialize layout module."
  ;; Auto refresh layout
  (add-hook 'window-configuration-change-hook 'exwm-layout--refresh)
  ;; Refresh when minibuffer grows
  (add-hook 'minibuffer-setup-hook 'exwm-layout--on-minibuffer-setup t)
  ;; Refresh upon window size change
  (add-to-list 'window-size-change-functions 'exwm-layout--on-window-size-change)
  ;;(add-to-list 'window-text-change-functions 'exwm-layout--on-window-text-change)
  (add-hook 'echo-area-clear-hook 'exwm-layout--on-echo-area-clear)
  (add-hook 'focus-out-hook 'exwm-layout--on-focus-out)
  (add-hook 'focus-in-hook 'exwm-layout--on-focus-out)
  )



(provide 'exwm-layout)

;;; exwm-layout.el ends here
