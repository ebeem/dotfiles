;;;; swm-gaps.lisp
(in-package #:swm-external-panel)

(export '(*heads*
          *heads-gaps*
          *gaps-on*
          *lower-panels-on-fullscreen-enter*
          *raise-panels-on-fullscreen-exit*
          toggle-gaps
          toggle-gaps-on
          toggle-gaps-off))

(defvar *heads* (list (stumpwm::current-screen))
  "Set this variable to a list of screens that will contain unmanaged external panels.")

(defvar *heads-gaps* (list (list 0 0 30 0))
  "Set this variable to a list of the gaps to be added for each defined head.
Each gap is a list of 4 values representing gaps/margin from left, right, top, bottom respectively.
This variable must have the exact length of the *heads* variable, which means a set of gaps for each head")

(defvar *gaps-on* nil
  "This variable defines the state of the external panel. t if enabled, nil if disabled
This variable should be managed by swm-external-panel and used externally only to know the
current status of the gaps")

(defvar *lower-panels-on-fullscreen-enter* t
  "Set this variable to t if the panels should be lowered on fullscreen mode,
nil to not lower the panels on fullscreen mode. Setting this to nil will most likely
cause the panel to appear on top of the fullscreen window which is usually not a desired behavior")

(defvar *raise-panels-on-fullscreen-exit* t
  "Set this variable to t if the panels should be raised on leaving fullscreen mode,
nil to not raise the panels on leaving fullscreen mode. Setting this to nil will most likely
cause the panel to appear behind other windows in case of an overlap which is usually not a desired behavior")

(defvar *heads-dimensions* nil
  "This variable stores the initial dimensions of the heads provided.
Keeping the original dimensions is important to resize the heads and add
or remove the gaps based on weather a window is occupying the fullscreen or not.
This variable is automatically assigned by swm-external-panel on initialization and
should be set and managed by swm-external-panel.")

(defun refresh-external-panels ()
  ;; add gap to every configured screen
  (dotimes (i (length *heads*))
    (when (not (null (find (stumpwm::head-number (nth i *heads*)) (screen-heads (current-screen)) :key 'stumpwm::head-number)))
      (let* ((head (nth i *heads*))
              (dimensions (nth i *heads-dimensions*))
              (gaps (nth i *heads-gaps*))
              (gap-left (nth 0 gaps))
              (gap-right (nth 1 gaps))
              (gap-top (nth 2 gaps))
              (gap-bottom (nth 3 gaps))
              (x (nth 0 dimensions))
              (y (nth 1 dimensions))
              (width (nth 2 dimensions))
              (height (nth 3 dimensions))
              (fullscreen (find 't (mapcar 'stumpwm::window-fullscreen
                                            (stumpwm::head-windows (stumpwm::current-group) head)))))
        (cond ((null fullscreen)
          (stumpwm::resize-head
            (stumpwm::head-number head)
              (+ x gap-left) (+ y gap-top)
              (- width (+ gap-left gap-right)) (- height (+ gap-top gap-bottom)))
          (when *raise-panels-on-fullscreen-exit*
            (run-shell-command "xdo raise -N 'Polybar'")))
          (t (stumpwm::resize-head
              (stumpwm::head-number head)
                x y
                width height)
            (when *lower-panels-on-fullscreen-enter*
              (run-shell-command "xdo lower -N 'Polybar'"))))))))

(defun hook-focus-change (to-window from-window)
  (refresh-external-panels))

(defun hook-fullscreen-change (window)
  (refresh-external-panels))

(defcommand toggle-gaps () ()
  "Toggle gaps"
  (if (null *gaps-on*)
      (toggle-gaps-on)
      (toggle-gaps-off)))

(defcommand toggle-gaps-on () ()
  "Turn gaps on"
  (setf *gaps-on* t)

  ;; reset heads and the dimensions variable
  (stumpwm:refresh-heads)
  (setf *heads-dimensions* (list))
  (when (= (length *heads*) (length *heads-gaps*))
    ;; store initial heads dimensions so we can revert back to it if needed (fullscreen on)
    (dolist (head *heads*)
      (setf *heads-dimensions* (append *heads-dimensions* (list (list (stumpwm::head-x head)
                                                            (stumpwm::head-y head)
                                                            (stumpwm::head-width head)
                                                            (stumpwm::head-height head))))))

    ;; a call to refresh-external-panels is needed to fix initial placement/gaps of the windows
    (refresh-external-panels)
    (stumpwm:add-hook stumpwm::*focus-window-hook* 'hook-focus-change)
    ;; a call to refresh-external-panels should be called on window-focus and fullscreen events
    ))

(defcommand toggle-gaps-off () ()
  "Turn gaps off"
  (stumpwm:remove-hook stumpwm::*focus-window-hook* 'hook-focus-change)
  (setf *gaps-on* nil)
  (stumpwm:refresh-heads))

;; FIXME: better to rely on stumpwm hooks rather than xlib if possible
(stumpwm::define-stump-event-handler :property-notify (window atom state)
  ;; fullscreen listener/hook
  (case atom
    (:_NET_WM_STATE
     (when *gaps-on*
       (hook-fullscreen-change window)))))
