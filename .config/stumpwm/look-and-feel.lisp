(in-package :stumpwm)

;; setting font, needs a stumpwm module and quicklisp package
(ql:quickload :clx-truetype)
(load-module "ttf-fonts")

;; include local fonts
(pushnew (concat (getenv "HOME")
                 "/.local/share/fonts/")
         xft:*font-dirs* :test #'string=)
;; (xft:cache-fonts) ;; NOTE: needs to be executed once, better to map it to one keybinding
(set-font `(,(make-instance 'xft:font :family "JetBrains Mono" :subfamily "Regular" :size 12 :antialias t)))

;; setting colors
(setf *colors*
      '("#282c34"  ; fg
        "#da8548"  ; orange
        "#98be65"  ; green
        "#46D9FF"  ; cyan
        "#51afef"  ; blue
        "#ff6c6b"  ; red
        "#c678dd"  ; magenta
        "#ECBE7B"  ; yellow
        ))

(update-color-map (current-screen))
(set-focus-color (elt *colors* 4))
(set-unfocus-color (elt *colors* 0))

;; module to create gaps between frames
(load-module "swm-gaps")
(setf swm-gaps:*head-gaps-size* 0
      swm-gaps:*inner-gaps-size* 5
      swm-gaps:*outer-gaps-size* 10)
(swm-gaps:toggle-gaps-on)
;; (setq mode-line-default-head (stumpwm::find-head-by-position (current-screen)
;;                                                 (/ (screen-width (current-screen)) 2)
;;                                                 (/ (screen-height (current-screen)) 2)))

(setf *message-window-gravity* :center
      *window-border-style* :thick
      *message-window-padding* 3
      *maxsize-border-width* 3
      *normal-border-width* 3
      *transient-border-width* 3
      stumpwm::*float-window-border* 2
      stumpwm::*float-window-title-height* 2)

;; workaround to fix polybar extra spacing and fullscreen
;; TODO: make it into a module
;; refreshing heads to get correct inital values after restart
(refresh-heads)
(let* ((head (stumpwm::find-head-by-position (current-screen)
                                            (/ (screen-width (current-screen)) 2)
                                            (/ (screen-height (current-screen)) 2))))
  (setf header-gap 30
      original-head-y (stumpwm::head-y head)
      original-head-height (stumpwm::head-height head)))

(defun eb/refresh-external-panel ()
  ;; add gap to middle screen (polybar)
  (dformat 0 "fixing external panel")
  (let* ((head (stumpwm::find-head-by-position (current-screen)
                                              (/ (screen-width (current-screen)) 2)
                                              (/ (screen-height (current-screen)) 2)))
          (height original-head-height)
          (width (stumpwm::head-width head))
          (y original-head-y)
          (x (stumpwm::head-x head))
          (fullscreen (find 't (mapcar 'stumpwm::window-fullscreen
                                        (stumpwm::head-windows (stumpwm::current-group) head))))
          (gap (cond ((null fullscreen) header-gap) (t 0)))
          (new-height (- height gap))
          (new-width width))
    (when (= gap 0)
      ;; TODO: run xlib command, remove dependencies on xdo
      (run-shell-command "xdo lower -N 'Polybar'"))
    (stumpwm::resize-head
      (stumpwm::head-number head)
      x (+ y gap)
      new-width new-height)))

;; calling the refresh-external-panel function to the gap on initialization
(eb/refresh-external-panel)

;; TODO: create hooks rather than override the stumpwm functions
;; (defun stumpwm::activate-fullscreen (window)
;;   (dformat 2 "client requests to go fullscreen~%")
;;   (add-wm-state (window-xwin window) :_NET_WM_STATE_FULLSCREEN)
;;   (setf (window-fullscreen window) t)
;;   (focus-window window)
;;   (eb/fix-middle-screen-bar-gap))

;; (defun stumpwm::deactivate-fullscreen (window)
;;   (setf (window-fullscreen window) nil)
;;   (dformat 2 "client requests to leave fullscreen~%")
;;   (stumpwm::remove-wm-state (window-xwin window) :_NET_WM_STATE_FULLSCREEN)
;;   (stumpwm::update-decoration window)
;;   (stumpwm::update-mode-lines (current-screen))
;;   (eb/fix-middle-screen-bar-gap))

(defun hook-focus-change (to-window from-window)
  (eb/refresh-external-panel))

(stumpwm:add-hook stumpwm::*focus-window-hook* 'hook-focus-change)

;; NOTE: better to rely on stumpwm rather than xlib
(define-stump-event-handler :property-notify (window atom state)
  ;; fullscreen listener/hook
  (case atom
    (:_NET_WM_STATE
     (eb/refresh-external-panel))))
