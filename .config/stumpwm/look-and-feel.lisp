(in-package :stumpwm)

;; include local fonts
(pushnew (concat (getenv "HOME")
            "/.local/share/fonts/")
    xft:*font-dirs* :test #'string= )
(pushnew "/run/current-system/profile/share/fonts/"
    xft:*font-dirs* :test #'string= )

;; (setf clx-truetype:+font-cache-filename+ (concat (getenv "HOME") "/.fonts/font-cache.sexp"))
(xft:cache-fonts) ;; NOTE: needs to be executed once, better to map it to one keybinding
(set-font `(,(make-instance 'xft:font :family "Iosevka" :subfamily "Bold" :size 13 :antialias t)))

;; setting colors
(setf *colors*
      '("#24273a"  ; bg
        "#cad3f5"  ; fg
        "#a6da95"  ; green
        "#7dc4e4"  ; cyan
        "#8aadf4"  ; blue
        "#ed8796"  ; red
        "#c6a0f6"  ; magenta
        "#eed49f"  ; yellow
        ))

(update-color-map (current-screen))
(set-bg-color (elt *colors* 0))
(set-fg-color (elt *colors* 1))
(set-border-color (elt *colors* 6))
(set-focus-color (elt *colors* 6))
(set-unfocus-color (elt *colors* 0))
(set-float-focus-color (elt *colors* 6))
(set-float-unfocus-color (elt *colors* 0))
(set-msg-border-width 3)

(setf *key-seq-color* "^4")
(setf *which-key-format* (concat *key-seq-color* "*~4a^n ~a"))

(setf *message-window-gravity* :top
      *message-window-input-gravity* :top
      *message-window-padding* 15
      *message-window-y-padding* 15
      *input-window-gravity*     :top
      *window-border-style* :thick
      *maxsize-border-width* 3
      *normal-border-width* 3
      *transient-border-width* 3
      *float-window-border* 2
      *float-window-title-height* 2)

;; configuration to create gaps between frames (if the swm-gaps module exist)
(when (find-module "swm-gaps")
   (progn
       (refresh-heads)
       (setf swm-gaps:*head-gaps-size* 0
             swm-gaps:*inner-gaps-size* 3
             swm-gaps:*outer-gaps-size* 3)
       (swm-gaps:toggle-gaps-on)))

;; configuration to create gaps between top-bar and frames (if the swm-external-panel module exist)
(when (find-module "swm-external-panel")
    (progn
        (setf swm-external-panel:*heads* (list (find-head-by-position (current-screen)
                                          (/ (screen-width (current-screen)) 2)
                                          (/ (screen-height (current-screen)) 2)))
              swm-external-panel:*heads-gaps* (list (list 0 0 35 0)))
        (swm-external-panel:toggle-gaps-on)))
