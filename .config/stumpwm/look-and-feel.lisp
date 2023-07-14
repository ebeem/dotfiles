(in-package :stumpwm)

;; setting font, needs a stumpwm module and quicklisp package
(ql:quickload :clx-truetype)
(load-module "ttf-fonts")

;; include local fonts
(pushnew (concat (getenv "HOME")
                "/.local/share/fonts/")
        xft:*font-dirs* :test #'string=)
(xft:cache-fonts) ;; NOTE: needs to be executed once, better to map it to one keybinding
(set-font `(,(make-instance 'xft:font :family "JetBrainsMono Nerd Font" :subfamily "Bold" :size 11 :antialias t)))

;; setting colors
(setf *colors*
      '("#1E2127"  ; bg
        "#FFFFFF"  ; fg
        "#98C379"  ; green
        "#46D9FF"  ; cyan
        "#61AFEF"  ; blue
        "#E06C75"  ; red
        "#c678dd"  ; magenta
        "#ffffff"  ; yellow
        ))

(update-color-map (current-screen))
(set-bg-color (elt *colors* 0))
(set-fg-color (elt *colors* 1))
(set-border-color (elt *colors* 4))
(set-focus-color (elt *colors* 4))
(set-unfocus-color (elt *colors* 0))
(set-float-focus-color (elt *colors* 4))
(set-float-unfocus-color (elt *colors* 0))
(set-msg-border-width 3)

(setf *key-seq-color* "^4")
(setf *which-key-format* (concat *key-seq-color* "*~4a^n ~a"))

;; module to create gaps between frames
(load-module :swm-gaps)
(refresh-heads)
(setf swm-gaps:*head-gaps-size* 0
      swm-gaps:*inner-gaps-size* 5
      swm-gaps:*outer-gaps-size* 5)
(swm-gaps:toggle-gaps-on)

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

(set-module-dir "~/workspace/git/stumpwm-contrib/")
(load-module "swm-external-panel")
(setf swm-external-panel:*heads* (list (find-head-by-position (current-screen)
                                  (/ (screen-width (current-screen)) 2)
                                  (/ (screen-height (current-screen)) 2)))
      swm-external-panel:*heads-gaps* (list (list 0 0 30 0)))
(swm-external-panel:toggle-gaps-on)
