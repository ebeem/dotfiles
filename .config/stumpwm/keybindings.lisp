(in-package :stumpwm)
(set-prefix-key (kbd "s-SPC"))

;; define top-map keybindings
;; media-keys
(define-key *top-map* (kbd "XF86AudioLowerVolume") "exec pactl set-sink-volume @DEFAULT_SINK@ -5%")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "exec pactl set-sink-volume @DEFAULT_SINK@ +5%")
(define-key *top-map* (kbd "XF86AudioMute") "exec pactl set-sink-mute @DEFAULT_SINK@ toggle")
(define-key *top-map* (kbd "XF86AudioNext") "exec mpc next")
(define-key *top-map* (kbd "XF86AudioPrev") "exec mpc prev")
(define-key *top-map* (kbd "XF86AudioPlay") "exec mpc toggle")

;; function-keys
(define-key *top-map* (kbd "XF86MonBrightnessUp") "brightnessctl set +10%")
(define-key *top-map* (kbd "XF86MonBrightnessDown") "brightnessctl set 10%-")

;; window and group management
(define-key *top-map* (kbd "s-f") "float-this")
(define-key *top-map* (kbd "s-F") "unfloat-this")

(define-key *top-map* (kbd "s-h") "improved-move-focus left")
(define-key *top-map* (kbd "s-j") "improved-move-focus down")
(define-key *top-map* (kbd "s-k") "improved-move-focus up")
(define-key *top-map* (kbd "s-l") "improved-move-focus right")

(define-key *top-map* (kbd "s-H") "move-window left")
(define-key *top-map* (kbd "s-J") "move-window down")
(define-key *top-map* (kbd "s-K") "move-window up")
(define-key *top-map* (kbd "s-L") "move-window right")

(define-key *top-map* (kbd "M-s-h") "exchange-direction left")
(define-key *top-map* (kbd "M-s-j") "exchange-direction down")
(define-key *top-map* (kbd "M-s-k") "exchange-direction up")
(define-key *top-map* (kbd "M-s-l") "exchange-direction right")

(define-key *top-map* (kbd "C-s-h") "gselect-direction left")
(define-key *top-map* (kbd "C-s-j") "gselect-direction down")
(define-key *top-map* (kbd "C-s-k") "gselect-direction up")
(define-key *top-map* (kbd "C-s-l") "gselect-direction right")

;; TODO: make it move window to current head
(define-key *top-map* (kbd "C-M-s-h") "gmove-direction left")
(define-key *top-map* (kbd "C-M-s-j") "gmove-direction down")
(define-key *top-map* (kbd "C-M-s-k") "gmove-direction up")
(define-key *top-map* (kbd "C-M-s-l") "gmove-direction right")

;; Tab like cycling
(define-key *top-map* (kbd "s-.") "next-in-frame")
(define-key *top-map* (kbd "s-,") "prev-in-frame")

(define-key *top-map* (kbd "s-w") "delete-window")


;; shortcuts
(define-key *top-map* (kbd "s-RET") "exec emacsclient -c -e '(+eshell/here nil)'")
(define-key *top-map* (kbd "C-s-SPC") "exec rofi -show drun")
(define-key *top-map* (kbd "M-s-SPC") "exec ~/.bin/switch-keyboard-layout")
(define-key *top-map* (kbd "C-s-r") "loadrc")
(define-key *top-map* (kbd "C-M-s-r") "restart-hard")

;; rofi commands
(defvar *menu-rofi-keymap*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "p") "exec ~/.config/rofi/bin/password-manager")
    ;; TODO: mount & unmount scripts
    ;; (define-key m (kbd "m") "exec rofi-mount")
    ;; (define-key m (kbd "u") "exec rofi-umount")
    ;; (define-key m (kbd "w") "exec .config/rofi/bin/wifi")
    (define-key m (kbd "b") "exec ~/.config/rofi/bin/bluetooth")
    (define-key m (kbd "f") "exec ~/.config/rofi/bin/finder")
    (define-key m (kbd "k") "exec ~/.config/rofi/bin/keyboard-layout")
    (define-key m (kbd "P") "exec ~/.config/rofi/bin/powermenu")
    (define-key m (kbd "s") "exec ~/.config/rofi/bin/sound-input")
    (define-key m (kbd "S") "exec ~/.config/rofi/bin/sound-output")
    m))

;; screenshot commands
(defvar *menu-screenshot-keymap*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "d") "exec flameshot gui -d 3000")
    (define-key m (kbd "s") "exec flameshot full")
    (define-key m (kbd "S") "exec flameshot gui")
    m))

;; applications
(defvar *menu-applications-keymap*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "b") "firefox")
    (define-key m (kbd "c") "exec chromium")
    (define-key m (kbd "d") "exec discord")
    (define-key m (kbd "e") "exec emacsclient -c")
    (define-key m (kbd "g") "exec gimp")
    (define-key m (kbd "f") "exec thunar")
    (define-key m (kbd "s") "exec slack")
    (define-key m (kbd "r") '*menu-rofi-keymap*)
    (define-key m (kbd "s") '*menu-screenshot-keymap*)
    (define-key m (kbd "w") "exec select-pape")
    m))

;; session-management
(defvar *session-keymap*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "q") "quit")
    (define-key m (kbd "l") "logout")
    (define-key m (kbd "s") "suspend-computer")
    (define-key m (kbd "S") "shutdown-computer")
    (define-key m (kbd "r") "loadrc")
    (define-key m (kbd "R") "restart-hard")
    (define-key m (kbd "C-r") "restart-computer")
    m))


(defcommand hsplit-and-focus () ()
  "create a new frame on the right and focus it."
  (hsplit)
  (move-focus :right))

(defcommand vsplit-and-focus () ()
  "create a new frame below and focus it."
  (vsplit)
  (move-focus :down))
-
;; window-management
(defvar *menu-windows-keymap*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "v") "hsplit-and-focus")
    (define-key m (kbd "h") "vsplit-and-focus")
    (define-key m (kbd "d") "remove-split")
    (define-key m (kbd "m") "only")
    m))

(define-key *root-map* (kbd "q") '*session-keymap*)
(define-key *root-map* (kbd "a") '*menu-applications-keymap*)
(define-key *root-map* (kbd "o") "exec rofi -show drun")
(define-key *root-map* (kbd "r") '*menu-rofi-keymap*)
(define-key *root-map* (kbd "s") '*menu-screenshot-keymap*)
(define-key *root-map* (kbd "w") '*menu-windows-keymap*)

(load-module "globalwindows")
(define-key *groups-map* (kbd "b") "global-pull-windowlist")
(define-key *groups-map* (kbd "g") "toggle-gaps")


; (al/define-key *root-map* "F"   "fselect")
;; (al/define-key *root-map* "="   "balance-frames")
;; (al/define-key *root-map* "s-c" "delete-window")
;; (al/define-key *root-map* "C"   "kill-window")
;; (al/define-key *root-map* "o"   "al/fother")
;; (al/define-key *root-map* "M-o" "fnext")

;; (al/define-key *top-map* "s-r"   "al/toggle-root")
;; (al/define-key *top-map* "s-w"   "windowlist")
;; (al/define-key *top-map* "M-s-w" "vgroups")
;; (al/define-key *top-map* "M-s-g" "grouplist")
;; (al/define-key *top-map* "H-o"   "al/other H-o")
;; (al/define-key *top-map* "H-M-o" "al/next H-M-o")
;; (al/define-key *top-map* "s-H-o" "al/toggle-ignore-emacs")
;; (al/define-key *top-map* "s-n"   "gother")
;; (al/define-key *top-map* "M-s-n" "al/gmove-to-other-group")
;; (al/define-key *top-map* "s-;"   "al/mpv")

;; (al/define-key *tile-group-root-map* "s-f" "fullscreen")
;; (al/define-key *tile-group-top-map* "s-z" "hsplit")
;; (al/define-key *tile-group-top-map* "M-s-z" "vsplit")
;; (al/define-key *tile-group-top-map* "s-x" "only")

;; ;;; Focusing windows

;; (al/define-key *tile-group-top-map*  "s-b"            "pull-hidden-other")
;; (al/define-key *tile-group-top-map*  "M-s-b"          "pull-hidden-next")
;; (al/define-key *tile-group-top-map*  "s-TAB"          "pull-hidden-next")
;; (al/define-key *tile-group-top-map*  "s-ISO_Left_Tab" "pull-hidden-previous")
;; (al/define-key *float-group-top-map* "s-b"            "al/float-window-other")
;; (al/define-key *float-group-top-map* "M-s-b"          "al/float-window-next")
;; (al/define-key *float-group-top-map* "s-TAB"          "al/float-window-next")
;; (al/define-key *float-group-top-map* "s-ISO_Left_Tab" "al/float-window-previous")

;; ;;; Moving/resizing windows

;; ;; Use numpad keys for manipulating windows:
;; ;;   [C-]M-<key> for resizing
;; ;;   [C-]s-<key> for moving
;; ;;   C-<key> for moving to the screen edges

;; (al/define-key *float-group-top-map*
;;                 "s-KP_Begin" "al/float-window-gravity center")

;; (defun al/define-numpad-key-xy (map modifier cmd val)
;;   (flet ((dk (key x y)
;;            (define-key map (kbd (concat modifier key))
;;              (format nil "~a ~D ~D" cmd x y))))
;;     (dk "KP_Home"      (- val) (- val))
;;     (dk "KP_Up"        0       (- val))
;;     (dk "KP_Page_Up"   val     (- val))
;;     (dk "KP_Right"     val     0)
;;     (dk "KP_Page_Down" val     val)
;;     (dk "KP_Down"      0       val)
;;     (dk "KP_End"       (- val) val)
;;     (dk "KP_Left"      (- val) 0)))

;; (al/define-numpad-key-xy *float-group-top-map* "s-"   "al/move-float-window" 10)
;; (al/define-numpad-key-xy *float-group-top-map* "C-s-" "al/move-float-window" 1)
;; (al/define-numpad-key-xy *float-group-top-map* "M-"   "al/resize-float-window" 10)
;; (al/define-numpad-key-xy *float-group-top-map* "C-M-" "al/resize-float-window" 1)

;; (defun al/define-numpad-key-gravity (map modifier cmd)
;;   (flet ((dk (key gravity)
;;            (define-key map (kbd (concat modifier key))
;;              (format nil "~a ~a" cmd gravity))))
;;     (dk "KP_Begin"     "center")
;;     (dk "KP_Home"      "top-left")
;;     (dk "KP_Up"        "top")
;;     (dk "KP_Page_Up"   "top-right")
;;     (dk "KP_Right"     "right")
;;     (dk "KP_Page_Down" "bottom-right")
;;     (dk "KP_Down"      "bottom")
;;     (dk "KP_End"       "bottom-left")
;;     (dk "KP_Left"      "left")))

;; (al/define-numpad-key-gravity *tile-group-top-map*  "C-" "gravity")
;; (al/define-numpad-key-gravity *float-group-top-map* "C-" "al/float-window-gravity")

;; ;;; Resizing frames

;; (al/define-key *top-map* "M-s-XF86AudioRaiseVolume"   "resize   0  10")
;; (al/define-key *top-map* "M-s-XF86AudioLowerVolume"   "resize   0 -10")
;; (al/define-key *top-map* "C-M-s-XF86AudioRaiseVolume" "resize   0   1")
;; (al/define-key *top-map* "C-M-s-XF86AudioLowerVolume" "resize   0  -1")
;; (al/define-key *top-map* "s-XF86AudioRaiseVolume"     "resize  10   0")
;; (al/define-key *top-map* "s-XF86AudioLowerVolume"     "resize -10   0")
;; (al/define-key *top-map* "C-s-XF86AudioRaiseVolume"   "resize   1   0")
;; (al/define-key *top-map* "C-s-XF86AudioLowerVolume"   "resize  -1   0")

;; ;;; Controlling WiFi

;; (al/define-key *top-map* "XF86WLAN"   "exec rfkill unblock wlan")
;; (al/define-key *top-map* "s-XF86WLAN" "exec rfkill block wlan")
;; ;; Pressing XF86RFKill blocks/unblocks wlan automatically (on the kernel
;; ;; level), but this key is still available for binding.
;; (al/define-key *top-map* "XF86RFKill" "echo The state of ^[^B^5*wlan^] has been changed")

;; ;;; Controlling brightness

;; (al/define-key *top-map* "XF86MonBrightnessUp"      "al/set-backlight -inc 3")
;; (al/define-key *top-map* "XF86MonBrightnessDown"    "al/set-backlight -dec 3")
;; (al/define-key *top-map* "C-XF86MonBrightnessUp"    "al/set-backlight -inc 1")
;; (al/define-key *top-map* "C-XF86MonBrightnessDown"  "al/set-backlight -dec 1")
;; (al/define-key *top-map* "M-XF86MonBrightnessUp"    "al/set-backlight -inc 10")
;; (al/define-key *top-map* "M-XF86MonBrightnessDown"  "al/set-backlight -dec 10")

;; ;;; Controlling sound

;; (al/define-key *top-map* "XF86AudioMute"            "al/sound-set-current-scontrol toggle")
;; (al/define-key *top-map* "M-XF86AudioMute"          "al/sound-next-scontrol")
;; (al/define-key *top-map* "C-XF86AudioMute"          "exec tvtime-command TOGGLE_MUTE")
;; (al/define-key *top-map* "XF86AudioRaiseVolume"     "al/sound-set-current-scontrol 3%+")
;; (al/define-key *top-map* "XF86AudioLowerVolume"     "al/sound-set-current-scontrol 3%-")
;; (al/define-key *top-map* "C-XF86AudioRaiseVolume"   "al/sound-set-current-scontrol 1%+")
;; (al/define-key *top-map* "C-XF86AudioLowerVolume"   "al/sound-set-current-scontrol 1%-")
;; (al/define-key *top-map* "M-XF86AudioRaiseVolume"   "al/sound-set-current-scontrol 10%+")
;; (al/define-key *top-map* "M-XF86AudioLowerVolume"   "al/sound-set-current-scontrol 10%-")
;; (al/define-key *top-map* "C-M-XF86AudioRaiseVolume" "al/sound-set-current-scontrol 60%")
;; (al/define-key *top-map* "C-M-XF86AudioLowerVolume" "al/sound-set-current-scontrol 20%")

;; ;;; Controlling EMMS

;; (al/define-key *top-map* "C-XF86AudioPlay"   "al/emms-eval (emms-play-file \"~/docs/audio/grass.wav\")")
;; (al/define-key *top-map* "XF86AudioPlay"     "al/emms-eval (emms-pause)")
;; (al/define-key *top-map* "XF86AudioStop"     "al/emms-eval (emms-pause)")
;; (al/define-key *top-map* "H-XF86AudioStop"   "al/emms-eval (emms-stop)")
;; (al/define-key *top-map* "s-XF86AudioStop"   "al/emms-eval (emms-stop)")
;; (al/define-key *top-map* "H-XF86AudioPrev"   "al/emms-eval (emms-previous)")
;; (al/define-key *top-map* "s-XF86AudioPrev"   "al/emms-eval (emms-previous)")
;; (al/define-key *top-map* "H-XF86AudioNext"   "al/emms-eval (emms-next)")
;; (al/define-key *top-map* "s-XF86AudioNext"   "al/emms-eval (emms-next)")
;; (al/define-key *top-map* "H-XF86AudioPlay"   "al/emms-eval (al/emms-first)")
;; (al/define-key *top-map* "s-XF86AudioPlay"   "al/emms-eval (al/emms-first)")
;; (al/define-key *top-map* "XF86AudioPrev"     "al/emms-eval (al/emms-seek-backward 10)")
;; (al/define-key *top-map* "XF86AudioNext"     "al/emms-eval (al/emms-seek-forward  10)")
;; (al/define-key *top-map* "C-XF86AudioPrev"   "al/emms-eval (al/emms-seek-backward 3)")
;; (al/define-key *top-map* "C-XF86AudioNext"   "al/emms-eval (al/emms-seek-forward  3)")
;; (al/define-key *top-map* "M-XF86AudioPrev"   "al/emms-eval (al/emms-seek-backward 60)")
;; (al/define-key *top-map* "M-XF86AudioNext"   "al/emms-eval (al/emms-seek-forward  60)")
;; (al/define-key *top-map* "C-M-XF86AudioPrev" "al/emms-eval (al/emms-seek-backward 180)")
;; (al/define-key *top-map* "C-M-XF86AudioNext" "al/emms-eval (al/emms-seek-forward  180)")
;; (al/define-key *top-map* "XF86AudioMedia"    "al/emms-eval (emms-smart-browse)")
;; (al/define-key *top-map* "XF86Music"         "al/emms-eval (al/emms-notify)")

;; ;;; Miscellaneous bindings
;; (defvar al/emacs-map (make-sparse-keymap)
;;   "Keymap for finding files (and doing other things) in emacs.")

;; (al/define-key *top-map* "s-f" 'al/emacs-map)
;; (al/define-key al/emacs-map "1" "al/frames1")
;; (al/define-key al/emacs-map "b" "al/emacs-eval-show (find-file \"~/notes/bookmarks.org\")")
;; (al/define-key al/emacs-map "n" "al/emacs-eval-show (org-capture 1 \"n\")")
;; (al/define-key al/emacs-map "t" "al/emacs-eval-show (find-file \"~/notes/tasks.org\")")
;; (al/define-key al/emacs-map "i" "al/emacs-eval-show (find-file \"~/notes/info.org\")")
;; (al/define-key al/emacs-map "m" "al/emms-eval (al/emms-notify)")

;; (al/define-key *top-map* "s-g" "abort")
;; (al/define-key *top-map* "s-h" '*help-map*)
;; (al/define-key *top-map* "s-u" "time")
;; (al/define-key *top-map* "s-i" "info")
;; (al/define-key *top-map* "s-l" "lastmsg")
;; (al/define-key *top-map* "s-d" "al/send-key-to-emacs XF86Spell")
;; (al/define-key *top-map* "XF86Documents"    "al/emacs-eval-show (find-file \"~/notes/bookmarks.org\")")
;; (al/define-key *top-map* "C-XF86Documents"  "al/emacs-eval-show (org-capture 1 \"n\")")
;; (al/define-key *top-map* "M-XF86Documents"  "al/emacs-eval-show (find-file \"~/notes/tasks.org\")")
;; (al/define-key *top-map* "S-XF86Documents"  "al/emacs-eval-show (find-file \"~/notes/info.org\")")
;; (al/define-key *top-map* "XF86Calculator"   "al/emacs-eval-show (calc)")
;; (al/define-key *top-map* "C-XF86Calculator" "al/emacs-eval-show (calendar)")
;; (al/define-key *top-map* "H-ESC" "exec hide-osds")
;; (al/define-key *top-map* "s-7" "al/set-layout 0 s-7")
;; (al/define-key *top-map* "s-8" "al/set-layout 1 s-8")
;; (al/define-key *top-map* "s-9" "al/set-layout 2 s-9")
;; (al/define-key *top-map* "s-Kanji" "al/toggle-caps-lock")
;; (al/define-key *top-map* "H-y" "al/yank-primary")
;; (al/define-key *top-map* "F12"                 "exec capture desktop")
;; (al/define-key *top-map* "M-F12"               "exec capture image")
;; (al/define-key *top-map* "M-SunPrint_Screen"   "exec capture image")
;; (al/define-key *top-map* "C-S-F12"             "exec capture video")
;; (al/define-key *top-map* "C-S-SunPrint_Screen" "exec capture video")
;; (al/define-key *top-map* "s-F12"               "exec toggle-osd-clock")
;; (al/define-key *top-map* "s-SunPrint_Screen"   "exec toggle-osd-clock")
;; (al/define-key *top-map* "XF86TouchpadToggle"  "exec toggle-touchpad")
;; (al/define-key *top-map* "XF86Sleep"           "exec monitor blank")
;; (al/define-key *top-map* "C-XF86Sleep"         "exec monitor suspend")
;; (al/define-key *top-map* "M-XF86Sleep"         "exec monitor off")
;; (al/define-key *top-map* "C-M-H-XF86Sleep"     "exec shutdown now")
;; ;; Using "exec suspend" is not possible, because "suspend" is a shell
;; ;; builtin command, and "sh -c" (which is called by "exec") does not
;; ;; load my "~/.bashrc" where it is disabled.
;; (al/define-key *top-map* "S-s-XF86Sleep"       "exec `which suspend`")

;; ;; root map
;; (al/define-key *root-map* "V"   "version")
;; (al/define-key *root-map* "c"   "colon")
;; (al/define-key *root-map* "v"   "eval")
;; (al/define-key *root-map* "i"   "list-window-properties")
;; (al/define-key *root-map* "s-b" "al/banish-pointer")
;; (al/define-key *root-map* "s-u" "al/toggle-unclutter")

;; ;; menu map
;; (al/define-key *menu-map* "s-c" 'menu-up)
;; (al/define-key *menu-map* "s-t" 'menu-down)
;; (al/define-key *menu-map* "C-." 'menu-up)
;; (al/define-key *menu-map* "C-e" 'menu-down)
;; (al/define-key *menu-map* "M-." 'menu-scroll-up)
;; (al/define-key *menu-map* "M-e" 'menu-scroll-down)
;; (al/define-key *menu-map* "s-m" 'menu-finish)
;; (al/define-key *menu-map* "s-g" 'menu-abort)

;; ;; input map
;; (al/define-key *input-map* "C-p" 'input-delete-backward-char)
;; (al/define-key *input-map* "M-p" 'input-backward-kill-word)
;; (al/define-key *input-map* "C-," 'input-delete-forward-char)
;; (al/define-key *input-map* "M-," 'input-forward-kill-word)
;; (al/define-key *input-map* "C-u" 'input-forward-char)
;; (al/define-key *input-map* "M-u" 'input-forward-word)
;; (al/define-key *input-map* "C-o" 'input-backward-char)
;; (al/define-key *input-map* "M-o" 'input-backward-word)
;; (al/define-key *input-map* "C-a" 'input-move-beginning-of-line)
;; (al/define-key *input-map* "C-i" 'input-move-end-of-line)
;; (al/define-key *input-map* "M-<" 'input-kill-line)
;; (al/define-key *input-map* "M-P" 'input-kill-to-beginning)
;; (al/define-key *input-map* "M-." 'input-history-back)
;; (al/define-key *input-map* "M-e" 'input-history-forward)
;; (al/define-key *input-map* "C-m" 'input-submit)

;; ;;; Web jumps

;; (defvar al/web-map (make-sparse-keymap)
;;   "Keymap for quick browsing.")
;; (defvar al/web-wiki-map (make-sparse-keymap)
;;   "Keymap for quick browsing wikipedia.")
;; (al/define-key *top-map* "F5" 'al/web-map)
;; (al/define-key al/web-map "F5" "al/browser --new-tab about:blank")
;; (al/define-key al/web-map "g"  "al/browse-show https://github.com/notifications")
;; (al/define-key al/web-map "y"  "al/browse-show https://www.youtube.com/feed/subscriptions")
;; (al/define-key al/web-map "z"  "al/browse-show zeus")
;; (al/define-key al/web-map "t"  "al/browse-show http://tv.yandex.ru/4/?period=all-day")
;; (al/define-key al/web-map "M"  "al/browse-show https://maps.google.com/maps?hl=ru")
;; (al/define-key al/web-map "W"  "al/browse-show http://www.gismeteo.ru/city/hourly/5039/")
;; (al/define-key al/web-map "w" 'al/web-wiki-map)

;; ;; tv and radio jumps
;; (defvar al/tv-radio-map (make-sparse-keymap)
;;   "Keymap for quick access to tv and radio resources.")
;; (al/define-key *top-map* "F6" 'al/tv-radio-map)
;; (al/define-key al/tv-radio-map "F6" "exec toggle-tvtime")
;; (al/define-key al/tv-radio-map "v" "al/emms-eval (emms-play-url \"mms://live.rfn.ru/vesti_fm\")")
;; (al/define-key al/tv-radio-map "o" "al/browse-show http://www.onlinetv.ru/")
;; (al/define-key al/tv-radio-map "e" "al/send-key-to-emacs C-M-s-e")

;; ;;; Executing progs

;; (defvar al/exec-map (make-sparse-keymap)
;;   "Keymap for executing shell commands or switching to running applications.")
;; (al/define-key *top-map* "s-m" 'al/exec-map)
;; (al/define-key al/exec-map "s-m" "exec")
;; (al/define-key al/exec-map "e" "al/emacs")
;; (al/define-key al/exec-map "E" "exec emacs --no-site-file --debug-init")
;; (al/define-key al/exec-map "t" "al/xterm")
;; (al/define-key al/exec-map "T" "exec xterm")
;; (al/define-key al/exec-map "b" "al/browser")

;; ;;; Mode line

;; (defvar al/mode-line-map (make-sparse-keymap)
;;   "Keymap for controlling the mode line.")
;; (al/define-key *top-map* "M-s-m" 'al/mode-line-map)
;; (al/define-key al/mode-line-map "M-s-m" "mode-line")
;; (al/define-key al/mode-line-map "t" "mode-line")
;; (al/define-key al/mode-line-map "." "al/mode-line-top")
;; (al/define-key al/mode-line-map "e" "al/mode-line-bottom")
;; (al/define-key al/mode-line-map "n" "al/mode-line-next-net-device")

;; ;;; Mana

;; (defvar al/mana-map (make-sparse-keymap)
;;   "Keymap for sending keys to Mana.")
;; (al/define-key *top-map* "s-j" 'al/mana-map)
;; (al/define-key al/mana-map "s-j" "mana-state")
;; (al/define-key al/mana-map "a" "mana-attack")
;; (al/define-key al/mana-map "A" "mana-quick-attack")
;; (al/define-key al/mana-map "H" "mana-bat-quest-full")
;; (al/define-key al/mana-map "h" "mana-bat-quest-cont")
;; (al/define-key al/mana-map "l" "al/emacs-eval-show (find-file \"~/.local/share/mana/logs/server.themanaworld.org/\")")
;; (al/define-key al/mana-map "m" "al/emacs-eval-show (find-file \"~/notes/tmw/monsters.org\")")
;; (al/define-key al/mana-map "q" "al/emacs-eval-show (find-file \"~/notes/tmw/quests.org\")")
;; (al/define-key al/mana-map "s" "al/emacs-eval-show (find-file \"~/notes/tmw/spells.org\")")
;; (al/define-key al/mana-map "S" "al/emacs-eval-show (find-file \"~/src/tmw/tmwa-server-data/world/map/npc\")")
;; (al/define-key al/mana-map "b" "mana-break")
;; (al/define-key al/mana-map "k" "mana-kill")

;;; keys.lisp ends here
