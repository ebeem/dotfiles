(in-package :stumpwm)
(set-prefix-key (kbd "s-SPC"))
(setf *group-root-map* (make-sparse-keymap)
      ;; *float-group-root-root* (make-sparse-keymap)
      *tile-group-root-map* (make-sparse-keymap)
      *root-map* (make-sparse-keymap)
      *top-map* (make-sparse-keymap))

;; define top-map keybindings
;; media-keys
(define-key *top-map* (kbd "XF86AudioLowerVolume") "exec pactl set-sink-volume @DEFAULT_SINK@ -5%")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "exec pactl set-sink-volume @DEFAULT_SINK@ +5%")
(define-key *top-map* (kbd "s-[") "exec pactl set-sink-volume @DEFAULT_SINK@ -5%")
(define-key *top-map* (kbd "s-]") "exec pactl set-sink-volume @DEFAULT_SINK@ +5%")
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
    ;; create a keybinding for each screen
    ;; for some reason, the screen selection doesn't work in flameshot
    (dolist (head (screen-heads (current-screen)))
        (let* ((number (head-number head))
                (height (head-height head))
                (width (head-width head))
                (x (head-x head))
                (y (head-y head)))
            (define-key m (kbd (write-to-string number)) (format nil "exec flameshot full -d 2500 --region ~dx~d+~d+~d" width height x y))))
    (define-key m (kbd "d") "exec flameshot gui -d 2500")
    (define-key m (kbd "s") "exec flameshot screen -d 2500")
    (define-key m (kbd "f") "exec flameshot full -d 2500")
    (define-key m (kbd "m") "exec flameshot gui -d 2500")
    (define-key m (kbd "m") "exec flameshot gui -d 2500 --last-region")
    m))

;; applications
(defvar *menu-applications-keymap*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "f") "exec firefox")
    (define-key m (kbd "c") "exec chromium")
    (define-key m (kbd "d") "exec discord")
    (define-key m (kbd "e") "exec emacsclient -c")
    (define-key m (kbd "g") "exec gimp")
    (define-key m (kbd "t") "exec thunar")
    (define-key m (kbd "s") "exec slack")
    (define-key m (kbd "r") '*menu-rofi-keymap*)
    (define-key m (kbd "s") '*menu-screenshot-keymap*)
    (define-key m (kbd "E") '*menu-emacs-keymap*)
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
(load-module "globalwindows")
(defvar *menu-windows-keymap*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "v") "hsplit-and-focus")
    (define-key m (kbd "h") "vsplit-and-focus")
    (define-key m (kbd "d") "remove-split")
    (define-key m (kbd "m") "only")
    (define-key m (kbd "p") "global-pull-windowlist")
    (define-key m (kbd "i") "list-window-properties")
    (define-key m (kbd "I") "info")
    (define-key m (kbd "g") "toggle-gaps")
    (define-key m (kbd "l") "windowlist")
    (define-key m (kbd "k") "kill-window")
    (define-key m (kbd "=") "balance-frames")
    (define-key m (kbd "f") "fullscreen")
    m))

(defvar *menu-group-keymap*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "v") "hsplit-and-focus")
    (define-key m (kbd "h") "vsplit-and-focus")
    (define-key m (kbd "d") "remove-split")
    (define-key m (kbd "m") "only")
    (define-key m (kbd "p") "global-pull-windowlist")
    (define-key m (kbd "i") "list-window-properties")
    (define-key m (kbd "g") "toggle-gaps")
    (define-key m (kbd "l") "grouplist")
    (define-key m (kbd "=") "balance-frames")
    (define-key m (kbd "f") "fullscreen")
    m))

;; menu map
(define-key *menu-map* (kbd "C-k") 'menu-up)
(define-key *menu-map* (kbd "C-j") 'menu-down)
(define-key *menu-map* (kbd "M-k") 'menu-scroll-up)
(define-key *menu-map* (kbd "M-j") 'menu-scroll-down)
;; (define-key *menu-map* (kbd "s-m") 'menu-finish)
(define-key *menu-map* (kbd "C-g") 'menu-abort)

;; ;; input map
(define-key *input-map* (kbd "C-p") 'input-delete-backward-char)
(define-key *input-map* (kbd "M-p") 'input-backward-kill-word)
(define-key *input-map* (kbd "C-,") 'input-delete-forward-char)
(define-key *input-map* (kbd "M-,") 'input-forward-kill-word)
(define-key *input-map* (kbd "C-u") 'input-forward-char)
(define-key *input-map* (kbd "M-u") 'input-forward-word)
(define-key *input-map* (kbd "C-o") 'input-backward-char)
(define-key *input-map* (kbd "M-o") 'input-backward-word)
(define-key *input-map* (kbd "C-a") 'input-move-beginning-of-line)
(define-key *input-map* (kbd "C-i") 'input-move-end-of-line)
(define-key *input-map* (kbd "M-<") 'input-kill-line)
(define-key *input-map* (kbd "M-P") 'input-kill-to-beginning)
(define-key *input-map* (kbd "C-k") 'input-history-back)
(define-key *input-map* (kbd "C-j") 'input-history-forward)
(define-key *input-map* (kbd "C-m") 'input-submit)


;; define root-map keybindings
(fill-keymap *top-map*
  *escape-key* '*root-map*)

(fill-keymap *root-map*
  (kbd "!")   "exec"
  (kbd "C-g") "abort"
  *escape-fake-key* "send-escape"
  (kbd ";")   "colon"
  (kbd ":")   "eval"
  (kbd "v")   "version"
  (kbd "m")   "lastmsg"
  (kbd "h")   '*help-map*)

(fill-keymap *group-top-map*
  *escape-key* '*group-root-map*)

(fill-keymap *group-root-map*)

(fill-keymap *tile-group-top-map*
  *escape-key* '*tile-group-root-map*)

(fill-keymap *tile-group-root-map*
    (kbd "q") "quit-confirm"
    (kbd "=") "balance-frames"
    (kbd "l") "redisplay"
    (kbd "q") '*session-keymap*
    (kbd "a") '*menu-applications-keymap*
    (kbd "o") "exec rofi -show drun"
    (kbd "r") '*menu-rofi-keymap*
    (kbd "s") '*menu-screenshot-keymap*
    (kbd "w") '*menu-windows-keymap*
    (kbd "F") "fullscreen"
    (kbd "I") "show-window-properties")

(fill-keymap *groups-map*)
(fill-keymap *exchange-window-map*
             (kbd "Up")    "exchange-direction up"
             (kbd "Down")  "exchange-direction down"
             (kbd "Left")  "exchange-direction left"
             (kbd "Right") "exchange-direction right"
             (kbd "p")     "exchange-direction up"
             (kbd "n")     "exchange-direction down"
             (kbd "b")     "exchange-direction left"
             (kbd "f")     "exchange-direction right"
             (kbd "k")     "exchange-direction up"
             (kbd "j")     "exchange-direction down"
             (kbd "h")     "exchange-direction left"
             (kbd "l")     "exchange-direction right")

(fill-keymap *help-map*
  (kbd "v") "describe-variable"
  (kbd "f") "describe-function"
  (kbd "k") "describe-key"
  (kbd "c") "describe-command"
  (kbd "w") "where-is")

(define-key *group-root-map* (kbd "q") '*session-keymap*)
(define-key *group-root-map* (kbd "a") '*menu-applications-keymap*)
(define-key *group-root-map* (kbd "o") "exec rofi -show drun")
(define-key *group-root-map* (kbd "r") '*menu-rofi-keymap*)
(define-key *group-root-map* (kbd "s") '*menu-screenshot-keymap*)
(define-key *group-root-map* (kbd "w") '*menu-windows-keymap*)
;; (define-key *group-root-map* (kbd "#")   "mark")
(define-key *group-root-map* (kbd "F") "fullscreen")
(define-key *group-root-map* (kbd "I")   "show-window-properties")
(define-key *group-root-map* (kbd "!")   "exec")
(define-key *group-root-map* (kbd "C-g") "abort")
(define-key *group-root-map* *escape-fake-key* "send-escape")
(define-key *group-root-map* (kbd ";")   "colon")
(define-key *group-root-map* (kbd ":")   "eval")
(define-key *group-root-map* (kbd "v")   "version")
(define-key *group-root-map* (kbd "m")   "lastmsg")
(define-key *group-root-map*  (kbd "h")   '*help-map*)
