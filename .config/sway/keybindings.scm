(use-modules (oop goops)
             (ice-9 popen)
             (ice-9 textual-ports))

(define (exec . args)
  "execute given shell command"
  (get-string-all (apply open-pipe* OPEN_READ args)))

(define (exec-raw command)
  "execute given shell command"
  (apply exec (string-split command #\space)))

(set-prefix-key (kbd "s-SPC"))
(setf group-root-map (make-sparse-keymap)
      tile-group-root-map (make-sparse-keymap)
      root-map (make-sparse-keymap)
      top-map (make-sparse-keymap))

;; media-keys
(define-key *top-map* (kbd "XF86AudioLowerVolume")
  (exec-raw "exec pactl set-sink-volume @DEFAULT_SINK@ -5%"))
(define-key *top-map* (kbd "XF86AudioRaiseVolume")
  (exec-raw "exec pactl set-sink-volume @DEFAULT_SINK@ +5%"))
(define-key *top-map* (kbd "s-[")
  (exec-raw "exec pactl set-sink-volume @DEFAULT_SINK@ -5%"))
(define-key *top-map* (kbd "s-]")
  (exec-raw "exec pactl set-sink-volume @DEFAULT_SINK@ +5%"))
(define-key *top-map* (kbd "XF86AudioMute")
  (exec-raw "exec pactl set-sink-mute @DEFAULT_SINK@ toggle"))
(define-key *top-map* (kbd "XF86AudioNext")
  (exec-raw "exec mpc next"))
(define-key *top-map* (kbd "XF86AudioPrev")
  (exec-raw "exec mpc prev"))
(define-key *top-map* (kbd "XF86AudioPlay")
  (exec-raw "exec mpc toggle"))

;; function-keys
(define-key *top-map* (kbd "XF86MonBrightnessUp")
  (exec-raw "brightnessctl set +10%"))
(define-key *top-map* (kbd "XF86MonBrightnessDown")
  (exec-raw "brightnessctl set 10%-"))

;; window and group management
(define-key *top-map* (kbd "s-f")
  (exec-raw "float-this"))
(define-key *top-map* (kbd "s-F")
  (exec-raw "unfloat-this"))

(define-key *top-map* (kbd "s-h")
  (exec-raw "move-focus left"))
(define-key *top-map* (kbd "s-j")
  (exec-raw "move-focus down"))
(define-key *top-map* (kbd "s-k")
  (exec-raw "move-focus up"))
(define-key *top-map* (kbd "s-l")
  (exec-raw "move-focus right"))

(define-key *top-map* (kbd "s-H")
  (exec-raw "move-window left"))
(define-key *top-map* (kbd "s-J")
  (exec-raw "move-window down"))
(define-key *top-map* (kbd "s-K")
  (exec-raw "move-window up"))
(define-key *top-map* (kbd "s-L")
  (exec-raw "move-window right"))

(define-key *top-map* (kbd "M-s-h") "exchange-direction left")
(define-key *top-map* (kbd "M-s-j")
  (exec-raw "exchange-direction down"))
(define-key *top-map* (kbd "M-s-k")
  (exec-raw "exchange-direction up"))
(define-key *top-map* (kbd "M-s-l")
  (exec-raw "exchange-direction right"))

(define-key *top-map* (kbd "C-s-h")
  (exec-raw "gselect-direction left"))
(define-key *top-map* (kbd "C-s-j")
  (exec-raw "gselect-direction down"))
(define-key *top-map* (kbd "C-s-k")
  (exec-raw "gselect-direction up"))
(define-key *top-map* (kbd "C-s-l")
  (exec-raw "gselect-direction right"))

;; TODO: make it move window to current head
(define-key *top-map* (kbd "C-M-s-h")
  (exec-raw "gmove-direction left"))
(define-key *top-map* (kbd "C-M-s-j")
  (exec-raw "gmove-direction down"))
(define-key *top-map* (kbd "C-M-s-k")
  (exec-raw "gmove-direction up"))
(define-key *top-map* (kbd "C-M-s-l")
  (exec-raw "gmove-direction right"))

;; Tab like cycling
(define-key *top-map* (kbd "s-.") "next-in-frame")
(define-key *top-map* (kbd "s-,") "prev-in-frame")

(define-key *top-map* (kbd "s-w") "delete-window")

;; shortcuts
(define-key *top-map* (kbd "s-RET")
  (exec-raw "exec emacsclient -c -e '(+eshell/here nil)'"))
(define-key *top-map* (kbd "C-s-SPC")
  (exec-raw "exec rofi -show drun"))
(define-key *top-map* (kbd "M-s-SPC")
  (exec-raw "exec ~/.bin/switch-keyboard-layout"))
(define-key *top-map* (kbd "C-s-r")
  (exec-raw "loadrc"))
(define-key *top-map* (kbd "C-M-s-r")
  (exec-raw "restart-hard"))

;; rofi commands
(defvar *menu-rofi-keymap*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "p")
      (exec-raw "exec ~/.config/rofi/bin/password-manager"))
    ;; TODO: mount & unmount scripts
    ;; (define-key m (kbd "m") "exec rofi-mount")
    ;; (define-key m (kbd "u") "exec rofi-umount")
    ;; (define-key m (kbd "w") "exec .config/rofi/bin/wifi")
    (define-key m (kbd "b")
      (exec-raw "exec ~/.config/rofi/bin/bluetooth"))
    (define-key m (kbd "f")
      (exec-raw "exec ~/.config/rofi/bin/finder"))
    (define-key m (kbd "k")
      (exec-raw "exec ~/.config/rofi/bin/keyboard-layout"))
    (define-key m (kbd "P")
      (exec-raw "exec ~/.config/rofi/bin/powermenu"))
    (define-key m (kbd "s")
      (exec-raw "exec ~/.config/rofi/bin/sound-input"))
    (define-key m (kbd "S")
      (exec-raw "exec ~/.config/rofi/bin/sound-output"))
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
    (define-key m (kbd "d")
      (exec-raw "exec flameshot gui -d 2500"))
    (define-key m (kbd "s")
      (exec-raw "exec flameshot screen -d 2500"))
    (define-key m (kbd "f")
      (exec-raw "exec flameshot full -d 2500"))
    (define-key m (kbd "m")
      (exec-raw "exec flameshot gui -d 2500"))
    (define-key m (kbd "m")
      (exec-raw "exec flameshot gui -d 2500 --last-region"))
    m))

;; applications
(defvar *menu-applications-keymap*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "f")
      (exec-raw "exec firefox"))
    (define-key m (kbd "c")
      (exec-raw "exec chromium"))
    (define-key m (kbd "d")
      (exec-raw "exec discord"))
    (define-key m (kbd "e")
      (exec-raw "exec emacsclient -c"))
    (define-key m (kbd "g")
      (exec-raw "exec gimp"))
    (define-key m (kbd "t")
      (exec-raw "exec thunar"))
    (define-key m (kbd "s")
      (exec-raw "exec slack"))
    (define-key m (kbd "r")
      (exec-raw '*menu-rofi-keymap*))
    (define-key m (kbd "s")
      (exec-raw '*menu-screenshot-keymap*))
    (define-key m (kbd "E")
      (exec-raw '*menu-emacs-keymap*))
    (define-key m (kbd "w")
      (exec-raw "exec select-pape"))
    m))

;; session-management
(defvar *session-keymap*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "q")
      (exec-raw "quit"))
    (define-key m (kbd "l")
      (exec-raw "logout"))
    (define-key m (kbd "s")
      (exec-raw "suspend-computer"))
    (define-key m (kbd "S")
      (exec-raw "shutdown-computer"))
    (define-key m (kbd "r")
      (exec-raw "loadrc"))
    (define-key m (kbd "R")
      (exec-raw "restart-hard"))
    (define-key m (kbd "C-r")
      (exec-raw "restart-computer"))
    m))


(defcommand hsplit-and-focus () ()
  "create a new frame on the right and focus it."
  (hsplit)
  (move-focus :right))

(defcommand vsplit-and-focus () ()
  "create a new frame below and focus it."
  (vsplit)
  (move-focus :down))

;; window-management
(defvar *menu-windows-keymap*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "v")
      (exec-raw "hsplit-and-focus"))
    (define-key m (kbd "h")
      (exec-raw "vsplit-and-focus"))
    (define-key m (kbd "d")
      (exec-raw "remove-split"))
    (define-key m (kbd "m")
      (exec-raw "only"))
    (define-key m (kbd "p")
      (exec-raw "global-pull-windowlist"))
    (define-key m (kbd "i")
      (exec-raw "list-window-properties"))
    (define-key m (kbd "I")
      (exec-raw "info"))
    (define-key m (kbd "g")
      (exec-raw "toggle-gaps"))
    (define-key m (kbd "l")
      (exec-raw "windowlist"))
    (define-key m (kbd "k")
      (exec-raw "kill-window"))
    (define-key m (kbd "=")
      (exec-raw "balance-frames"))
    (define-key m (kbd "f")
      (exec-raw "fullscreen"))
    m))

(defvar *menu-group-keymap*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "v")
      (exec-raw "hsplit-and-focus"))
    (define-key m (kbd "h")
      (exec-raw "vsplit-and-focus"))
    (define-key m (kbd "d")
      (exec-raw "remove-split"))
    (define-key m (kbd "m")
      (exec-raw "only"))
    (define-key m (kbd "p")
      (exec-raw "global-pull-windowlist"))
    (define-key m (kbd "i")
      (exec-raw "list-window-properties"))
    (define-key m (kbd "g")
      (exec-raw "toggle-gaps"))
    (define-key m (kbd "l")
      (exec-raw "grouplist"))
    (define-key m (kbd "=")
      (exec-raw "balance-frames"))
    (define-key m (kbd "f")
      (exec-raw "fullscreen"))
    m))

;; menu map
(define-key *menu-map* (kbd "C-k")
  (exec-raw 'menu-up))
(define-key *menu-map* (kbd "C-j")
  (exec-raw 'menu-down))
(define-key *menu-map* (kbd "M-k")
  (exec-raw 'menu-scroll-up))
(define-key *menu-map* (kbd "M-j")
  (exec-raw 'menu-scroll-down))
;; (define-key *menu-map* (kbd "s-m") 'menu-finish)
(define-key *menu-map* (kbd "C-g")
  (exec-raw 'menu-abort))

;; ;; input map
(define-key *input-map* (kbd "C-p")
  (exec-raw 'input-delete-backward-char))
(define-key *input-map* (kbd "M-p")
  (exec-raw 'input-backward-kill-word))
(define-key *input-map* (kbd "C-,")
  (exec-raw 'input-delete-forward-char))
(define-key *input-map* (kbd "M-,")
  (exec-raw 'input-forward-kill-word))
(define-key *input-map* (kbd "C-u")
  (exec-raw 'input-forward-char))
(define-key *input-map* (kbd "M-u")
  (exec-raw 'input-forward-word))
(define-key *input-map* (kbd "C-o")
  (exec-raw 'input-backward-char))
(define-key *input-map* (kbd "M-o")
  (exec-raw 'input-backward-word))
(define-key *input-map* (kbd "C-a")
  (exec-raw 'input-move-beginning-of-line))
(define-key *input-map* (kbd "C-i")
  (exec-raw 'input-move-end-of-line))
(define-key *input-map* (kbd "M-<")
  (exec-raw 'input-kill-line))
(define-key *input-map* (kbd "M-P")
  (exec-raw 'input-kill-to-beginning))
(define-key *input-map* (kbd "C-k")
  (exec-raw 'input-history-back))
(define-key *input-map* (kbd "C-j")
  (exec-raw 'input-history-forward))
(define-key *input-map* (kbd "C-m")
  (exec-raw 'input-submit))


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
(define-key *group-root-map*  (kbd "h")  '*help-map*)
