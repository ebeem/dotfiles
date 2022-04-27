;;; ui/exwm/config.el -*- lexical-binding: t; -*-

;; Make the launcher only show app names
;; (use-package! counsel
;;   :custom
;;   (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only))

(defun ebeem/run-application (command)
  "run the specified command as an application"
  (call-process "gtk-launch" nil 0 nil command))

(defun ebeem/run-in-background (command &optional args)
  "Run the specified command as a daemon"
  (ebeem/kill-process--action (assoc command ebeem/process-alist))
  (setq ebeem/process-alist
        (cons `(,command . ,(start-process-shell-command command nil (format "%s %s" command (or args "")))) ebeem/process-alist)))

(defun ebeem/exwm-init-hook ()
  "Various init processes for exwm"
  (ebeem/run-application "~/.config/polybar/cuts/launch.sh")
  ;; Daemon applications
  ;; (ebeem/run-in-background "pasystray")
  ;; (ebeem/run-in-background "megasync")
  ;; (ebeem/run-in-background "nm-applet")

  ;; Startup applications
  ;; (ebeem/run-application "spotify")
  ;; (ebeem/run-application "discord")
  ;; (ebeem/run-application "firefox")

  ;; Default emacs behaviours
  ;; TODO Take this out of emacs
  (when (require 'elfeed nil t)
    (run-at-time nil (* 8 60 60) #'elfeed-update))
  (mu4e t))

(defvar ebeem/process-alist '())

(defun ebeem/kill-process--action (process)
  "Do the actual process killing"
  (when process
    (ignore-errors
      (kill-process (cdr process))))
  (setq ebeem/process-alist (remove process ebeem/process-alist)))

(defun ebeem/kill-process ()
  "Kill a background process"
  (interactive)
  (ivy-read "Kill process: " ebeem/process-alist
            :action #'ebeem/kill-process--action
            :caller 'ebeem/kill-process))

;; Used to handle screen locking (currently unused), media keys and screenshotting
(use-package! desktop-environment
  :after exwm
  :config
  (setq desktop-environment-screenshot-command "flameshot gui")
  (desktop-environment-mode))

;; The meat and potatoes as they say
(use-package! exwm
  :commands (exwm-enable)
  :config
  ;; Enabled debugging when doom is in debug mode
  (when doom-debug-p
    (advice-add #'exwm-input--on-buffer-list-update :before
                (lambda (&rest r)
                (exwm--log "CALL STACK: %s" (cddr (reverse (xcb-debug:-call-stack))))))
    (exwm-debug))

  ;; Show all buffers for switching
  (setq exwm-workspace-show-all-buffers t)

  ;; Set a sane number of default workspaces
  (setq exwm-workspace-number 27)

  ;; Init hook
  (add-hook 'exwm-init-hook #'ebeem/exwm-init-hook)

  (defun ebeem/send-polybar-hook (module-name hook-index)
    (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" module-name hook-index)))

  (defun ebeem/update-polybar-exwm (&optional path)
    (ebeem/send-polybar-hook "exwm-workspace" 1))

  (defun ebeem/polybar-exwm-workspace ()
    (pcase exwm-workspace-current-index
      (0 "💀")
      (1 "🔥")
      (2 "📡")
      (3 "✨")
      (4 "💣")))

  (add-hook 'exwm-workspace-switch-hook #'ebeem/update-polybar-exwm)

  ;; /Always/ pass these to emacs
  (setq exwm-input-prefix-keys
        '(?\C-x
          ?\M-x
          ?\C-g))

  ;; Shortcut to passthrough next keys
  (map! :map exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Setup screen layout
  (require 'exwm-randr)

;; odd numbered workspaces are mapped to HDMI-1, even-numbered ones to eDP-1 (ugly)
;; (setq exwm-randr-workspace-output-plist '(0 "eDP-1" 1 "HDMI-1" 2 "eDP-1" 3 "HDMI-1" 4 "eDP-1" 5 "HDMI-1"
;; 6 "eDP-1" 7 "HDMI-1" 8 "eDP-1" 9 "HDMI-1" ))
  (setq exwm-randr-workspace-output-plist '(
            0 "HDMI-0"
            1 "DP-4"
            2 "DP-2"
            3 "HDMI-0"
            4 "DP-4"
            5 "DP-2"
            6 "HDMI-0"
            7 "DP-4"
            8 "DP-2"
            9 "HDMI-0"
            10 "DP-4"
            11 "DP-2"
            12 "HDMI-0"
            13 "DP-4"
            14 "DP-2"
            15 "HDMI-0"
            16 "DP-4"
            17 "DP-2"
            18 "HDMI-0"
            19 "DP-4"
            20 "DP-2"
            21 "HDMI-0"
            22 "DP-4"
            23 "DP-2"
            24 "HDMI-0"
            25 "DP-4"
            26 "DP-2"
  ))

  (setq exwm-randr-workspace-monitor-plist '(
            0 "HDMI-0"
            1 "DP-4"
            2 "DP-2"
            3 "HDMI-0"
            4 "DP-4"
            5 "DP-2"
            6 "HDMI-0"
            7 "DP-4"
            8 "DP-2"
            9 "HDMI-0"
            10 "DP-4"
            11 "DP-2"
            12 "HDMI-0"
            13 "DP-4"
            14 "DP-2"
            15 "HDMI-0"
            16 "DP-4"
            17 "DP-2"
            18 "HDMI-0"
            19 "DP-4"
            20 "DP-2"
            21 "HDMI-0"
            22 "DP-4"
            23 "DP-2"
            24 "HDMI-0"
            25 "DP-4"
            26 "DP-2"
  ))
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
	      (start-process-shell-command
	       "xrandr" nil "xrandr --output HDMI-0 --right-of DP-2 --output DP-4 --left-of DP-2")
        (start-process-shell-command
	       "exec" nil "exec ~/.config/polybar/cuts/launch.sh")))
  (exwm-randr-enable)
  
  ;; (exwm-input-set-key (kbd "<s-return>") '+eshell/toggle)

  (defun exwm-workspace-switch-group-hack (list-index)
  "Switch to a group of workspaces"
  (dolist (workspace list-index)
    (exwm-workspace-switch-create workspace)))

  (defun exwm-workspace-switch-group (index size)
    "Activate workspaces in [index; index+size["
    (interactive)
    (exwm-workspace-switch-group-hack
    (number-sequence
      (* index size)
      (- (* size (+ index 1)) 1))))

  (setq exwm-input-global-keys
        '(
          ([?\s-1] . (lambda () (interactive) (exwm-workspace-switch-group 0 3)))
          ([?\s-2] . (lambda () (interactive) (exwm-workspace-switch-group 1 3)))
          ([?\s-3] . (lambda () (interactive) (exwm-workspace-switch-group 2 3)))
          ([?\s-4] . (lambda () (interactive) (exwm-workspace-switch-group 3 3)))
          ([?\s-5] . (lambda () (interactive) (exwm-workspace-switch-group 4 3)))
          ([?\s-6] . (lambda () (interactive) (exwm-workspace-switch-group 5 3)))
          ([?\s-7] . (lambda () (interactive) (exwm-workspace-switch-group 6 3)))
          ([?\s-8] . (lambda () (interactive) (exwm-workspace-switch-group 7 3)))
          ([?\s-9] . (lambda () (interactive) (exwm-workspace-switch-group 8 3)))
          ([?\s-w] . ace-window)
          ([?\s-W] . exwm-workspace-switch)
          ([?\s- ] . counsel-linux-app)
          ([?\s-r] . exwm-reset)
          ([s-h] . evil-window-left)
          ([s-l] . evil-window-right)
          ([s-k] . evil-window-up)
          ([s-j] . evil-window-down)
          ([s-H] . evil-window-move-left)
          ([s-I] . evil-window-move-right)
          ([s-K] . evil-window-move-up)
          ([s-J] . evil-window-move-down)
          ([?\s-&] . (lambda (command) (interactive (list (read-shell-command "[command] $ ")))
                       (start-process-shell-command command nil command)))
          ([?\s-e] . (lambda () (interactive) (dired "~")))
          ([?\s-q] . (lambda () (interactive) (kill-buffer)))
        )))
