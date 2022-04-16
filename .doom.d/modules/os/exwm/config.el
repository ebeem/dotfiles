;;; ui/exwm/config.el -*- lexical-binding: t; -*-

;; Make the launcher only show app names
(use-package! counsel
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only))

(defun elken/run-application (command)
  "run the specified command as an application"
  (call-process "gtk-launch" nil 0 nil command))

(defun elken/run-in-background (command &optional args)
  "Run the specified command as a daemon"
  (elken/kill-process--action (assoc command elken/process-alist))
  (setq elken/process-alist
        (cons `(,command . ,(start-process-shell-command command nil (format "%s %s" command (or args "")))) elken/process-alist)))

(defun elken/exwm-init-hook ()
  "Various init processes for exwm"
  ;; Daemon applications
  ;; (elken/run-in-background "pasystray")
  ;; (elken/run-in-background "megasync")
  ;; (elken/run-in-background "nm-applet")

  ;; Startup applications
  ;; (elken/run-application "spotify")
  ;; (elken/run-application "discord")
  ;; (elken/run-application "firefox")

  ;; Default emacs behaviours
  ;; TODO Take this out of emacs
  (mu4e t))

(defvar elken/process-alist '())

(defun elken/kill-process--action (process)
  "Do the actual process killing"
  (when process
    (ignore-errors
      (kill-process (cdr process))))
  (setq elken/process-alist (remove process elken/process-alist)))

(defun elken/kill-process ()
  "Kill a background process"
  (interactive)
  (ivy-read "Kill process: " elken/process-alist
            :action #'elken/kill-process--action
            :caller 'elken/kill-process))

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
  (setq exwm-workspace-number 9)

  ;; Init hook
  (add-hook 'exwm-init-hook #'elken/exwm-init-hook)

  ;; /Always/ pass these to emacs
  (setq exwm-input-prefix-keys
        '(?\C-x
          ?\M-x
          ?\C-g))

  ;; Shortcut to passthrough next keys
  (map! :map exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Setup screen layout
  (require 'exwm-randr)
  (setq exwm-randr-workspace-monitor-plist '(0 "DP-4"
            1 "DP-2"
            2 "HDMI-0"))
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
	      (start-process-shell-command
	       "xrandr" nil "xrandr --output DP-4 --left-of DP-2 --output DP-2 --left-of HDMI-0")))
  (exwm-randr-enable)

  ;; (exwm-input-set-key (kbd "<s-return>") '+eshell/toggle)

  (setq exwm-input-global-keys
        '(
          ([?\s- ] . counsel-linux-app)
          ([?\s-r] . exwm-reset)
          ([s-left] . windmove-left)
          ([s-right] . windmove-right)
          ([s-up] . windmove-up)
          ([s-down] . windmove-down)
          ([?\s-&] . (lambda (command) (interactive (list (read-shell-command "[command] $ ")))
                       (start-process-shell-command command nil command)))
          ([?\s-e] . (lambda () (interactive) (dired "~")))
          ([?\s-w] . exwm-workspace-switch)
          ([?\s-q] . (lambda () (interactive) (kill-buffer))))))
