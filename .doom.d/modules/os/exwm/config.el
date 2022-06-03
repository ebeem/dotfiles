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
  ;; (require 'exwm-randr)
(unless (fboundp 'comment)
  (defmacro comment (&rest args)
    `nil))

(defvar ewg/max-group-size nil "the number of monitors")
(defvar ewg/max-num-groups 10 "the maximum number of groups")

(defun ewg/init (monitor-names)
  (setq ewg/monitor-names monitor-names)
  (if ewg/monitor-names
      (setq ewg/max-group-size (length monitor-names))
    (setq ewg/max-group-size 1))
  ;; initial num of workspaces
  (setq exwm-workspace-number ewg/max-group-size)
  (setq exwm-randr-workspace-monitor-plist (ewg/get-exwm-randr-workspace-monitor-plist ewg/max-num-groups))
  (let ((xrandr-update
         (pcase ewg/max-group-size
           (1 (comment "do nothing"))
           (2 'ewg/xrandr-dual-monitor)
           (3 'ewg/xrandr-triple-monitor)
           (t (error "The layout is not implemented")))))
    (when xrandr-update
      (add-hook 'exwm-randr-screen-change-hook xrandr-update)))
  (exwm-randr-enable))

(progn
  ;; utilities

  (defun ewg/get-group-index (workspace-idx)
    (/ workspace-idx ewg/max-group-size))

  (defun ewg/get-last-group-index ()
    (ewg/get-group-index (1- (exwm-workspace--count))))

  (defun ewg/get-group-member-idx (workspace-idx)
    (- workspace-idx (* (ewg/get-group-index workspace-idx)
                        ewg/max-group-size)))
  (progn
    (defun ewg/other-workspace-in-group (count)
      (interactive "p")
      (let* ((group-idx (ewg/get-group-index exwm-workspace-current-index))
             (group-size (min (- (exwm-workspace--count)
                                 (* group-idx ewg/max-group-size))
                              ewg/max-group-size))
             (member-idx (- exwm-workspace-current-index
                            (* group-idx ewg/max-group-size)))
             (next-member-idx (% (+ count member-idx group-size) group-size))
             (next-workspace-idx (+ (* group-idx ewg/max-group-size)
                                    next-member-idx)))
        (exwm-workspace-switch next-workspace-idx)))

    (defun ewg/other-workspace-in-group-backwards () (interactive) (ewg/other-workspace-in-group -1)))

  (defun ewg/switch-create (group-idx)
    (let* ((current-member-idx (ewg/get-group-member-idx exwm-workspace-current-index))
           (new-workspace-idx (+ (* group-idx ewg/max-group-size)
                                 current-member-idx)))
      (dotimes (i ewg/max-group-size)
        (exwm-workspace-switch-create (+ (* group-idx ewg/max-group-size) i)))
      (exwm-workspace-switch new-workspace-idx)))

  (defun ewg/delete (group-idx)
    (let ((prev-workspace-idx exwm-workspace-current-index))
      (dolist (i (reverse (number-sequence 0 (1- ewg/max-group-size))))
        (exwm-workspace-delete (+ (* group-idx ewg/max-group-size) i)))
      (let ((new-workspace-idx (% (+ prev-workspace-idx (exwm-workspace--count))
                                  (exwm-workspace--count))))
        (exwm-workspace-switch new-workspace-idx))))

  (defun ewg/switch-next-group (count)
    (interactive "p")
    (if (<= (exwm-workspace--count) ewg/max-group-size)
        (user-error "There's no other workspace group")
      (let* ((current-group-idx (ewg/get-group-index exwm-workspace-current-index))
             (num-groups (1+ (ewg/get-last-group-index)))
             (next-group-idx (% (+ current-group-idx count num-groups) num-groups)))
        (ewg/switch-create next-group-idx))))

  (defun ewg/switch-previous-group ()
    (interactive)
    (ewg/switch-next-group -1))

  (defun ewg/add-group ()
    (interactive)
    (let* ((current-group-idx (ewg/get-group-index exwm-workspace-current-index))
           (next-group-idx (1+ current-group-idx))
           (current-member-idx (ewg/get-group-member-idx exwm-workspace-current-index))
           (new-workspace-idx (+ (* next-group-idx ewg/max-group-size)
                                 current-member-idx)))
      (dotimes (i ewg/max-group-size)
        (exwm-workspace-add (+ (* next-group-idx ewg/max-group-size) i)))
      (exwm-workspace-switch new-workspace-idx)))

  (defun ewg/delete-current-group ()
    (interactive)
    (if (<= (exwm-workspace--count) ewg/max-group-size)
        (user-error "Attempt to delete the sole workspace group")
      (if (y-or-n-p (format "Are you sure you want to close this workspace group? "))
	      (ewg/delete
           (ewg/get-group-index exwm-workspace-current-index))
        (message "Canceled closing the current workspace group"))))

  (defun ewg/delete-other-groups ()
    (interactive)
    (if (<= (exwm-workspace--count) ewg/max-group-size)
        (user-error "There's no other workspace group")
      (if (y-or-n-p (format "Are you sure you want to close other workspace groups? "))
          (let ((prev-workspace-idx exwm-workspace-current-index))
            (let* ((group-idx (ewg/get-group-index exwm-workspace-current-index))
                   (first-workspace-idx-in-group (* group-idx ewg/max-group-size))
                   (workspace-indices-in-group
                    (number-sequence first-workspace-idx-in-group
                                     (+ first-workspace-idx-in-group
                                        (1- ewg/max-group-size)))))
              (dolist (i (reverse (number-sequence 0 (1- (exwm-workspace--count)))))
                (unless (member i workspace-indices-in-group)
                  (exwm-workspace-delete i))))
            (exwm-workspace-switch (% prev-workspace-idx (exwm-workspace--count))))
        (message "Canceled closing other workspace groups"))))

  (defun ewg/workspace-swap-by-workspace-indices (index1 index2)
    (exwm-workspace-swap (exwm-workspace--workspace-from-frame-or-index index1)
                         (exwm-workspace--workspace-from-frame-or-index index2)))

  (defun ewg/swap (group-idx1 group-idx2)
    (dotimes (i ewg/max-group-size)
      (ewg/workspace-swap-by-workspace-indices
       (+ (* group-idx1 ewg/max-group-size) i)
       (+ (* group-idx2 ewg/max-group-size) i))))

  (defun ewg/swap-current-group-number (group-number)
    (interactive "nEnter workspace group number: ")
    (if (> group-number (exwm-workspace--count))
        (user-error "Workspace group number is out of range")
      (let ((group-idx (- group-number exwm-my-workspace-start-number))
            (current-group-idx (ewg/get-group-index exwm-workspace-current-index)))
        (if (= group-idx current-group-idx)
            (user-error "Cannot swap with the same workspace group")
          (ewg/swap current-group-idx group-idx))))))


(progn
  ;; xrandr config
  ;; https://github.com/daviwil/emacs-from-scratch/blob/5ebd390119a48cac6258843c7d5e570f4591fdd4/show-notes/Emacs-Desktop-04.org

  (require 'exwm-randr)
  (defvar ewg/monitor-names nil "a list of monitor names")

  (progn
    (require 'cl-lib)
    (defun ewg/get-exwm-randr-workspace-monitor-plist (max-num-groups)
      "mapping workspace indices with monitors"
      (let ((max-num-workspaces (* max-num-groups ewg/max-group-size)))
        (cl-labels
            ((get-plist (num)
                        (when (< num max-num-workspaces)
                          (cons num (cons (nth (% num ewg/max-group-size)
                                               ewg/monitor-names)
                                          (get-plist (1+ num)))))))
          (get-plist 0)))))

  (progn
    ;; monitor deployment layout
    (defun ewg/xrandr-dual-monitor ()
      (assert (= (length ewg/monitor-names) 2))
      (start-process-shell-command
       "xrandr" nil
       (format "xrandr --output %s --auto \
                       --output %s --auto --left-of %s"
               (nth 1 ewg/monitor-names)
               (nth 0 ewg/monitor-names) (nth 1 ewg/monitor-names)))
      (progn
        ;; this prevent wrong frame deployment when
        ;; `exwm-base-input-simulation-keys' has many commands
        (exwm-randr-refresh)))

    (defun ewg/xrandr-triple-monitor ()
      (assert (= (length ewg/monitor-names) 3))
      (start-process-shell-command
       "xrandr" nil
       (format "xrandr --output %s --auto \
                       --output %s --auto --left-of %s \
                       --output %s --auto --right-of %s"
               (nth 1 ewg/monitor-names)
               (nth 0 ewg/monitor-names) (nth 1 ewg/monitor-names)
               (nth 2 ewg/monitor-names) (nth 1 ewg/monitor-names)))
      (progn
        ;; this prevent wrong frame deployment when
        ;; `exwm-base-input-simulation-keys' has many commands
        (exwm-randr-refresh))))

  (comment
    ;; example
    (setq ewg/monitor-names (list "HDMI-1-1" "DVI-I-1" "HDMI-0"))
    (progn
      (setq exwm-randr-workspace-monitor-plist (ewg/get-exwm-randr-workspace-monitor-plist 10))
      (comment
        ;; alternative case
        (setq exwm-randr-workspace-monitor-plist
              '(0 "HDMI-1-1" 1 "DVI-I-1" 2 "HDMI-0"
                3 "HDMI-1-1" 4 "DVI-I-1" 5 "HDMI-0" ...)))

      ;; run xrandr
      (add-hook 'exwm-randr-screen-change-hook #'ewg/xrandr-triple-monitor))))


  (ewg/init (list "DP-2" "DP-1" "HDMI-1"))
;; odd numbered workspaces are mapped to HDMI-1, even-numbered ones to eDP-1 (ugly)
;; (setq exwm-randr-workspace-output-plist '(0 "eDP-1" 1 "HDMI-1" 2 "eDP-1" 3 "HDMI-1" 4 "eDP-1" 5 "HDMI-1"
;; 6 "eDP-1" 7 "HDMI-1" 8 "eDP-1" 9 "HDMI-1" ))
  ;; (setq exwm-randr-workspace-output-plist '(
  ;;           0 "HDMI-0"
  ;;           1 "DP-4"
  ;;           2 "DP-2"
  ;;           3 "HDMI-0"
  ;;           4 "DP-4"
  ;;           5 "DP-2"
  ;;           6 "HDMI-0"
  ;;           7 "DP-4"
  ;;           8 "DP-2"
  ;;           9 "HDMI-0"
  ;;           10 "DP-4"
  ;;           11 "DP-2"
  ;;           12 "HDMI-0"
  ;;           13 "DP-4"
  ;;           14 "DP-2"
  ;;           15 "HDMI-0"
  ;;           16 "DP-4"
  ;;           17 "DP-2"
  ;;           18 "HDMI-0"
  ;;           19 "DP-4"
  ;;           20 "DP-2"
  ;;           21 "HDMI-0"
  ;;           22 "DP-4"
  ;;           23 "DP-2"
  ;;           24 "HDMI-0"
  ;;           25 "DP-4"
  ;;           26 "DP-2"
  ;; ))

  ;; (setq exwm-randr-workspace-monitor-plist '(
  ;;           0 "HDMI-0"
  ;;           1 "DP-4"
  ;;           2 "DP-2"
  ;;           3 "HDMI-0"
  ;;           4 "DP-4"
  ;;           5 "DP-2"
  ;;           6 "HDMI-0"
  ;;           7 "DP-4"
  ;;           8 "DP-2"
  ;;           9 "HDMI-0"
  ;;           10 "DP-4"
  ;;           11 "DP-2"
  ;;           12 "HDMI-0"
  ;;           13 "DP-4"
  ;;           14 "DP-2"
  ;;           15 "HDMI-0"
  ;;           16 "DP-4"
  ;;           17 "DP-2"
  ;;           18 "HDMI-0"
  ;;           19 "DP-4"
  ;;           20 "DP-2"
  ;;           21 "HDMI-0"
  ;;           22 "DP-4"
  ;;           23 "DP-2"
  ;;           24 "HDMI-0"
  ;;           25 "DP-4"
  ;;           26 "DP-2"
  ;; ))
  ;; (add-hook 'exwm-randr-screen-change-hook
  ;;           (lambda ()
  ;;             (start-process-shell-command
  ;;              "xrandr" nil "xrandr --output HDMI-0 --right-of DP-2 --output DP-4 --left-of DP-2")
  ;;       (start-process-shell-command
  ;;              "exec" nil "exec ~/.config/polybar/cuts/launch.sh")))
  ;; (exwm-randr-enable)

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
