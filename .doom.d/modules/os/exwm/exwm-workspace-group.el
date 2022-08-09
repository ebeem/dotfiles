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

(provide 'exwm-workspace-group)
