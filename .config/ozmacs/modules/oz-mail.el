

;;; Code:
(use-package mu4e
  :elpaca nil
  :hook ((mu4e-headers-mode . (lambda () (display-line-numbers-mode -1)))
	 (mu4e-view-mode . (lambda () (mu4e-view-mode-enhanced))))
  :config
  (setq mu4e-update-interval (* 60 2)
        mu4e-root-maildir "~/.mail"
        mu4e-change-filenames-when-moving t
        mu4e-get-mail-command "mbsync -a"
        mu4e-display-update-status-in-modeline t
	mu4e-view-show-addresses t
	mu4e-view-show-images t
	mu4e-view-image-max-width 800
	mu4e-view-use-gnus t
        mu4e-sent-messages-behavior 'sent
        mu4e-hide-index-messages t
        mu4e-context-policy 'pick-first
        mu4e-compose-context-policy 'ask-if-none
        mu4e-completing-read-function #'completing-read
        mu4e-confirm-quit nil
        mu4e-headers-thread-single-orphan-prefix '("─>" . "─▶")
        mu4e-headers-thread-orphan-prefix        '("┬>" . "┬▶ ")
        mu4e-headers-thread-connection-prefix    '("│ " . "│ ")
        mu4e-headers-thread-first-child-prefix   '("├>" . "├▶")
        mu4e-headers-thread-child-prefix         '("├>" . "├▶")
        mu4e-headers-thread-last-child-prefix    '("└>" . "╰▶")
	mu4e-headers-date-format "%d/%m/%y %H:%M:%S"
	mu4e-headers-time-format "⧖ %H:%M"
	mu4e-headers-results-limit 1000
	mu4e-index-cleanup t
	mu4e-headers-field
	  '((:human-date    .   17)
	    (:flags         .    10)
	    ;; (:mailing-list  .   30)
	    (:from          .   30)
	    (:subject       .   nil)))
  (setq message-send-mail-function #'smtpmail-send-it
        smtpmail-stream-type 'starttls
        message-kill-buffer-on-exit t)
  (with-eval-after-load "mm-decode"
    (add-to-list 'mm-discouraged-alternatives "text/html")
    (add-to-list 'mm-discouraged-alternatives "text/richtext")))

(use-package mu4e-column-faces
  :hook (mu4e-headers-mode . mu4e-column-faces-mode))

(use-package mu4e-alert
  :after mu4e
  :config
  (setq mu4e-alert-email-notification-types '(count)))

(defun mu4e-view-mode-enhanced ()
  (display-line-numbers-mode -1)
  (setq-local truncate-lines nil
	      visual-fill-column-width 120
	      visual-fill-column-center-text t
	      default-text-properties '(line-height 1.1))
  (let ((inhibit-read-only t)
	(inhibit-modification-hooks t))
  (visual-fill-column-mode)
  (set-buffer-modified-p nil)))

(provide 'oz-mail)
;;; oz-mail.el ends here
