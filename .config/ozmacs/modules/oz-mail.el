;;; Code:
(defvar mu4e-secret-mail-path "~/.secret/mail.yml")

(use-package yaml)
(use-package mu4e
  :ensure nil
  :after yaml
  :commands (mu4e)
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
        mu4e-context-policy nil
        mu4e-compose-context-policy 'ask-if-none
        mu4e-completing-read-function 'completing-read
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
          (:subject       .   nil))
		message-send-mail-function 'smtpmail-send-it
      	smtpmail-stream-type 'starttls
      	message-kill-buffer-on-exit t)

  (custom-set-faces '(mu4e-header-highlight-face ((t (:inherit hl-line :extend t :underline t)))))
  (custom-set-faces '(mu4e-unread-face ((t (:inherit font-lock-keyword-face :extend t)))))

  (with-eval-after-load "mm-decode"
    (add-to-list 'mm-discouraged-alternatives "text/html")
    (add-to-list 'mm-discouraged-alternatives "text/richtext"))

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

  ;; append each context from mail.yml config file
  (defun read-mail-contexts (config-path)
    "Return all contexts from the provided `config-path` yaml file file"
    (with-temp-buffer
      (insert-file-contents config-path)
      (let* ((contexts '())
             (mails-parsed (yaml-parse-string (buffer-string) :object-type 'plist)))
        (dolist (a mails-parsed)
          (let* ((nickname (plist-get a :nick-name))
				 (id (plist-get a :id))
				 (mail (plist-get a :mail))
				 (fullname (plist-get a :sender-name))
				 (smtp-server (plist-get a :smtp-host))
				 (smtp-port (plist-get a :smtp-port))
				 (smtp-protocol (plist-get a :smtp-protocol))
				 (sent-folder (plist-get a :sent-folder))
				 (draft-folder (plist-get a :draft-folder))
				 (trash-folder (plist-get a :trash-folder))
				 (archive-folder (plist-get a :archive-folder)))
            (add-to-list 'contexts (simple-make-mu4e-context id fullname mail smtp-server smtp-port smtp-protocol sent-folder trash-folder archive-folder draft-folder) t)))
        (cdr contexts))))

  (defun eb/mu4e-compose-context-switch ()
    "Prompt in minibuffer to switch mu4e context and open new compose buffer."
    (interactive)
    (let* ((context-alist
            (mapcar (lambda (ctx)
                      (cons (mu4e-context-name ctx) ctx))
                    mu4e-contexts))
           (choice
            (completing-read "Compose with context: " (mapcar #'car context-alist) nil t))
           (selected-context (cdr (assoc choice context-alist))))
      (when selected-context
        (message (mu4e-context-name selected-context))
        (mu4e-compose-context-switch nil (mu4e-context-name selected-context)))))

  (defun eb/mu4e-context-switch ()
    "Switch mu4e context using minibuffer selection."
    (interactive)
    (let* ((context-alist
            (mapcar (lambda (ctx)
                      (cons (mu4e-context-name ctx) ctx))
                    mu4e-contexts))
           (choice
            (completing-read "Switch to context: " (mapcar #'car context-alist) nil t))
           (selected-context (cdr (assoc choice context-alist))))
      (when selected-context
        (message (mu4e-context-name selected-context))
        (mu4e-context-switch nil (mu4e-context-name selected-context)))))


  (defun simple-make-mu4e-context (context-name full-name mail-address smtp-server smtp-port smtp-protocol sent-folder trash-folder archive-folder draft-folder)
    "Return a mu4e context named CONTEXT-NAME with :match-func matching
  folder name CONTEXT-NAME in Maildir. The context's `user-mail-address',
  `user-full-name' and `mu4e-compose-signature' is set to MAIL-ADDRESS
  FULL-NAME and SIGNATURE respectively.
  Special folders are set to context specific folders."
    (let ((dir-name (concat "/" context-name)))
      (make-mu4e-context
       :name context-name
       ;; we match based on the maildir of the message
       ;; this matches maildir /Arkham and its sub-directories
       :match-func
       `(lambda (msg)
          (when msg
            (string-match-p
  	         ,(concat "^" dir-name)
  	         (mu4e-message-field msg :maildir))))
       :vars
       `((user-mail-address    . ,mail-address)
         (user-full-name       . ,full-name)
         (mu4e-sent-folder     . ,(concat dir-name sent-folder))
         (mu4e-drafts-folder   . ,(concat dir-name draft-folder))
         (mu4e-trash-folder    . ,(concat dir-name trash-folder))
         (mu4e-refile-folder   . ,(concat dir-name archive-folder))
         (smtpmail-smtp-server  . ,smtp-server)
         (smtpmail-smtp-service . ,smtp-port)
         (smtpmail-stream-type  . starttls)))))			  ;; TODO: stream type from variable
  ;; (mu4e-compose-signature . ,signature))))    ;; TODO: optional signature

  (setq mu4e-contexts (read-mail-contexts mu4e-secret-mail-path)))

(use-package mu4e-column-faces
  :hook (mu4e-headers-mode . mu4e-column-faces-mode))

(use-package mu4e-alert
  :after mu4e
  :config
  (setq mu4e-alert-set-default-style 'notifications)
  (mu4e-alert-enable-notifications))

(use-package gnus
  :ensure nil
  :config
  (setq user-mail-address "ibraheem.marhoon@gmail.com"
        user-full-name "Ibraheem Almarhoon")
  (setq gnus-article-html-prefetch-allowed nil
        shr-inhibit-images nil        ;; keep images
        shr-use-fonts nil             ;; disable CSS fonts
        shr-use-colors nil)           ;; disable CSS colors

  ;; Optional: convert HTML to plain text
  (setq mm-text-html-renderer 'shr) ;; still uses shr, but minimal rendering  
  (setq gnus-user-date-format-alist
        '(((gnus-seconds-today) . "%d/%m/%Y %H:%M:%S")
          ((+ 86400 (gnus-seconds-today)) . "%d/%m/%Y %H:%M:%S")
          (t . "%d/%m/%Y %H:%M:%S")))

  (setq gnus-summary-line-format
        "%U%R%z%I %&user-date; %(%[%4L: %-23,23f%]%) %s\n")

  (setq gnus-select-method '(nnnil nil))
  (setq gnus-secondary-select-methods
        '((nnimap "gmeb2"
                  (nnimap-address "imap.gmail.com"))
          (nnimap "gmibm"
                  (nnimap-address "imap.gmail.com"))
          (nnimap "gmozb"
                  (nnimap-address "imap.gmail.com"))
          (nnimap "gmozm"
                  (nnimap-address "imap.gmail.com"))
          (nnimap "gmtwi"
                  (nnimap-address "imap.gmail.com"))
          (nnimap "gmblb"
                  (nnimap-address "imap.gmail.com"))))
  ;; (nnimap "rsibm"
  ;;         (nnimap-address "imap.mail.ovh.ca:993"))))
  ;; (nnimap "otibm"
  ;;         (nnimap-address "imap-mail.outlook.com"))
  ;; (nnimap "ottwi"
  ;;         (nnimap-address "imap-mail.outlook.com"))))

  (setq smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
  
  :bind (:map gnus-summary-mode-map
              ("c-j" . gnus-summary-next-article)
              ("c-k" . gnus-summary-prev-article)
              ("RET" . nil)))
  
  ;; Reply to mails with matching email address
  ;; (setq gnus-posting-styles
  ;;       '((".*" ; Matches all groups of messages
  ;;          (address "Nicolas Cavigneaux <home@gmail.com>"))
  ;;         ("work" ; Matches Gnus group called "work"
  ;;          (address "Nicolas Cavigneaux <work@gmail.com>")
  ;;          (organization "Corp")
  ;;          (signature-file "~/.signature-work")
  ;;          ("X-Message-SMTP-Method" "smtp smtp.gmail.com 587 work@gmail.com")))))


(provide 'oz-mail)
;;; oz-mail.el ends here
