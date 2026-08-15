;;; Code:
(use-package notmuch
  :ensure t
  :defer t
  :commands (notmuch notmuch-mua-new-mail)
  :config
  (setq notmuch-show-logo nil
        notmuch-column-control 1.0
        notmuch-hello-auto-refresh t
        notmuch-hello-recent-searches-max 20
        notmuch-hello-thousands-separator ""
        notmuch-hello-sections '(notmuch-hello-insert-saved-searches)
        notmuch-show-all-tags-list t)

  (setq notmuch-search-oldest-first nil)
  (setq notmuch-search-result-format
        '(("date" . "%12s  ")
          ("count" . "%-7s  ")
          ("authors" . "%-20s  ")
          ("subject" . "%-80s  ")
          ("tags" . "(%s)")))
  (setq notmuch-tree-result-format
        '(("date" . "%12s  ")
          ("authors" . "%-20s  ")
          ((("tree" . "%s")
            ("subject" . "%s"))
           . " %-80s  ")
          ("tags" . "(%s)")))
  (setq notmuch-search-line-faces
        '(("unread" . notmuch-search-unread-face)
            ("flag" . italic)))
  (setq notmuch-show-empty-saved-searches t)

  (setq notmuch-saved-searches
        `(( :name "📥 inbox"
            :query "tag:inbox"
            :sort-order newest-first
            :key ,(kbd "i"))
          ( :name "💬 all unread (inbox)"
            :query "tag:unread and tag:inbox"
            :sort-order newest-first
            :key ,(kbd "u"))
          ( :name "🛠️ unread git"
            :query "tag:unread and tag:git"
            :sort-order newest-first
            :key ,(kbd "p"))))

  (setq notmuch-archive-tags nil
        notmuch-message-replied-tags '("+replied")
        notmuch-message-forwarded-tags '("+forwarded")
        notmuch-show-mark-read-tags '("-unread")
        notmuch-draft-tags '("+draft")
        notmuch-draft-folder "drafts"
        notmuch-draft-save-plaintext 'ask)

  (setq notmuch-tag-formats
        '(("unread" (propertize tag 'face 'notmuch-tag-unread))
          ("flag" (propertize tag 'face 'notmuch-tag-flagged)
           (concat tag "🚩")))
        notmuch-tag-deleted-formats
        '(("unread" (notmuch-apply-face bare-tag 'notmuch-tag-deleted)
           (concat "👁️‍🗨️" tag))
          (".*" (notmuch-apply-face tag 'notmuch-tag-deleted)
           (concat "🚫" tag)))
        notmuch-tag-added-formats
        '(("del" (notmuch-apply-face tag 'notmuch-tag-added)
           (concat "💥" tag))
          (".*" (notmuch-apply-face tag 'notmuch-tag-added)
           (concat "🏷️" tag))))

  (setq notmuch-mua-compose-in 'current-window)
  (setq notmuch-mua-hidden-headers nil)
  (setq notmuch-address-command 'internal)
  (setq notmuch-address-use-company nil)
  (setq notmuch-always-prompt-for-sender t)
  (setq notmuch-mua-cite-function 'message-cite-original-without-signature)
  (setq notmuch-mua-reply-insert-header-p-function 'notmuch-show-reply-insert-header-p-never)
  (setq notmuch-mua-user-agent-function nil)
  (setq notmuch-maildir-use-notmuch-insert t)
  (setq notmuch-crypto-process-mime t)
  (setq notmuch-crypto-get-keys-asynchronously t)
  (setq notmuch-mua-attachment-regexp   ; see `notmuch-mua-send-hook'
        (concat "\\b\\(attache\?ment\\|attached\\|attach\\|"
                "pi[èe]ce\s+jointe?\\|"
                "συνημμ[εέ]νο\\|επισυν[αά]πτω\\)\\b"))

  (defun eb/notmuch-message-tab ()
    "Override for `message-tab' to enforce header line check.
More specifically, perform address completion when on a relevant header
line, because `message-tab' sometimes (not sure when/how) fails to do
that and instead tries to complete against dictionary entries."
    (interactive nil message-mode)
    (cond
     ((save-excursion
        (goto-char (line-beginning-position))
        (looking-at notmuch-address-completion-headers-regexp))
      (notmuch-address-expand-name)
      ;; Completion was performed; nothing else to do.
      nil)
     (message-tab-body-function (funcall message-tab-body-function))
     (t (funcall (or (lookup-key text-mode-map "\t")
                     (lookup-key global-map "\t")
                     'indent-relative)))))

  (advice-add #'message-tab :override #'eb/notmuch-message-tab)

  (setq notmuch-show-relative-dates t)
  (setq notmuch-show-all-multipart/alternative-parts nil)
  (setq notmuch-show-indent-messages-width 0)
  (setq notmuch-show-indent-multipart nil)
  (setq notmuch-show-part-button-default-action 'notmuch-show-view-part)
  (setq notmuch-show-text/html-blocked-images ".") ; block everything
  (setq notmuch-wash-wrap-lines-length 120)
  (setq notmuch-unthreaded-show-out nil)
  (setq notmuch-message-headers '("To" "Cc" "Subject" "Date"))
  (setq notmuch-message-headers-visible t)

  (let ((count most-positive-fixnum))
    (setq notmuch-wash-citation-lines-prefix count
          notmuch-wash-citation-lines-suffix count))

  :bind
  ( :map global-map
    ("C-x m" . notmuch-mua-new-mail) ; override `compose-mail'
    :map eb/open-map
    ("m" . notmuch)
    :map notmuch-search-mode-map ; I normally don't use the tree view, otherwise check `notmuch-tree-mode-map'
    ("a" . nil) ; the default is too easy to hit accidentally and I do not archive stuff
    ("A" . nil)
    ("/" . notmuch-search-filter) ; alias for l
    ("r" . notmuch-search-reply-to-thread) ; easier to reply to all by default
    ("R" . notmuch-search-reply-to-thread-sender)
    :map notmuch-show-mode-map
    ("a" . nil) ; the default is too easy to hit accidentally and I do not archive stuff
    ("A" . nil)
    ("r" . notmuch-show-reply) ; easier to reply to all by default
    ("R" . notmuch-show-reply-sender)
    :map notmuch-hello-mode-map
    ("C-<tab>" . nil)))

(use-package gnus
  :defer t
  :init
  (defvar gnus-unread-mails-count 0)

  :ensure nil
  :config
  ;; watch mail directory for changes
  (require 'notifications)
  (add-hook 'gnus-after-getting-new-news-hook #'gnus-notify-unread-inbox)

  ;; notify when there are unread mails
  (defun gnus-notify-unread-inbox ()
  "Notify if there are unread messages in the Gnus topic named 'Inbox'."
  (interactive)
  (let ((groups (gnus-topic-find-groups "Inbox"))
      (total 0)
      (unread-groups '()))
  (dolist (group groups)
    (let* ((name (gnus-info-group (car (cdr group))))
           (unread (gnus-group-unread name)))
      (when (numberp unread)
        (push group unread-groups)
        (setq total (+ total unread)))))
  (setq gnus-unread-mails-count total)
  (when (> total 0)
    (notifications-notify
     :title "📬 New Mail"
     :body (format "You have %d unread message%s"
                   total
                   (if (= total 1) "" "s"))
     :app-name "Emacs Gnus"
     :urgency 'normal))))
  
  (setq user-mail-address "ibraheem.marhoon@gmail.com"
        user-full-name "Ibraheem Almarhoon")
  (setq shr-inhibit-images t          ;; images
        shr-use-fonts nil             ;; disable CSS fonts
        shr-use-colors nil)           ;; disable CSS colors

  ;; Optional: convert HTML to plain text
  (setq mm-text-html-renderer 'shr) ;; still uses shr, but minimal rendering  
  (setq gnus-user-date-format-alist
        '(((gnus-seconds-today) . "%d/%m/%Y %H:%M:%S")
          ((+ 86400 (gnus-seconds-today)) . "%d/%m/%Y %H:%M:%S")
          (t . "%d/%m/%Y %H:%M:%S")))

  (setq gnus-topic-display-empty-topics t) ;; hides empty topics
  (setq gnus-topic-save-on-exit t)           ;; save layout on exit
  (setq gnus-permanently-visible-groups ".*")
  (setq gnus-summary-line-format
        "%U%R%z%I %&user-date; %(%[%4L: %-23,23f%]%) %s\n")

  ;; (add-hook 'gnus-summary-prepare-hook 'gnus-summary-sort-by-most-recent-date)
  (setq gnus-asynchronous t)
  (setq gnus-use-article-prefetch 15)
  (setq gnus-summary-ignore-duplicates t)
  (setq gnus-summary-goto-unread nil)
  (setq gnus-thread-sort-functions
        '((not gnus-thread-sort-by-date)
          (not gnus-thread-sort-by-number)))
  (setq gnus-subthread-sort-functions
        'gnus-thread-sort-by-date)
  (setq gnus-thread-hide-subtree nil)
  (setq gnus-thread-ignore-subject nil)
  (setq gnus-user-date-format-alist
        '(((gnus-seconds-today) . "Today at %R")
          ((+ (* 60 60 24) (gnus-seconds-today)) . "Yesterday, %R")
          (t . "%d/%m/%y %R")))
  
  (setq gnus-summary-line-format "%U%R %-18,18&user-date; %4L:%-25,25f %B%s\n")
  (setq gnus-group-line-format "%M%p%P%5y:%B%(%g%)\n")
  
  (setq gnus-sum-thread-tree-indent " "
        gnus-sum-thread-tree-root "└> "
        gnus-sum-thread-tree-false-root "└> "
        gnus-sum-thread-tree-single-indent " "
        gnus-sum-thread-tree-leaf-with-other "└> "
        gnus-sum-thread-tree-single-leaf "└> "
        gnus-sum-thread-tree-vertical "| "
        gnus-sum-thread-tree-horizontal "- "
        gnus-sum-thread-tree-down "└> ")
  
  (setq gnus-group-sort-function
        '((gnus-group-sort-by-unread)
          (gnus-group-sort-by-alphabet)
          (gnus-group-sort-by-rank)))
  
  (setq gnus-inhibit-images t)
  (setq gnus-article-sort-functions
        '((not gnus-article-sort-by-number)
          (not gnus-article-sort-by-date)))
  
  (setq gnus-select-method '(nnnil nil))
  (setq gnus-secondary-select-methods
        '((nnmaildir "gmeb2"
                     (directory "~/.mail/gmeb2/[Gmail]")
                     (get-new-mail t))
          (nnmaildir "gmibm"
                     (directory "~/.mail/gmibm/[Gmail]")
                     (get-new-mail t))
          (nnmaildir "gmozb"
                     (directory "~/.mail/gmozb/[Gmail]")
                     (get-new-mail t))
          (nnmaildir "gmozm"
                     (directory "~/.mail/gmozm/[Gmail]")
                     (get-new-mail t))
          (nnmaildir "gmtwi"
                     (directory "~/.mail/gmtwi/[Gmail]")
                     (get-new-mail t))
          (nnmaildir "gmblm"
                     (directory "~/.mail/gmblm/[Gmail]")
                     (get-new-mail t))
          (nnmaildir "ozbif"
                     (directory "~/.mail/ozbif")
                     (get-new-mail t))))
        
        ;; '((nnimap "gmeb2"
        ;;           (nnimap-address "imap.gmail.com"))
        ;;   (nnimap "gmibm"
        ;;           (nnimap-address "imap.gmail.com"))
        ;;   (nnimap "gmozb"
        ;;           (nnimap-address "imap.gmail.com"))
        ;;   (nnimap "gmozm"
        ;;           (nnimap-address "imap.gmail.com"))
        ;;   (nnimap "gmtwi"
        ;;           (nnimap-address "imap.gmail.com"))
        ;;   (nnimap "gmblb"
        ;;           (nnimap-address "imap.gmail.com"))
        ;;   (nnimap "ozbif"
        ;;           (nnimap-address "imappro.zoho.com"))))
  
  ;; (nnimap "rsibm"
  ;;         (nnimap-address "imap.mail.ovh.ca:993"))))
  ;; (nnimap "otibm"
  ;;         (nnimap-address "imap-mail.outlook.com"))
  ;; (nnimap "ottwi"
  ;;         (nnimap-address "imap-mail.outlook.com"))))

  (setq smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
  
  :hook (gnus-group-mode . gnus-topic-mode)
  :bind (:map gnus-summary-mode-map
              ("c-j" . gnus-summary-next-article)
              ("c-k" . gnus-summary-prev-article)
              ("RET" . nil)))

(provide 'oz-mail)
;;; oz-mail.el ends here
