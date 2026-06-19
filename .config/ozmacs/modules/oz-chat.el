;;; Code:
;; (use-package plz
;;   :ensure (:host github :repo "alphapapa/plz.el"))

(use-package ement  
  :ensure t
  :commands (ement-connect)
  :bind (:map ement-room-mode-map
              ("C-p" . my-ement-previous-line-or-retro))
  :config
  (defun my-ement-filter-noisy-events (orig-fun event &rest args)
    "Filter out membership and state events to keep Ement rooms compact.
Allows only messages, encrypted messages, reactions, and redactions."
    (let ((type (ement-event-type event)))
      ;; replies and edits are just "m.room.message" under the hood,
      ;; so they will automatically be preserved by this check.
      (if (member type '("m.room.message" 
                         "m.room.encrypted" 
                         "m.reaction" 
                         "m.room.redaction"))
          (apply orig-fun event args)
        nil)))
  (defun my-ement-previous-line-or-retro (&optional arg try-vscroll)
    "Move to the previous line. If at the top of the buffer, load history."
    (interactive "^p\np")
    (condition-case nil
        ;; attempt to move up normally
        (previous-line arg try-vscroll)
      ;; catch the error when you hit the top of the buffer
      (beginning-of-buffer
       (message "Fetching older messages...")
       (call-interactively #'ement-room-retro))))
    (advice-add 'ement-room--insert-event :around #'my-ement-filter-noisy-events))

(use-package erc
  :ensure nil
  :init
  (defun eb/connect-irc ()
    "Connect to irc using password store."
    (interactive)
    (erc-tls
     :server "irc.libera.chat"
     :port 6697
     :nick "ebeem"
     :password (password-store-get "IRC/irc.libera.chat")))
  (defun eb/connect-znc ()
    "Connect to irc using password store."
    (interactive)
    (erc
     :server (password-store-get "IRC/irc.libera.chat.server")
     :port 1200
     :nick "ebeem"
	 :user "ebeem/libera"
     :password (password-store-get "IRC/irc.libera.chat.password")))

  :config
  (add-to-list 'erc-modules 'notifications)
  (setq erc-fill-function 'erc-fill-static
        erc-fill-static-center 20
        erc-hide-list '("JOIN" "NICK" "PART" "QUIT" "MODE" "AWAY")))

(use-package password-store
  :ensure t
  :defer t)

(use-package erc-hl-nicks
  :after erc
  :config
  (add-to-list 'erc-modules 'hl-nicks))

(use-package erc-image
  :after erc
  :config
  (setq erc-image-inline-rescale 300)
  (add-to-list 'erc-modules 'image))

(use-package mastodon
  :ensure t
  :defer t
  :commands (mastodon)
  :config
  (setq mastodon-instance-url "https://mastodon.social"
        mastodon-active-user "ebeem"))

;; TODO: whatsapp client
;; https://github.com/tulir/whatsmeow
;; https://github.com/asternic/wuzapi
;; https://codeberg.org/berkeley/whatsappel

;; TODO: discord client
;; maybe ditch it

(provide 'oz-chat)
;;; oz-chat.el ends here
