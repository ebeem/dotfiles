;;; Code:
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
  :ensure t
  :after erc
  :config
  (add-to-list 'erc-modules 'hl-nicks))

(use-package erc-image
  :ensure t
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
