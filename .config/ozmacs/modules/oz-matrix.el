

;;; Code:
;; (use-package plz
;;   :ensure (:host github :repo "alphapapa/plz.el"))

;; (use-package ement
;;   :after plz
;;   :commands (ement-connect)
;;   :ensure (:host github :repo "alphapapa/ement.el")
;;   :hook (ement-room-view . (lambda (room session) (ement-view-mode-enhanced)))
;;   :init
;;   (with-eval-after-load 'savehist
;;       (defun ement--savehist-save-hook ()
;;         "Remove all `ement-' commands from `command-history'.
;;     Because when `savehist' saves `command-history', it includes the
;;     interactive arguments passed to the command, which in our case
;;     includes large data structures that should never be persisted!"
;;         (setf command-history
;;               (cl-remove-if (pcase-lambda (`(,command . ,_))
;;                               (string-match-p (rx bos "ement-") (symbol-name command)))
;;                             command-history)))
;;       (add-hook 'begining-of-buffer-hook (lambda () (ement-room-mwheel-scroll)))
;;       (cl-pushnew 'ement--savehist-save-hook savehist-save-hook))
;;   (setq ement-save-sessions t)
;;   :config
;;   (defun ement-view-mode-enhanced ()
;;     (setq-local visual-fill-column-width 150
;;                 visual-fill-column-center-text t)
;;     ;; (variable-pitch-mode t)
;;     (visual-fill-column-mode 1))
;;   (setq ement-room-message-format-spec "%S%L:\s%B%r%R%t"
;;         ement-room-left-margin-width 10
;;         ement-room-right-margin-width 0))

(use-package erc
  :ensure nil
  :config
  (add-to-list 'erc-modules 'notifications)
  (setq erc-fill-function 'erc-fill-static
      erc-fill-static-center 20
      erc-hide-list '("JOIN" "NICK" "PART" "QUIT" "MODE" "AWAY")))

;; (defun erc-history-ubuntu-message-parser (msg)
;; "Parse a chat log MSG and return a list of (time nickname message).
;; example: [23:22] <Bashing-om> UWN: Opening 842 for Saturday."
;; (let ((regex "\\[\\([0-9:]+\\)\\] <\\([^>]+\\)> \\(.*\\)"))
;;   (when (string-match regex msg)
;;     (let* ((time (match-string 1 msg))
;;           (nick (match-string 2 msg))
;;           (content (match-string 3 msg))
;;           (full-date (format-time-string
;;                       (concat "%Y-%m-%dT" time ":00+0000")
;;                       erc-history-last-pulled-date)))
;;       (list (encode-time (parse-time-string full-date))
;;             nick
;;             content)))))

;; (use-package erc-history
;;   :after erc
;;   :ensure (:host github :repo "ebeem/erc-history")
;;   ;; :load-path "~/workspace/emacs/erc-history/"
;;   :hook (erc-mode . erc-history-mode)
;;   :config
;;   (defun erc-history-ubuntu-message-parser (msg)
;;   "Parse a chat log MSG and return a list of (time nickname message).
;; example: [23:22] <Bashing-om> UWN: Opening 842 for Saturday."
;;   (let ((regex "\\[\\([0-9:]+\\)\\] <\\([^>]+\\)> \\(.*\\)"))
;;     (when (string-match regex msg)
;;       (let* ((time (match-string 1 msg))
;;             (nick (match-string 2 msg))
;;             (content (match-string 3 msg))
;;             (full-date (format-time-string
;;                         (concat "%Y-%m-%dT" time ":00+0000")
;;                         erc-history-last-pulled-date)))
;;         (list (encode-time (parse-time-string full-date))
;;               nick
;;               content)))))

  ;; (setq erc-history-sources
  ;;       ;; my personal logs
  ;;       '(("http://myhost/grc-history/#CHANNEL#/%Y/%m/%d.txt"
  ;;          ("#systemcrafters" "#erc-history"))

  ;;         ;; ubuntu logs
  ;;         ("https://irclogs.ubuntu.com/%Y/%m/%d/#CHANNEL#.txt"
  ;;          ("#cloud-init" "#kubuntu-devel" "#kubuntu"
  ;;           "#launchpad-dev" "#launchpad" "#lubuntu-devel"
  ;;           "#lubuntu" "#maas" "#mir-server" "#netplan"
  ;;           "#snappy" "#ubports" "#ubuntu-au" "#ubuntu-bd"
  ;;           "#ubuntu-bugs" "#ubuntu-community-team" "#ubuntu-de"
  ;;           "#ubuntu-desktop" "#ubuntu-devel" "#ubuntu-discuss"
  ;;           "#ubuntu-doc" "#ubuntu-es" "#ubuntu-hr" "#ubuntu-ir"
  ;;           "#ubuntu-irc" "#ubuntu-it" "#ubuntu-kernel" "#ubuntu-kr"
  ;;           "#ubuntu-lt" "#ubuntu-mate" "#ubuntu-meeting" "#ubuntu-mirrors"
  ;;           "#ubuntu-news" "#ubuntu-next" "#ubuntu-nl" "#ubuntu-on-air"
  ;;           "#ubuntu-ops" "#ubuntu-pl" "#ubuntu-qt" "#ubuntu-quality"
  ;;           "#ubuntu-release" "#ubuntu-ru" "#ubuntu-sa" "#ubuntu-security"
  ;;           "#ubuntu-server" "#ubuntu-tw" "#ubuntu-uk" "#ubuntu-us-mi"
  ;;           "#ubuntu-us-oh" "#ubuntu-us-pa" "#ubuntu" "#ubuntustudio-devel"
  ;;           "#ubuntustudio" "#xubuntu-devel" "#xubuntu")
  ;;          erc-history-ubuntu-message-parser))))

(use-package password-store)

(defun eb/connect-irc ()
  "Connect to irc using password store."
  (interactive)
  (erc-tls
      :server "irc.libera.chat"
      :port 6697
      :nick "ebeem"
      :password (password-store-get "IRC/irc.libera.chat")))

(use-package erc-hl-nicks
  :after erc
  :config
  (add-to-list 'erc-modules 'hl-nicks))

(use-package erc-image
  :after erc
  :config
  (setq erc-image-inline-rescale 300)
  (add-to-list 'erc-modules 'image))

(provide 'oz-matrix)
;;; oz-matrix.el ends here
