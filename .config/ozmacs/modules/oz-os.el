;;; Code:

(add-to-list 'load-path "~/workspace/emacs/topel")
(add-to-list 'load-path "~/workspace/emacs/bluel")
(add-to-list 'load-path "~/workspace/emacs/duelzer")
(add-to-list 'load-path "~/workspace/emacs/dmenuel")
(add-to-list 'load-path "~/workspace/emacs/skyel")
(add-to-list 'load-path "~/workspace/emacs/ytbel")
(add-to-list 'load-path "~/workspace/emacs/academel")
(add-to-list 'load-path "~/workspace/emacs/typel")

(require 'topel)
(require 'bluel)
(require 'duelzer)
(require 'dmenuel)
(require 'skyel)
(require 'ytbel)
(require 'academel)
(require 'typel)

(setq academel-progress-file (expand-file-name ".cache/academel-progress.json" user-emacs-directory)
	  dmenuel-app-history-file (expand-file-name ".cache/dmmenuel-app-history.el" user-emacs-directory)
	  typel-stats-file (expand-file-name ".cache/typelstats.el" user-emacs-directory)
	  ytbel-history-file (expand-file-name ".cache/ytbel-history.el" user-emacs-directory)
	  ytbel-subscriptions-file (expand-file-name ".cache/ytbel-subscriptions.el" user-emacs-directory)
	  ytbel-bookmarks-file (expand-file-name ".cache/ytbel-bookmarks.el" user-emacs-directory)
	  skyel-use-nerd-icons t
	  skyel-locations
	  '(("Dammam"      26.2859   50.2655  "Asia/Riyadh")
		("New York"    40.7128  -74.0060  "America/New_York")
		("London"      51.5074   -0.1278  "Europe/London")))


;; (add-to-list 'load-path "~/workspace/emacs/empdel")
;; (require 'empdel)

(use-package empdel
  :ensure t
  :vc (:url "https://codeberg.org/ebeem/empdel"))

(add-to-list 'load-path "~/workspace/emacs/erc-history")
(use-package erc-history
  :after erc
  :ensure t
;  :vc (:url "https://github.com/ebeem/erc-history")
  :hook (erc-mode . erc-history-mode)
  :init
  (defun erc-history-ubuntu-message-parser (msg)
  "Parse a chat log MSG and return a list of (time nickname message).
example: [23:22] <Bashing-om> UWN: Opening 842 for Saturday."
  (let ((regex "\\[\\([0-9:]+\\)\\] <\\([^>]+\\)> \\(.*\\)"))
    (when (string-match regex msg)
      (let* ((time (match-string 1 msg))
            (nick (match-string 2 msg))
            (content (match-string 3 msg))
            (full-date (format-time-string
                        (concat "%Y-%m-%dT" time ":00+0000")
                        erc-history-last-pulled-date)))
        (list (encode-time (parse-time-string full-date))
              nick
              content)))))

  (setq erc-history-sources
        ;; my personal logs
        '(;; ubuntu logs
          ("https://irclogs.ubuntu.com/%Y/%m/%d/#CHANNEL#.txt"
           ("#cloud-init" "#kubuntu-devel" "#kubuntu"
            "#launchpad-dev" "#launchpad" "#lubuntu-devel"
            "#lubuntu" "#maas" "#mir-server" "#netplan"
            "#snappy" "#ubports" "#ubuntu-au" "#ubuntu-bd"
            "#ubuntu-bugs" "#ubuntu-community-team" "#ubuntu-de"
            "#ubuntu-desktop" "#ubuntu-devel" "#ubuntu-discuss"
            "#ubuntu-doc" "#ubuntu-es" "#ubuntu-hr" "#ubuntu-ir"
            "#ubuntu-irc" "#ubuntu-it" "#ubuntu-kernel" "#ubuntu-kr"
            "#ubuntu-lt" "#ubuntu-mate" "#ubuntu-meeting" "#ubuntu-mirrors"
            "#ubuntu-news" "#ubuntu-next" "#ubuntu-nl" "#ubuntu-on-air"
            "#ubuntu-ops" "#ubuntu-pl" "#ubuntu-qt" "#ubuntu-quality"
            "#ubuntu-release" "#ubuntu-ru" "#ubuntu-sa" "#ubuntu-security"
            "#ubuntu-server" "#ubuntu-tw" "#ubuntu-uk" "#ubuntu-us-mi"
            "#ubuntu-us-oh" "#ubuntu-us-pa" "#ubuntu" "#ubuntustudio-devel"
            "#ubuntustudio" "#xubuntu-devel" "#xubuntu")
           erc-history-ubuntu-message-parser))))

(provide 'oz-os)
;;; oz-os.el ends here
