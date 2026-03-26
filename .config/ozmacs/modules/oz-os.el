;;; Code:

(add-to-list 'load-path "~/workspace/emacs/topel")
(add-to-list 'load-path "~/workspace/emacs/bluel")
(add-to-list 'load-path "~/workspace/emacs/duelzer")
(add-to-list 'load-path "~/workspace/emacs/skyel")
(add-to-list 'load-path "~/workspace/emacs/ytbel")
(add-to-list 'load-path "~/workspace/emacs/empdel")
(add-to-list 'load-path "~/workspace/emacs/academel")
(add-to-list 'load-path "~/workspace/emacs/typel")

(require 'topel)
(require 'bluel)
(require 'duelzer)
(require 'skyel)
(require 'ytbel)
(require 'empdel)
(require 'academel)
(require 'typel)

(setq academel-progress-file (expand-file-name ".cache/academel-progress.json" user-emacs-directory)
	  typel-stats-file (expand-file-name ".cache/typelstats.el" user-emacs-directory)
	  ytbel-history-file (expand-file-name ".cache/ytbel-history.el" user-emacs-directory)
	  ytbel-subscriptions-file (expand-file-name ".cache/ytbel-subscriptions.el" user-emacs-directory)
	  ytbel-bookmarks-file (expand-file-name ".cache/ytbel-bookmarks.el" user-emacs-directory)
	  skyel-use-nerd-icons t
	  skyel-locations
	  '(("Dammam"      26.2859   50.2655  "Asia/Riyadh")
		("New York"    40.7128  -74.0060  "America/New_York")
		("London"      51.5074   -0.1278  "Europe/London")))

(provide 'oz-os)
;;; oz-os.el ends here
