;;; Code:

;; music player
(use-package emms
  :ensure nil
  :init
  (require 'emms-setup)
  (require 'emms-mark)
  (emms-all)
  (add-to-list 'emms-info-functions 'emms-info-mpd)
  (add-to-list 'emms-player-list    'emms-player-mpd)
  (emms-player-mpd-sync-from-mpd)
  (emms-player-mpd-connect)
  (emms-mode-line-disable)
  :custom
  ((emms-source-file-default-directory (expand-file-name "~/Music"))
   (emms-player-mpd-server-name "localhost")
   (emms-player-mpd-server-port "6600")
   (emms-player-mpd-music-directory (expand-file-name "~/Music"))
   (emms-browser-thumbnail-small-size 64)
   (emms-browser-thumbnail-medium-size 128)
   (emms-browser-covers #'emms-browser-cache-thumbnail-async)
   (emms-playlist-default-major-mode 'emms-mark-mode)))

(use-package mpc
  :ensure nil)

;; (use-package mingus
;;   :init
;;   (setq mingus-mpd-port 6600
;;         mingus-mpd-host "localhost"
;;         mingus-mpd-playlist-dir "~/Music/playlists")
;;    (evil-collection-define-key 'normal 'mingus-playlist-mode-map
;;     "q" 'mingus-git-out
;;     "p" 'mingus-toggle
;;     "J" 'mingus-move-down
;;     "K" 'mingus-move-up
;;     "h" 'mingus-seek-backward
;;     "l" 'mingus-seek
;;     "H" 'mingus-next
;;     "L" 'mingus-prev
;;     "[" 'mingus-vol-up
;;     "]" 'mingus-vol-down
;;     "<" 'mingus-prev
;;     ">" 'mingus-next
;;     "?" 'mingus-help
;;     "c" 'mingus-clear
;;     "C" 'mingus-remove-playlist
;;     "g" 'mingus-refresh
;;     "W" 'mingus-save-playlist
;;     "o" 'mingus-load-playlist-and-play
;;     "d" 'mingus-del
;;     "D" 'mingus-del-marked

;;     "m" 'mingus-mark
;;     "u" 'mingus-unmark

;;     "bb" 'mingus-browse

;;     (kbd "RET") 'mingus-play)

;;   (evil-collection-define-key 'normal 'mingus-browse-mode-map
;;     "q" 'mingus-git-out
;;     "^" 'mingus-open-parent
;;     (kbd "RET") 'mingus-down-dir-or-play-song))

(provide 'oz-media)
;;; oz-media.el ends here
