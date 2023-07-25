;;; evil-collection-mingus.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Ibraheem Almarhoon
;;
;; Author: Ibraheem Almarhoon <ibraheem.marhoon@gmail.com>
;; Maintainer: Ibraheem Almarhoon <ibraheem.marhoon@gmail.com>
;; Created: July 22, 2023
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, mingus, media

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Evil bindings for Mingus.

;;; Code:
(require 'mingus)
(require 'evil-collection)

(defconst evil-collection-mingus-mode-maps '(mingus-playlist-mode-map mingus-browse-mode-map))

;;;###autoload
(defun evil-collection-mingus-setup ()
  "Set up `evil' bindings for `mingus'."
  (evil-collection-define-key 'normal 'mingus-playlist-mode-map
    "q" 'mingus-git-out
    "p" 'mingus-toggle
    "J" 'mingus-move-down
    "K" 'mingus-move-up
    "h" 'mingus-seek-backward
    "l" 'mingus-seek
    "H" 'mingus-next
    "L" 'mingus-prev
    "[" 'mingus-vol-up
    "]" 'mingus-vol-down
    "<" 'mingus-prev
    ">" 'mingus-next
    "?" 'mingus-help
    "C" 'mingus-clear
    "g" 'mingus-refresh
    "W" 'mingus-save-playlist
    "o" 'mingus-open-playlist
    "D" 'mingus-remove-playlist

    "m" 'mingus-mark
    "u" 'mingus-unmark

    "bb" 'mingus-browse

    (kbd "RET") 'mingus-play)

  (evil-collection-define-key 'normal 'mingus-browse-mode-map
    "q" 'mingus-git-out
    "^" 'mingus-open-parent
    (kbd "RET") 'mingus-down-dir-or-play-song))

(provide 'evil-collection-mingus)
;;; evil-collection-mingus.el ends here
