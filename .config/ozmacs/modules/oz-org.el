;;; Code:

(use-package toc-org ; auto-table of contents
  :hook (org-mode . toc-org-enable)
  :config
  (setq toc-org-hrefify-default "gh"))

(use-package org-modern ; modern org
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-auto-align-tags nil
	org-tags-column 0
	org-catch-invisible-edits 'show-and-error
	org-special-ctrl-a/e t
	org-insert-heading-respect-content t

	;; Org styling, hide markup etc.
	org-hide-emphasis-markers t
	org-pretty-entities t
	org-ellipsis "…"

	;; Agenda styling
	org-agenda-tags-column 0
	org-agenda-block-separator ?─
	org-agenda-time-grid
	'((daily today require-timed)
	  (800 1000 1200 1400 1600 1800 2000)
	  " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
	org-agenda-current-time-string
	"⭠ now ─────────────────────────────────────────────────"))

;; Org-transclusion lets you insert a copy of text content via a file
;; link or ID link within an Org file. It lets you have the same content
;; present in different buffers at the same time without copy-and-pasting it.
(use-package org-transclusion
  :after org)

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/cloud/"))
  (org-roam-db-location (expand-file-name ".cache/org-roam.db" user-emacs-directory))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol))

;; TODO
;; https://github.com/alphapapa/org-super-agenda
;; https://github.com/alphapapa/org-ql
;; https://github.com/fniessen/org-html-themes
;; https://github.com/abo-abo/org-download

;; horizontally scroll org mode tables
(use-package org-phscroll
  :elpaca (:host github :repo "misohena/phscroll")
  :after org
  :init
  (setq org-startup-truncated nil)
  (with-eval-after-load "org"
    (require 'org-phscroll)))

(with-eval-after-load 'org-faces (dolist (
	 face '((org-level-1 . 1.1)
                (org-level-2 . 1.1)
                (org-level-3 . 1.1)
                (org-level-4 . 1.1)
                (org-level-5 . 1.05)
                (org-level-6 . 1.05)
                (org-level-7 . 1.05)
                (org-level-8 . 1.05)))
  (set-face-attribute (car face) nil :font "Jetbrains Mono" :weight 'bold :height (cdr face))))

;; auto tangle org-mode files
(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

(provide 'oz-org)
;;; oz-code.el ends here
