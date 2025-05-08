;;; Code:

;; (use-package toc-org ; auto-table of contents
;;   :hook (org-mode . toc-org-enable)
;;   :config
;;   (setq toc-org-hrefify-default "gh"))

(use-package org-modern ; modern org
  :hook
  (org-mode . +org-mode-enhanced-view)
  :config
  
  (defun +org-mode-enhanced-view ()
    (setq-local truncate-lines nil)
    (org-modern-mode))
  
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

(use-package org-mode
  :ensure nil
  :init
  (which-key-add-key-based-replacements "C-c n" "Note")
  :hook (org-mode . org-indent-mode)
  :bind (("C-c n a" . org-agenda)
         ("C-c n b" . org-babel-tangle)
         ("C-c n e" . org-export-dispatch)
         ("C-c n i" . org-toggle-item)
         ("C-c n t" . org-todo)
         ("C-c n T" . org-todo-list)
         ("C-c n s" . consult-org-heading)
         ("C-c n d" . org-time-stamp)))

(use-package denote
    :bind (("C-c n f" . denote-open-or-create)))

;; TODO
;; https://github.com/alphapapa/org-super-agenda
;; https://github.com/alphapapa/org-ql
;; https://github.com/fniessen/org-html-themes
;; https://github.com/abo-abo/org-download

;; horizontally scroll org mode tables
;; (use-package org-phscroll
;;   :ensure (:host github :repo "misohena/phscroll")
;;   :after org
;;   :init
;;   (setq org-startup-truncated nil)
;;   (with-eval-after-load "org"
;;     (require 'org-phscroll)))

;; org mode presentations
(use-package org-present
  :after org
  :hook (
         (org-mode . eb/org-mode-start)
         (org-present-mode . eb/org-present-start)
         (org-present-mode-quit . eb/org-present-end))
  :bind (:map org-present-mode-keymap
              ("C-c C-j" . eb/org-present-next)
              ("C-c C-k" . eb/org-present-prev))
  :init
  (defun eb/org-present-prepare-slide ()
    (org-overview)
    (org-show-entry)
    (org-show-children))

  (defun eb/org-present-start ()
    (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                      (header-line (:height 4.5) variable-pitch)
                                      (org-document-title (:height 1.75) variable-pitch)
                                      (org-code (:height 1.55) fixed-pitch)
                                      (org-verbatim (:height 1.55) variable-pitch)
                                      (org-block (:height 1.25) fixed-pitch)
                                      (org-block-begin-line (:height 0.7) fixed-pitch))
                header-line-format " ")

    (setq header-line-format " ")
    (org-display-inline-images)
    (eb/org-present-prepare-slide)
    (when (fboundp 'eb/kill-panel)
      (eb/kill-panel)))

  (defun eb/org-present-end ()
    (setq-local face-remapping-alist '((default fixed-pitch default))
                header-line-format nil)
    (setq header-line-format nil)
    (org-present-small)
    (org-remove-inline-images)
    (when (fboundp 'eb/start-panel)
      (eb/start-panel)))

  (defun eb/org-mode-start ()
    (setq-local visual-fill-column-width 150
                visual-fill-column-center-text t)
    (visual-fill-column-mode 1))

  (defun eb/org-present-prev ()
    (interactive)
    (org-present-prev)
    (eb/org-present-prepare-slide))

  (defun eb/org-present-next ()
    (interactive)
    (org-present-next)
    (eb/org-present-prepare-slide)
    (when (fboundp 'live-crafter-add-timestamp)
      (live-crafter-add-timestamp (substring-no-properties (org-get-heading t t t t))))))

;; Load org-faces to make sure we can set appropriate faces
(require 'org-faces)

;; Hide emphasis markers on formatted text
(setq org-hide-emphasis-markers t)


;; Make the document title a bit bigger
(set-face-attribute 'org-document-title nil :inherit 'fixed-pitch :weight 'bold :height 1.2)

;; Make sure certain org faces use the fixed-pitch face when variable-pitch-mode is on
(set-face-attribute 'org-block nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

;; auto tangle org-mode files
(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

;; spell checking
;; (use-package jinx
;;   :hook (emacs-startup . global-jinx-mode)
;;   :bind (("M-$" . jinx-correct)
;;          ("C-M-$" . jinx-languages)))
;; Basic ispell setup using use-package

(use-package ispell
  :ensure nil
  :config
  (setq ispell-alternate-dictionary "/usr/share/dict/words"
        ispell-program-name "ispell"
        ispell-dictionary "english"))

(use-package flyspell
  :ensure nil
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :custom
  (flyspell-issue-message-flag nil))

(use-package flyspell-correct
  :bind (("C-c s c" . flyspell-correct-wrapper)))

(provide 'oz-org)
;;; oz-code.el ends here
