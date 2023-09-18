;;; Code:
(use-package elfeed
  :commands (elfeed);
  :hook ((elfeed-show-mode . (lambda () (display-line-numbers-mode -1)))
     (elfeed-search-update . (lambda () (display-line-numbers-mode -1))))
  :config
  (setq elfeed-search-filter "@2-week-ago"
        elfeed-search-title-min-width 80
        elfeed-show-entry-switch #'pop-to-buffer
        shr-max-image-proportion 0.6)

  ;; override some ui behaviors
  (defun elfeed-kill-buffer ()
      "Kill the current buffer."
      (interactive)
      (evil-window-up 1)
      (kill-buffer (other-buffer))
      (delete-other-windows))

  (defun elfeed-show-entry (entry)
    "Display ENTRY in the current buffer."
    (let ((title (elfeed-entry-title entry)))
      (evil-window-split)
      (switch-to-buffer (get-buffer-create (format "*elfeed %s*" title)))
      (unless (eq major-mode 'elfeed-show-mode)
        (elfeed-show-mode))
      (setq elfeed-show-entry entry)
      (evil-window-increase-height 8)
      (elfeed-view-mode-enhanced)
      (elfeed-show-refresh)))

  (defun elfeed-view-mode-enhanced ()
    (display-line-numbers-mode -1)
    (setq-local truncate-lines nil
            visual-fill-column-width 120
            visual-fill-column-center-text t
            default-text-properties '(line-height 1.1))
    (let ((inhibit-read-only t)
      (inhibit-modification-hooks t))
    (visual-fill-column-mode)
    (set-buffer-modified-p nil))))

(use-package elfeed-org
  :after elfeed
  :init
  (elfeed-org)
  :config
  (setq rmh-elfeed-org-files (list (expand-file-name "elfeed.org" user-emacs-directory))))

;; TODO: youtube integration - https://github.com/karthink/elfeed-tube

(provide 'oz-news)
;;; oz-news.el ends here
