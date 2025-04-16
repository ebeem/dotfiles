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

  (defun elfeed-reload-db ()
      "Reload database."
      (interactive)
      (elfeed-db-load))

  ;; override some ui behaviors
  (defun elfeed-kill-buffer ()
      "Kill the current buffer."
      (interactive)
      (other-window -1)
      (kill-buffer (other-buffer))
      (delete-other-windows))

  (defun elfeed-show-entry (entry)
    "Display ENTRY in the current buffer."
    (let ((title (elfeed-entry-title entry)))
      (split-window-below)
      (other-window 1)
      (switch-to-buffer (get-buffer-create (format "*elfeed %s*" title)))
      (unless (eq major-mode 'elfeed-show-mode)
        (elfeed-show-mode))
      (setq elfeed-show-entry entry)
      (enlarge-window 6)
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

;; override some ui behaviors
(defun elfeed-kill-buffer ()
  "Kill the current buffer and restore the previous one."
  (interactive)
  (kill-buffer)
  (when (one-window-p)
    (switch-to-buffer (other-buffer)))
  (delete-other-windows))

(defun elfeed-show-entry (entry)
  "Display ENTRY in a new window."
  (let ((title (elfeed-entry-title entry)))
    (split-window-below) ;; or use split-window-right for horizontal split
    (other-window 1)
    (switch-to-buffer (get-buffer-create (format "*elfeed %s*" title)))
    (unless (eq major-mode 'elfeed-show-mode)
      (elfeed-show-mode))
    (setq elfeed-show-entry entry)
    (resize-window-vertically 8)
    (elfeed-view-mode-enhanced)
    (elfeed-show-refresh)))

(defun resize-window-vertically (delta)
  "Resize the current window vertically by DELTA lines."
  (enlarge-window delta))

(defun elfeed-view-mode-enhanced ()
  (display-line-numbers-mode -1)
  (setq-local truncate-lines nil
              visual-fill-column-width 120
              visual-fill-column-center-text t
              default-text-properties '(line-height 1.1))
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t))
    (visual-fill-column-mode)
    (set-buffer-modified-p nil)))

(use-package elfeed-org
  :after elfeed
  :init
  (elfeed-org)
  :config
  (setq rmh-elfeed-org-files (list (expand-file-name "elfeed.org" user-emacs-directory))))

;; TODO: configure newsticker based on elfeed's configuration
;; configure the sources and gruops from elfeed.org
(use-package newsticker
  :ensure nil
  :config
  (setq newsticker-url-list-defaults nil
        newsticker-treeview-listwindow-height 13
        newsticker-retrieval-method 'extern
        newsticker-automatically-mark-items-as-old t
        newsticker-automatically-mark-visited-items-as-old t
        newsticker-retrieval-interval 600
        newsticker-groups '("Linux" "Data Science" "Godot" "Games")
        newsticker-url-list
        '(("Arch Linux" "https://archlinux.org/feeds/news/" nil nil nil)
          ("Its Foss" "https://itsfoss.com/feed/" nil nil nil)
          ("Cyber Citi" "https://www.cyberciti.com/faq/feed/" nil nil nil)
          ("Data Science" "http://feeds.feedburner.com/FeaturedBlogPosts-DataScienceCentral" nil nil nil)
          ("Data Science" "https://towardsdatascience.com/feed" nil nil nil)
          ("Hacking News" "https://latesthackingnews.com/feed/" nil nil nil)
          ("The Hacker News" "http://feeds.feedburner.com/TheHackersNews" nil nil nil)
          ("Godot" "https://godotengine.org/rss.xml" nil nil nil)
          ("Games" "https://store.steampowered.com/feeds/news/app/3443650/?cc=SA&l=englis" nil nil nil)))
  
  (with-eval-after-load 'newsticker
    (define-key newsticker-treeview-item-mode-map (kbd "C-j") #'newsticker-treeview-next-item)
    (define-key newsticker-treeview-item-mode-map (kbd "C-k") #'newsticker-treeview-prev-item)))
    
(provide 'oz-news)
;;; oz-news.el ends here
