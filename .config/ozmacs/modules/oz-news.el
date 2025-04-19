;;; Code:
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
          
          ("Reddit Emacs" "https://www.reddit.com/r/emacs.rss" nil nil nil)
          ("Reddit Godot" "https://www.reddit.com/r/godot.rss" nil nil nil)
          ("Reddit Sway" "https://www.reddit.com/r/swaywm.rss" nil nil nil)
          
          ("Games" "https://store.steampowered.com/feeds/news/app/3443650/?cc=SA&l=englis" nil nil nil)))
  
  (with-eval-after-load 'newsticker
    (define-key newsticker-treeview-item-mode-map (kbd "RET") nil)
    (define-key newsticker-treeview-item-mode-map (kbd "C-j") #'newsticker-treeview-next-item)
    (define-key newsticker-treeview-item-mode-map (kbd "C-k") #'newsticker-treeview-prev-item)))
    
(provide 'oz-news)
;;; oz-news.el ends here
