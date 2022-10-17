;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ibraheem Almarhoon"
      user-mail-address "ibraheem.marhoon@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
      ;; doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 17 :weight 'bold))

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 17 :weight 'bold)
      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 17))
      ;; doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 17 :weight 'bold))

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type `relative)
;; (setq completion-styles `(orderless usbstring))


;;; :ui doom-dashboard
(setq fancy-splash-image (concat doom-private-dir "splash.png"))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; keybindings
;;
(map! (:when (featurep! :ui window-select)
       :g "M-1"   #'winum-select-window-1
       :g "M-2"   #'winum-select-window-2
       :g "M-3"   #'winum-select-window-3
       :g "M-4"   #'winum-select-window-4
       :g "M-5"   #'winum-select-window-5
       :g "M-6"   #'winum-select-window-6
       :g "M-7"   #'winum-select-window-7
       :g "M-8"   #'winum-select-window-8
       :g "M-9"   #'winum-select-window-9
       :g "M-0"   #'winum-select-window-0))

;;; <leader> w --- windows
(map! :leader
      (:prefix-map ("W" . "windows")
       (:when (featurep! :ui window-select)
        :desc "Delete window"             "d" #'delete-window
        :desc "Switch to window 3"        "3" #'winum-select-window-3
        :desc "Switch to window 4"        "4" #'winum-select-window-4
        :desc "Switch to window 5"        "5" #'winum-select-window-5
        :desc "Switch to window 6"        "6" #'winum-select-window-6
        :desc "Switch to window 7"        "7" #'winum-select-window-7
        :desc "Switch to window 8"        "8" #'winum-select-window-8
        :desc "Switch to window 9"        "9" #'winum-select-window-9
        :desc "Switch to window 0"        "0" #'winum-select-window-0)
       :desc "Split Window vertically"    "v" #'split-window-horizontally
       :desc "Split window horizontally"  "h" #'split-window-vertically
       :desc "Create frame"               "f" #'make-frame
       :desc "Undo window config"         "u" #'winner-undo
       :desc "Redo window config"         "U" #'winner-redo)

      (:prefix-map ("w" . "workspaces")
       (:when (featurep! :ui workspaces)
        :desc "Display workspaces"           "d" #'+workspace/display
        :desc "Rename workspace"             "r" #'+workspace/rename
        :desc "Create workspace"             "c" #'+workspace/new
        :desc "Delete workspace"             "k" #'+workspace/delete
        :desc "Save workspace"               "S" #'+workspace/save
        :desc "Switch to other workspace"    "o" #'+workspace/other
        :desc "Switch to left workspace"     "p" #'+workspace/switch-left
        :desc "Switch to workspace 7"        "7" #'+workspace/switch-to-6
        :desc "Switch to workspace 8"        "8" #'+workspace/switch-to-7
        :desc "Switch to workspace 9"        "9" #'+workspace/switch-to-8
        :desc "Switch to last workspace"     "0" #'+workspace/switch-to-final)
       :desc "Autosave session"             "a" #'doom/quicksave-session
       :desc "Save session"                 "s" #'doom/save-session
       :desc "Load session"                 "l" #'doom/load-session
       :desc "Load last autosaved session"  "L" #'doom/quickload-session
       :desc "Create frame"               "f" #'make-frame)
      )

(setq lsp-file-watch-threshold nil)
;; (setq bidi-paragraph-direction 'right-to-left)

(setq projectile-project-search-path '(("~/workspace/" . 3) ("~/" . 2)))
;; (setq projectile-enable-caching nil)
;; (setq projectile-auto-discover t)

;; (setq lsp-csharp-server-path "/usr/bin/omnisharp")

;; always reload buffers changed externally
(global-auto-revert-mode t)

;; org-mode configuration
;; add time stamp and note on task done
(setq org-log-done 'time)


;; elfeed
(setq rmh-elfeed-org-files (list "~/.doom.d/elfeed.org"))
(setq elfeed-goodies/entry-pane-position 'bottom)
;; (elfeed-goodies/setup)

;; (projectile-register-project-type 'godot '("project.godot")
;;                                   :project-file "project.godot"
;; 				  :compile "msbuild"
;; 				  :run "godot-mono -d --remote-debug localhost:45000")

;;(require 'emms-setup)
;;(emms-all)
;;(emms-default-players)
(setq emms-player-mpd-server-name "localhost"
      emms-player-mpd-server-port "6600"
      emms-player-mpd-music-directory "~/Music"
      emms-player-list '(emms-player-mpd))

;; Load cover images
;; (add-to-list 'emms-info-functions 'emms-info-mpd)
;; (setq emms-browser-covers 'emms-browser-emaccache-thumbnail-async)
;; (emms-player-mpd-connect)
;; (emms-cache-set-from-mpd-all)
;; (emms-mode-line-disable)

(setq mingus-mpd-port 6600
      mingus-mpd-host "localhost")

;; ;; Keyboard shortcuts
;; (global-set-key (kbd "<XF86AudioPrev>") 'emms-previous)
;; (global-set-key (kbd "<XF86AudioNext>") 'emms-next)
;; (global-set-key (kbd "<XF86AudioPlay>") 'emms-pause)


;; saving desktop frames to session files
;;(desktop-save-mode 1)

(setq company-minimum-prefix-length 1   ; Show suggestions after entering one character.
      company-idle-delay 1              ; 1 second delay before showing suggestions.
      company-selection-wrap-around t)


;;emacs-howdoyou configuration
(setq howdoyou-number-of-answers 10)
(map! :leader
      (:prefix-map ("s" . "search")
       :desc "How do you" "h" #'howdoyou-query))

(eval-after-load "howdoyou"
  '(progn
     (define-key howdoyou-mode-map (kbd "C-k") #'howdoyou-previous-link)
     (define-key howdoyou-mode-map (kbd "C-j") #'howdoyou-next-link)))

;; modeline
(setq doom-modeline-buffer-file-name-style 'truncate-with-project
      doom-modeline-icon (display-graphic-p)
      doom-modeline-major-mode-icon t
      doom-modeline-major-mode-color-icon t
      doom-modeline-persp-name t
      doom-modeline-persp-icon t
      doom-modeline-github t
      doom-modeline-mu4e t
      doom-modeline-height 1)

;; vterm
(add-hook 'vterm-mode-hook (lambda () (hide-mode-line-mode -1)))

;; load env file
(doom-load-envvars-file "~/.doom.d/env")

;; mu4e

(after! mu4e
  (setq mu4e-alert-email-notification-types '(count))
  (setq mu4e-update-interval (* 60 3)
        mu4e-root-maildir "~/.mail"
        mu4e-change-filenames-when-moving t
        mu4e-get-mail-command "mbsync -a"
        mu4e-display-update-status-in-modeline t))

   (with-eval-after-load "mm-decode"
        (add-to-list 'mm-discouraged-alternatives "text/html")
        (add-to-list 'mm-discouraged-alternatives "text/richtext"))

;; (setq mu4e-contexts
;;       (list
;;        ;; Work account
;;        (make-mu4e-context
;;         :name "Work"
;;         :match-func
;;           (lambda (msg)
;;             (when msg
;;               (string-prefix-p "/gmibm" (mu4e-message-field msg :maildir))))
;;         :vars '((user-mail-address . "ibraheem.marhoon@gmail.com")
;;                 (user-full-name    . "Ibraheem Almarhoon")
;;                 (mu4e-drafts-folder  . "/gmibm/drafts")
;;                 (mu4e-sent-folder  . "/gmibm/[Gmail]/Sent Mail")
;;                 (mu4e-refile-folder  . "/gmibm/[Gmail]/All Mail")
;;                 (mu4e-trash-folder  . "/gmibm/trash"))),
;;        (make-mu4e-context
;;         :name "Work"
;;         :match-func
;;           (lambda (msg)
;;             (when msg
;;               (string-prefix-p "/gmeb2" (mu4e-message-field msg :maildir))))
;;         :vars '((user-mail-address . "ebeem2@gmail.com")
;;                 (user-full-name    . "Ibraheem Almarhoon") (mu4e-drafts-folder  . "/gmeb2/drafts")
;;                 (mu4e-sent-folder  . "/gmeb2/[Gmail]/Sent Mail")
;;                 (mu4e-refile-folder  . "/gmeb2/[Gmail]/All Mail")
;;                 (mu4e-trash-folder  . "/gmeb2/trash")))
;;        ))


;; assumed Maildir layout
;; ~/Maildir/Account0/{Inbox,Sent,Trash}
;; ~/Maildir/Account1/{Inbox,Sent,Trash}
;; where Account0 is context name
(defun def-mu4e-context (context-name full-name mail-address smtp-server signature)
  "Return a mu4e context named CONTEXT-NAME with :match-func matching
   folder name CONTEXT-NAME in Maildir. The context's `user-mail-address',
   `user-full-name' and `mu4e-compose-signature' is set to MAIL-ADDRESS
   FULL-NAME and SIGNATURE respectively.
   Special folders are set to context specific folders."
  (let ((dir-name (concat "/" context-name)))
    (make-mu4e-context
     :name context-name
     ;; we match based on the maildir of the message
     ;; this matches maildir /Arkham and its sub-directories
     :match-func
     `(lambda (msg)
        (when msg
          (string-match-p
           ,(concat "^" dir-name)
           (mu4e-message-field msg :maildir))))
     :vars
     `((user-mail-address    . ,mail-address)
       (user-full-name       . ,full-name)
       (smtpmail-smtp-server . ,smtp-server)
       (smtpmail-smtp-service . , 465)
       (smtpmail-stream-type . , ssl)
       (mu4e-sent-folder     . ,(concat dir-name "/sent"))
       (mu4e-drafts-folder   . ,(concat dir-name "/drafts"))
       (mu4e-trash-folder    . ,(concat dir-name "/trash"))
       (mu4e-compose-signature . ,signature)))))

;;Fixing duplicate UID errors when using mbsync and mu4e
(setq mu4e-change-filenames-when-moving t)
(setq mu4e-dashboard-file "~/.doom.d/mu4e-dashboard.org")
;; TODO: mu4e bookmarks
;;

;; automatically highlight symbols under the curse
(setq highlight-symbol-idle-delay 0)
(add-hook 'prog-mode-hook 'highlight-symbol-mode)

;; custom vim keybindings
;; (global-set-key (kbd "<C-k>") 'drag-stuff-up)
;; (global-set-key (kbd "<C-j>") 'drag-stuff-down)

;; desktop-environment
(setq desktop-environment-volume-get-command "pamixer --get-volume"
      desktop-environment-volume-set-command "pamixer %s"
      desktop-environment-volume-toggle-command "pamixer -t"
      desktop-environment-volume-toggle-microphone-command "pamixer -t"
      desktop-environment-volume-get-regexp "\\([0-9]+\\)"
      desktop-environment-volume-normal-increment "-i 5"
      desktop-environment-volume-normal-decrement "-d 5"
      desktop-environment-volume-small-increment "-i 1"
      desktop-environment-volume-small-decrement "-d 1")


(map! :map elfeed-search-mode-map
      :after elfeed-search
      [remap kill-this-buffer] "q"
      [remap kill-buffer] "q"
      :n doom-leader-key nil
      :n "q" #'+rss/quit
      :n "e" #'elfeed-update
      :n "b" #'elfeed-search-browse-url)
(map! :map elfeed-show-mode-map
      :after elfeed-show
      [remap kill-this-buffer] "q"
      [remap kill-buffer] "q"
      :n doom-leader-key nil
      :nm "Y" #'elfeed-show-yank)
      
(after! elfeed
  (elfeed-org)
  (use-package! elfeed-link)

  (setq elfeed-search-filter "@1-week-ago +unread"
        elfeed-search-print-entry-function '+rss/elfeed-search-print-entry
        elfeed-search-title-min-width 80
        elfeed-show-entry-switch #'pop-to-buffer
        elfeed-show-entry-delete #'+rss/delete-pane
        elfeed-show-refresh-function #'+rss/elfeed-show-refresh--better-style
        shr-max-image-proportion 0.6)

  (add-hook! 'elfeed-show-mode-hook (hide-mode-line-mode 1))
  (add-hook! 'elfeed-search-update-hook #'hide-mode-line-mode)

  (defface elfeed-show-title-face '((t (:weight ultrabold :slant italic :height 1.5)))
    "title face in elfeed show buffer"
    :group 'elfeed)
  (defface elfeed-show-author-face `((t (:weight light)))
    "title face in elfeed show buffer"
    :group 'elfeed)
  (set-face-attribute 'elfeed-search-title-face nil
                      :foreground 'nil
                      :weight 'light)

  (defadvice! +rss-elfeed-wrap-h-nicer ()
    "Enhances an elfeed entry's readability by wrapping it to a width of
`fill-column' and centering it with `visual-fill-column-mode'."
    :override #'+rss-elfeed-wrap-h
    (setq-local truncate-lines nil
                shr-width 120
                visual-fill-column-center-text t
                default-text-properties '(line-height 1.1))
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t))
      (visual-fill-column-mode)
      ;; (setq-local shr-current-font '(:family "Merriweather" :height 1.2))
      (set-buffer-modified-p nil)))

  (defun +rss/elfeed-search-print-entry (entry)
    "Print ENTRY to the buffer."
    (let* ((elfeed-goodies/tag-column-width 40)
           (elfeed-goodies/feed-source-column-width 30)
           (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
           (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
           (feed (elfeed-entry-feed entry))
           (feed-title
            (when feed
              (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
           (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
           (tags-str (concat (mapconcat 'identity tags ",")))
           (title-width (- (window-width) elfeed-goodies/feed-source-column-width
                           elfeed-goodies/tag-column-width 4))

           (tag-column (elfeed-format-column
                        tags-str (elfeed-clamp (length tags-str)
                                               elfeed-goodies/tag-column-width
                                               elfeed-goodies/tag-column-width)
                        :left))
           (feed-column (elfeed-format-column
                         feed-title (elfeed-clamp elfeed-goodies/feed-source-column-width
                                                  elfeed-goodies/feed-source-column-width
                                                  elfeed-goodies/feed-source-column-width)
                         :left)))

      (insert (propertize feed-column 'face 'elfeed-search-feed-face) " ")
      (insert (propertize tag-column 'face 'elfeed-search-tag-face) " ")
      (insert (propertize title 'face title-faces 'kbd-help title))
      (setq-local line-spacing 0.2)))

  (defun +rss/elfeed-show-refresh--better-style ()
    "Update the buffer to match the selected entry, using a mail-style."
    (interactive)
    (let* ((inhibit-read-only t)
           (title (elfeed-entry-title elfeed-show-entry))
           (date (seconds-to-time (elfeed-entry-date elfeed-show-entry)))
           (author (elfeed-meta elfeed-show-entry :author))
           (link (elfeed-entry-link elfeed-show-entry))
           (tags (elfeed-entry-tags elfeed-show-entry))
           (tagsstr (mapconcat #'symbol-name tags ", "))
           (nicedate (format-time-string "%a, %e %b %Y %T %Z" date))
           (content (elfeed-deref (elfeed-entry-content elfeed-show-entry)))
           (type (elfeed-entry-content-type elfeed-show-entry))
           (feed (elfeed-entry-feed elfeed-show-entry))
           (feed-title (elfeed-feed-title feed))
           (base (and feed (elfeed-compute-base (elfeed-feed-url feed)))))
      (erase-buffer)
      (insert "\n")
      (insert (format "%s\n\n" (propertize title 'face 'elfeed-show-title-face)))
      (insert (format "%s\t" (propertize feed-title 'face 'elfeed-search-feed-face)))
      (when (and author elfeed-show-entry-author)
        (insert (format "%s\n" (propertize author 'face 'elfeed-show-author-face))))
      (insert (format "%s\n\n" (propertize nicedate 'face 'elfeed-log-date-face)))
      (when tags
        (insert (format "%s\n"
                        (propertize tagsstr 'face 'elfeed-search-tag-face))))
      ;; (insert (propertize "Link: " 'face 'message-header-name))
      ;; (elfeed-insert-link link link)
      ;; (insert "\n")
      (cl-loop for enclosure in (elfeed-entry-enclosures elfeed-show-entry)
               do (insert (propertize "Enclosure: " 'face 'message-header-name))
               do (elfeed-insert-link (car enclosure))
               do (insert "\n"))
      (insert "\n")
      (if content
          (if (eq type 'html)
              (elfeed-insert-html content base)
            (insert content))
        (insert (propertize "(empty)\n" 'face 'italic)))
      (goto-char (point-min)))))


(after! elfeed-show
  (require 'url)

  (defvar elfeed-pdf-dir
    (expand-file-name "pdfs/"
                      (file-name-directory (directory-file-name elfeed-enclosure-default-dir))))

  (defvar elfeed-link-pdfs
    '(("https://www.jstatsoft.org/index.php/jss/article/view/v0\\([^/]+\\)" . "https://www.jstatsoft.org/index.php/jss/article/view/v0\\1/v\\1.pdf")
      ("http://arxiv.org/abs/\\([^/]+\\)" . "https://arxiv.org/pdf/\\1.pdf"))
    "List of alists of the form (REGEX-FOR-LINK . FORM-FOR-PDF)")

  (defun elfeed-show-pdf (entry)
    (interactive
     (list (or elfeed-show-entry (elfeed-search-selected :ignore-region))))
    (let ((link (elfeed-entry-link entry))
          (feed-name (plist-get (elfeed-feed-meta (elfeed-entry-feed entry)) :title))
          (title (elfeed-entry-title entry))
          (file-view-function
           (lambda (f)
             (when elfeed-show-entry
               (elfeed-kill-buffer))
             (pop-to-buffer (find-file-noselect f))))
          pdf)

      (let ((file (expand-file-name
                   (concat (subst-char-in-string ?/ ?, title) ".pdf")
                   (expand-file-name (subst-char-in-string ?/ ?, feed-name)
                                     elfeed-pdf-dir))))
        (if (file-exists-p file)
            (funcall file-view-function file)
          (dolist (link-pdf elfeed-link-pdfs)
            (when (and (string-match-p (car link-pdf) link)
                       (not pdf))
              (setq pdf (replace-regexp-in-string (car link-pdf) (cdr link-pdf) link))))
          (if (not pdf)
              (message "No associated PDF for entry")
            (message "Fetching %s" pdf)
            (unless (file-exists-p (file-name-directory file))
              (make-directory (file-name-directory file) t))
            (url-copy-file pdf file)
            (funcall file-view-function file))))))

  )


(use-package! org-pretty-table
  :commands (org-pretty-table-mode global-org-pretty-table-mode))

(defvar mixed-pitch-modes '(org-mode zen-mode LaTeX-mode markdown-mode gfm-mode Info-mode)
  "Modes that `mixed-pitch-mode' should be enabled in, but only after UI initialisation.")
(defun init-mixed-pitch-h ()
  "Hook `mixed-pitch-mode' into each mode in `mixed-pitch-modes'.
Also immediately enables `mixed-pitch-modes' if currently in one of the modes."
  (when (memq major-mode mixed-pitch-modes)
    (mixed-pitch-mode 1))
  (dolist (hook mixed-pitch-modes)
    (add-hook (intern (concat (symbol-name hook) "-hook")) #'mixed-pitch-mode)))
(add-hook 'doom-init-ui-hook #'init-mixed-pitch-h)

(autoload #'mixed-pitch-serif-mode "mixed-pitch"
  "Change the default face of the current buffer to a serifed variable pitch, while keeping some faces fixed pitch." t)

(after! mixed-pitch
  (defface variable-pitch-serif
    '((t (:family "serif")))
    "A variable-pitch face with serifs."
    :group 'basic-faces)
  (setq mixed-pitch-set-height t)
  (defun mixed-pitch-serif-mode (&optional arg)
    "Change the default face of the current buffer to a serifed variable pitch, while keeping some faces fixed pitch."
    (interactive)
    (let ((mixed-pitch-face 'variable-pitch-serif))
      (mixed-pitch-mode (or arg 'toggle)))))

(defvar +zen-serif-p nil
  "Whether to use a serifed font with `mixed-pitch-mode'.")
(after! writeroom-mode
  (defvar-local +zen--original-org-indent-mode-p t)
  (defvar-local +zen--original-mixed-pitch-mode-p t)
  (defvar-local +zen--original-org-pretty-table-mode-p t)
  (defun +zen-enable-mixed-pitch-mode-h ()
    "Enable `mixed-pitch-mode' when in `+zen-mixed-pitch-modes'."
    (when (apply #'derived-mode-p +zen-mixed-pitch-modes)
      (if writeroom-mode
          (progn
            (setq +zen--original-mixed-pitch-mode-p mixed-pitch-mode)
            (funcall (if +zen-serif-p #'mixed-pitch-serif-mode #'mixed-pitch-mode) 1))
        (funcall #'mixed-pitch-mode (if +zen--original-mixed-pitch-mode-p 1 -1)))))
  (pushnew! writeroom--local-variables
            'display-line-numbers
            'visual-fill-column-width
            'org-adapt-indentation
            'org-superstar-headline-bullets-list
            'org-superstar-remove-leading-stars)
  (add-hook 'writeroom-mode-enable-hook
            (defun +zen-prose-org-h ()
              "Reformat the current Org buffer appearance for prose."
              (when (eq major-mode 'org-mode)
                (setq display-line-numbers nil
                      visual-fill-column-width 60
                      org-adapt-indentation nil)
                (when (featurep 'org-superstar)
                  (setq-local org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")                              ;; org-superstar-headline-bullets-list '("🙐" "🙑" "🙒" "🙓" "🙔" "🙕" "🙖" "🙗")
                              org-superstar-remove-leading-stars nil)
                  (org-superstar-restart))
                (setq
                 +zen--original-org-indent-mode-p org-indent-mode
                 +zen--original-org-pretty-table-mode-p (bound-and-true-p org-pretty-table-mode))
                ;; (org-indent-mode -1)
                (org-pretty-table-mode 1))))
  (add-hook 'writeroom-mode-disable-hook
            (defun +zen-nonprose-org-h ()
              "Reverse the effect of `+zen-prose-org'."
              (when (eq major-mode 'org-mode)
                (when (featurep 'org-superstar)
                  (org-superstar-restart))
                (when +zen--original-org-indent-mode-p (org-indent-mode 1))
                ;; (unless +zen--original-org-pretty-table-mode-p (org-pretty-table-mode -1))
                ))))


(global-org-modern-mode)

(setq visual-fill-column-width 120
        visual-fill-column-center-text t
        default-text-properties '(line-height 1.1))

;; mu4e
(add-hook! 'mu4e-view-mode-hook (defun mu4e-view-mode-enter ()
                                        (hide-mode-line-mode 1)
                                        (setq-local truncate-lines nil
                                                        shr-width 120
                                                        visual-fill-column-center-text t
                                                        default-text-properties '(line-height 1.1))
                                        (let ((inhibit-read-only t)
                                                (inhibit-modification-hooks t))
                                        (visual-fill-column-mode)
                                        (variable-pitch-mode t)
                                        (set-buffer-modified-p nil))))

(add-hook! 'mu4e-headers-mode-hook (defun mu4e-headers-enter ()
              (hide-mode-line-mode 1)
              (mu4e-column-faces-mode)))

(setq mu4e-headers-field
  '( (:human-date    .   17)
     (:flags         .    10)
     ;; (:mailing-list  .   30)
     (:from          .   30)
     (:subject       .   nil))
      +mu4e-min-header-frame-width 142
      mu4e-headers-date-format "%d/%m/%y %H:%M:%S"
      mu4e-headers-time-format "⧖ %H:%M"
      mu4e-headers-results-limit 1000
      mu4e-index-cleanup t)


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/cloud/org/")

;; Make sure org-indent face is available
(require 'org-indent)

;; Make sure certain org faces use the fixed-pitch face when variable-pitch-mode is on
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

;; Resize Org headings
(dolist (face '((org-level-1 . 1.3)
                (org-level-2 . 1.2)
                (org-level-3 . 1.15)
                (org-level-4 . 1.1)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1))))

;; org-present
(defun org-present-prepare-slide (buffer-name heading)
  ;; Show only top-level headlines
  (org-overview)

  ;; Unfold the current entry
  (org-show-entry)

  ;; Show only direct subheadings of the slide but don't expand them
  (org-show-children))


  (evil-define-key 'normal 'org-present-mode-keymap
    "j" 'evil-next-line
    "k" 'evil-previous-line
    "q" 'org-present-quit
    "\C-j" 'org-present-next
    "\C-k" 'org-present-prev)


(map! :leader
      (:prefix-map ("t" . "toggle")
       :desc "Org Present"    "p" #'org-present))

(defun org-present-start ()
  ;; Tweak font sizes
  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 3.0) variable-pitch)
                                     (org-document-title (:height 1.75) org-document-title)
                                     (org-code (:height 1.55) org-code)
                                     (org-level-1 (:height 1.30) org-level-1)
                                     (org-level-2 (:height 1.25) org-level-2)
                                     (org-level-3 (:height 1.10) org-level-3)
                                     (org-level-4 (:height 1.10) org-level-4)
                                     (org-level-5 (:height 1.10) org-level-5)
                                     (org-level-6 (:height 1.10) org-level-6)
                                     (org-level-7 (:height 1.10) org-level-7)
                                     (org-verbatim (:height 1.55) org-verbatim)
                                     (org-block (:height 1.35) org-block)
                                     (org-block-begin-line (:height 0.9) org-block)))

    ;; Make sure certain org faces use the fixed-pitch face when variable-pitch-mode is on
    (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

  (setq header-line-format " ")
  (setq display-line-numbers nil)
  ;; Center the presentation and wrap lines
  (visual-fill-column-mode 1))

(defun org-present-end ()
  ;; Reset font customizations
  (setq-local face-remapping-alist '((default variable-pitch default)))
  (setq display-line-numbers t)

  (evil-define-key 'normal 'org-present-mode-keymap
    "q" nil
    "\C-j" nil
    "\C-k" nil)

  ;; Stop centering the document
  (visual-fill-column-mode 0))

;; Turn on variable pitch fonts in Org Mode buffers
(add-hook 'org-mode-hook 'variable-pitch-mode)

;; Register hooks with org-present
(add-hook 'org-present-mode-hook 'org-present-start)
(add-hook 'org-present-mode-quit-hook 'org-present-end)
(add-hook 'org-present-after-navigate-functions 'org-present-prepare-slide)

;; neotree
;; (setq doom-themes-neotree-line-spacing 2
;;       doom-themes-neotree-project-size 1.2
;;       doom-themes-neotree-folder-size 0.9
;;       doom-themes-neotree-chevron-size 0.9
;;       doom-themes-neotree-file-icons 'simple
;;       doom-themes-neotree-enable-variable-pitch t)

(setq frameset-filter-alist '((treemacs-workspace . :never)
        (treemacs-id . :never)
        (tabs . frameset-filter-tabs)
        (background-color . frameset-filter-sanitize-color)
        (buffer-list . :never)
        (buffer-predicate . :never)
        (buried-buffer-list . :never)
        (client . :never)
        (delete-before . :never)
        (font . frameset-filter-font-param)
        (font-backend . :never)
        (foreground-color . frameset-filter-sanitize-color)
        (frameset--text-pixel-height . :save)
        (frameset--text-pixel-width . :save)
        (fullscreen . frameset-filter-shelve-param)
        (GUI:font . frameset-filter-unshelve-param)
        (GUI:fullscreen . frameset-filter-unshelve-param)
        (GUI:height . frameset-filter-unshelve-param)
        (GUI:width . frameset-filter-unshelve-param)
        (height . frameset-filter-shelve-param)
        (outer-window-id . :never)
        (parent-frame . :never)
        (parent-id . :never)
        (posframe-parent-buffer . :never)
        (mouse-wheel-frame . :never)
        (tty . frameset-filter-tty-to-GUI)
        (tty-type . frameset-filter-tty-to-GUI)
        (width . frameset-filter-shelve-param)
        (window-id . :never)
        (window-system . :never)
        (name . :never)
        (left . frameset-filter-iconified)
        (minibuffer . frameset-filter-minibuffer)
        (top . frameset-filter-iconified))
)


;; dired
(setq delete-by-moving-to-trash t)

;; enable midnight (dark-mode) in pdf viewer by default
(add-hook 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode)


(setq ement-save-sessions t)

;; evil mode
(setq evil-kill-on-visual-paste nil)
(setq pop-up-frames t)

;; dired
;;; file opening procedures
(defun dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (call-process "xdg-open" nil 0 nil file)))

(defun eb/evil-keybindings-hook (mode mode-keymaps &rest _rest)
  (when (equal mode 'dired)
    (evil-define-key 'normal dired-mode-map
      "o" 'dired-open-file)))

(add-hook 'evil-collection-setup-hook #'eb/evil-keybindings-hook)

;; custom functions to get properties from Xresources
(defun xresources-value (app var)
  "Returns the app and variable values from Xresources"
  (string-trim (shell-command-to-string (concat "xrdb -get " app "." var))))

(defun xresources-preprocessor (app code)
  "Replace placeholders {xrdb:VARIABLE} with values from Xresources."
  (loop
   (when (null (string-match "\$\{xrdb:\\(.+\\)\}" code)) (return code))
   (setq code (string-replace (match-string 0 code)
                              (xresources-value app (match-string 1 code))
                              code))))

;; (xresources-value "rofi" "red")
;; (xresources-preprocessor "rofi" "Rest here ${xrdb:red}")
