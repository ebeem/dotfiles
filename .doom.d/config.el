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

(setq doom-font (font-spec :family "JetBrains Mono" :size 17 :weight 'bold)
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
        mu4e-maildir "~/.mail"
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
      (goto-char (point-min))))

  )


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
;; org-present
(defun my/org-present-prepare-slide (buffer-name heading)
  ;; Show only top-level headlines
  (org-overview)

  ;; Unfold the current entry
  (org-show-entry)

  ;; Show only direct subheadings of the slide but don't expand them
  (org-show-children))

(defun my/org-present-start ()
  ;; Tweak font sizes
  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 4.0) variable-pitch)
                                     (org-document-title (:height 1.75) org-document-title)
                                     (org-code (:height 1.55) org-code)
                                     (org-verbatim (:height 1.55) org-verbatim)
                                     (org-block (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))
  (setq header-line-format " ")
  (setq display-line-numbers nil)
  (org-present-read-only)
  ;; Center the presentation and wrap lines
  (visual-fill-column-mode 1))

(defun my/org-present-end ()
  ;; Reset font customizations
  (setq-local face-remapping-alist '((default variable-pitch default)))
  (setq display-line-numbers t)

  (org-present-read-write)
  ;; Stop centering the document
  (visual-fill-column-mode 0))

;; Turn on variable pitch fonts in Org Mode buffers
(add-hook 'org-mode-hook 'variable-pitch-mode)

;; Register hooks with org-present
(add-hook 'org-present-mode-hook 'my/org-present-start)
(add-hook 'org-present-mode-quit-hook 'my/org-present-end)
(add-hook 'org-present-after-navigate-functions 'my/org-present-prepare-slide)

;; neotree
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





;;; mu4e-dashboard.el --- Dashboards for mu4e   -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021 Nicolas P. Rougier

;; Author: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; Homepage: https://github.com/rougier/mu4e-dashboard
;; Keywords: convenience
;; Version: 0.1.1

;; Package-Requires: ((emacs "26.1"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; mu4e-dashboard provides enhanced org-mode links that allow you to
;; define custom dashboards that link back to the mu4e email client.
;;


(require 'subr-x)
(require 'ob-shell)
(require 'org)
(require 'mu4e-headers)

;;; Code:

(defconst mu4e-dashboard-version "0.1.1")

;; Install the mu4e link type
(defgroup mu4e-dashboard nil
  "Provides a new Org mode link type for mu4e queries."
  :group 'comm)

(defcustom mu4e-dashboard-file "~/.emacs.d/mu4e-dashboard.org"
  "Path to the dashboard org file."
  :type 'string)

(defcustom mu4e-dashboard-link-name "mu"
  "Default link name."
  :type 'string)

(defcustom mu4e-dashboard-mu-program "mu"
  "Default name of the mu command."
  :type 'string)

(defcustom mu4e-dashboard-lighter " mu4ed"
  "Minor mode lighter indicating that this mode is active."
  :type 'string)

(defcustom mu4e-dashboard-propagate-keymap t
  "Propagate dashboard defined keymap to mu4e header view"
  :type 'boolean)

(org-link-set-parameters
 mu4e-dashboard-link-name
 :follow #'mu4e-dashboard-follow-mu4e-link)

(defvar mu4e-dashboard--prev-local-keymap nil
  "Buffer-local variable to save the prior keymap.")

(make-variable-buffer-local 'mu4e-dashboard--prev-local-keymap)

(defvar mu4e-dashboard--async-update-in-progress nil
  "Set tot if an async update is in progress.
This is a buffer-local variable that will be t if the current
buffer is in the process of being updated asynchronously.")

(make-variable-buffer-local 'mu4e-dashboard--async-update-in-progress)

;;;###autoload
(define-minor-mode mu4e-dashboard-mode
  "Minor mode for \"live\" mu4e dashboards."
  :lighter mu4e-dashboard-lighter
  :init-value nil
  (if mu4e-dashboard-mode
      (progn
        (setq buffer-read-only t)
        ;; Make a copy of the current local keymap (this will, in
        ;; general, have been setup by org-mode, but I don't want to
        ;; assume that)
        (setq mu4e-dashboard--prev-local-keymap (current-local-map))
	(use-local-map (make-composed-keymap (mu4e-dashboard-parse-keymap) (current-local-map)))
	;; If buffer corresponds to the dashboard, add a special key (buffer-name is harcoded). Dashboard should be open with a special function naming a defcustom buffer name  and then install the minor mode. 
	;; install the keymap as local with current map as parent (this might generate some problem?)
	(if (string= (buffer-file-name) (expand-file-name mu4e-dashboard-file))
	    (local-set-key (kbd "<return>") #'org-open-at-point))
	(add-hook 'mu4e-index-updated-hook #'mu4e-dashboard-update)
	(if mu4e-dashboard-propagate-keymap
	;; install minor mode to mu4e headers view when called (should it be to message hook too?) 
	(add-hook 'mu4e-headers-found-hook #'mu4e-dashboard-mode))
        (mu4e-dashboard-update))
    (if mu4e-dashboard--async-update-in-progress
        (user-error "Update in progress; try again when it is complete"))
    (remove-hook 'mu4e-index-updated-hook #'mu4e-dashboard-update)
    ;; clear hook when dashboard disable
    (remove-hook 'mu4e-headers-found-hook #'mu4e-dashboard-mode)
    (use-local-map mu4e-dashboard--prev-local-keymap)
    (setq buffer-read-only nil)))

(defun mu4e-dashboard ()
  "If the dashboard file exists, switch to it and run mu4e-dashboard-mode on it"
  (interactive)
  (if (file-exists-p mu4e-dashboard-file)
      (progn
        (find-file mu4e-dashboard-file)
        (mu4e-dashboard-mode))
    (message (concat mu4e-dashboard-file " does not exist"))
    ))


(defun mu4e-dashboard-follow-mu4e-link (path)
  "Process a mu4e link with path PATH.
PATH shall be of the form [[mu4e:query|fmt|limit][(---------)]].
If FMT is not specified or is nil, clicking on the link calls
mu4e with the specified QUERY (with or without the given
LIMIT).  If FMT is specified, the description of the link is
updated with the QUERY count formatted using the provided
format (for example \"%4d\")."

  (let* ((link    (org-element-context))
         (query   (string-trim (nth 0 (split-string path "[]|]"))))
         (fmt     (nth 1 (split-string path "[]|]")))
         (count   (nth 2 (split-string path "[]|]"))))
    (cond
     ;; Regular query without limit
     ((and (not fmt) (not count))
      (progn
        (if (get-buffer-window "*mu4e-headers*" t)
            (switch-to-buffer"*mu4e-headers*"))
        (mu4e-headers-search query)))
     
     ;; Regular query with limit
     ((and count (> (length count) 0))
      (progn
        (if (get-buffer-window "*mu4e-headers*" t)
            (switch-to-buffer"*mu4e-headers*"))
        (let ((mu4e-headers-results-limit (string-to-number count)))
          (mu4e-headers-search query))))

     ;; Query count and link description update
     ((and fmt (> (length fmt) 0))
       (mu4e-dashboard-update-link link)))))

(defun mu4e-dashboard-update-link (link)
  "Update content of a formatted mu4e LINK.
A formatted link is a link of the form
[[mu4e:query|limit|fmt][(---------)]] where fmt is a non nil
string describing the format.  When a link is cleared, the
description is replaced by a string for the form \"(---)\" and
have the same size as the current description. If the given
format is too big for the current description, description is
replaced with + signs."

  (let* ((path  (org-element-property :path link))
         (query (string-trim (nth 0 (split-string path "|"))))
         (fmt   (nth 1 (split-string path "|")))
         (beg   (org-element-property :contents-begin link))
         (end   (org-element-property :contents-end link))
         (size  (- end beg)))
    (if (and fmt (> (length fmt) 0))
        (let* ((command (format "%s find %s 2> /dev/null | wc -l" mu4e-dashboard-mu-program query))
               (output (string-to-number (shell-command-to-string command)))
               (output  (format fmt output)))
          (let ((modified (buffer-modified-p))
                (inhibit-read-only t))
            (save-excursion
              (delete-region beg end)
              (goto-char beg)
              (insert (if (<= (length output) size) output
                        (make-string size ?+))))
            (set-buffer-modified-p modified))))))

(defun mu4e-dashboard--async-shell-command-to-string (command callback)
  "Run COMMAND asynchronously; call CALLBACK on completion.
Run a shell command in an asynchronous way.  Once the call
terminates, callback is called with the result."

  (let* ((display-buffer-alist (list (cons "\\*Async Shell Command\\*.*"
                                       (cons #'display-buffer-no-window nil))))
         (output-buffer (generate-new-buffer "*Async Shell Command*"))
         (proc (progn
                 (async-shell-command command output-buffer)
                 (get-buffer-process output-buffer))))
    (if (process-live-p proc)
        (set-process-sentinel proc
                              (lambda (process _signal)
                                (when (memq (process-status process) '(exit signal))
                                  (with-current-buffer output-buffer
                                    (funcall callback (buffer-string)))
                                  (kill-buffer output-buffer))))
      (message "No process running."))))

(defun mu4e-dashboard-update-all-async ()
  "Update content of all formatted mu4e links in an asynchronous way.
A formatted link is a link of the form
[[mu4e:query|limit|fmt][(---------)]] where fmt is a non nil
string describing the format.  When a link is cleared, the
description is replaced by a string for the form \"(---)\" and
have the same size as the current description."

  (if mu4e-dashboard--async-update-in-progress
      (user-error "Cannot update while an update is in progress!"))
  (setq mu4e-dashboard--async-update-in-progress t)
  (let ((buffer (current-buffer)))
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (when (string= (org-element-property :type link) mu4e-dashboard-link-name)
          (let* ((path  (org-element-property :path link))
                 (query (string-trim (nth 0 (split-string path "|"))))
                 (fmt   (nth 1 (split-string path "|")))
                 (beg   (org-element-property :contents-begin link))
                 (end   (org-element-property :contents-end link))
                 (size  (if (and beg end) (- end beg) 0)))
            (when (and fmt (> (length fmt) 0))
                ;; The rest of this function will execute successfully with a
                ;; `size' of zero, but since there would be no reason to
                ;; proceed with no output, we signal an error.
                (if (eq size 0)
                    (error "The link ``%s'' has a format clause, but no output width" path))
                (let ((command (format "%s find '%s' 2> /dev/null | wc -l" mu4e-dashboard-mu-program query)))
                  (mu4e-dashboard--async-shell-command-to-string command
                      (lambda (output)
                        (with-current-buffer buffer
                          (let ((modified (buffer-modified-p))
                                (inhibit-read-only t)
                                (output (format fmt (string-to-number output))))
                            (save-excursion
                              (delete-region beg end)
                              (goto-char beg)
                              (insert (if (<= (length output) size) output
                                        (make-string size ?+))))
                            (set-buffer-modified-p modified))))))))))))
  (setq mu4e-dashboard--async-update-in-progress nil))

(defun mu4e-dashboard-update-all-sync ()
  "Update content of all mu4e formatted links in a synchronous way.
A formatted link is a link of the form
[[mu4e:query|limit|fmt][(---------)]] where fmt is a non nil
string describing the format.  When a link is cleared, the
description is replaced by a string for the form \"(---)\" and
have the same size as the current description."

  (mu4e-dashboard-clear-all)
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (string= (org-element-property :type link) mu4e-dashboard-link-name)
        (mu4e-dashboard-update-link link)
        (redisplay t)))))

(defun mu4e-dashboard-clear-link (link)
  "Clear a formatted mu4e link LINK.
A formatted link is a link of the form
[[mu4e:query|limit|fmt][(---------)]] where fmt is a non nil
string describing the format.  When a link is cleared, the
description is replaced by a string for the form \"(---)\" and
having the same size as the current description."

  (let* ((path (org-element-property :path link))
         (fmt  (nth 1 (split-string path "|")))
         (beg  (org-element-property :contents-begin link))
         (end  (org-element-property :contents-end link))
         (size (- end beg)))
    (if (and fmt (> (length fmt) 0))
        (let ((modified (buffer-modified-p))
              (inhibit-read-only t))
          (save-excursion
            (delete-region beg end)
            (goto-char beg)
            (insert (format "(%s)" (make-string (- size 2) ?-))))
          (set-buffer-modified-p modified)))))

(defun mu4e-dashboard-clear-all ()
  "Clear all formatted mu4e links.
A formatted link is a link of the form
[[mu4e:query|limit|fmt][(---------)]] where fmt is a non nil
string describing the format.  When a link is cleared, the
description is replaced by a string for the form \"(---)\" and
have the same size as the current description."

  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (string= (org-element-property :type link) mu4e-dashboard-link-name)
        (mu4e-dashboard-clear-link link))))
  (redisplay t))

(defun mu4e-dashboard-update ()
  "Update the current dashboard."
  (interactive)
  (message
   (concat "[" (propertize "mu4e dashboard" 'face 'bold) "] "
           (format-time-string "Update (%H:%M)")))
  (dolist (buffer (buffer-list (current-buffer)))
    (with-current-buffer buffer
      (if (bound-and-true-p mu4e-dashboard-mode)
          (mu4e-dashboard-update-all-async)))))

(defun mu4e-dashboard-parse-keymap ()
  "Parse the current buffer file for keybindings.
Keybindings are defined by keywords of type KEYMAP:VALUE and
install the corresponding key bindings in the mu4e-dashboard
minor mode keymap.  The previous keymap (if any) is erased.
VALUE is composed of \"keybinding | function-call\" with
keybinding begin a string describing a key sequence and a call to
an existing function. For example, to have 'q' to kill the
current buffer, the syntax would be:
#+KEYMAP: q | kill-current-buffer
This can be placed anywhere in the org file even though I advised
to group keymaps at the same place."

  (let ((map (make-sparse-keymap)))
    (org-element-map (org-element-parse-buffer) 'keyword
      (lambda (keyword)
	(when (string= (org-element-property :key keyword) "KEYMAP")
          (let* ((value (org-element-property :value keyword))
		 (key   (string-trim (nth 0 (split-string value "|"))))
		 (call  (string-trim (nth 1 (split-string value "|")))))
            (define-key map
	      (kbd key)
	      `(lambda () (interactive) ,(car (read-from-string (format "(%s)" call)))))
            (message "mu4e-dashboard: binding %s to %s"
		     key
		     (format "(lambda () (interactive) (%s))" call))))))
    map))

(provide 'mu4e-dashboard)

;;; mu4e-dashboard.el ends here



(setq ement-save-sessions t)

;; evil mode
(setq evil-kill-on-visual-paste nil)
