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

;; (setq doom-font (font-spec :family "Iosevka Fixed" :size 20 :height 110 :weight 'extra-bold)
(setq doom-font (font-spec :family "JetBrains Mono" :size 17 :weight 'bold)
      doom-variable-pitch-font (font-spec :family "Noto Sans" :size 17 :height 110)
      +bidi-arabic-font (font-spec :family "Noto Sans Arabic Black" :size 16 :height 110))
      ;; doom-variable-pitch-font (font-spec :family "Noto Sans Arabic Black" :size 22 :height 110))
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
(setq doom-modeline-buffer-file-name-style 'truncate-with-project)

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

;; automatically highlight symbols under the curse
;; (setq highlight-symbol-idle-delay 0.4)
;; (add-hook 'prog-mode-hook 'idle-highlight-mode)
;; red :TODO :FIXME #ff0000 

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

(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

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

(setq doom-themes-neotree-line-spacing 2
      doom-themes-neotree-project-size 1.2
      doom-themes-neotree-folder-size 1.1
      doom-themes-neotree-chevron-size 0.95
      doom-themes-neotree-file-icons 'simple
      doom-themes-neotree-enable-variable-pitch t)

(setq frameset-filter-alist '((tabs . frameset-filter-tabs)
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

(setq evil-mc-custom-known-commands
'((custom/evil-mc-evil-escape-move-back-fake-cursors
        (:default . evil-mc-execute-default-call))))

(defun custom/evil-mc-evil-escape-move-back-fake-cursors ()
    "Move the fake cursors to the left once,
unless they already are at the beginning of the line."
  (unless (bolp) (backward-char)))

  (defun custom/evil-mc-evil-escape-fix ()
    "Prevent the first evil-escape-key-sequence key (default: j),
from being typed at all of the fake cursors.
And move back the fake cursors when the real insert state cursor is at the end
of a line."
    (when (evil-mc-has-cursors-p)
      (evil-mc-pause-cursors)
      (run-with-idle-timer
       0 nil '(lambda ()
                (evil-mc-resume-cursors)
                (let ((evil-mc-command '((:name . custom/evil-mc-evil-escape-move-back-fake-cursors))))
                  (evil-mc-execute-for-all))))))

(advice-add 'evil-escape-func :before 'custom/evil-mc-evil-escape-fix)
;; (advice-remove 'evil-escape-func 'custom/evil-mc-evil-escape-fix)

(setq evil-escape-delay 1.0)
(setq lsp-idle-delay 0.04)
(setq company-idle-delay nil)
(setq company-minimum-prefix-length 0)

(add-to-list 'load-path "~/.doom.d/modules/")
(require 'evil-collection-mingus)
(evil-collection-mingus-setup)

;; org mode bidi support
(defun set-bidi-env ()
  "interactive"
  (setq bidi-paragraph-direction 'nil))
(add-hook 'org-mode-hook 'set-bidi-env)

(setq org-startup-truncated nil)
(with-eval-after-load "org"
  (require 'org-phscroll))

;; transparency
(set-frame-parameter nil 'alpha-background 87)
(add-to-list 'default-frame-alist '(alpha-background . 87))
