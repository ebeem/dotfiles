;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file
(add-hook 'window-setup-hook 'toggle-frame-maximized t)


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ibraheem Almarhoon"
      user-mail-address "ibraheem.marhoon@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "UbuntuMono Nerd Font Mono" :size 20 :weight 'bold))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;;; :ui doom-dashboard
(setq fancy-splash-image (concat doom-private-dir "splash.png"))

;; Here are some additional functions/macros that could help you configure Doom:
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

(setq doom-localleader-alt-key "M-m")

(defun exts/move-buffer-to-window (windownum follow-focus-p)
  "Moves a buffer to a window, using the spacemacs numbering. follow-focus-p
  controls whether focus moves to new window (with buffer), or stays on current"
  (interactive)
  (if (> windownum (length (window-list-1 nil nil t)))
      (message "No window numbered %s" windownum)
    (let ((b (current-buffer))
          (w1 (selected-window))
          (w2 (winum-get-window-by-number windownum)))
      (unless (eq w1 w2)
        (set-window-buffer w2 b)
        (switch-to-prev-buffer)
        (unrecord-window-buffer w1 b))
      (when follow-focus-p
        (select-window (winum-get-window-by-number windownum))))))

(defun exts/swap-buffers-to-window (windownum follow-focus-p)
  "Swaps visible buffers between active window and selected window.
  follow-focus-p controls whether focus moves to new window (with buffer), or
  stays on current"
  (interactive)
  (if (> windownum (length (window-list-1 nil nil t)))
      (message "No window numbered %s" windownum)
    (let* ((b1 (current-buffer))
           (w1 (selected-window))
           (w2 (winum-get-window-by-number windownum))
           (b2 (window-buffer w2)))
      (unless (eq w1 w2)
        (set-window-buffer w1 b2)
        (set-window-buffer w2 b1)
        (unrecord-window-buffer w1 b1)
        (unrecord-window-buffer w2 b2)))
    (when follow-focus-p (winum-select-window-by-number windownum))))

;; define and evaluate three numbered functions:
;; buffer-to-window-1 to 9
(dotimes (i 9)
  (let ((n (+ i 1)))
    (eval `(defun ,(intern (format "buffer-to-window-%s" n)) (&optional arg)
             ,(format "Move buffer to the window with number %i." n)
             (interactive "P")
             (exts/move-buffer-to-window ,n t)))
    (eval `(defun ,(intern (format "swap-buffers-to-window-%s" n)) (&optional arg)
             ,(format "Move buffer to the window with number %i." n)
             (interactive "P")
             (exts/move-buffer-to-window ,n t)))
    ))



;;; <leader> w --- windows
(map! :leader
      (:prefix-map ("w" . "windows")
       (:when (featurep! :ui window-select)
        :desc "Delete window"             "d" #'delete-window
        :desc "Switch window"             "s" #'switch-window
        :desc "Switch window"             "w" #'switch-window
        :desc "Switch to window 1"        "1" #'winum-select-window-1
        :desc "Switch to window 2"        "2" #'winum-select-window-2
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
       :desc "Create frame"               "f" #'make-frame)

      (:prefix-map ("b" . "buffers")
       :desc "Kill current buffer"       "d" #'kill-current-buffer
       :desc "Switch buffer"             "s" #'switch-to-buffer
       :desc "Switch buffer"             "b" #'switch-to-buffer
       :desc "Kill a buffer"             "k" #'kill-buffer
       :desc "Rename buffer"             "r" #'rename-buffer
       :desc "Move buffer to window 1"   "1" #'swap-buffers-to-window-1
       :desc "Move buffer to window 2"   "2" #'swap-buffers-to-window-2
       :desc "Move buffer to window 3"   "3" #'swap-buffers-to-window-3
       :desc "Move buffer to window 4"   "4" #'swap-buffers-to-window-4
       :desc "Move buffer to window 5"   "5" #'swap-buffers-to-window-5
       :desc "Move buffer to window 6"   "6" #'swap-buffers-to-window-6
       :desc "Move buffer to window 7"   "7" #'swap-buffers-to-window-7
       :desc "Move buffer to window 8"   "8" #'swap-buffers-to-window-8
       :desc "Move buffer to window 9"   "9" #'swap-buffers-to-window-9
       :desc "Move buffer to window 0"   "0" #'swap-buffers-to-window-0
       :desc "New buffer"                "n" #'+default/new-buffer)


      (:prefix-map ("W" . "workspaces")
       (:when (featurep! :ui workspaces)
        :desc "Display workspaces"           "d" #'+workspace/display
        :desc "Rename workspace"             "r" #'+workspace/rename
        :desc "Create workspace"             "c" #'+workspace/new
        :desc "Delete workspace"             "k" #'+workspace/delete
        :desc "Save workspace"               "S" #'+workspace/save
        :desc "Switch to other workspace"    "o" #'+workspace/other
        :desc "Switch to left workspace"     "p" #'+workspace/switch-left
        :desc "Switch to right workspace"    "n" #'+workspace/switch-right
        :desc "Switch to"                    "w" #'+workspace/switch-to
        :desc "Switch to workspace 1"        "1" #'+workspace/switch-to-0
        :desc "Switch to workspace 2"        "2" #'+workspace/switch-to-1
        :desc "Switch to workspace 3"        "3" #'+workspace/switch-to-2
        :desc "Switch to workspace 4"        "4" #'+workspace/switch-to-3
        :desc "Switch to workspace 5"        "5" #'+workspace/switch-to-4
        :desc "Switch to workspace 6"        "6" #'+workspace/switch-to-5
        :desc "Switch to workspace 7"        "7" #'+workspace/switch-to-6
        :desc "Switch to workspace 8"        "8" #'+workspace/switch-to-7
        :desc "Switch to workspace 9"        "9" #'+workspace/switch-to-8
        :desc "Switch to last workspace"     "0" #'+workspace/switch-to-final)
       :desc "Autosave session"             "a" #'doom/quicksave-session
       :desc "Save session"                 "s" #'doom/save-session
       :desc "Load session"                 "l" #'doom/load-session
       :desc "Load last autosaved session"  "L" #'doom/quickload-session
       :desc "Undo window config"           "u" #'winner-undo
       :desc "Redo window config"           "U" #'winner-redo)
      )


;; TODO: Mode this to another section
(setq-default fill-column 80)

;; Turn on indentation and auto-fill mode for Org files
(defun dw/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil)
  (diminish org-indent-mode))

(use-package org
  :defer t
  :hook (org-mode . dw/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 2
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'content
        org-cycle-separator-lines 2)

  (setq org-refile-targets '((nil :maxlevel . 1)
                             (org-agenda-files :maxlevel . 3)))

  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path t)
  (setq org-hide-emphasis-markers t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (ledger . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes)
  )
;; NOTE: Subsequent sections are still part of this use-package block!

;; Set the font face based on platform
(pcase system-type
  ((or 'gnu/linux 'windows-nt 'cygwin)
   (set-face-attribute 'default nil
                       :font "UbuntuMono Nerd Font Mono"
                       :weight 'light
                       :height 110))
  ('darwin (set-face-attribute 'default nil :font "UbuntuMono Nerd Font Mono" :height 110)))

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil
                    :font "UbuntuMono Nerd Font Mono"
                    :weight 'light
                    :height 110)


;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil
                    :font "Ubuntu"
                    :height 120)

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

(add-hook! org-mode
;; Increase the size of various headings
  ;; (set-face-attribute 'fixed-pitch nil :font "UbuntuMono Nerd Font Mono" :weight 'bold :height 1.35)
  (set-face-attribute 'org-document-title nil :font "Iosevka Aile" :weight 'bold :height 1.2)
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.17)
                  (org-level-3 . 1.15)
                  (org-level-4 . 1.14)
                  (org-level-5 . 1.12)
                  (org-level-6 . 1.10)
                  (org-level-7 . 1.08)
                  (org-level-8 . 1.08)))
    (set-face-attribute (car face) nil :font "Iosevka Aile" :weight 'bold :height (cdr face)))
  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-document-info-keyword nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-meta-line nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-property-value nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-special-keyword nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-tag nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-verbatim nil :inherit 'fixed-pitch)

  ;; Get rid of the background on column views
  (set-face-attribute 'org-column nil :background nil)
  (set-face-attribute 'org-column-title nil :background nil)
)


  (setq org-fancy-priorities-list '((?A . "❗")
                                  (?B . "🚩")
                                  (?C . "🏴")
                                  (?D . "☕")))
                                  ;;(?1 . "🏳")

(custom-set-faces
  '(mode-line ((t (:inherit 'fixed-pitch))))
  '(mode-line-inactive ((t (:inherit 'fixed-pitch)))))

(defcustom lsp-file-watch-threshold 100000
  "Show warning if the files to watch are more than.
Set to nil to disable the warning."
  :type 'number
  :group 'lsp-mode)

;;(setq bidi-paragraph-direction 'right-to-left)

;; override the default behavior of emacs in backward-word and forward-word, also fix and disable same behavior in delete backward and delete forward
(defun backward-same-syntax (&optional arg)
  (interactive "^p")
  (or arg (setq arg 1))
  (forward-same-syntax (- arg)))

(defun delete-word (&optional arg)
  "Delete characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-same-syntax arg) (point))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

(global-set-key (kbd "C-<left>") 'backward-same-syntax)
(global-set-key (kbd "C-<right>") 'forward-same-syntax)
(global-set-key (kbd "C-<delete>") 'delete-word)
(global-set-key (kbd "C-<backspace>") 'backward-delete-word)


;;(setq projectile-project-search-path '(("~/workspace/" . 9) ("/mnt/repository/projects/" . 9) ("/mnt/repository/workspace/" . 9) ("/mnt/storage/workspace/" . 9)))
;; (setq projectile-project-search-path '(("~/workspace/" . 4) ("/mnt/repository/projects/" . 4) ("/mnt/repository/workspace/" . 4) ("/mnt/storage/workspace/" . 4)))
(setq projectile-project-search-path '(("~/workspace/" . 3) ("~/" . 2)))
(setq emms-source-file-default-directory "/mnt/repository/media/music/")
(setq projectile-enable-caching nil)
(setq projectile-auto-discover nil)
(setq org-support-shift-select t)
;; (remove-hook 'projectile-relevant-known-projects 'projectile-discover-projects-in-search-path t)
;; (remove-hook 'projectile-relevant-known-projects 'projectile-cleanup-known-projects t)
;; (remove-hook 'projectile-relevant-known-projects 'projectile-discover-projects-in-search-path)

;; (setq lsp-csharp-server-path "/usr/bin/omnisharp")



;; duplicate line function using C-d
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  )
(global-set-key (kbd "C-d") 'duplicate-line)


;; always reload buffers changed externally
(global-auto-revert-mode t)

;; org-mode configuration
;; add time stamp and note on task done
(setq org-log-done 'note)


;; elfeed
(setq rmh-elfeed-org-files (list "~/.doom.d/elfeed.org"))
;; (elfeed-goodies/setup)

(projectile-register-project-type 'godot '("project.godot")
                                  :project-file "project.godot"
				  :compile "msbuild"
				  :run "godot-mono -d --remote-debug localhost:45000")

(require 'emms-setup)
(emms-all)
(emms-default-players)
(setq emms-source-file-default-directory "~/Music/") ;; Change to your music folder




;; Choose one of these
(setq emms-info-functions '(emms-info-tinytag))  ;; When using Tinytag
;;(setq emms-info-functions '(emms-info-exiftool)) When using Exiftool




;; Load cover images
(setq emms-browser-covers 'emms-browser-cache-thumbnail-async)


;; Keyboard shortcuts
(global-set-key (kbd "<XF86AudioPrev>") 'emms-previous)
(global-set-key (kbd "<XF86AudioNext>") 'emms-next)
(global-set-key (kbd "<XF86AudioPlay>") 'emms-pause)


;; saving desktop frames to session files
;;(desktop-save-mode 1)

; No delay in showing suggestions.
(setq company-idle-delay 0)

; Show suggestions after entering one character.
(setq company-minimum-prefix-length 1)
(setq company-selection-wrap-around t)


;;emacs-howdoyou configuration
(setq howdoyou-number-of-answers 10)
(global-set-key (kbd "C-c s h") 'howdoyou-query)
 (eval-after-load "howdoyou"
    '(progn
       (define-key howdoyou-mode-map (kbd "C-c <left>") #'howdoyou-previous-link)
       (define-key howdoyou-mode-map (kbd "C-c <right>") #'howdoyou-next-link)))

;; treemacs configuration
(setq doom-themes-treemacs-theme "doom-colors")
(setq doom-themes-treemacs-enable-variable-pitch t)
(setq doom-themes-treemacs-variable-pitch-face 'variable-pitch)

;; modeline
(setq doom-modeline-buffer-file-name-style 'truncate-with-project)


;; saving/restoring sessions with frames


;; vterm
(add-hook 'vterm-mode-hook (lambda () (hide-mode-line-mode -1)))
