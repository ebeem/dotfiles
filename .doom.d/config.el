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
(setq doom-font (font-spec :family "JetBrains Mono" :size 17 :weight 'bold)
      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 16))

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
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/cloud/org/")

;; Make sure org-indent face is available
(require 'org-indent)

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
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

(defcustom lsp-file-watch-threshold 100000
  "Show warning if the files to watch are more than.
Set to nil to disable the warning."
  :type 'number
  :group 'lsp-mode)

;; (setq bidi-paragraph-direction 'right-to-left)

(setq projectile-project-search-path '(("~/workspace/" . 3) ("~/" . 2)))
(setq projectile-enable-caching nil)
(setq projectile-auto-discover nil)

;; (setq lsp-csharp-server-path "/usr/bin/omnisharp")

;; always reload buffers changed externally
(global-auto-revert-mode t)

;; org-mode configuration
;; add time stamp and note on task done
(setq org-log-done 'note)


;; elfeed
(setq rmh-elfeed-org-files (list "~/.doom.d/elfeed.org"))
(setq elfeed-goodies/entry-pane-position 'bottom)
(elfeed-goodies/setup)

;; (projectile-register-project-type 'godot '("project.godot")
;;                                   :project-file "project.godot"
;; 				  :compile "msbuild"
;; 				  :run "godot-mono -d --remote-debug localhost:45000")

;;(require 'emms-setup)
;;(emms-all)
;;(emms-default-players)
(setq emms-player-mpd-server-name "localhost"
      emms-player-mpd-server-port "6601"
      emms-player-mpd-music-directory "~/Music"
      emms-player-list '(emms-player-mpd))

;; Load cover images
(setq emms-browser-covers 'emms-browser-cache-thumbnail-async)
(emms-player-mpd-connect)

;; Keyboard shortcuts
(global-set-key (kbd "<XF86AudioPrev>") 'emms-previous)
(global-set-key (kbd "<XF86AudioNext>") 'emms-next)
(global-set-key (kbd "<XF86AudioPlay>") 'emms-pause)


;; saving desktop frames to session files
;;(desktop-save-mode 1)

(setq company-minimum-prefix-length 1   ; Show suggestions after entering one character.
      company-idle-delay 0              ; No delay in showing suggestions.
      company-selection-wrap-around t)


;;emacs-howdoyou configuration
(setq howdoyou-number-of-answers 10)
(map! :leader
      (:prefix-map ("s" . "search")
       :desc "How do you" "h" #'howdoyou-query))

(eval-after-load "howdoyou"
  '(progn
     (define-key howdoyou-mode-map (kbd "C-c k") #'howdoyou-previous-link)
     (define-key howdoyou-mode-map (kbd "C-c j") #'howdoyou-next-link)))

;; modeline
(setq doom-modeline-buffer-file-name-style 'truncate-with-project)
(setq doom-modeline-icon (display-graphic-p))
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-major-mode-color-icon t)
(setq doom-modeline-height 1)

;; vterm
(add-hook 'vterm-mode-hook (lambda () (hide-mode-line-mode -1)))

;; load env file
(doom-load-envvars-file "~/.doom.d/env")

;; mu4e
(setq mu4e-update-interval 30
      mu4e-change-filenames-when-moving t
      mu4e-get-mail-command "mbsync -a"
      mu4e-view-prefer-html nil
      mu4e-view-show-images t
      mu4e-display-update-status-in-modeline t)

;; TODO: mu4e bookmarks
;;

;; automatically highlight symbols under the curse
(setq highlight-symbol-idle-delay 0)
(add-hook 'prog-mode-hook 'highlight-symbol-mode)

;; custom vim keybindings
(global-set-key (kbd "<C-k>") 'drag-stuff-up)
(global-set-key (kbd "<C-j>") 'drag-stuff-down)
