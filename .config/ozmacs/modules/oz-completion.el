;;; Code:
;; https://github.com/justbur/emacs-which-key
;; displays the key bindings following your currently entered incomplete command
;; TODO: keybindings to cycle next/prev options
(use-package which-key
  :ensure nil
  :defer 1
  :init
  (with-eval-after-load 'page-ext
    (which-key-add-key-based-replacements
      "C-x C-p" "page-extras"))
  (with-eval-after-load 'org
    (which-key-add-keymap-based-replacements org-mode-map
      "C-c \""      "org-plot"
      "C-c C-v"     "org-babel"
      "C-c C-x"     "org-extra-commands"))
  :diminish
  :config
  (which-key-add-key-based-replacements
    "<f1> 4"        "help-other-win"
    "<f1>"          "help"
    "<f2>"          "2-column"
    "C-c"           "mode-and-user"
    "C-h 4"         "help-other-win"
    "C-h"           "help"
    "C-x 4"         "other-window"
    "C-x 5"         "other-frame"
    "C-x 6"         "2-column"
    "C-x 8"         "insert-special"
    "C-x C-k C-q"   "kmacro-counters"
    "C-x C-k C-r a" "kmacro-add"
    "C-x C-k C-r"   "kmacro-register"
    "C-x C-k"       "keyboard-macros"
    "C-x RET"       "encoding/input"
    "C-x a i"       "abbrevs-inverse-add"
    "C-x a"         "abbrevs"
    "C-x n"         "narrowing"
    "C-x p"         "projects"
    "C-x r"         "reg/rect/bkmks"
    "C-x t ^"       "tab-bar-detach"
    "C-x t"         "tab-bar"
    "C-x v M"       "vc-mergebase"
    "C-x v b"       "vc-branch"
    "C-x v"         "version-control"
    "C-x w ^"       "window-detach"
    "C-x w"         "window-extras"
    "C-x x"         "buffer-extras"
    "C-x"           "extra-commands"
    "M-g"           "goto-map"
    "M-s h"         "search-highlight"
    "M-s"           "search-map")
  (which-key-add-key-based-replacements
    "C-c b" "buffers"
    "C-c c" "code"
    "C-c d" "dired"
    "C-c e" "evaluate"
    "C-c f" "files"
    "C-c g" "git"
    "C-c h" "help"
    "C-c n" "notes"
    "C-c o" "open"
    "C-c p" "projects"
    "C-c s" "search"
    "C-c w" "windows")
  (which-key-mode)
  (setq which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-key-order-alpha
        which-key-allow-imprecise-window-fit nil
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-side-window-max-height 0.35
        which-key-idle-delay 2.5
        which-key-idle-secondary-delay 1
        which-key-max-description-length 25
        which-key-allow-imprecise-window-fit nil
        which-key-separator " → " ))

;; (use-package which-key-posframe
;;   :config
;;   (setq which-key-posframe-border-width 2
;;         which-key-posframe-poshandler 'posframe-poshandler-frame-center)
;;   (which-key-posframe-mode))

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-delay 0.5)
  (corfu-auto-prefix 2)
  (corfu-popupinfo-delay 0.5)
  (corfu-scroll-margin 5)
  :hook ((prog-mode . corfu-mode)
		 (eshell-mode . corfu-mode)
         (corfu-mode . corfu-popupinfo-mode)))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
  (setq vertico-resize nil
        vertico-count 15
        vertico-cycle t)
  :bind
  (:map vertico-map
        ("C-j" . vertico-next)
        ("C-k" . vertico-previous)
        ("C-M-j" . vertico-next-group)
        ("C-M-k" . vertico-previous-group)
        ("C-SPC" . +vertico/embark-preview)
        ("TAB" . vertico-insert)
        ("?" . minibuffer-completion-help)
        ("C-'" . vertico-quick-jump)))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :ensure t
  :defer t
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :bind
  (
   ([remap bookmark-jump] . consult-bookmark)
   ([remap evil-show-marks] . consult-mark)
   ([remap evil-show-jumps] . +vertico/jump-list)
   ([remap evil-show-registers] . consult-register)
   ([remap goto-line] . consult-goto-line)
   ([remap imenu] . consult-imenu)
   ([remap Info-search] . consult-info)
   ([remap locate] . consult-locate)
   ([remap load-theme] . consult-theme)
   ([remap man] . consult-man)
   ([remap recentf-open-files] . consult-recent-file)
   ([remap switch-to-buffer] . consult-buffer)
   ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
   ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
   ([remap project-switch-to-buffer] . consult-project-buffer)
   ([remap yank-pop] . consult-yank-pop)
   ([remap persp-switch-to-buffer] . +vertico/switch-workspace-buffer))
  :bind (:map eb/project-map
              ("s" . consult-ripgrep))
  :config
  (setq consult-ripgrep-args "rg --hidden --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --with-filename --line-number --search-zip -g !.git"
		consult-narrow-key "<"
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay  0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1)

  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key "C-SPC")

  (consult-customize
   consult-theme
   :preview-key (list "C-SPC" :debounce 0.5 'any)))


;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :ensure t
  :config
  (file-name-shadow-mode 1)
  :init
  (marginalia-mode))

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         :map minibuffer-local-map
         ("C-c C-c" . embark-collect)
         ("C-c C-e" . embark-export)))

(use-package embark-consult
  :ensure t
  :after (embark consult))

(use-package minibuffer
  :ensure nil
  :config
  (setq completion-show-help nil
		completion-show-inline-help nil
		completions-detailed t
		completions-format 'one-column
		completions-max-height 12
		completions-sort 'historical
		completion-auto-help t
		completion-auto-select ni
        minibuffer-visible-completions t
		completion-eager-display t
		completion-eager-update t))

(provide 'oz-completion)
;;; oz-completion.el ends here
