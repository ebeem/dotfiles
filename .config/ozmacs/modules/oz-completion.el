;;; Code:
;; https://github.com/justbur/emacs-which-key
;; displays the key bindings following your currently entered incomplete command
;; TODO: keybindings to cycle next/prev options
(use-package which-key
  :ensure nil
  :init
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

  ;; Upon loading, the built-in `page-ext' package turns "C-x C-p" into
  ;; a prefix-key.  If you know of other built-in packages that have
  ;; this behavior, please let me know, so I can add them.
  (with-eval-after-load 'page-ext
    (which-key-add-key-based-replacements
      "C-x C-p" "page-extras"))

  ;; Org-mode provides some additional prefix-keys in `org-mode-map'.
  (with-eval-after-load 'org
    (which-key-add-keymap-based-replacements org-mode-map
      "C-c \""      "org-plot"
      "C-c C-v"     "org-babel"
      "C-c C-x"     "org-extra-commands"))

  ;; custom keybinding
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
  :diminish
  :config
  (setq which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-key-order-alpha
        which-key-allow-imprecise-window-fit nil
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-side-window-max-height 0.35
        which-key-idle-delay 0.2
        which-key-idle-secondary-delay 0
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
         (corfu-mode . corfu-popupinfo-mode)))
  
(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; (use-package company
;;   :defer 2
;;   :diminish
;;   :hook (company-mode . evil-normalize-keymaps)
;;   :init (global-company-mode)
;;   :config
;;   (setq company-minimum-prefix-length     2
;;         company-toolsip-limit             14
;;         company-tooltip-align-annotations t
;;         company-require-match             nil
;;         company-global-modes              '(not erc-mode message-mode help-mode gud-mode)
;;         company-backends '(company-capf)
;;         company-auto-commit         nil
;;         company-auto-complete-chars nil
;;         company-dabbrev-other-buffers nil
;;         company-dabbrev-ignore-case nil
;;         company-dabbrev-downcase    nil
;;         company-begin-commands '(self-insert-command)
;;         company-idle-delay 0.1
;;         company-show-numbers t))

;; ;; TODO: configure icons with nerd
;; (use-package company-box
;;   :after (company nerd-icons-completion)
;;   :diminish
;;   :hook (company-mode . company-box-mode))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :ensure t
  :config
  (setq vertico-resize nil
        vertico-count 17
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

;; lets 'vertico' use 'posframe' to show its candidate menu
;; (use-package vertico-posframe
;;   :after vertico
;;   :init
;;   (vertico-posframe-mode 1)
;;   :config
;;   (advice-add 'vertico-posframe--show
;;               :before
;;               (defun vertico-posframe--show/before (&rest _args)
;;                 (setq vertico-posframe-truncate-lines
;;                       (< (point) (* 0.8 (window-width (active-minibuffer-window)))))))
;;   (setq vertico-posframe-parameters
;;         '((left-fringe . 12)
;;           (right-fringe . 12))))

;; Optionally use the `orderless' completion style.
;; (use-package orderless
;;   :ensure t
;;   :config
;;   (setq completion-styles '(orderless basic)
;;         completion-category-defaults nil
;;         completion-category-overrides '((file (styles partial-completion)))))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :ensure t
  :defer t
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
   ([remap switch-to-buffer-other-window] . nsult-buffer-other-window)
   ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
   ([remap project-switch-to-buffer] . consult-project-buffer)
   ([remap yank-pop] . consult-yank-pop)
   ([remap persp-switch-to-buffer] . +vertico/switch-workspace-buffer))
  :bind (:map eb/project-map
              ("s" . consult-ripgrep))
  :config
  (setq consult-narrow-key "<"
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
  :hook (rfn-eshadow-update-overlay-hook . vertico-directory-tidy)
  :config
  (setq file-name-shadow-mode 1)
  :init
  (marginalia-mode))

;; (use-package helpful
;;   :bind
;;   ([premap describe-command] . helpful-command)
;;   ([premap describe-key] . helpful-key))

(provide 'oz-completion)
;;; oz-completion.el ends here
