;;; Code:

;; https://github.com/justbur/emacs-which-key
;; displays the key bindings following your currently entered incomplete command
;; TODO: keybindings to cycle next/prev options
(use-package which-key
  :init
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
        which-key-idle-delay 1
        which-key-idle-secondary-delay 2
        which-key-max-description-length 25
        which-key-allow-imprecise-window-fit nil
        which-key-separator " → " ))

(use-package company
  :defer 2
  :diminish
  :hook (company-mode . evil-normalize-keymaps)
  :init (global-company-mode)
  :config
  (setq company-minimum-prefix-length     2
        company-toolsip-limit             14
        company-tooltip-align-annotations t
        company-require-match             nil
        company-global-modes              '(not erc-mode message-mode help-mode gud-mode)
        company-backends '(company-capf)
        company-auto-commit         nil
        company-auto-complete-chars nil
        company-dabbrev-other-buffers nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase    nil
        company-begin-commands '(self-insert-command)
        company-idle-delay 0.1
        company-show-numbers t))

;; TODO: configure icons with nerd
(use-package company-box
  :after (company nerd-icons-completion)
  :diminish
  :hook (company-mode . company-box-mode))

(use-package vertico
  :init
  (vertico-mode)
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
(use-package vertico-posframe
  :after vertico
  :init
  (vertico-posframe-mode 1)
  :config
  (setq vertico-posframe-parameters
        '((left-fringe . 12)
          (right-fringe . 12))))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :elpaca nil
  :init
  (savehist-mode))

(use-package consult
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
  :init
  (marginalia-mode))

(use-package helpful
  :bind
  ([premap describe-command] . helpful-command)
  ([premap describe-key] . helpful-key))

(provide 'oz-completion)
;;; oz-completion.el ends here
