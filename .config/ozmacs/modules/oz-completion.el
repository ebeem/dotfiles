
;;; Code:
(use-package company
  :defer 2
  :diminish
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 0)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t))

(use-package company-box
  :after company
  :diminish
  :hook (company-mode . company-box-mode))

(use-package vertico
  :init
  (vertico-mode)
  :config
  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle t)
  (setq-default completion-in-region-function
                (lambda (&rest args)
                  (apply (if vertico-mode
                             #'consult-completion-in-region
                           #'completion--in-region)
                         args)))

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
    ([remap yank-pop] . consult-yank-pop)
    ([remap persp-switch-to-buffer] . +vertico/switch-workspace-buffer))
  :config
  (defadvice! +vertico--consult-recent-file-a (&rest _args)
    "`consult-recent-file' needs to have `recentf-mode' on to work correctly"
    :before #'consult-recent-file
    (recentf-mode +1))

  (setq consult-narrow-key "<"
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay  0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1)

  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   +default/search-project +default/search-other-project
   +default/search-project-for-symbol-at-point
   +default/search-cwd +default/search-other-cwd
   +default/search-notes-for-symbol-at-point
   +default/search-emacsd
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


(use-package which-key
  :init
  (setq which-key-side-window-location 'bottom
	which-key-sort-order #'which-key-key-order-alpha
	which-key-allow-imprecise-window-fit nil
	which-key-sort-uppercase-first nil
	which-key-add-column-padding 1
	which-key-max-display-columns nil
	which-key-min-display-lines 6
	which-key-side-window-slot -10
	which-key-side-window-max-height 0.35
	which-key-idle-delay 0.3
	which-key-idle-secondary-delay 0.1
	which-key-max-description-length 25
	which-key-allow-imprecise-window-fit nil
	which-key-separator " → " )
  (which-key-mode 1)
  :diminish
  :config)

(use-package helpful
  :bind
  ([premap describe-command] . helpful-command)
  ([premap describe-key] . helpful-key))

(provide 'oz-completion)
;;; oz-completion.el ends here
