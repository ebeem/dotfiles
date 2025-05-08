(use-package dired
  :ensure nil
  :init
  (defvar-keymap eb/dired-map)
  (defvar-keymap eb/open-map)
  (defvar-keymap eb/files-map)
  :bind-keymap (("C-c d" . eb/dired-map))
  :bind-keymap (("C-c f" . eb/files-map))
  :bind-keymap (("C-c o" . eb/open-map))
  :bind (
         :map eb/dired-map
         ("d" . dired-jump) 
         ("j" . dired-jump) 
         ("p" . peep-dired)
         
         :map eb/open-map
         ("d" . dired-jump)
         ("-" . dired-jump)
         
         :map eb/files-map
         ("c" . editorconfig-find-current-editorconfig)
         ("c" . copy-this-file)
         ("u" . sudo-edit)
         ("U" . sudo-edit-find-file)
         ("d" . delete-current-file)
         ("f" . find-file)
         ("p" . find-file-at-point)
         ("s" . save-buffer)
         ("r" . counsel-recentf))
  :config
  ;; file opening procedures
  (defun dired-open-file ()
    "In dired, open the file named on this line."
    (interactive)
    (let* ((file (dired-get-filename nil t)))
      (call-process "xdg-open" nil 0 nil file))))

;; "b" (cons "Buffer" my-test-buffer-map)
(use-package emacs
  :init
  (defvar-keymap eb/buffer-map :doc "Buffer")
  (defvar-keymap eb/evaluate-map :doc "Evaluate")
  :bind-keymap (("C-c b" . eb/buffer-map)
                ("C-c e" . eb/evaluate-map)
                ("C-c h" . help-map))
  :bind (
         :map global-map
         ("C-c :" . eval-expression)
         ("C-c ;" . execute-extended-command)
         ("C-c x" . scratch-buffer)
         ("C-c C" . org-capture)
         ("C-c >" . next-buffer)
         ("C-c <" . previous-buffer)

         :map eb/buffer-map
         ("b" . project-switch-to-buffer)
         ("B" . switch-to-buffer)
         ("c" . clone-indirect-buffer)
         ("d" . kill-current-buffer)
         ("h" . previous-buffer)
         ("k" . kill-current-buffer)
         ("l" . next-buffer)
         ("m" . bookmark-set)
         ("M" . bookmark-delete)
         ("n" . create-scratch-buffer) ;; TODO: improve
         ("p" . paste-buffer)
         ("r" . revert-buffer)
         ("R" . rename-buffer)
         ("s" . save-buffer)
         ("S" . save-all-buffers)
         ("x" . scratch-buffer)
         ("y" . yank-buffer)
         
         :map eb/evaluate-map
         ("d" . eval-defun)
         ("e" . eval-expression)
         ("" . counsel-esh-history)
         ("l" . eval-last-sexp)
         ("r" . eval-region)
         ("s" . eshell))
  
  :config
  ;; delete current file
  (defun delete-current-file ()
    "Delete the file associated with the current buffer"
    (interactive)
    (let ((filename (buffer-file-name)))
      (if (not filename)
          (message "No file is associated with this buffer.")
        (when (file-exists-p filename)
          (when (yes-or-no-p (format "Really delete file \"%s\"? " filename))
            (delete-file filename t)
            (message "Deleted file: 「%s」." filename))))
      (let ((buffer-offer-save nil))
        (set-buffer-modified-p nil)
        (kill-buffer (current-buffer)))))

  (defun copy-this-file
      (new-path &optional force-p)
    "Copy current buffer's file to NEW-PATH then open NEW-PATH.\n\nIf FORCE-P, overwrite the destination file if it exists, without confirmation."
    (interactive
     (list
      (read-file-name "Copy file to: ")
      current-prefix-arg))
    (if
        (and buffer-file-name
             (file-exists-p buffer-file-name))
        nil
      (user-error "Buffer is not visiting any file"))
    (let
        ((old-path
          (buffer-file-name
           (buffer-base-buffer)))
         (new-path
          (expand-file-name new-path)))
      (make-directory
       (file-name-directory new-path)
       't)
      (copy-file old-path new-path
                 (or force-p 1))
      (find-file new-path)
      (message "File copied to %S"
               (abbreviate-file-name new-path))))

  (defun create-scratch-buffer ()
    "Create a new buffer titled 'Scratch', or 'Scratch<N>' if 'Scratch' already exists."
    (interactive)
    (let ((base-name "Scratch")
          (counter 1)
          (buffer-name "Scratch"))
      ;; Find an unused buffer name
      (while (get-buffer buffer-name)
        (setq buffer-name (format "%s<%d>" base-name counter))
        (setq counter (1+ counter)))
      ;; Create and switch to the new buffer
      (switch-to-buffer (get-buffer-create buffer-name))))

  (defun yank-buffer ()
    "Yank current buffer's content"
    (interactive)
    (clipboard-kill-ring-save (point-min) (point-max)))

  (defun paste-buffer ()
    "Paste current buffer's content"
    (interactive)
    (delete-region (point-min) (point-max))
    (yank)))

(use-package undo-fu-session
  :init
  (undo-fu-session-global-mode))

(use-package sudo-edit)
(use-package visual-fill-column)

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook (pdf-view-mode . (lambda () (display-line-numbers-mode -1)))
  :hook (pdf-tools-enabled . pdf-view-midnight-minor-mode)
  :config
  (setq pdf-view-use-scaling t
        pdf-view-display-size 'fit-page
        pdf-view-use-imagemagick nil))

;; Keep track of recently opened files
(use-package recentf
  :ensure nil
  :custom (recentf-save-file (expand-file-name ".cache/recentf" user-emacs-directory))
  :config
  (setq recentf-auto-cleanup nil
        recentf-max-saved-items 200))

(use-package savehist
  :ensure nil
  :init
  (savehist-mode)
  :custom (savehist-file (expand-file-name ".cache/history" user-emacs-directory)))

(use-package project
  :ensure nil
  :init
  (defvar-keymap eb/project-map :doc "Project")
  :bind-keymap (("C-c p" . eb/project-map))
  :bind (:map eb/project-map
              ("!" . project-shell-command)
              ("&" . project-async-shell-command)
              ("f" . project-or-external-find-file)
              ("g" . project-or-external-find-regexp)
              ("b" . project-switch-to-buffer)
              ("c" . project-compile)
              ("d" . project-find-dir)
              ("D" . project-dired)
              ("e" . project-eshell)
              ("f" . project-find-file)
              ("g" . project-find-regexp)
              ("k" . project-kill-buffers)
              ("p" . project-switch-project)
              ("r" . project-query-replace-regexp)
              ("S" . project-search)
              ("v" . project-vc-dir)
              ("x" . project-execute-extended-command))
  :config
  (setq project-list-file (expand-file-name ".cache/projects" user-emacs-directory)))

(use-package windmove
  :ensure nil
  :init 
  (defvar-keymap eb/window-map :doc "Window")
  :bind-keymap (("C-c w" . eb/window-map))
  :bind (
         :map eb/window-map
         ("f" . make-frame)
         ("d" . delete-window)
         ("s" . split-window-vertically-and-focus)
         ("v" . split-window-horizontally-and-focus)
         ("h" . windmove-left)
         ("j" . windmove-down)
         ("k" . windmove-up)
         ("l" . windmove-right))
  :config
  (defun split-window-horizontally-and-focus ()
    "Split the window horizontally and focus on the new window."
    (interactive)
    (let ((new-window (split-window-horizontally)))
      (select-window new-window)))

  (defun split-window-vertically-and-focus ()
    "Split the window vertically and focus on the new window."
    (interactive)
    (let ((new-window (split-window-vertically)))
      (select-window new-window))))


(use-package burly
  :config
  (setq project-list-file (expand-file-name ".cache/projects" user-emacs-directory))
  (setq burly-frameset-filter-alist '((name . nil)
                                      (posframe-parent-buffer . :never))))

(use-package proced
  :ensure nil
  :commands proced
  :custom
  (proced-auto-update-flag t)
  (proced-goal-attribute nil)
  (proced-show-remote-processes t)
  (proced-enable-color-flag t)
  (proced-format 'custom)
  :config
  (add-to-list
   'proced-format-alist
   '(custom user pid ppid sess tree pcpu pmem rss start time state (args comm))))

(provide 'oz-utilities)
;;; oz-utilities.el ends here
