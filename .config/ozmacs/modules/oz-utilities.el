;;; Code:
;; reload configurations
(defun utils-reload-init ()
  "Reload configurations and ignore elpaca"
  (interactive)
  (load-file (locate-user-emacs-file "init.el"))
  (ignore (elpaca-process-queues)))

;; file opening procedures
(defun dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (call-process "xdg-open" nil 0 nil file)))

;; doom's escape hook
(defun eb/escape (&optional interactive)
  (interactive (list 'interactive))
  (cond ((minibuffer-window-active-p (minibuffer-window))
      ;; quit the minibuffer if open.
      (when interactive
        (setq this-command 'abort-recursive-edit))
      (abort-recursive-edit))
      ;; Run all escape hooks. If any returns non-nil, then stop there.
      ((run-hook-with-args-until-success 'keyboard-escape-hook))
      ;; don't abort macros
      ((or defining-kbd-macro executing-kbd-macro) nil)
      ;; Back to the default
      ((unwind-protect (keyboard-quit)
        (when interactive
          (setq this-command 'keyboard-quit))))))

(add-hook 'evil-collection-setup-hook #'eb/evil-keybindings-hook)
(global-set-key [remap keyboard-quit] #'eb/escape)

(defun eb/escape-multiple-cursors ()
   "Clear evil-mc cursors and restore state."
   (when (evil-mc-has-cursors-p)
    (evil-mc-undo-all-cursors)
    (evil-mc-resume-cursors)
        t))
(add-hook 'keyboard-escape-hook 'eb/escape-multiple-cursors)

;;;###autoload
(defun color-color (name &optional type)
  "Retrieve a specific color named NAME (a symbol) from the current theme."
  (let ((colors (if (listp name)
                    name
                  (cdr-safe (assq name doom-themes--colors)))))
    (and colors
         (cond ((listp colors)
                (let ((i (or (plist-get '(256 1 16 2 8 3) type) 0)))
                  (if (> i (1- (length colors)))
                      (car (last colors))
                    (nth i colors))))
               (t colors)))))

(defun color-name-to-rgb (color)
  "Retrieves the hexidecimal string repesented the named COLOR (e.g. \"red\")
for FRAME (defaults to the current frame)."
  (cl-loop with div = (float (car (tty-color-standard-values "#ffffff")))
           for x in (tty-color-standard-values (downcase color))
           collect (/ x div)))


(defun color-blend (color1 color2 alpha)
  "Blend two colors (hexidecimal strings) together by a coefficient ALPHA (a
float between 0 and 1)"
  (when (and color1 color2)
    (cond ((and color1 color2 (symbolp color1) (symbolp color2))
           (color-blend (color-color color1) (color-color color2) alpha))

          ((or (listp color1) (listp color2))
           (cl-loop for x in color1
                    when (if (listp color2) (pop color2) color2)
                    collect (color-blend x it alpha)))

          ((and (string-prefix-p "#" color1) (string-prefix-p "#" color2))
           (apply (lambda (r g b) (format "#%02x%02x%02x" (* r 255) (* g 255) (* b 255)))
                  (cl-loop for it    in (color-name-to-rgb color1)
                           for other in (color-name-to-rgb color2)
                           collect (+ (* alpha it) (* other (- 1 alpha))))))

          (color1))))


(defun color-darken (color alpha)
  "Darken a COLOR (a hexidecimal string) by a coefficient ALPHA (a float between
0 and 1)."
  (cond ((and color (symbolp color))
         (color-darken (color-color color) alpha))

        ((listp color)
         (cl-loop for c in color collect (color-darken c alpha)))

        ((color-blend color "#000000" (- 1 alpha)))))

;;;###autoload
(defun color-lighten (color alpha)
  "Brighten a COLOR (a hexidecimal string) by a coefficient ALPHA (a float
between 0 and 1)."
  (cond ((and color (symbolp color))
         (color-lighten (color-color color) alpha))

        ((listp color)
         (cl-loop for c in color collect (color-lighten c alpha)))

        ((color-blend color "#FFFFFF" (- 1 alpha)))))

;; delete current file
(defun delete-current-file ()
  "Delete current buffer/file and close the buffer"
  (interactive)
  (progn
    (when (buffer-file-name)
      (when (file-exists-p (buffer-file-name))
        (progn
          (delete-file (buffer-file-name))
          (message "Deleted file: 「%s」." (buffer-file-name)))))
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

;; https://gitlab.com/tsc25/undo-tree
;; recover any past state of a buffer. To do this, Emacs treats “undo” itself
;; as just another editing action that can be undone
(use-package undo-tree
  :defer t
  :custom
  (undo-tree-history-directory-alist
   `(("." . ,(expand-file-name (file-name-as-directory ".cache/undo-tree-hist")
                               user-emacs-directory))))
  :init
  (global-undo-tree-mode)
  :config
  (setq undo-tree-visualizer-diff       t
        undo-tree-visualizer-timestamps t
        undo-tree-auto-save-history     t
        undo-tree-enable-undo-in-region t
        undo-limit        (* 200 1024)
        undo-strong-limit (* 12 1024 1024)
        undo-outer-limit  (* 12 1024 1024)))

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
  :elpaca nil
  :custom (recentf-save-file (expand-file-name ".cache/recentf" user-emacs-directory))
  :config
  (setq recentf-auto-cleanup nil
        recentf-max-saved-items 200))

(use-package savehist
  :elpaca nil
  :custom (savehist-file (expand-file-name ".cache/history" user-emacs-directory)))

(use-package project
  :elpaca nil
  :config
  (setq project-list-file (expand-file-name ".cache/projects" user-emacs-directory)))

(provide 'oz-utilities)
;;; oz-utilities.el ends here
