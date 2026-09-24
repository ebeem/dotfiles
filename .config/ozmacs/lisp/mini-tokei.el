;;; mini-tokei.el --- Transient interface for tokei in Org mode -*- lexical-binding: t; -*-

(require 'transient)
(require 'project)
(require 'org)

(defgroup mini-tokei nil
  "Transient front-end for tokei."
  :group 'tools
  :prefix "mini-tokei-")

(defcustom mini-tokei-executable "tokei"
  "Path to the tokei executable."
  :type 'string)

(defcustom mini-tokei-buffer-name "*mini-tokei*"
  "Name of the buffer used to display tokei output."
  :type 'string)

(defvar-local mini-tokei--last-dir nil
  "Directory used for the last tokei run in this buffer.")

(defvar-local mini-tokei--last-args nil
  "Arguments used for the last tokei run in this buffer.")

(defun mini-tokei-rerun ()
  "Re-run tokei with the last used directory and arguments."
  (interactive)
  (if mini-tokei--last-dir
      (mini-tokei-run mini-tokei--last-dir mini-tokei--last-args)
    (user-error "No prior tokei run recorded in this buffer")))

;; Inherit from org-mode so formatting works, but isolate keybindings
(defvar mini-tokei-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'mini-tokei-rerun)
    map)
  "Keymap for `mini-tokei-mode`.")

(define-derived-mode mini-tokei-mode org-mode "Mini-Tokei"
  "Major mode for viewing tokei statistics."
  :group 'mini-tokei)

(defun mini-tokei--render-org (dir output-str &optional args)
  "Render tokei OUTPUT-STR for DIR in `mini-tokei-mode`."
  (let ((buf (get-buffer-create mini-tokei-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (mini-tokei-mode)
        (setq mini-tokei--last-dir dir)
        (setq mini-tokei--last-args args)

        (insert "#+TITLE: Tokei Code Statistics\n")
        (insert (format "#+DATE: %s\n" (format-time-string "[%Y-%m-%d %H:%M]")))
        (insert (format "#+PROPERTY: DIR %s\n\n" dir))
        (insert (format "* Statistics for =%s=\n\n" (abbreviate-file-name dir)))

        (insert "#+begin_example\n")
        (insert output-str)
        (unless (string-suffix-p "\n" output-str)
          (insert "\n"))
        (insert "#+end_example\n\n")

        (insert "* Navigation\n")
        (insert "- Press =q= to bury this buffer.\n")
        (insert "- Press =g= to re-run.\n"))
      (setq buffer-read-only t)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun mini-tokei-run (dir &optional args)
  "Execute tokei in DIR with optional command-line ARGS list."
  (unless (executable-find mini-tokei-executable)
    (user-error "Executable '%s' not found. Please install tokei" mini-tokei-executable))
  (let* ((default-directory (file-name-as-directory (expand-file-name dir)))
         (cmd-args (append (flatten-tree (ensure-list args)) (list default-directory)))
         (output (with-output-to-string
                   (with-current-buffer standard-output
                     (apply #'process-file mini-tokei-executable nil t nil cmd-args)))))
    (mini-tokei--render-org default-directory output args)))

(defun mini-tokei--find-project-root ()
  "Find current project root, prompting if none is found."
  (if-let* ((pr (project-current nil default-directory)))
      (project-root pr)
    (if-let* ((pr-prompt (project-current t default-directory)))
        (project-root pr-prompt)
      (user-error "No project selected"))))

;;;###autoload (autoload 'mini-tokei-dispatch "mini-tokei" nil t)
(transient-define-prefix mini-tokei-dispatch ()
  "Transient menu for running tokei."
  :value '()
  ["Arguments"
   ("-f" "Files count" "--files")
   ("-c" "Compact display" "--compact")
   ("-s" "Sort by lines" ("-s" "lines"))
   ("-C" "Sort by comments" ("-s" "comments"))
   ("-t" "Types / File extensions" "--types")]
  ["Actions"
   ("p" "Current Project"
    (lambda (&optional args)
      (interactive (list (transient-args 'mini-tokei-dispatch)))
      (mini-tokei-run (mini-tokei--find-project-root) args)))
   ("d" "Current Directory"
    (lambda (&optional args)
      (interactive (list (transient-args 'mini-tokei-dispatch)))
      (mini-tokei-run default-directory args)))
   ("f" "Prompt for Directory..."
    (lambda (&optional args)
      (interactive (list (transient-args 'mini-tokei-dispatch)))
      (let ((chosen-dir (read-directory-name "Run tokei on: " default-directory)))
        (mini-tokei-run chosen-dir args))))])

(provide 'mini-tokei)
;;; mini-tokei.el ends here
