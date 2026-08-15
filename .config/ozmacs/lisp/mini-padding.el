;;; mini-padding.el --- Increase the padding/spacing of frames and windows -*- lexical-binding: t -*-
;;; stripped version from https://github.com/protesilaos/spacious-padding/blob/main/spacious-padding.el
;;; Code:

(defgroup mini-padding ()
  "Increase the padding/spacing of frames and windows."
  :group 'faces
  :group 'frames)

;; NOTE 2025-01-06: This is what `use-package' does with its own
;; theme, so it is probably the right approach for us too.
(eval-and-compile
  ;; Declare a synthetic theme for :custom variables.
  ;; Necessary in order to avoid having those variables saved by custom.el.
  (deftheme mini-padding))

(enable-theme 'mini-padding)
;; Remove the synthetic mini-padding theme from the enabled themes, so
;; iterating over them to "disable all themes" won't disable it.
(setq custom-enabled-themes (remq 'mini-padding custom-enabled-themes))

(defcustom mini-padding-widths
  '( :internal-border-width 15
     :mode-line-width 6
     :right-divider-width 30
     :scroll-bar-width 8
     :fringe-width 8)
  "Set the pixel width of individual User Interface elements.
This is a plist of the form (:key1 value1 :key2 value2).  The
value is always a natural number."
  :type '(plist
          :key-type (choice (const :internal-border-width)
                            (const :right-divider-width)
                            (const :fringe-width)
                            (const :left-fringe-width)
                            (const :right-fringe-width)
                            (const :mode-line-width)
                            (const :scroll-bar-width))
          :value-type (choice natnum (const nil)))
  :package-version '(mini-padding . "0.8.0")
  :group 'mini-padding)

(define-obsolete-face-alias
  'mini-padding-subtle-mode-line-active
  'mini-padding-line-active
  "0.8.0")

(defface mini-padding-line-active
  '((((class color) (min-colors 88) (background light))
     :foreground "#0033dd")
    (((class color) (min-colors 88) (background dark))
     :foreground "#88aaff"))
  "Optional face for the active mode line.
This is something the user can define per the documentation of
`mini-padding-subtle-frame-lines'.")

(define-obsolete-face-alias
  'mini-padding-subtle-mode-line-inactive
  'mini-padding-line-inactive
  "0.8.0")

(defface mini-padding-line-inactive
  '((((class color) (min-colors 88) (background light))
     :foreground "#cfcfcf")
    (((class color) (min-colors 88) (background dark))
     :foreground "#585858"))
  "Optional face for the inactive mode line.
This is something the user can define per the documentation of
`mini-padding-subtle-frame-lines'.")

(define-obsolete-variable-alias
  'mini-padding-subtle-mode-line
  'mini-padding-subtle-frame-lines
  "0.8.0")

(defcustom mini-padding-subtle-frame-lines nil
  "Remove the background from the mode lines and add overlines."
  :type '(choice boolean
                 (plist
                  :key-type (choice (const :mode-line-active)
                                    (const :mode-line-inactive))
                  :value-type (choice string face)))
  :package-version '(mini-padding . "0.8.0")
  :group 'mini-padding)

;; NOTE 2023-12-05: The `keycast-key' is treated as a mode line face
;; which means that the mode line padding will be applied to it.
(defvar mini-padding--mode-line-faces
  '(mode-line mode-line-active mode-line-inactive mode-line-highlight keycast-key)
  "Mode line faces relevant to `mini-padding-mode'.")

(defun mini-padding--get-window-divider-width (&optional no-fallback)
  "Get the width of window divider.
With optional NO-FALLBACK return nil if there is no value.  Else return
a reasonable fallback value."
  (cond
   ((plist-get mini-padding-widths :right-divider-width))
   (no-fallback nil)
   (t 30)))

(defun mini-padding--get-box-width (key &optional no-fallback)
  "Get width for :box of face represented by KEY in `mini-padding-widths'.
Return 4 if KEY does not have a value.  If optional NO-FALLBACK
is non-nil, do not return a fallback value: just nil."
  (cond
   ((plist-get mini-padding-widths key))
   (no-fallback nil)
   (t 4)))

(defun mini-padding--get-face-width (face)
  "Return width of FACE from `mini-padding-widths'."
  (cond
   ((memq face mini-padding--mode-line-faces)
    (mini-padding--get-box-width :mode-line-width))
   (t (error "`%s' is not relevant to `mini-padding-mode'" face))))

;; NOTE 2026-07-23: Without this I was getting "unspecified-bg" and
;; "unspecified-fg" when (i) running Emacs as a daemon, (ii)
;; connecting via emacsclient, and (iii) checking the Messages.
(defun mini-padding--face-attribute (face attribute &optional frame inherit)
  "Wrapper for `face-attribute' to also do the right thing in a TTY.
FACE, ATTRIBUTE, FRAME, and INHERIT have the same meaning as in
`face-attribute'."
  (cond
   ((when-let* ((value (face-attribute face attribute frame inherit))
                (_ (not (member value '("unspecified-bg" "unspecified-fg")))))
      value))
   ((when-let* ((_ (eq attribute :background))
                (background (frame-parameter nil 'background-color))
                (_ (not (string= background "unspecified-bg"))))
      background))
   ((when-let* ((_ (eq attribute :foreground))
                (foreground (frame-parameter nil 'foreground-color))
                (_ (not (string= foreground "unspecified-fg"))))
      foreground))
   ((eq (frame-parameter nil 'background-mode) 'light)
    (if (eq attribute :foreground)
        "black"
      "white"))
   ((eq (frame-parameter nil 'background-mode) 'dark)
    (if (eq attribute :foreground)
        "white"
      "black"))))

(defun mini-padding--face-background (face &optional frame inherit)
  "Wrapper for `face-background'.
FACE, FRAME, and INHERIT as the same as in `face-background'."
  (mini-padding--face-attribute face :background frame inherit))

(defun mini-padding--face-foreground (face &optional frame inherit)
  "Wrapper for `face-foreground'.
FACE, FRAME, and INHERIT as the same as in `face-foreground'."
  (mini-padding--face-attribute face :foreground frame inherit))

(defun mini-padding--get-face-line-color (face fallback subtle-key)
  "Get overline foreground.
Use SUBTLE-KEY to check `mini-padding-subtle-frame-lines', falling
back to FACE, then FALLBACK."
  (let ((subtle-value (plist-get mini-padding-subtle-frame-lines subtle-key)))
    (cond
     ((stringp subtle-value) subtle-value)
     ((facep subtle-value) (mini-padding--face-foreground subtle-value nil face))
     (t (mini-padding--face-foreground face nil fallback)))))

(defun mini-padding-set-face-box-padding (face fallback &optional subtle-key)
  "Return face attributes for FACE with FALLBACK face background.
With optional SUBTLE-KEY, read its value from the
`mini-padding-subtle-frame-lines' and apply it to FACE as an
overline."
  (when (facep face)
    (let* ((original-bg (or (mini-padding--face-background face nil fallback) 'unspecified))
           (subtle-bg (mini-padding--face-background 'default))
           (subtlep (and subtle-key mini-padding-subtle-frame-lines))
           (bg (if subtlep subtle-bg original-bg))
           (face-width (mini-padding--get-face-width face)))
      `(,@(when subtlep
            (list :background bg
                  :overline (or (mini-padding--get-face-line-color face fallback subtle-key) t)))
        ,@(unless (eq face-width 0)
            (list
             :box
             `( :line-width ,face-width
                :color ,(if (eq bg 'unspecified) nil bg)
                :style nil)))))))

(defun mini-padding-set-window-divider (face color)
  "Set window divider FACE to COLOR if its width is greater than 1."
  (list
   face
   `((t
      ,(when (> (mini-padding--get-window-divider-width) 1)
         (list :background color :foreground color))))))

(define-obsolete-function-alias
  'mini-padding-set-invisible-dividers
  'mini-padding-set-faces
  "0.5.0")

;;;###autoload
(defun mini-padding-set-faces (&rest _)
  "Make window dividers invisible and add padding.
Ignore any arguments.  This is useful to add the function to abnormal
hooks that pass one or more arguments to it, such as
`after-make-frame-functions'."
  (let ((bg-main (mini-padding--face-background 'default))
        (fg-main (mini-padding--face-foreground 'default))
        custom--inhibit-theme-enable)
    (custom-theme-set-faces
     'mini-padding
     `(fringe ((t :background ,bg-main)))
     `(margin ((t :background ,bg-main)))
     `(line-number ((t :background ,bg-main)))
     `(keycast-key ((t ,@(mini-padding-set-face-box-padding 'keycast-key 'default))))
     `(mode-line ((t ,@(mini-padding-set-face-box-padding 'mode-line 'default :mode-line-active))))
     ;; We cannot use :inherit mode-line because it does not get our version of it...
     `(mode-line-active ((t ,@(mini-padding-set-face-box-padding 'mode-line-active 'mode-line :mode-line-active))))
     `(mode-line-inactive ((t ,@(mini-padding-set-face-box-padding 'mode-line-inactive 'mode-line :mode-line-inactive))))
     `(mode-line-highlight ((t :box (:color ,fg-main))))
     `(vertical-border ((t :background ,bg-main :foreground ,bg-main)))
     `(,@(mini-padding-set-window-divider 'window-divider bg-main))
     `(,@(mini-padding-set-window-divider 'window-divider-first-pixel bg-main))
     `(,@(mini-padding-set-window-divider 'window-divider-last-pixel bg-main)))))

(defun mini-padding-unset-invisible-dividers ()
  "Unset face definitions for `mini-padding'."
  (let (custom--inhibit-theme-enable)
    (custom-theme-set-faces
     'mini-padding
     '(fringe (( )))
     '(margin (( )))
     '(line-number (( )))
     '(keycast-key (( )))
     '(mode-line (( )))
     '(mode-line-active (( )))
     '(mode-line-inactive (( )))
     '(mode-line-highlight (( )))
     '(vertical-border (( )))
     '(window-divider (( )))
     '(window-divider-first-pixel (( )))
     '(window-divider-last-pixel (( ))))))

(defvar mini-padding--internal-border-width nil
  "Default value of frame parameter `internal-border-width'.")

(defvar mini-padding--right-divider-width nil
  "Default value of frame parameter `right-divider-width'.")

(defvar mini-padding--fringe-width nil
  "Default value of frame parameters `left-fringe' and `right-fringe'.")

(defvar mini-padding--left-fringe-width nil
  "Default value of frame parameter `left-fringe'.")

(defvar mini-padding--right-fringe-width nil
  "Default value of frame parameter `right-fringe'.")

(defvar mini-padding--scroll-bar-width nil
  "Default value of frame parameter `scroll-bar-width'.")

(defun mini-padding--store-default-parameters ()
  "Store default frame parameter values."
  (unless mini-padding--internal-border-width
    (setq mini-padding--internal-border-width
          (frame-parameter nil 'internal-border-width)))
  (unless mini-padding--right-divider-width
    (setq mini-padding--right-divider-width
          (frame-parameter nil 'right-divider-width)))
  (unless mini-padding--fringe-width
    (setq mini-padding--fringe-width 8)) ; 8 is the default per `fringe-mode'
  (unless mini-padding--left-fringe-width
    (setq mini-padding--left-fringe-width
          (frame-parameter nil 'left-fringe-width)))
  (unless mini-padding--right-fringe-width
    (setq mini-padding--right-fringe-width
          (frame-parameter nil 'right-fringe-width)))
  (unless mini-padding--scroll-bar-width
    (setq mini-padding--scroll-bar-width
          (frame-parameter nil 'scroll-bar-width))))

(defmacro mini-padding--define-get-frame-param (parameter fallback)
  "Define function to return frame PARAMETER or reset it with FALLBACK value."
  `(defun ,(intern (format "mini-padding--get-%s" parameter)) (&optional reset)
     ,(format "Return value of frame parameter `%s'.
With optional RESET argument as non-nil, restore the default
parameter value."
              parameter)
     (or
      (if reset
          ,(intern (format "mini-padding--%s" parameter))
        (plist-get mini-padding-widths ,(intern (concat ":" parameter))))
      ,fallback)))

(mini-padding--define-get-frame-param "internal-border-width" 15)
(mini-padding--define-get-frame-param "right-divider-width" 30)
(mini-padding--define-get-frame-param "fringe-width" 8)
(mini-padding--define-get-frame-param "left-fringe-width" nil)
(mini-padding--define-get-frame-param "right-fringe-width" nil)
(mini-padding--define-get-frame-param "scroll-bar-width" 8)

(defun mini-padding-modify-frame-parameters (&optional frame reset)
  "Modify spacing of all frames or optional FRAME.
With optional RESET argument as non-nil, restore the default
parameter values."
  (let ((parameters `((internal-border-width . ,(mini-padding--get-internal-border-width reset))
                      (right-divider-width . ,(mini-padding--get-right-divider-width reset))
                      (left-fringe . ,(or (mini-padding--get-left-fringe-width reset)
                                          (mini-padding--get-fringe-width reset)))
                      (right-fringe . ,(or (mini-padding--get-right-fringe-width reset)
                                           (mini-padding--get-fringe-width reset)))
                      (scroll-bar-width  . ,(mini-padding--get-scroll-bar-width reset)))))
    (if frame
        (modify-frame-parameters frame parameters)
      (modify-all-frames-parameters parameters))))

;;;###autoload
(defun mini-padding-set-parameters-of-frame (frame)
  "Set the layout parameters of FRAME and update the faces."
  (mini-padding-modify-frame-parameters frame)
  (mini-padding-set-faces))

;;;###autoload
(defun mini-padding-set-parameters-of-selected-frame ()
  "Use `mini-padding-set-parameters-of-frame' for the `selected-frame'."
  (when-let* ((frame (selected-frame)))
    (mini-padding-set-parameters-of-frame frame)))

(defun mini-padding--enable-mode ()
  "Enable `mini-padding-mode'."
  (mini-padding--store-default-parameters)
  (mini-padding-modify-frame-parameters)
  (mini-padding-set-faces)
  (add-hook 'window-divider-mode-hook #'mini-padding--enable-mode)
  (add-hook 'enable-theme-functions #'mini-padding-set-faces)
  (add-hook 'after-make-frame-functions #'mini-padding-set-parameters-of-frame)
  (add-hook 'server-after-make-frame-hook #'mini-padding-set-parameters-of-selected-frame))

(defun mini-padding--disable-mode ()
  "Disable `mini-padding-mode'."
  (mini-padding-modify-frame-parameters nil :reset)
  (mini-padding-unset-invisible-dividers)
  (remove-hook 'window-divider-mode-hook #'mini-padding--enable-mode)
  (remove-hook 'enable-theme-functions #'mini-padding-set-faces)
  (remove-hook 'after-make-frame-functions #'mini-padding-set-parameters-of-frame)
  (remove-hook 'server-after-make-frame-hook #'mini-padding-set-parameters-of-selected-frame))

;;;###autoload
(define-minor-mode mini-padding-mode
  "Increase the padding/spacing of frames and windows."
  :global t
  (if mini-padding-mode
      (mini-padding--enable-mode)
    (mini-padding--disable-mode)))

(provide 'mini-padding)
;;; mini-padding.el ends here
