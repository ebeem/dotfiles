;;; Code:
;;; Commentary

;; UI configuration
;; Remove some unneeded UI elements (the user can turn back on anything they wish)
(setq package-enable-at-startup nil     ; disable default package manager
      inhibit-startup-message     t     ; disable some warning messages
      frame-resize-pixelwise      t     ; fine resize
      native-comp-jit-compilation t
      max-lisp-eval-depth		  10000
      warning-minimum-level 	  :emergency
      native-comp-eln-load-path   (list (expand-file-name ".cache/eln-cache/" user-emacs-directory))
      package-native-compile      t)    ; native compile packages
(scroll-bar-mode -1)                ; disable scrollbar
(tool-bar-mode -1)                  ; disable toolbar
(tooltip-mode -1)                   ; disable tooltips
(set-fringe-mode -1)
(menu-bar-mode -1)                  ; disable menubar
(blink-cursor-mode 0)               ; disable blinking cursor

(setq gc-cons-threshold (* 1024 1024 1024))

;;; early-init.el ends here
