;; emacs keybinds
(define-configuration buffer
  ((default-modes
       (pushnew 'nyxt/mode/emacs:emacs-mode %slot-value%))))
