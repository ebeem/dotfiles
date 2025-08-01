;;; init.el --- basic lisp subroutines for Emacs  -*- lexical-binding:t -*-

;; Maintainer: ebeem2@gmail.com
;; Keywords: internal
;; Package: emacs

;;; Commentary:

;;; Code:

;; add modules and themes to load paths
;; (profiler-start 'cpu)

(add-to-list 'load-path
  (expand-file-name "modules" user-emacs-directory))
(add-to-list 'custom-theme-load-path
  (expand-file-name "themes" user-emacs-directory))

(require 'oz-settings)        ;;
(require 'oz-utilities)       ;; ('sudo-edit')
(require 'oz-terminal)        ;; ('vterm', 'eshell')
(require 'oz-completion)      ;; ('compeny', 'vertico', 'which-key')
(require 'oz-ui)              ;; ('dashboard', 'faces', 'doom-themes', 'doom-modeline', 'diredfl', 'diminish')
(require 'oz-code)            ;; ('flycheck') ;; TODO: optimize
(require 'oz-org)             ;; ('org-mode', 'toc-org', 'org-modern', 'org-transclusion', 'org-roam', 'org-phscroll')
(require 'oz-git)             ;; ('magit', 'forge')
(require 'oz-mail)            ;; ('mu4e')
(require 'oz-chat)            ;; ('erc', 'ement')
(require 'oz-news)            ;; ('elfeed')
(require 'oz-media)           ;; ('mingus')

;; (require 'oz-package-manager) ;; ('elpaca')
;; (require 'oz-evil)            ;; ('evil', 'evil-collection', 'evil-escape')
;; (require 'oz-meow)            ;; ('meow')
;; (require 'oz-keybindings)     ;; ('general')
;; (require 'oz-ai)              ;; ('aider')
;; (require 'oz-dashboard)

;; (profiler-stop)

