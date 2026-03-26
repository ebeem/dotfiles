;;; init.el --- basic lisp subroutines for Emacs  -*- lexical-binding:t -*-

;; Maintainer: ebeem2@gmail.com
;; Keywords: internal
;; Package: emacs

;;; Commentary:

;;; Code:

;; add modules and themes to load paths
;; (profiler-start 'cpu)

(dolist (dir '("modules" "themes" "lisp"))
  (let ((path (expand-file-name dir user-emacs-directory)))
    (add-to-list 'load-path path)
    (add-to-list 'custom-theme-load-path path)))

(setq package-vc-allow-build-commands t)

(require 'oz-settings)        ;; (0.20)
(require 'oz-utilities)       ;; (0.25)('sudo-edit')
(require 'oz-terminal)        ;; (0.02)('vterm', 'eshell')
(require 'oz-completion)      ;; (0.25)('compeny', 'vertico', 'which-key')
(require 'oz-ui)              ;; (0.15)('dashboard', 'faces', 'doom-themes', 'doom-modeline', 'diredfl', 'diminish')
(require 'oz-code)            ;; (0.65)('flycheck') ;; TODO: optimize
(require 'oz-org)             ;; (0.04)('org-mode', 'toc-org', 'org-modern', 'org-transclusion', 'org-roam', 'org-phscroll')
(require 'oz-git)             ;; (0.04)('magit', 'forge')
(require 'oz-mail)            ;; (0.12)('mu4e')
(require 'oz-chat)            ;; (0.26)('erc', 'ement')
(require 'oz-news)            ;; (0.08)('elfeed')
(require 'oz-media)           ;; (0.00)('mingus')
(require 'oz-ai)
(require 'oz-os)

;; (require 'oz-package-manager) ;; ('elpaca')
;; (require 'oz-evil)            ;; ('evil', 'evil-collection', 'evil-escape')
;; (require 'oz-meow)            ;; ('meow')
;; (require 'oz-keybindings)     ;; ('general')
;; (require 'oz-ai)              ;; ('aider')
;; (require 'oz-dashboard)

;; (profiler-stop)

;;(setq youtube-browser-api-key "AIzaSyB__eInZK-mOvxULY8cwonn0Km-b7ZuT94")
;;(require 'empty)
;;(require 'youtube-browser)
