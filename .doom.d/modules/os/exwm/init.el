;;; ui/exwm/init.el -*- lexical-binding: t; -*-


(defconst IS-EXWM (not (null (member "--with-exwm" command-line-args))))
(add-to-list 'command-switch-alist '("--with-exwm" . (lambda (_) (pop command-line-args-left))))

(when (and doom-interactive-p
           (not doom-reloading-p)
           IS-EXWM)  
  (require 'exwm)
  (exwm-enable))
  ;; (setq exwm-randr-workspace-output-plist '(1 "DP-1"))
  ;; (require 'exwm-randr)
  ;; (add-hook 'exwm-randr-screen-change-hook
  ;;         (lambda ()
  ;;           (start-process-shell-command
  ;;            "xrandr" nil "xrandr --output DP-1 --right-of DP-2 --auto")))
  ;; (exwm-randr-enable)
  ;; (require 'exwm-systemtray)
  ;; (exwm-systemtray-enable)
  ;; (require 'exwm)
  ;; (require 'exwm-config)
  ;; (exwm-config-example))
  ;; (exwm-enable))
