(when *initializing*
  ;; (ql:quickload :swank)
  ;; (swank:create-server
  ;;  :dont-close t
  ;;  :port swank::default-server-port)

  (run-shell-command "xset r rate 300 150")
  (run-shell-command "xsettingsd")
  (run-shell-command "picom -b --experimental-backends --dbus")
  (run-shell-command "~/.fehbg")
  (run-shell-command "xrdb ~/.Xresources")
  ;; initialize modeline/polybar
  (run-shell-command "~/.config/polybar/launch.sh")

  ;; startup applications
  (run-or-raise "firefox" '(:class "firefox-init"))
  (run-or-raise "emacs" '(:class "emacs-init"))
  (run-or-raise "discord" '(:class "discord"))
  (run-or-raise "slack" '(:class "slack"))
  (run-or-raise "steam-runtime" '(:class "steam"))
  (run-or-raise "gtk-launch 'elfeed'" '(:class "elfeed"))
  (run-or-raise "gtk-launch 'mu4e'" '(:class "mu4e"))
  (run-or-raise "gtk-launch 'ement'" '(:class "ement"))
  (run-or-raise "chromium 'https://web.whatsapp.com/'" '(:class "whatsapp"))
  (run-or-raise "ckb-next -b" '(:class "ckb-next"))
  (run-or-raise app '(:class "firefox")))
