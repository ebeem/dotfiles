(when *initializing*
  ;; (ql:system-apropos "swank")
  ;; (ql:quickload :swank)
  ;; (swank:create-server
  ;;  :dont-close t
  ;;  :port swank::default-server-port)

  (run-shell-command "xset r rate 300 150")
  (run-shell-command "xset -dpms")
  (run-shell-command "xset s off")
  (run-shell-command "xsettingsd")
  (run-shell-command "xsetroot -cursor_name left_ptr")
  (run-shell-command "picom -b --experimental-backends --dbus")
  (run-shell-command "nitrogen --restore")
  ;; (run-shell-command "~/.fehbg")
  ;; initialize modeline/polybar
  (run-shell-command "~/.config/polybar/launch.sh")

  (dolist (app (list "sleep 10 &&emacsclient -c" "firefox" "sleep 10 && emacsclient -c -e '(+eshell/here nil)')"
                      "chromium 'https://web.whatsapp.com/'" "discord" "sleep 10 && slack" "obs" "sleep 10 && steam-runtime"
                      "ckb-next -b" "sleep 10 && gtk-launch 'elfeed'" "sleep 10 && gtk-launch 'mu4e'" "sleep 10 && gtk-launch 'ement'"))
    (run-shell-command app)))
