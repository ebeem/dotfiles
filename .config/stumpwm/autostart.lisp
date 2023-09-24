(when *initializing*
  ;; (ql:system-apropos "swank")
  ;; (ql:quickload :swank)
  ;; (swank:create-server
  ;;  :dont-close t
  ;;  :port swank::default-server-port)

  ;; (run-shell-command "sleep 5000 && polybar-launcher")
  (run-shell-command "xrdb -merge /home/ebeem/.Xresources")
  (run-shell-command "xset r rate 400 50")
  (run-shell-command "xset -dpms")
  (run-shell-command "xset s off")
  (run-shell-command "xsettingsd")
  (run-shell-command "xsetroot -cursor_name left_ptr")
  (run-shell-command "picom -b --dbus")
  (run-shell-command "nitrogen --restore")
  (run-shell-command "polybar -r")
  ;; (run-shell-command "xrandr --output DP-1 --mode 1920x1080 --rate 144.00 --output DP-2 --mode 1920x1080 --rate 144.00 --right-of DP-1 --output HDMI-2 --mode 1920x1080 --rate 120.00 --left-of DP-1")

  ;; (run-shell-command "~/.fehbg")
  ;; initialize modeline/polybar

 (dolist (app (list "sleep 10 && emacsclient -c" "firefox" "sleep 10 && emacsclient -c -e '(+eshell/here nil)')"
                     "chromium 'https://web.whatsapp.com/'" "discord" "sleep 10 && slack" "obs" "sleep 10"
                     "ckb-next -b" "sleep 10 && gtk-launch 'elfeed'" "sleep 10 && gtk-launch 'mu4e'" "sleep 10 && gtk-launch 'ement'"))
   (run-shell-command app)))
