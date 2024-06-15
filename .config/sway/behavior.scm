(use-modules (swayipc dispatcher)
             (ice-9 popen)
             (srfi srfi-18)
             (ice-9 textual-ports))

(sway-output "HDMI-A-2" "resolution 1920x1080 position 0,0")
(sway-output "DP-1" "resolution 1920x1080 position 0,0")
(sway-output "DP-2" "resolution 1920x1080 position 0,0")

(sway-output "*" "bg /home/ebeem/dotfiles/.wallpapers/fixed/flat-20.png fill")

(sway-focus-follow-mouse SWAY-FOCUS-FOLLOW-MOUSE-FLAG-NO)
