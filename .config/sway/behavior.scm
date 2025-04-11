(use-modules (guile-swayer swayipc))

;; (sway-output "HDMI-A-1" "pos 0 0 res 1920x1080@120Hz")
;; (sway-output "DP-1" "pos 1920 0 res 1920x1080@144Hz")
;; (sway-output "DP-2" "pos 3840 0 res 1920x1080@144Hz")

;; (sway-output "*" "bg ~/.wallpapers/fixed/flat-16.png fill")
;; (sway-output "*" "bg ~/.wallpapers/fixed/cat-01.jpeg fill")

(sway-output "HDMI-A-1"
             #:position '(0 0)
             #:resolution '(1920 1080)
             #:refresh-rate 120
             #:background "~/.wallpapers/fixed/flat-16.png")
(sway-output "DP-1"
             #:position '(1920 0)
             #:resolution '(1920 1080)
             #:refresh-rate 144
             #:background "~/.wallpapers/fixed/flat-16.png")
(sway-output "DP-2"
             #:position '(3840 0)
             #:resolution '(1920 1080)
             #:refresh-rate 144
             #:background "~/.wallpapers/fixed/flat-16.png")

(sway-focus-follow-mouse SWAY-FOCUS-FOLLOW-MOUSE-FLAG-NO)
(sway-default-gaps SWAY-GAPS-OPTION-INNER 10)
(sway-title-format "")
(sway-workspace-layout SWAY-LAYOUT-TABBED)
(sway-titlebar-border-thickness 0)
(sway-titlebar-padding 25 0)
(sway-default-border-style SWAY-BORDER-STYLE-PIXEL #:n 3)

;; TODO: better move theme colors to another module for all guile scripts to inherit from
(define rosewater "#f4dbd6")
(define flamingo "#f0c6c6")
(define pink "#f5bde6")
(define mauve "#c6a0f6")
(define red "#ed8796")
(define maroon "#ee99a0")
(define peach "#f5a97f")
(define yellow "#eed49f")
(define green "#a6da95")
(define teal "#8bd5ca")
(define sky "#91d7e3")
(define sapphire "#7dc4e4")
(define blue "#8aadf4")
(define lavender "#b7bdf8")
(define text "#cad3f5")
(define subtext1 "#b8c0e0")
(define subtext0 "#a5adcb")
(define overlay2 "#939ab7")
(define overlay1 "#8087a2")
(define overlay0 "#6e738d")
(define surface2 "#5b6078")
(define surface1 "#494d64")
(define surface0 "#363a4f")
(define base "#24273a")
(define mantle "#1e2030")
(define crust "#181926")

(sway-client-background base)

(sway-client-focused-color mauve mauve crust
                           #:indictor-color mauve
                           #:child-border-color mauve)

(sway-client-focused-inactive-color overlay0 overlay0 text
                           #:indictor-color overlay0
                           #:child-border-color overlay0)

(sway-client-unfocused-color overlay0 overlay0 text
                           #:indictor-color overlay0
                           #:child-border-color overlay0)

(sway-client-urgent-color peach base peach
                           #:indictor-color overlay0
                           #:child-border-color peach)

(sway-client-placeholder-color overlay0 base text
                           #:indictor-color overlay0
                           #:child-border-color overlay0)

(sway-font "Iosevka Bold 11")
(sway-workspace-auto-back-and-forth #f)
(sway-mouse-warping SWAY-MOUSE-WARPING-NONE)
;; (sway-opacity SWAY-OPACITY-SET 0.95)
(sway-show-marks SWAY-SHOW-MARKS-NO)

(sway-input "10429:2359:UGTABLET_UGEE_S640_Mouse" "map_to_output DP-1")
(sway-input "10429:2359:UGTABLET_UGEE_S640_Keyboard" "map_to_output DP-1")
(sway-input "10429:2359:UGTABLET_UGEE_S640_Pen" "map_to_output DP-1")
(sway-input "type:keyboard" "repeat_delay 200")
(sway-input "type:keyboard" "repeat_rate 30")
(sway-input "type:keyboard" "xkb_layout 'us,ara'")
