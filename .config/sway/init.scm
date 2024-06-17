#!/usr/bin/guile 
!#
;; #!/usr/bin/guile --fresh-auto-compile
;; ensure that the swayipc module is available under the same directory as the init file
;; otherwise, the module should be referenced from packaging system or via custom load path

(add-to-load-path "/home/ebeem/workspace/guile/swayipc")
(add-to-load-path
 (dirname (or (current-filename)
              (string-append (getenv "HOME") "/.config/sway/init.scm"))))

(use-modules (oop goops)
             (srfi srfi-18)
             (modules workspace-groups)
             (modules workspace-grid)
             (swayipc connection)
             (swayipc records)
             (swayipc info)
             (swayipc events)
             (swayipc dispatcher))

(load "behavior.scm")

;; init keybindings
(load "keybindings.scm")
(keybindings-init)

;; subscribe to all events
(sway-subscribe-all)

(define OUTPUTS '("HDMI-A-2" "DP-1" "DP-2"))
(define GROUPS
  '(("11-browser" 		"21-browser" 		"31-browser")
    ("12-development" 	"22-development" 	"32-development")
    ("13-databases" 	"23-databases" 		"33-databases")
    ("14-communication" "24-communication" 	"34-communication")
    ("15-development" 	"25-development" 	"35-development")
    ("16-gaming" 		"26-gaming" 		"36-gaming")
    ("17-mail" 			"27-mail" 			"37-mail")
    ("18-development" 	"28-development" 	"38-development")
    ("19-media" 		"29-media" 			"39-media")))

(define ROWS 3)
(define COLUMNS 3)

(workspace-groups-configure #:outputs OUTPUTS #:groups GROUPS)
(workspace-groups-init)

(workspace-grid-configure #:rows ROWS #:columns COLUMNS #:workspaces (apply map list GROUPS))
(workspace-grid-init)

;; TODO: load which key module

(start-commands-listener-thread)
(start-event-listener-thread)
(thread-join! LISTENER-THREAD)
(thread-join! COMMANDS-LISTENER-THREAD)
