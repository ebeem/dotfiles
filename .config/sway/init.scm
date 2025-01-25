#!/usr/bin/env guile
!#
;; #!/usr/bin/guile --fresh-auto-compile
;; !#

(add-to-load-path "/home/ebeem/workspace/guile/guile-swayer")
(add-to-load-path
 (dirname (or (current-filename)
              (string-append (getenv "HOME") "/.config/sway/init.scm"))))

(use-modules (oop goops)
             (srfi srfi-1)
             (srfi srfi-18)
             (ice-9 regex)
             (ice-9 rdelim)
             (ice-9 popen)
             (ice-9 textual-ports)
             (guile-swayer modules workspace-groups)
             (guile-swayer modules workspace-grid)
             (guile-swayer modules auto-reload)
             (guile-swayer modules which-key)
             (guile-swayer swayipc))

(sway-connect-sockets!)

;; TODO: make a module
;; kill any existing sway init.scm file other than this file
(define (kill-duplicate-processes)
  (let* ((ps "ps -ewwo pid,cmd")
         (pipe (open-input-pipe ps)))
    (let loop ((line (read-line pipe)))
      (unless (eof-object? line)
        (let* ((fields (filter (lambda (x) (and x (> (string-length x) 0)))
                               (string-split line #\space)))
               (pid (car fields))
               (cmd (string-join (cdr fields) " ")))
          (when (and (not (equal? pid (number->string (getpid))))
                     (string-match ".*guile.*sway.*init.*scm" cmd))
            (system* "kill" "-9" pid)))
        (loop (read-line pipe))))
    (close-pipe pipe)))

(kill-duplicate-processes)

;; TODO: make a module
(define* (apply-gtk-settings #:optional (settings-path "~/.config/gtk-3.0/settings.ini"))
  (let ((schema "org.gnome.desktop.interface")
        (theme "gtk-theme")
        (icon "icon-theme")
        (cursor "cursor-theme")
        (font "font-name"))
    (system "gsettings set org.gnome.desktop.interface gtk-theme 'Colloid-Purple-Dark-Catppuccin'")
    (system "gsettings set org.gnome.desktop.interface icon-theme 'Colloid-nord-dark'")
    (system "gsettings set org.gnome.desktop.interface cursor-theme 'Breeze_Snow'")
    (system "export QT_QPA_PLATFORMTHEME='qt6ct'")))

(apply-gtk-settings)

;; load look and feel
(load "behavior.scm")

;; init keybindings
(load "keybindings.scm")
(keybindings-init)

;; subscribe to all events
(sway-subscribe-all)

(define OUTPUTS '("HDMI-A-1" "DP-1" "DP-2"))
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

(workspace-groups-configure #:groups GROUPS #:outputs OUTPUTS)
(workspace-groups-init)

(define ROWS 3)
(define COLUMNS 3)
(define WORKSPACES (apply map list GROUPS))

(workspace-grid-configure #:rows ROWS #:columns COLUMNS #:workspaces WORKSPACES)
(workspace-grid-init)

(auto-reload-configure #:directories
                       `(,(string-append (getenv "HOME") "/.config/sway/")))
(auto-reload-init)

;; init which-key
(which-key-configure #:delay-idle 1.2)
(which-key-init)

(define (show-rofi-message msg)
  (hide-rofi-message)
  (let ((rofi-style "window { location: north; anchor: north; width: 20%; x-offset: 10px; y-offset: 40px; }"))
    (system (format #f "rofi -theme-str '~a' -e \"~a\"" rofi-style msg))))

(define (hide-rofi-message)
  (system "pkill -f '.*rofi -theme-str .* -e.*'"))

(define (show-which-key submap bindings)
  ;; show your which-key viewer (rofi, eww, etc.)
  (format #t "Displaying Submap ~a Bindings:\n" submap)
  (let ((message ""))
    (for-each
     (lambda (ls)
       (let ((nmsg (format #f "    - ~a -> ~a\n" (list-ref ls 1) (list-ref ls 3))))
        (display nmsg)
        (set! message
              (string-append message
                             (format #f "    - <span color='#c6a0f6'><b>~a</b></span> -> ~a\n" (list-ref ls 1) (list-ref ls 3))))))
     bindings)
    (show-rofi-message message)))

(define (hide-which-key submap)
  ;; hide your which-key viewer (rofi, eww, etc.)
  (format #t "Hiding Submap Bindings:\n")
  (hide-rofi-message))

(add-hook! which-key-display-keybindings-hook show-which-key)
(add-hook! which-key-hide-keybindings-hook hide-which-key)

(sway-start-event-listener-thread)
(thread-join! SWAY-LISTENER-THREAD)
