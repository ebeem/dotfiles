
(use-modules (ice-9 popen)
             (ice-9 textual-ports))

(define (main args)
  (cond ((equal? (list-ref args 1) "gotoworkspace")
         (hyprctl-focus-workspace-direction (list-ref args 2)))))

;; (hyprctl-focus-workspace-direction (list-ref args 2))

;; workspaces format defines the format that workspaces must follow
;; if it's 10, each monitor is capable of holding 10 workspaces from 0-9
;; for instance, workspace 1 of monitor 1 will have the id 00, while workspace 3
;; of monitor 2 will have the id 12. No workspace management feature will function
;; properly without following this convention. The format can have any number of zeros
;; padded to the right, this will increase the number of workspaces each monitor is
;; capable of holding. For example, the value 100 will allow each monitor to hold 100 workspaces
(define WORKSPACES-FORMAT 10)

;; grid rows and columns
(define ROWS 3)
(define COLUMNS 3)

(define (hyprctl-get-active-workspace)
  "get active workspace information"
  (get-string-all (open-pipe* OPEN_READ "hyprctl" "activeworkspace")))

(define (hyprctl-get-monitors)
  "get connected monitors information"
  (get-string-all (open-pipe* OPEN_READ "hyprctl" "monitors")))

(define (hyprctl-get-active-workspace-id)
  "get active workspace id, the workspace name MUST be a valid number"
  (let* ((out (hyprctl-get-active-workspace)))
	(string->number (substring out 13
               (- (string-index out #\( 13 20) 1)))))

(define (hyprctl-get-monitors-active-workspaces)
  "get active workspace id, the workspace name MUST be a valid number"
  (let* ((out (hyprctl-get-monitors))
         (workspaces '()))
    (map (lambda (line)
           (when (string-contains line "active workspace:")
             (set! workspaces (append workspaces
                     (list (string->number (list-ref (string-split line #\space) 2)))))))
         (string-split out #\newline))
    workspaces))

(define (hyprctl-focus-workspace id)
  (system* "hyprctl" "dispatch" "workspace" (number->string id)))

(define* (hyprctl-get-grid-direction direction #:optional (current-workspace (hyprctl-get-active-workspace-id)))
  (let* ((current-workspace-group (floor (/ current-workspace WORKSPACES-FORMAT)))
         (current-workspace-index (modulo current-workspace WORKSPACES-FORMAT))
         (current-row (floor (/ current-workspace-index COLUMNS)))
         (current-column (modulo current-workspace-index COLUMNS))
         (target-row
          (cond ((equal? direction "up") (- current-row 1))
                ((equal? direction "down") (+ current-row 1))
                (else current-row)))
         (target-column
          (cond ((equal? direction "left") (- current-column 1))
                ((equal? direction "right") (+ current-column 1))
                (else current-column))))
    (+ (* current-workspace-group WORKSPACES-FORMAT)
       (+ (* COLUMNS (modulo target-row ROWS))
          (modulo target-column COLUMNS)))))

(define (hyprctl-focus-workspace-direction direction)
  "moves the current workspace to "
  (hyprctl-focus-workspace (hyprctl-get-grid-direction direction)))
