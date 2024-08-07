(use-modules (oop goops)
             (hypr info)
             (hypr listener)
             (hypr dispatcher)
             (hypr records))

;; workspaces format defines the format that workspaces must follow
;; if it's 10, each monitor is capable of holding 10 workspaces from 0-9
;; for instance, workspace 1 of monitor 1 will have the id 00, while workspace 3
;; of monitor 2 will have the id 12. No workspace management feature will function
;; properly without following this convention. The format can have any number of zeros
;; padded to the right, this will increase the number of workspaces each monitor is
;; capable of holding. For example, the value 100 will allow each monitor to hold 100 workspaces
(define WORKSPACES-FORMAT 10)
(define MONITORS-FORMAT '((10 "HDMI-A-2") (20 "DP-1") (30 "DP-2")))

;; grid rows and columns
(define ROWS 3)
(define COLUMNS 3)
(define ENABLE-WORKSPACE-SPAN #t)

(add-hook! workspace-changed-v2-hook
           (lambda (workspace-id workspace-name)
             (when ENABLE-WORKSPACE-SPAN
               (span-workspace workspace-id))))

(define (span-workspace current-workspace)
  (let* ((current-workspace-group (floor (/ current-workspace WORKSPACES-FORMAT)))
         (current-workspace-index (modulo current-workspace WORKSPACES-FORMAT))
         (workspaces (hypr-info-workspaces))
         (monitors (hypr-info-monitors)))
    (for-each
     (lambda (monitor)
       (let* ((target-workspace-id (+ (car monitor) current-workspace-index))
              (filtered-workspaces (filter
                                    (lambda (workspace)
                                      (eq? (hypr-workspace-id workspace) target-workspace-id)) workspaces)))

         ;; ensure the workspace is on the proper monitor
          (cond
            ((and (> (length filtered-workspaces) 0)
                    (not (equal? (list-ref monitor 1)
                                 (hypr-workspace-monitor (car filtered-workspaces)))))
             (begin
               ;; force rerender fixes the workspaces sometimes
               (dispatch-force-renderer-reload)
               (dispatch-move-workspace-to-monitor
                (number->string target-workspace-id) (list-ref monitor 1))))
            (else
             (begin
               ;; move to workspace
               ;; TODO: make sure it's not active/current on other monitor
               (when (not (eq? current-workspace target-workspace-id))
                 (dispatch-workspace (number->string target-workspace-id))))))))

     MONITORS-FORMAT)))
