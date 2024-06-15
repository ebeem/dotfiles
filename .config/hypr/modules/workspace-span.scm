(use-modules (oop goops)
             (hypr info)
             (hypr listener)
             (hypr dispatcher)
             (hypr records))

;; workspaces groups defines the list of the workspaces that should span together
;; each list should have a list of pairs, workspace-id followed by monitor-name
;; e.g, to sync workspace id '11' on monitor 'mon-1', and workspace id '21' on monitor 'mon-2'
;; (define WORKSPACE-GROUPS '(
;;	((11 "mon-1") (21 "mon-2"))
;;	((12 "mon-1") (22 "mon-2"))))
(define WORKSPACE-GROUPS '())

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
