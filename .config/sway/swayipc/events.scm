(define-module (swayipc events)
  #:use-module (swayipc connection)
  #:use-module (swayipc records)
  #:use-module (json)

  #:export (sway-subscribe-event
            sway-subscribe-workspace-change
            sway-subscribe-workspace-event
            sway-subscribe-output-event
            sway-subscribe-binding-mode-event
            sway-subscribe-window-event
            sway-subscribe-barconfig-update-event
            sway-subscribe-binding-event
            sway-subscribe-shutdown-event
            sway-subscribe-tick-event
            sway-subscribe-bar-state-event
            sway-subscribe-input-event
            sway-subscribe-all

            sway-workspace-hook
            sway-output-hook
            sway-mode-hook
            sway-window-hook
            sway-bar-config-hook
            sway-shutdown-hook
            sway-tick-hook
            sway-bar-state-update-hook
            sway-input-hook))

(define (sway-subscribe-event event)
  "A client can subscribe to any events it wants to be notified of changes for."
  (write-msg LISTENER-SOCKET SUBSCRIBE-MSG-ID event)
  (json->sway-tick (list-ref (read-msg LISTENER-SOCKET) 1)))

(define (sway-subscribe-workspace-event)
  "Sent whenever an event involving a workspace occurs such as initialization
   of a new workspace or a different workspace gains focus."
  (sway-subscribe-event "['workspace']"))

(define (sway-subscribe-output-event)
  "Sent when outputs are updated."
  (sway-subscribe-event "['output']"))

(define (sway-subscribe-binding-mode-event)
  "Sent whenever the binding mode changes."
  (sway-subscribe-event "['mode']"))

(define (sway-subscribe-window-event)
 "Sent whenever an event involving a view occurs such as being reparented, focused, or closed."
  (sway-subscribe-event "['window']"))

(define (sway-subscribe-barconfig-update-event)
 "Sent whenever a bar config changes."
  (sway-subscribe-event "['barconfig_update']"))

(define (sway-subscribe-binding-event)
  "Sent when a configured binding is executed." 
  (sway-subscribe-event "['binding']"))

(define (sway-subscribe-shutdown-event)
 "Sent when the ipc shuts down because sway is exiting." 
  (sway-subscribe-event "['shutdown']"))

(define (sway-subscribe-tick-event)
  "Sent when an ipc client sends a SEND_TICK message."
  (sway-subscribe-event "['tick']"))

(define (sway-subscribe-bar-state-event)
  "Send when the visibility of a bar should change due to a modifier."
  (sway-subscribe-event "['bar_state_update']"))

(define (sway-subscribe-input-event)
  "Sent when something related to input devices changes."
  (sway-subscribe-event "['input']"))

(define (sway-subscribe-all)
  "subscribe to all available events."
  (sway-subscribe-event "['workspace', 'output', 'mode', 'window', 'barconfig_update',
	'binding', 'shutdown', 'tick', 'bar_state_update', 'input']"))

(add-hook! data-received-hook
           (lambda (command-id payload)
             (handle-event command-id payload)))

(define (handle-event command-id payload)
  ;; (display (string-append "handling " (number->string command-id) "\n"))
  (cond
   ((= command-id WORKSPACE-EVENT-REPLY)
    (run-hook sway-workspace-hook (json->sway-workspace-event payload)))
   ((= command-id OUTPUT-EVENT-REPLY)
    (run-hook sway-output-hook (json->sway-output-event payload)))
   ((= command-id MODE-EVENT-REPLY)
    (run-hook sway-mode-hook (json->sway-mode-event payload)))
   ((= command-id WINDOW-EVENT-REPLY)
    (run-hook sway-window-hook (json->sway-window-event payload)))
   ((= command-id BAR-CONFIG-UPDATE-EVENT-REPLY)
    (run-hook sway-bar-config-hook (json->sway-bar-config payload)))
   ((= command-id BINDING-EVENT-REPLY)
    (run-hook sway-binding-hook (json->sway-binding-event payload)))
   ((= command-id SHUTDOWN-EVENT-REPLY)
    (run-hook sway-shutdown-hook (json->sway-shutdown-event payload)))
   ((= command-id TICK-EVENT-REPLY)
    (run-hook sway-tick-hook (json->sway-tick-event payload)))
   ((= command-id BAR-STATE-UPDATE-EVENT-REPLY)
    (run-hook sway-bar-state-update-hook (json->sway-bar-state-update-event payload)))
   ((= command-id INPUT-EVENT-REPLY)
    (run-hook sway-input-hook (json->sway-input-event payload)))))

(define sway-workspace-hook
  ;; workspace changed: emitted on workspace change.
  ;; Parameters:
  ;;   - arg1: sway-workspace-event.
  (make-hook 1))

(define sway-output-hook
  ;; output changed: emitted on output change.
  ;; Parameters:
  ;;   - arg1: sway-output-event.
  (make-hook 1))

(define sway-mode-hook
  ;; mode changed: emitted on mode change.
  ;; Parameters:
  ;;   - arg1: sway-mode-event.
  (make-hook 1))

(define sway-window-hook
  ;; window changed: emitted on window change.
  ;; Parameters:
  ;;   - arg1: sway-window-event.
  (make-hook 1))

(define sway-binding-hook
  ;; binding changed: emitted on binding change.
  ;; Parameters:
  ;;   - arg1: sway-binding-event.
  (make-hook 1))

(define sway-bar-config-hook
  ;; bar-config changed: emitted on bar-config change.
  ;; Parameters:
  ;;   - arg1: sway-bar-config-event.
  (make-hook 1))

(define sway-shutdown-hook
  ;; shutdown changed: emitted on shutdown change.
  ;; Parameters:
  ;;   - arg1: sway-shutdown-event.
  (make-hook 1))

(define sway-tick-hook
  ;; tick changed: emitted on tick change.
  ;; Parameters:
  ;;   - arg1: sway-tick-event.
  (make-hook 1))

(define sway-bar-state-update-hook
  ;; bar-state-update changed: emitted on bar-state-update change.
  ;; Parameters:
  ;;   - arg1: sway-bar-state-update-event.
  (make-hook 1))

(define sway-input-hook
  ;; input changed: emitted on input change.
  ;; Parameters:
  ;;   - arg1: sway-input-event.
  (make-hook 1))
