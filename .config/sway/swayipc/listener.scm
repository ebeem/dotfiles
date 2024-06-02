(define-module (swayipc listener)
  ;; #:use-module (swayipc utility)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 regex)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (oop goops)
  #:use-module (json)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)

  #:export (SOCKET-PATH
            start-event-listener
            handle-event
            monitor-changed-hook
            window-changed-hook
            window-changed-v2-hook
            fullscreen-hook
            monitor-removed-hook
            monitor-added-hook
            monitor-added-v2-hook
            workspace-created-hook
            workspace-created-v2-hook
            workspace-destoryed-hook
            workspace-destroyed-v2-hook
            workspace-moved-hook
            workspace-moved-v2-hook
            workspace-renamed-hook
            workspace-special-change-hook
            layout-change-hook
            window-open-hook
            window-close-hook
            window-move-hook
            window-move-v2-hook
            layer-open-hook
            layer-close-hook
            submap-hook
            floating-mode-change-hook
            window-urgent-hook
            window-minimize-hook
            screencast-hook
            window-title-change-hook
            ignore-group-lock-toggled-hook
            group-lock-toggled-hook
            config-reloaded-hook
            window-pin-changed-hook
            workspace-changed-v2-hook
            workspace-changed-hook))

(use-modules (ice-9 popen))
(use-modules (ice-9 rdelim))
(use-modules (ice-9 textual-ports))
(use-modules (ice-9 regex))
(use-modules (rnrs bytevectors))
(use-modules (rnrs io ports))
(use-modules (oop goops))
(use-modules (json))
(use-modules (srfi srfi-9))
(use-modules (srfi srfi-1))
(use-modules (ice-9 threads))

(define MSG-MAGIC "i3-ipc")
(define MSG-MAGIC-BV (string->utf8 MSG-MAGIC))
;; TODO: maybe also get from sway and i3 binaries
(define SOCKET-PATH
  (and (getenv "SWAYSOCK")
       (getenv "I3SOCK")))

(define WRITE-SOCKET (socket AF_UNIX SOCK_STREAM 0))
(connect WRITE-SOCKET (make-socket-address AF_UNIX SOCKET-PATH))

(define (bytevector-concatenate . bvs)
  (let* ((len (apply + (map (lambda (bv) (bytevector-length bv)) bvs)))
         (result (make-bytevector len))
         (index 0))
    (for-each
     (lambda (bv)
       (bytevector-copy! bv 0 result index (bytevector-length bv))
       (set! index (+ index (bytevector-length bv))))
     bvs)
    result))

;; The format for messages and replies is:
;;     <magic-string> <payload-length> <payload-type> <payload>
;; Where
;;      <magic-string> is i3-ipc, for compatibility with i3
;;      <payload-length> is a 32-bit integer in native byte order
;;      <payload-type> is a 32-bit integer in native byte order
(define (encode-msg command-id payload)
  (let* ((bv (make-bytevector (+ 14 (string-length payload)))))
    ;; <magic-string> <payload-length> <payload-type> <payload>
    (bytevector-copy! (string->utf8 "i3-ipc") 0 bv 0 6)
    (bytevector-u32-set! bv 6 command-id (endianness little))
    (bytevector-u32-set! bv 10 (string-length payload) (endianness little))

    ;; payload is optional
    (when (> (string-length payload) 0)
      (bytevector-copy! (string->utf8 payload) 0 bv 14 (string-length payload)))
    bv))

(define (send-command command-id payload)
  (write-to-socket WRITE-SOCKET command-id payload)
  (receive-reply WRITE-SOCKET)
  (display "done\n"))

(define (write-to-socket sock command-id payload)
  (display "write-to-socket\n")
  (put-bytevector sock (encode-msg command-id payload)))

(define (receive-reply sock)
  (display "receive-reply\n")
  (let* ((bv-header (get-bytevector-n sock 14))
         (command-id (bytevector-u32-ref bv-header 6 (endianness little)))
         (payload-length (bytevector-u32-ref bv-header 10 (endianness little)))
         (payload (utf8->string (get-bytevector-n sock payload-length))))
    (display "received")
    (display payload)
    command-id payload))

(define (read-from-socket socket-path)
  (let* ((sock (socket AF_UNIX SOCK_STREAM 0))
         (socket-addr (make-socket-address AF_UNIX socket-path))
         (bv (make-bytevector 14)))

    (connect sock socket-addr)
    (let loop ()
      (let ((data (read-line sock)))
        (if (eof-object? data)
            (begin
              (close-port sock)
              (display "Connection closed\n"))
            (begin
              (display data)
              (let* ((command-id (substring data 0 (string-index data #\>)))
                     (params (string-split (substring data (+ 2 (string-index data #\>))) #\,)))
                (handle-event command-id params)
                (loop))))))))

(define (start-event-listener)
  (read-from-socket SOCKET-PATH))

;; (define listener-thread (make-thread start-event-listener))
;; (thread-start! listener-thread)

(define (call-hook hook params types)
  (apply run-hook
         (cons hook
               (fix-arguments-type
                (fix-arguments-length params (string-length types)) types))))

(define (fix-arguments-type args types)
  (map
   (lambda (arg type)
     (cond
      ((eq? <real> type) (string->number arg))
      ((eq? <number> type) (string->number arg))
      ((eq? <integer> type) (string->number arg))
      (else arg)))
   args types))

(define (handle-event command-id params)
  ;; (display (string-append "handling command " command-id "\n"))
  ;; (display params)
  ;; (display "\n")
  (cond
    ((string=? command-id "workspace") (call-hook workspace-changed-hook params (list <string>)))
    ((string=? command-id "workspacev2") (call-hook workspace-changed-v2-hook params (list <integer> <string>)))
    ((string=? command-id "focusedmon") (call-hook monitor-changed-hook params (list <string> <string>)))
    ((string=? command-id "activewindow") (call-hook window-changed-hook params (list <string> <string>)))
    ((string=? command-id "activewindowv2") (call-hook window-changed-v2-hook params (list <string>)))
    ((string=? command-id "fullscreen") (call-hook fullscreen-hook params (list <integer>)))
    ((string=? command-id "monitorremoved") (call-hook monitor-removed-hook params (list <string>)))
    ((string=? command-id "monitoradded") (call-hook monitor-added-hook params (list <string>)))
    ((string=? command-id "monitoraddedv2") (call-hook monitor-added-v2-hook params (list <integer> <string> <string>)))
    ((string=? command-id "createworkspace") (call-hook workspace-created-hook params (list <string>)))
    ((string=? command-id "createworkspacev2") (call-hook workspace-created-v2-hook params (list <integer> <string>)))
    ((string=? command-id "destroyworkspace") (call-hook workspace-destoryed-hook params (list <string>)))
    ((string=? command-id "destroyworkspacev2") (call-hook workspace-destroyed-v2-hook params (list <integer> <string>)))
    ((string=? command-id "moveworkspace") (call-hook workspace-moved-hook params (list <integer> <string>)))
    ((string=? command-id "moveworkspacev2") (call-hook workspace-moved-v2-hook params (list <integer> <string> <string>)))
    ((string=? command-id "renameworkspace") (call-hook workspace-renamed-hook params (list <integer> <string>)))
    ((string=? command-id "activespecial") (call-hook workspace-special-change-hook params (list <string> <string>)))
    ((string=? command-id "activelayout") (call-hook layout-change-hook params (list <string> <string>)))
    ((string=? command-id "openwindow") (call-hook window-open-hook params (list <string> <string> <string> <string>)))
    ((string=? command-id "closewindow") (call-hook window-close-hook params (list <string>)))
    ((string=? command-id "movewindow") (call-hook window-move-hook params (list <string> <string>)))
    ((string=? command-id "movewindowv2") (call-hook window-move-v2-hook params (list <string> <integer> <string>)))
    ((string=? command-id "openlayer") (call-hook layer-open-hook params (list <string>)))
    ((string=? command-id "closelayer") (call-hook layer-close-hook params (list <string>)))
    ((string=? command-id "submap") (call-hook submap-hook params (list <string>)))
    ((string=? command-id "changefloatingmode") (call-hook floating-mode-change-hook params (list <string> <integer>)))
    ((string=? command-id "urgent") (call-hook window-urgent-hook params (list <string>)))
    ((string=? command-id "minimize") (call-hook window-minimize-hook params (list <string> <integer>)))
    ((string=? command-id "screencast") (call-hook screencast-hook params (list <integer> <integer>)))
    ((string=? command-id "windowtitle") (call-hook window-title-change-hook params (list <string>)))
    ((string=? command-id "ignoregrouplock") (call-hook ignore-group-lock-toggled-hook params (list <string>)))
    ((string=? command-id "lockgroups") (call-hook group-lock-toggled-hook params (list <string>)))
    ((string=? command-id "configreloaded") (call-hook config-reloaded-hook params '()))
    ((string=? command-id "pin") (call-hook window-pin-changed-hook params (list <string> <integer>)))
    (else (begin
            (display "Unknown command")
            (newline)))))

(define workspace-changed-hook
  ;; workspace: emitted on workspace change. Is emitted ONLY when a user requests a workspace change,
  ;; and is not emitted on mouse movements (see activemon).

  ;; Parameters:
  ;;   - arg1: workspace-name.
  (make-hook 1))

(define workspace-changed-v2-hook
  ;; workspacev2: emitted on workspace change. Is emitted ONLY when a user requests a workspace change,
  ;; and is not emitted on mouse movements (see activemon).

  ;; Parameters:
  ;;   - arg1: workspace-id
  ;;   - arg2: workspace-name
  (make-hook 2))

(define monitor-changed-hook
  ;; focusedmon: emitted on the active monitor being changed.

  ;; Parameters:
  ;;   - arg1: monitor-name
  ;;   - arg2: workspace-name
  (make-hook 2))

(define window-changed-hook
  ;; activewindow: emitted on the active window being changed.

  ;; Parameters:
  ;;   - arg1: window-class
  ;;   - arg2: window-title
  (make-hook 2))

(define window-changed-v2-hook
  ;; activewindowv2: emitted on the active window being changed.

  ;; Parameters:
  ;;   - arg1: window-address
  (make-hook 1))

(define fullscreen-hook
  ;; fullscreen: emitted when a fullscreen status of a window changes.

  ;; Parameters:
  ;;   - arg1: 0/1 (exit fullscreen / enter fullscreen)
  (make-hook 1))

(define monitor-removed-hook
  ;; monitorremoved: emitted when a monitor is removed (disconnected).

  ;; Parameters:
  ;;   - arg1: monitor-name
  (make-hook 1))

(define monitor-added-hook
  ;; monitoradded: emitted when a monitor is added (connected).

  ;; Parameters:
  ;;   - arg1: monitor-name
  (make-hook 1))

(define monitor-added-v2-hook
  ;; monitoraddedv2: emitted when a monitor is added (connected).

  ;; Parameters:
  ;;   - arg1: monitor-id
  ;;   - arg2: monitor-name
  ;;   - arg3: monitor-description
  (make-hook 3))

(define workspace-created-hook
  ;; createworkspace: emitted when a workspace is created.

  ;; Parameters:
  ;;   - arg1: workspace-name
  (make-hook 1))

(define workspace-created-v2-hook
  ;; createworkspacev2: emitted when a workspace is created.

  ;; Parameters:
  ;;   - arg1: workspace-id
  ;;   - arg2: workspace-name
  (make-hook 2))

(define workspace-destoryed-hook
  ;; destroyworkspace: emitted when a workspace is destroyed.

  ;; Parameters:
  ;;   - arg1: workspace-name
  (make-hook 1))

(define workspace-destroyed-v2-hook
  ;; destroyworkspacev2: emitted when a workspace is destroyed.

  ;; Parameters:
  ;;   - arg1: workspace-id
  ;;   - arg2: workspace-name
  (make-hook 2))

(define workspace-moved-hook
  ;; moveworkspace: emitted when a workspace is moved to a different monitor.

  ;; Parameters:
  ;;   - arg1: workspace-name
  ;;   - arg2: monitor-name
  (make-hook 2))

(define workspace-moved-v2-hook
  ;; moveworkspacev2: emitted when a workspace is moved to a different monitor.

  ;; Parameters:
  ;;   - arg1: workspace-id
  ;;   - arg2: workspace-name
  ;;   - arg3: monitor-name
  (make-hook 3))

(define workspace-renamed-hook
  ;; renameworkspace: emitted when a workspace is renamed.

  ;; Parameters:
  ;;   - arg1: workspace-id
  ;;   - arg2: workspace-name
  (make-hook 2))

(define workspace-special-change-hook
  ;; activespecial: emitted when the special workspace opened in a monitor changes (closing results in an empty WORKSPACENAME).

  ;; Parameters:
  ;;   - arg1: workspace-name
  ;;   - arg2: monitor-name
  (make-hook 2))

(define layout-change-hook
  ;; activelayout: emitted on a layout change of the active keyboard.

  ;; Parameters:
  ;;   - arg1: keyboard-name
  ;;   - arg2: layout-name
  (make-hook 2))

(define window-open-hook
  ;; openwindow: emitted when a window is opened.

  ;; Parameters:
  ;;   - arg1: window-address
  ;;   - arg2: workspace-name
  ;;   - arg3: window-class
  ;;   - arg4: window-tilte
  (make-hook 4))

(define window-close-hook
  ;; closewindow: emitted when a window is closed.

  ;; Parameters:
  ;;   - arg1: window-address
  (make-hook 1))

(define window-move-hook
  ;; movewindow: emitted when a window is moved to a workspace.

  ;; Parameters:
  ;;   - arg1: window-address
  ;;   - arg2: workspace-name
  (make-hook 2))

(define window-move-v2-hook
  ;; movewindowv2: emitted when a window is moved to a workspace.

  ;; Parameters:
  ;;   - arg1: window-address
  ;;   - arg2: workspace-id
  ;;   - arg3: workspace-name
  (make-hook 3))

(define layer-open-hook
  ;; openlayer: emitted when a layerSurface is mapped.

  ;; Parameters:
  ;;   - arg1: namespace
  (make-hook 1))

(define layer-close-hook
  ;; closelayer: emitted when a layerSurface is unmapped.

  ;; Parameters:
  ;;   - arg1: namespace
  (make-hook 1))

(define submap-hook
  ;; submap: emitted when a keybind submap changes. Empty means default.

  ;; Parameters:
  ;;   - arg1: submap-name
  (make-hook 1))

(define floating-mode-change-hook
  ;; changefloatingmode: emitted when a window changes its floating mode.
  ;; FLOATING is either 0 or 1.

  ;; Parameters:
  ;;   - arg1: window-address
  ;;   - arg2: floating
  (make-hook 2))

(define window-urgent-hook
  ;; urgent: emitted when a window requests an urgent state.

  ;; Parameters:
  ;;   - arg1: window-address
  (make-hook 1))

(define window-minimize-hook
  ;; minimize: emitted when a window requests a change to its minimized state.
  ;; MINIMIZED is either 0 or 1.

  ;; Parameters:
  ;;   - arg1: window-address
  ;;   - arg2: minimized
  (make-hook 2))

(define screencast-hook
  ;; screencast: emitted when a screencopy state of a client changes.
  ;; Keep in mind there might be multiple separate clients.
  ;; State is 0/1
  ;; owner is 0 - monitor share, 1 - window share.

  ;; Parameters:
  ;;   - arg1: state
  ;;   - arg2: owner
  (make-hook 2))

(define window-title-change-hook
  ;; windowtitle: emitted when a window title changes.

  ;; Parameters:
  ;;   - arg1: window-address
  (make-hook 1))

(define ignore-group-lock-toggled-hook
  ;; ignoregrouplock: emitted when ignoregrouplock is toggled.

  ;; Parameters:
  ;;   - arg1: window-address
  (make-hook 1))

(define group-lock-toggled-hook
  ;; lockgroups: emitted when lockgroups is toggled.	0/1.

  ;; Parameters:
  ;;   - arg1: window-address
  (make-hook 1))

(define config-reloaded-hook
  ;; configreloaded: emitted when the config is done reloading.

  ;; Parameters: nil
  (make-hook 0))

(define window-pin-changed-hook
  ;; pin: emitted when a window is pinned or unpinned.

  ;; Parameters:
  ;;   - arg1: window-address
  ;;   - arg2: pin-state
  (make-hook 2))

(thread-join! listener-thread)
