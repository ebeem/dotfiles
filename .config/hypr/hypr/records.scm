(define-module (hypr records)
  #:use-module (oop goops)
  #:use-module (json)

  #:export (<hypr-monitor>
            <hypr-version>
            <hypr-workspace>

            scm->hypr-workspace
            scm->hypr-version
            scm->hypr-monitor

            hypr-version-branch
            hypr-version-commit
            hypr-version-dirty
            hypr-version-commit-message
            hypr-version-commit-date
            hypr-version-tag
            hypr-version-commits
            hypr-version-flags

            hypr-workspace-id
            hypr-workspace-name
            hypr-workspace-monitor
            hypr-workspace-monitor-id
            hypr-workspace-windows
            hypr-workspace-has-fullscreen
            hypr-workspace-last-window
            hypr-workspace-last-window-title

            hypr-monitor-id
            hypr-monitor-name
            hypr-monitor-description
            hypr-monitor-make
            hypr-monitor-model
            hypr-monitor-serial
            hypr-monitor-width
            hypr-monitor-height
            hypr-monitor-refresh-rate
            hypr-monitor-x
            hypr-monitor-y
            hypr-monitor-active-workspace
            hypr-monitor-special-workspace
            hypr-monitor-reserved
            hypr-monitor-scale
            hypr-monitor-transform
            hypr-monitor-focused
            hypr-monitor-dpms-status
            hypr-monitor-vrr
            hypr-monitor-active-tearing
            hypr-monitor-disabled
            hypr-monitor-current-format
            hypr-monitor-available-modes

            json->hypr-workspace
            json->hypr-version
            json->hypr-monitor))

(define-json-type <hypr-version>
    (branch)
    (commit)
    (dirty)
    (commit-message "commit_message")
    (commit-date "commit_date")
    (tag)
    (commits)
    (flags))

(define-json-type <hypr-workspace>
    (id)
    (name)
    (monitor)
    (monitor-id "monitorID")
    (windows)
    (has-fullscreen "hasfullscreen")
    (last-window "lastwindow")
    (last-window-title "lastwindowtitle"))

(define-json-type <hypr-monitor>
    (id)
    (name)
    (description)
    (make)
    (model)
    (serial)
    (width)
    (height)
    (refresh-rate "refreshRate")
    (x)
    (y)
    (active-workspace "activeWorkspace" <hypr-workspace>)
    (special-workspace "specialWorkspace" <hypr-workspace>)
    (reserved)
    (scale)
    (transform)
    (focused)
    (dpms-status "dpmsStatus")
    (vrr)
    (active-tearing "activeTearing")
    (disabled)
    (current-format "currentFormat")
    (available-modes "available-modes"))
