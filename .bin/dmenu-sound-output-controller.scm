#!/usr/bin/env guile
!#
(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 regex)
             (ice-9 format)
			 (srfi srfi-9)
             (srfi srfi-13)
			 (srfi srfi-18)
			 (srfi srfi-26)
			 (srfi srfi-1))

(include "dmenu-base.scm")
(define devices (run-command "pactl -f json list sinks | jq -r '.[].description'"))

(let* ((choice (dmenu-prompt "Output Devics Menu > " devices)))
  (when (not (string-null? choice))
	(run-command
	 (string-append "pactl set-default-sink \"$("
					"pactl -f json list sinks | jq -r --arg d \"$desc\" '.[]"
					" | select((.description|ascii_downcase)"
					" | contains(\"" choice "\"|ascii_downcase)) | .name' | head -n1)\""))))
