#!/usr/bin/env guile
!#
(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 regex)
			 (srfi srfi-18)
			 (srfi srfi-9)
			 (srfi srfi-26)
			 (srfi srfi-1))

(include "dmenu-base.scm")

;; bluetoothctl wrapper
(define (bluetooth-powered?)
  (any (cut string-match "Powered: yes" <>)
       (run-command "bluetoothctl show")))

(define (toggle-power)
  (if (bluetooth-powered?)
      (run-command "bluetoothctl power off")
      (run-command "bluetoothctl power on")))

(define (device-connected? mac)
  (any (cut string-match "^\\s*Connected: yes" <>)
       (run-command (string-append "bluetoothctl info " mac))))

(define (device-battery mac)
  (let* ((info (run-command (string-append "bluetoothctl info " mac)))
         (line (find (lambda (l) (string-match "Battery Percentage: ([0-9]+)%" l)) info)))
    (and line
         (let ((m (string-match "Battery Percentage: ([0-9]+)%" line)))
           (match:substring m 1)))))   ;; returns e.g. "87" or #f

(define (device-status-label mac)
  (let* ((conn? (device-connected? mac))
         (bat  (device-battery mac))
         (dot  (if conn? "🟢" "🔴"))
         (txt  (if conn? "connected" "disconnected")))
    (if bat
        (string-append dot " " "[" bat "%]")
        (string-append dot))))

(define (get-paired-devices)
  (filter-map
   (lambda (line)
     (let ((m (string-match "Device ([[:xdigit:]:]+) (.+)" line)))
       (and m (list (match:substring m 1) (match:substring m 2)))))
   (run-command "bluetoothctl devices Paired")))

(define (get-paired-macs)
  (filter-map
   (lambda (line)
     (let ((m (string-match "Device ([[:xdigit:]:]+) " line)))
       (and m (match:substring m 1))))
   (run-command "bluetoothctl devices Paired")))

(define (get-unpaired-devices)
  (let* ((paired (get-paired-macs))
         (lines  (run-command "bluetoothctl devices")))
    (filter-map
     (lambda (line)
       (let ((m (string-match "Device ([[:xdigit:]:]+) (.+)" line)))
         (and m
              (let* ((mac  (match:substring m 1))
                     (name (match:substring m 2)))
                (if (member mac paired)         ; already paired? skip it
                    #f
                    (string-append mac " " name))))))
     lines)))

(define (connect-device mac)
  (run-command (string-append "bluetoothctl connect " mac) #:echo? #t))

(define (pair-device mac)
  (run-command (string-append "bluetoothctl pair " mac) #:echo? #t))

(define (trust-device mac)
  (run-command (string-append "bluetoothctl trust " mac) #:echo? #t))

(define (disconnect-device mac)
  (run-command (string-append "bluetoothctl disconnect " mac) #:echo? #t))

(define (forget-device mac)
  (run-command (string-append "bluetoothctl remove " mac) #:echo? #t))  ;; fixed typo

(define (scan-for-devices duration)
  (run-command-async (string-append "bluetoothctl --timeout " (number->string duration) " scan on")))

(define (get-discovered-devices)
  (let ((lines (run-command "bluetoothctl devices")))
    (filter-map
     (lambda (line)
       (let ((m (string-match "Device ([[:xdigit:]:]+) (.+)" line)))
         (and m (string-append (match:substring m 1) " " (match:substring m 2)))))
     lines)))

(define (device-action-menu mac name)
  (let ((action (dmenu-prompt (string-append name " > ") '("Connect" "Disconnect" "Forget" "Back"))))
    (cond
     ((string=? action "Connect") (connect-device mac))
     ((string=? action "Disconnect") (disconnect-device mac))
     ((string=? action "Forget") (forget-device mac))
     ((string=? action "Back") (select-paired-device))
     (else #f))))

(define (select-paired-device)
  (let* ((entries (get-paired-devices)))
    (if (null? entries)
        (begin
          (run-command "notify-send Bluetooth 'No paired devices found'")
          #f)
        ;; Build labeled options with status, and keep an assoc to recover (mac name)
        (let* ((option-map
                (map (lambda (entry)
                       (let* ((mac  (car entry))
                              (name (cadr entry))
                              (stat (device-status-label mac))
                              (label (string-append stat " " mac " " name)))
                         (cons label entry)))
                     entries))
               (labels (map car option-map))
               (selected (dmenu-prompt "Select Paired Device > " labels)))
          (if (string-null? selected)
              #f
              (let* ((found (assoc selected option-map))
                     (mac   (car (cdr found)))
                     (name  (cadr (cdr found))))
                (device-action-menu mac name)))))))

(define (scan-and-pair)
  (scan-for-devices 60)
  (run-command "notify-send Bluetooth 'Scanning for 5 seconds...'")
  (sleep 5)
  (let ((devices (get-unpaired-devices)))
    (if (null? devices)
        (run-command "notify-send Bluetooth 'No new devices found'")
        (let ((selected (dmenu-prompt "Select Device to Pair > " devices)))
          (when (not (string-null? selected))
            (let ((mac (car (string-split selected #\space))))
              (pair-device mac)
			  (trust-device mac)
			  (sleep 2)
              ;;(connect-device mac)
			  ))))))

;; --- Main Menu ---
(define (main)
  (let* ((power-choice (string-append
                        "Turn Power "
                        (if (bluetooth-powered?) "Off" "On")))
         (choice (dmenu-prompt "Bluetooth Menu > "
                  (list "List Paired Devices"
                        "Scan & Pair New Device"
                        power-choice
                        "Exit"))))
    (cond
     ((string=? choice power-choice) (toggle-power))
     ((string=? choice "List Paired Devices") (select-paired-device))
     ((string=? choice "Scan & Pair New Device") (scan-and-pair))
     (else #f))))

(main)
