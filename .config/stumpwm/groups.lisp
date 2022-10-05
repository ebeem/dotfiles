(in-package :stumpwm)

(defun gget-direction (dir)
  ;; get the group index based on the current group and a direction
  ;; the direction must be a member of :up :down :left :right
  (declare (type (member :up :down :left :right) dir))
  (let ((c-row (floor (- (group-number (current-group)) 1) *groups-columns*))
        (c-col (mod (- (group-number (current-group)) 1) *groups-columns*)))
    (ecase dir
      (:left (setq c-col (mod (- c-col 1) *groups-columns*)))
      (:right (setq c-col (mod (+ c-col 1) *groups-columns*)))
      (:up (setq c-row (mod (- c-row 1) *groups-rows*)))
      (:down (setq c-row (mod (+ c-row 1) *groups-rows*))))
    (+ 1 (+ c-col (* c-row *groups-columns*)))))

(defcommand gselect-direction (dir) ((:direction "Direction: "))
  ;; select the group based on a direction
  (gselect (write-to-string (gget-direction dir))))

(defcommand gmove-direction (dir) ((:direction "Direction: "))
  ;; moves the current window and follows a group based on a direction
  (gmove-and-follow (select-group (current-screen) (write-to-string (gget-direction dir)))))

(grename (car stumpwm::*groups-names*))
(dolist (g (cdr stumpwm::*groups-names*))
  (gnewbg g))

;; default windows placement
(clear-window-placement-rules)

(define-frame-preference "4-communication"
  (0 nil   t   :instance "chromium" :type :normal)
  (1 nil   t   :instance "Discord")
  (2 nil   t   :instance "slack"))

(define-frame-preference "6-gaming"
  (0 nil   t   :instance "steam" :type :normal))
