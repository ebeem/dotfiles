(define hyprctl-get-active-workspace-id
  (let* ((out (system "hyprctl activeworkspace")))
    (display "output from terminal command is")
    (display out)
	(substring out (string-index out "(") (string-index out ")"))))

(display (string-concatenate "your current workspace id is " ))
