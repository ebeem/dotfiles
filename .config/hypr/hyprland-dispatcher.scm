(use-modules (ice-9 popen)
             (ice-9 textual-ports))

(define (dispatch command args)
  )

(define (exec cmd)
  (async-command-send (string-append "/dispatch exec " cmd)))

(define (execr cmd)
  (async-command-send (string-append "/dispatch execr " cmd)))

(define (pass w)
  (async-command-send (string-append "/dispatch pass " w)))

(define (kill-active)
  (async-command-send "/dispatch killactive"))

(define (close-window window)
  (async-command-send (string-append "/dispatch closewindow " window)))

(define (workspace w)
  (async-command-send (string-append "/dispatch workspace " w)))

(define (move-to-workspace w)
  (async-command-send (string-append "/dispatch movetoworkspace " w)))

(define (move-to-workspace-silent w)
  (async-command-send (string-append "/dispatch movetoworkspacesilent " w)))

(define (toggle-floating w)
  (async-command-send (string-append "/dispatch togglefloating " w)))

(define (fullscreen mode)
  (async-command-send (string-append "/dispatch fullscreen " mode)))

(define (dpms m active)
  (async-command-send (string-append "/dispatch dpms " (if active "on" "off") " " m)))

(define (pseudo)
  (async-command-send "/dispatch pseudo"))

(define (pin)
  (async-command-send "/dispatch pin"))

(define (move-focus direction)
  (async-command-send (string-append "/dispatch movefocus " direction)))

(define (move-window direction)
  (async-command-send (string-append "/dispatch movewindow " direction)))

(define (center-window)
  (async-command-send "/dispatch centerwindow"))

(define (resize-active x y)
  (async-command-send (string-append "/dispatch resizeactive " x " " y)))

(define (move-active x y)
  (async-command-send (string-append "/dispatch moveactive " x " " y)))

(define (resize-window-pixel window x y)
  (async-command-send (string-append "/dispatch resizewindowpixel " x " " y "," window)))

(define (move-window-pixel window x y)
  (async-command-send (string-append "/dispatch movewindowpixel " x " " y "," window)))

(define (cycle-next prev)
  (async-command-send (string-append "/dispatch cyclenext " (if prev "prev" ""))))

(define (swap-next prev)
  (async-command-send (string-append "/dispatch swapnext " (if prev "prev" ""))))

(define (focus-window w)
  (async-command-send (string-append "/dispatch focuswindow " w)))

(define (focus-monitor m)
  (async-command-send (string-append "/dispatch focusmonitor " m)))

(define (split-ratio x)
  (async-command-send (string-append "/dispatch splitratio " (number->string x))))

(define (toggle-opaque)
  (async-command-send "/dispatch toggleopaque"))

(define (move-cursor-to-corner c)
  (async-command-send (string-append "/dispatch movecursortocorner " c)))

(define (workspace-opt opt)
  (async-command-send (string-append "/dispatch workspaceopt " opt)))

(define (exit)
  (async-command-send "/dispatch exit"))

(define (force-renderer-reload)
  (async-command-send "/dispatch forcerendererreload"))

(define (move-current-workspace-to-monitor m)
  (async-command-send (string-append "/dispatch movecurrentworkspacetomonitor " m)))

(define (move-workspace-to-monitor w m)
  (async-command-send (string-append "/dispatch moveworkspacetomonitor " w " " m)))

(define (swap-active-workspaces m1 m2)
  (async-command-send (string-append "/dispatch swapactiveworkspaces " m1 " " m2)))

(define (bring-active-to-top)
  (async-command-send "/dispatch bringactivetotop"))

(define (toggle-special-workspace sw)
  (async-command-send (string-append "/dispatch togglespecialworkspace " (if sw sw ""))))

(define (reload-config)
  (async-command-send "reload"))

(define (kill-mode)
  (async-command-send "kill"))

(define (cursor-theme theme size)
  (async-command-send (string-append "cursor " theme " " size)))

(define (switch-xkb-layout device cmd)
  (async-command-send (string-append "switchxkblayout " device " " cmd)))

;; TODO: exec should support rules
(define (exec cmd)
  "Executes a shell command
params: command to execute"
  (dispatch "exec" cmd))

(define (execr cmd)
  "Executes a raw shell command (does not support rules)
params: command to execute"
  (dispatch "execr" cmd))

(define (pass window)
  "Passes the key (with mods) to a specified window. Can be used
as a workaround to global keybinds not working on Wayland.
params: window"
  (dispatch "pass" window))

(define (kill-active)
  "Closes (not kills) the active window
params: none"
  (dispatch "killactive"))

(define (close-window window)
  "closes a specified window
params: window"
  (dispatch "closewindow" window))
(define (workspace)
  "Changes the workspace	workspace"
  (dispatch "workspace"))

(define (move-to-workspace)
  "moves the focused window to a workspace
params: workspace OR workspace,window for a specific window"
  (dispatch "movetoworkspace"))

(define (move-to-workspace-silent)
  "same as above, but doesnt switch to the workspace
params: workspace OR workspace,window for a specific window"
  (dispatch "movetoworkspacesilent"))

(define (toggle-floating)
  "toggles the current window’s floating state
params: left empty / active for current, or window for a specific window"
  (dispatch "togglefloating"))

(define (fullscreen)
  "toggles the focused window’s fullscreen state
params: 0 - fullscreen (takes your entire screen),
		1 - maximize (keeps gaps and bar(s)),
		2 - fullscreen (same as fullscreen except doesn’t alter window’s internal fullscreen state)"
  (dispatch "fullscreen"))

(define (fake-fullscreen)
  "toggles the focused window’s internal fullscreen state without altering the geometry
params: none"
  (dispatch "fakefullscreen"))

(define (dpms)
  "sets all monitors’ DPMS status. Do not use with a keybind directly.
params: on, off, or toggle. For specific monitor add monitor name after a space"
  (dispatch "dpms"))

(define (pin)
  "pins a window (i.e. show it on all workspaces) note: floating only
params: left empty / active for current, or window for a specific window"
  (dispatch "pin"))
