(use-modules (ice-9 popen)
             (ice-9 textual-ports))

(define (async-command-send command args)
  ())

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
  (async-command-send "pin"))






(define exec (command)
"""executes a shell command

Parameters:
	-	command"""
	(dispatch-command (string-append "exec" command)))

(define execr (command)
  """executes a raw shell command (does not support rules)

Parameters:
	-	command"""
  (dispatch-command (string-append "execr" command)))

(define pass (window)
  """passes the key (with mods) to a specified window.
Can be used as a workaround to global keybinds not working on Wayland.

Parameters:
	- window"""
  (dispatch-command (string-append "pass" window)))

(define killactive ()
  """closes (not kills) the active window

Parameters:
	- none"""
  (dispatch-command "killactive"))

(define closewindow (window)
  """closes a specified window

Parameters:
	- window"""
  (dispatch-command (string-append "closewindow" window)))

(define workspace (workspace)
  """changes the workspace

Parameters:
	- workspace"""
  (dispatch-command (string-append "workspace" workspace)))

(define movetoworkspace (workspace)
  """moves the focused window to a workspace

Parameters:
	- workspace OR workspace,window for a specific window"""
  (dispatch-command (string-append "movetoworkspace" workspace)))

(define movetoworkspacesilent (workspace)
  """same as above, but doesn’t switch to the workspace

Parameters:
	- workspace OR workspace,window for a specific window"""
  (dispatch-command (string-append "movetoworkspacesilent" workspace)))

(define togglefloating (window)
  """toggles the current window’s floating state

Parameters:
	- left empty / active for current, or window for a specific window"""
  (dispatch-command (string-append "togglefloating" window)))

(define setfloating (window)
  """sets the current window’s floating state to true

Parameters:
	- left empty / active for current, or window for a specific window"""
  (dispatch-command (string-append "setfloating" window)))

(define settiled (window)
  """sets the current window’s floating state to false

Parameters:
	- left empty / active for current, or window for a specific window"""
  (dispatch-command (string-append "settiled" window)))

(define fullscreen (option)
  """toggles the focused window’s fullscreen state

Parameters:
	- 0 - fullscreen (takes your entire screen), 1 - maximize (keeps gaps and bar(s)), 2 - fullscreen (same as fullscreen except doesn’t alter window’s internal fullscreen state)"""
  (dispatch-command (string-append "fullscreen" option)))

(define fakefullscreen ()
  """toggles the focused window’s internal fullscreen state without altering the geometry

Parameters:
	- none"""
  (dispatch-command (string-append "fakefullscreen")))

(define dpms (option)
  """sets all monitors’ DPMS status. Do not use with a keybind directly.

Parameters:
	- on, off, or toggle. For specific monitor add monitor name after a space"""
  (dispatch-command (string-append "dpms" option)))

(define pin (window)
  """pins a window (i.e. show it on all workspaces) note: floating only

Parameters:
	- left empty / active for current, or window for a specific window"""
  (dispatch-command (string-append "pin" window)))

(define movefocus (direction)
  """moves the focus in a direction

Parameters:
	- direction"""
  (dispatch-command (string-append "movefocus" direction)))

(define movewindow (direction)
  """moves the active window in a direction or to a monitor. For floating windows, moves the window to the screen edge in that direction

Parameters:
	- direction or mon: and a monitor, optionally followed by a space and silent to prevent the focus from moving with the window"""
  (dispatch-command (string-append "movewindow" direction)))

(define swapwindow (direction)
  """swaps the active window with another window in the given direction

Parameters:
	- direction"""
  (dispatch-command (string-append "swapwindow" direction)))

(define centerwindow (option)
  """center the active window note: floating only

Parameters:
	- none (for monitor center) or 1 (to respect monitor reserved area)"""
  (dispatch-command (string-append "centerwindow" option)))

(define resizeactive (resizeparams)
  """resizes the active window

Parameters:
	- resizeparams"""
  (dispatch-command (string-append "resizeactive" resizeparams)))

(define moveactive (resizeparams)
  """moves the active window

Parameters:
	- resizeparams"""
  (dispatch-command (string-append "moveactive" resizeparams)))

(define resizewindowpixel (resizeparams)
  """resizes a selected window

Parameters:
	- resizeparams,window, e.g. 100 100,^(kitty)$"""
  (dispatch-command (string-append "resizewindowpixel" resizeparams)))

(define movewindowpixel (resizeparams)
  """moves a selected window

Parameters:
	- resizeparams,window"""
  (dispatch-command (string-append "movewindowpixel" resizeparams)))

(define cyclenext (option)
  """focuses the next window on a workspace

Parameters:
	- none (for next) or prev (for previous) additionally tiled for only tiled, floating for only floating. prev tiled is ok."""
  (dispatch-command (string-append "cyclenext" option)))

(define swapnext (option)
  """swaps the focused window with the next window on a workspace

Parameters:
	- none (for next) or prev (for previous)"""
  (dispatch-command (string-append "swapnext" option)))

(define focuswindow (window)
  """focuses the first window matching

Parameters:
	- window"""
  (dispatch-command (string-append "focuswindow" window)))

(define focusmonitor (monitor)
  """focuses a monitor

Parameters:
	- monitor"""
  (dispatch-command (string-append "focusmonitor" monitor)))

(define splitratio (ratio)
  """changes the split ratio

Parameters:
	- floatvalue"""
  (dispatch-command (string-append "splitratio" ratio)))

(define toggleopaque ()
  """toggles the current window to always be opaque. Will override the opaque window rules.

Parameters:
	- none"""
  (dispatch-command (string-append "toggleopaque")))

(define movecursortocorner (direction)
  """moves the cursor to the corner of the active window

Parameters:
	- direction, 0 - 3, bottom left - 0, bottom right - 1, top right - 2, top left - 3"""
  (dispatch-command (string-append "movecursortocorner" direction)))

(define movecursor (x y)
  """moves the cursor to a specified position

Parameters:
	- x
	- y"""
  (dispatch-command (string-append "movecursor" x y)))

(define renameworkspace (id name)
  """rename a workspace

Parameters:
	- id
	- name,
e.g. 2 work"""
  (dispatch-command (string-append "renameworkspace")))

(define exit ()
  """exits the compositor with no questions asked.

Parameters:
	- none"""
  (dispatch-command (string-append "exit")))

(define forcerendererreload ()
  """forces the renderer to reload all resources and outputs

Parameters:
	- none"""
  (dispatch-command (string-append "forcerendererreload")))

(define movecurrentworkspacetomonitor (monitor)
  """Moves the active workspace to a monitor

Parameters:
	- monitor"""
  (dispatch-command (string-append "movecurrentworkspacetomonitor" monitor)))

(define focusworkspaceoncurrentmonitor (workspace)
  """Focuses the requested workspace on the current monitor, swapping the current workspace to a different monitor if necessary. If you want XMonad/Qtile-style workspace switching, replace workspace in your config with this.

Parameters:
	- workspace"""
  (dispatch-command (string-append "focusworkspaceoncurrentmonitor" workspace)))

(define moveworkspacetomonitor
  """Moves a workspace to a monitor

Parameters:
	- workspace and a monitor separated by a space"""
  (dispatch-command (string-append "moveworkspacetomonitor")))

(define swapactiveworkspaces (monitor1 monitor2)
  """Swaps the active workspaces between two monitors

Parameters:
	- two monitors separated by a space"""
  (dispatch-command (string-append "swapactiveworkspaces" monitor1 " " monitor2)))

(define bringactivetotop ()
  """Deprecated in favor of alterzorder.
Brings the current window to the top of the stack

Parameters:
	- none"""
  (dispatch-command (string-append "bringactivetotop")))

(define alterzorder (zheight window)
  """Modify the window stack order of the active or specified window. Note: this cannot be used to move a floating window behind a tiled one.

Parameters:
	- zheight
	- window"""
  (dispatch-command (string-append "alterzorder" (and ())))

(define togglespecialworkspace (name)
  """toggles a special workspace on/off

Parameters:
	- none (for the first) or name for named (name has to be a special workspace’s name)"""
  (dispatch-command (string-append "togglespecialworkspace" name)))

(define focusurgentorlast ()
  """Focuses the urgent window or the last window

Parameters:
	- none"""
  (dispatch-command (string-append "focusurgentorlast")))

(define togglegroup ()
  """toggles the current active window into a group

Parameters:
	- none"""
  (dispatch-command (string-append "togglegroup")))

(define changegroupactive (index)
  """switches to the next window in a group.

Parameters:
	- b - back, f - forward, or index start at 1"""
  (dispatch-command (string-append "changegroupactive" index)))

(define focuscurrentorlast ()
  """Switch focus from current to previously focused window

Parameters:
	- none"""
  (dispatch-command (string-append "focuscurrentorlast")))

(define lockgroups (option)
  """Locks the groups (all groups will not accept new windows)

Parameters:
	- lock for locking, unlock for unlocking, toggle for toggle"""
  (dispatch-command (string-append "lockgroups" option)))

(define lockactivegroup (option)
  """Lock the focused group (the current group will not accept new windows or be moved to other groups)

Parameters:
	- lock for locking, unlock for unlocking, toggle for toggle"""
  (dispatch-command (string-append "lockactivegroup" option)))

(define moveintogroup (direction)
  """Moves the active window into a group in a specified direction. No-op if there is no group in the specified direction.

Parameters:
	- direction"""
  (dispatch-command (string-append "moveintogroup" direction)))

(define moveoutofgroup (window)
  """Moves the active window out of a group. No-op if not in a group

Parameters:
	- left empty / active for current, or window for a specific window"""
  (dispatch-command (string-append "moveoutofgroup" window)))

(define movewindoworgroup (direction)
  """Behaves as moveintogroup if there is a group in the given direction. Behaves as moveoutofgroup if there is no group in the given direction relative to the active group. Otherwise behaves like movewindow.

Parameters:
	- direction"""
  (dispatch-command (string-append "movewindoworgroup" direction)))

(define movegroupwindow (direction)
  """Swaps the active window with the next or previous in a group

Parameters:
	- b for back, anything else for forward"""
  (dispatch-command (string-append "movegroupwindow" direction)))

(define denywindowfromgroup (option)
  """Prohibit the active window from becoming or being inserted into group

Parameters:
	- on, off or, toggle"""
  (dispatch-command (string-append "denywindowfromgroup" option)))

(define setignoregrouplock (option)
  """Temporarily enable or disable binds:ignore_group_lock

Parameters:
	- on, off, or toggle"""
  (dispatch-command (string-append "setignoregrouplock" option)))

(define global (name)
  """Executes a Global Shortcut using the GlobalShortcuts portal. See here

Parameters:
	- name"""
  (dispatch-command (string-append "global" name)))

(define submap (name)
  """Change the current mapping group. See Submaps

Parameters:
	- reset or name"""
  (dispatch-command (string-append "submap" name)))
