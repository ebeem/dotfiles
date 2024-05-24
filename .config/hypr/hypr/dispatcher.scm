(define-module (hypr dispatcher)
  #:use-module (hypr utility)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)

  #:export (dispatch-pass
            dispatch-kill-active
            dispatch-close-window
            dispatch-workspace
            dispatch-move-to-workspace
            dispatch-move-to-workspace-silent
            dispatch-toggle-floating
            dispatch-set-floating
            dispatch-set-tiled
            dispatch-fullscreen
            dispatch-fake-fullscreen
            dispatch-dpms
            dispatch-pin
            dispatch-move-focus
            dispatch-move-window
            dispatch-swap-window
            dispatch-center-window
            dispatch-resize-active
            dispatch-move-active
            dispatch-resize-window-pixel
            dispatch-move-window-pixel
            dispatch-cycle-next
            dispatch-swap-next
            dispatch-focus-window
            dispatch-focus-monitor
            dispatch-split-ratio
            dispatch-toggle-opaque
            dispatch-move-cursor-to-corner
            dispatch-move-cursor
            dispatch-rename-workspace
            dispatch-exit
            dispatch-force-renderer-reload
            dispatch-move-current-workspace-to-monitor
            dispatch-focus-workspace-on-current-monitor
            dispatch-move-workspace-to-monitor
            dispatch-swap-active-workspaces
            dispatch-bring-active-to-top
            dispatch-alter-zorder
            dispatch-toggle-special-workspace
            dispatch-focus-urgent-or-last
            dispatch-toggle-group
            dispatch-change-group-active
            dispatch-focus-current-or-last
            dispatch-lock-groups
            dispatch-lock-active-group
            dispatch-move-in-to-group
            dispatch-move-out-of-group
            dispatch-move-window-or-group
            dispatch-move-group-window
            dispatch-deny-window-from-group
            dispatch-set-ignore-group-lock
            dispatch-global
            dispatch-submap
            dispatch-execr
            dispatch-exec))

(define (dispatch-exec command)
"""executes a shell command

Parameters:
	-	command"""
	(dispatch-command (string-append "exec " command)))

(define (dispatch-execr command)
  """executes a raw shell command (does not support rules)

Parameters:
	-	command"""
  (dispatch-command (string-append "execr " command)))

(define (dispatch-pass window)
  """passes the key (with mods) to a specified window.
Can be used as a workaround to global keybinds not working on Wayland.

Parameters:
	- window"""
  (dispatch-command (string-append "pass " window)))

(define (dispatch-kill-active)
  """closes (not kills) the active window

Parameters:
	- none"""
  (dispatch-command "killactive"))

(define (dispatch-close-window window)
  """closes a specified window

Parameters:
	- window"""
  (dispatch-command (string-append "closewindow " window)))

(define (dispatch-workspace workspace)
  """changes the workspace

Parameters:
	- workspace"""
  (dispatch-command (string-append "workspace " workspace)))

(define (dispatch-move-to-workspace workspace)
  """moves the focused window to a workspace

Parameters:
	- workspace OR workspace,window for a specific window"""
  (dispatch-command (string-append "movetoworkspace " workspace)))

(define (dispatch-move-to-workspace-silent workspace)
  """same as above, but doesn’t switch to the workspace

Parameters:
	- workspace OR workspace,window for a specific window"""
    (display (string-append "go to target workspace " workspace "\n"))
    (dispatch-command (string-append "movetoworkspacesilent " workspace)))

(define (dispatch-toggle-floating window)
  """toggles the current window’s floating state

Parameters:
	- left empty / active for current, or window for a specific window"""
  (dispatch-command (string-append "togglefloating " window)))

(define (dispatch-set-floating window)
  """sets the current window’s floating state to true

Parameters:
	- left empty / active for current, or window for a specific window"""
  (dispatch-command (string-append "setfloating " window)))

(define (dispatch-set-tiled window)
  """sets the current window’s floating state to false

Parameters:
	- left empty / active for current, or window for a specific window"""
  (dispatch-command (string-append "settiled " window)))

(define (dispatch-fullscreen option)
  """toggles the focused window’s fullscreen state

Parameters:
	- 0 - fullscreen (takes your entire screen), 1 - maximize (keeps gaps and bar(s)), 2 - fullscreen (same as fullscreen except doesn’t alter window’s internal fullscreen state)"""
  (dispatch-command (string-append "fullscreen " option)))

(define (dispatch-fake-fullscreen)
  """toggles the focused window’s internal fullscreen state without altering the geometry

Parameters:
	- none"""
  (dispatch-command "fakefullscreen"))

(define (dispatch-dpms option)
  """sets all monitors’ DPMS status. Do not use with a keybind directly.

Parameters:
	- on, off, or toggle. For specific monitor add monitor name after a space"""
  (dispatch-command (string-append "dpms " option)))

(define (dispatch-pin window)
  """pins a window (i.e. show it on all workspaces) note: floating only

Parameters:
	- left empty / active for current, or window for a specific window"""
  (dispatch-command (string-append "pin " window)))

(define (dispatch-move-focus direction)
  """moves the focus in a direction

Parameters:
	- direction"""
  (dispatch-command (string-append "movefocus " direction)))

(define (dispatch-move-window direction)
  """moves the active window in a direction or to a monitor. For floating windows, moves the window to the screen edge in that direction

Parameters:
	- direction or mon: and a monitor, optionally followed by a space and silent to prevent the focus from moving with the window"""
  (dispatch-command (string-append "movewindow " direction)))

(define (dispatch-swap-window direction)
  """swaps the active window with another window in the given direction

Parameters:
	- direction"""
  (dispatch-command (string-append "swapwindow " direction)))

(define (dispatch-center-window option)
  """center the active window note: floating only

Parameters:
	- none (for monitor center) or 1 (to respect monitor reserved area)"""
  (dispatch-command (string-append "centerwindow " option)))

(define (dispatch-resize-active resizeparams)
  """resizes the active window

Parameters:
	- resizeparams"""
  (dispatch-command (string-append "resizeactive " resizeparams)))

(define (dispatch-move-active resizeparams)
  """moves the active window

Parameters:
	- resizeparams"""
  (dispatch-command (string-append "moveactive " resizeparams)))

(define (dispatch-resize-window-pixel resizeparams)
  """resizes a selected window

Parameters:
	- resizeparams,window, e.g. 100 100,^(kitty)$"""
  (dispatch-command (string-append "resizewindowpixel " resizeparams)))

(define (dispatch-move-window-pixel resizeparams)
  """moves a selected window

Parameters:
	- resizeparams,window"""
  (dispatch-command (string-append "movewindowpixel " resizeparams)))

(define (dispatch-cycle-next option)
  """focuses the next window on a workspace

Parameters:
	- none (for next) or prev (for previous) additionally tiled for only tiled, floating for only floating. prev tiled is ok."""
  (dispatch-command (string-append "cyclenext " option)))

(define (dispatch-swap-next option)
  """swaps the focused window with the next window on a workspace

Parameters:
	- none (for next) or prev (for previous)"""
  (dispatch-command (string-append "swapnext " option)))

(define (dispatch-focus-window window)
  """focuses the first window matching

Parameters:
	- window"""
  (dispatch-command (string-append "focuswindow " window)))

(define (dispatch-focus-monitor monitor)
  """focuses a monitor

Parameters:
	- monitor"""
  (dispatch-command (string-append "focusmonitor " monitor)))

(define (dispatch-split-ratio ratio)
  """changes the split ratio

Parameters:
	- floatvalue"""
  (dispatch-command (string-append "splitratio " ratio)))

(define (dispatch-toggle-opaque)
  """toggles the current window to always be opaque. Will override the opaque window rules.

Parameters:
	- none"""
  (dispatch-command "toggleopaque"))

(define (dispatch-move-cursor-to-corner direction)
  """moves the cursor to the corner of the active window

Parameters:
	- direction, 0 - 3, bottom left - 0, bottom right - 1, top right - 2, top left - 3"""
  (dispatch-command (string-append "movecursortocorner " direction)))

(define (dispatch-move-cursor x y)
  """moves the cursor to a specified position

Parameters:
	- x
	- y"""
  (dispatch-command (string-append "movecursor " x " " y)))

(define (dispatch-rename-workspace id name)
  """rename a workspace

Parameters:
	- id
	- name,
e.g. 2 work"""
  (dispatch-command (string-append "renameworkspace " id " " name)))

(define (dispatch-exit)
  """exits the compositor with no questions asked.

Parameters:
	- none"""
  (dispatch-command "exit"))

(define (dispatch-force-renderer-reload)
  """forces the renderer to reload all resources and outputs

Parameters:
	- none"""
  (dispatch-command "forcerendererreload"))

(define (dispatch-move-current-workspace-to-monitor monitor)
  """Moves the active workspace to a monitor

Parameters:
	- monitor"""
  (dispatch-command (string-append "movecurrentworkspacetomonitor " monitor)))

(define (dispatch-focus-workspace-on-current-monitor workspace)
  """Focuses the requested workspace on the current monitor, swapping the current workspace to a different monitor if necessary. If you want XMonad/Qtile-style workspace switching, replace workspace in your config with this.

Parameters:
	- workspace"""
  (dispatch-command (string-append "focusworkspaceoncurrentmonitor " workspace)))

(define (dispatch-move-workspace-to-monitor workspace monitor)
  """Moves a workspace to a monitor

Parameters:
	- workspace and a monitor separated by a space"""
  (dispatch-command (string-append "moveworkspacetomonitor " workspace " " monitor)))

(define (dispatch-swap-active-workspaces monitor1 monitor2)
  """Swaps the active workspaces between two monitors

Parameters:
	- two monitors separated by a space"""
  (dispatch-command (string-append "swapactiveworkspaces " monitor1 " " monitor2)))

(define (dispatch-bring-active-to-top)
  """Deprecated in favor of alterzorder.
Brings the current window to the top of the stack

Parameters:
	- none"""
  (dispatch-command "bringactivetotop"))

(define (dispatch-alter-zorder zheight window)
  """Modify the window stack order of the active or specified window. Note: this cannot be used to move a floating window behind a tiled one.

Parameters:
	- zheight
	- window"""
    ;; FIXME
  (dispatch-command (string-append "alterzorder")))

(define (dispatch-toggle-special-workspace name)
  """toggles a special workspace on/off

Parameters:
	- none (for the first) or name for named (name has to be a special workspace’s name)"""
  (dispatch-command (string-append "togglespecialworkspace " name)))

(define (dispatch-focus-urgent-or-last)
  """Focuses the urgent window or the last window

Parameters:
	- none"""
  (dispatch-command "focusurgentorlast"))

(define (dispatch-toggle-group)
  """toggles the current active window into a group

Parameters:
	- none"""
  (dispatch-command "togglegroup"))

(define (dispatch-change-group-active index)
  """switches to the next window in a group.

Parameters:
	- b - back, f - forward, or index start at 1"""
  (dispatch-command (string-append "changegroupactive " index)))

(define (dispatch-focus-current-or-last)
  """Switch focus from current to previously focused window

Parameters:
	- none"""
  (dispatch-command "focuscurrentorlast"))

(define (dispatch-lock-groups option)
  """Locks the groups (all groups will not accept new windows)

Parameters:
	- lock for locking, unlock for unlocking, toggle for toggle"""
  (dispatch-command (string-append "lockgroups " option)))

(define (dispatch-lock-active-group option)
  """Lock the focused group (the current group will not accept new windows or be moved to other groups)

Parameters:
	- lock for locking, unlock for unlocking, toggle for toggle"""
  (dispatch-command (string-append "lockactivegroup " option)))

(define (dispatch-move-in-to-group direction)
  """Moves the active window into a group in a specified direction. No-op if there is no group in the specified direction.

Parameters:
	- direction"""
  (dispatch-command (string-append "moveintogroup " direction)))

(define (dispatch-move-out-of-group window)
  """Moves the active window out of a group. No-op if not in a group

Parameters:
	- left empty / active for current, or window for a specific window"""
  (dispatch-command (string-append "moveoutofgroup " window)))

(define (dispatch-move-window-or-group direction)
  """Behaves as moveintogroup if there is a group in the given direction. Behaves as moveoutofgroup if there is no group in the given direction relative to the active group. Otherwise behaves like movewindow.

Parameters:
	- direction"""
  (dispatch-command (string-append "movewindoworgroup " direction)))

(define (dispatch-move-group-window direction)
  """Swaps the active window with the next or previous in a group

Parameters:
	- b for back, anything else for forward"""
  (dispatch-command (string-append "movegroupwindow " direction)))

(define (dispatch-deny-window-from-group option)
  """Prohibit the active window from becoming or being inserted into group

Parameters:
	- on, off or, toggle"""
  (dispatch-command (string-append "denywindowfromgroup " option)))

(define (dispatch-set-ignore-group-lock option)
  """Temporarily enable or disable binds:ignore_group_lock

Parameters:
	- on, off, or toggle"""
  (dispatch-command (string-append "setignoregrouplock " option)))

(define (dispatch-global name)
  """Executes a Global Shortcut using the GlobalShortcuts portal. See here

Parameters:
	- name"""
  (dispatch-command (string-append "global " name)))

(define (dispatch-submap name)
  """Change the current mapping group. See Submaps

Parameters:
	- reset or name"""
  (dispatch-command (string-append "submap " name)))
