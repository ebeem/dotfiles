(define-module (swayipc dispatcher)
  #:use-module (swayipc connection)
  #:use-module (swayipc records)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (json)

  #:export (RUN-COMMMAND-MSG-ID
            GET-WORKSPACES-MSG-ID
            SUBSCRIBE-MSG-ID
            GET-OUTPUTS-MSG-ID
            GET-TREE-MSG-ID
            GET-MARKS-MSG-ID
            GET-BAR-CONFIG-MSG-ID
            GET-VERSION-MSG-ID
            GET-BINDINGS-MODES-MSG-ID
            GET-CONFIG-MSG-ID

            dispatch-command
            dispatch-commands
            dispatch-switch-workspace))

(define RUN-COMMMAND-MSG-ID 0)
(define GET-WORKSPACES-MSG-ID 1)
(define SUBSCRIBE-MSG-ID 2)
(define GET-OUTPUTS-MSG-ID 3)
(define GET-TREE-MSG-ID 4)
(define GET-MARKS-MSG-ID 5)
(define GET-BAR-CONFIG-MSG-ID 6)
(define GET-VERSION-MSG-ID 7)
(define GET-BINDINGS-MODES-MSG-ID 8)
(define GET-CONFIG-MSG-ID 9)

(define (dispatch-command command)
  "Parses and runs the payload as sway command.
Parameters:
	- a sway command
Response:
    An  array of objects corresponding to each command that was parsed. Each
    object has the property success."
  (dispatch-commands command))

(define (dispatch-commands . commands)
  "Parses and runs the payload as sway commands
Parameters:
	- list of sway commands
Response:
    An  array of objects corresponding to each command that was parsed. Each
    object has the property success."
  (write-msg COMMAND-SOCKET
             RUN-COMMMAND-MSG-ID
             (string-join commands " "))
  (map
      (lambda (res)
        (scm->sway-success res))
      (vector->list
      (json-string->scm
        (read-msg COMMAND-SOCKET)))))


;;        bar [<bar-id>] <bar-subcommands...>
;;            For details on bar subcommands, see sway-bar(5).

(define SWAY-ORIENTATION-HORIZONTAL "horizontal")
(define SWAY-ORIENTATION-VERTICAL "vertical")
(define SWAY-ORIENTATION-AUTO "auto")

(define (sway-default-orientation orientation)
  "Sets the default container layout for tiled containers.
  parameters:
    - orientation: horizontal|vertical|auto"
  (dispatch-command
   (string-append "default_orientation " orientation)))

(define (sway-include file-path)
  "Includes another file from path.
  parameters:
    - file-path: string"
  (dispatch-command
   (string-append "include " file-path)))

(define (sway-swaybg-command command)
  "Executes custom background command.  Default  is  swaybg.
  parameters:
    - command: string"
  (dispatch-command
   (string-append "swaybg_command " command)))

(define (sway-swaynag-command command)
  "Executes custom command for swaynag. Default is swaynag.
  parameters:
    - command: string"
  (dispatch-command
   (string-append "swaynag_command " command)))

(define SWAY-LAYOUT-DEFAULT "default")
(define SWAY-LAYOUT-STAKCING "vertical")
(define SWAY-LAYOUT-TABBED "tabbed")

(define (sway-workspace-layout layout)
  "Specifies the initial layout for new containers in  an  empty  workspace.
  parameters:
    - layout: default|stacking|tabbed"
  (dispatch-command
   (string-append "workspace_layout " layout)))

(define (sway-xwayland option)
  "Enables  or disables Xwayland support, which allows X11 applications to be used.
  parameters:
    - option: enable|disable|force"
  (dispatch-command
   (string-append "xwayland " option)))

(define SWAY-BORDER-NONE "none")
(define SWAY-BORDER-NORMAL "normal")
(define SWAY-BORDER-CSD "csd")
(define SWAY-BORDER-PIXEL "pixel")

(define (sway-border option thickness)
  "Enables  or disables Xwayland support, which allows X11 applications to be used.
  parameters:
    - option: none|normal|csd|pixel
	- thickness: int"
  (dispatch-command
   (string-append "border " option (number->string thickness))))

(define (sway-border-toggle)
  "Cycles through the available border styles."
  (dispatch-command
   (string-append "border toggle")))

(define (sway-exit)
  "Exit sway and end your Wayland session."
  (dispatch-command
   (string-append "exit")))

(define SWAY-FLOATING-ENABLED "enabled")
(define SWAY-FLOATING-DISABLED "disabled")
(define SWAY-FLOATING-TOGGLE "toggle")

(define (sway-floating option)
  "Make focused view floating, non-floating, or the opposite of what it is now.
  parameters:
    - layout: default|stacking|tabbed"
  (dispatch-command
   (string-append "floating " option)))

(define (sway-focus-container-criteria criteria)
  "Moves focus to the container that matches the specified criteria.
  parameters:
    - criteria: sway criteria"
  (dispatch-command
   (string-append criteria " focus")))

(define SWAY-DIRECTION-UP "up")
(define SWAY-DIRECTION-RIGHT "right")
(define SWAY-DIRECTION-DOWN "down")
(define SWAY-DIRECTION-LEFT "left")
(define SWAY-SIBLING-NEXT "next")
(define SWAY-SIBLING-PREV "prev")

(define (sway-focus-container-direction direction)
  "Moves focus to the next container in the specified direction.
  parameters:
    - direction: up|right|down|left"
  (dispatch-command
   (string-append "focus " direction)))

(define (sway-focus-container-sibling sibling)
  "Moves focus to the previous or next container in the current layout.
  parameters:
    - sibling: next|prev"
  (dispatch-command
   (string-append "focus " sibling)))

(define (sway-focus-container-child)
  "Moves focus to the last-focused child of the focused container."
  (dispatch-command
   (string-append "focus child")))

(define (sway-focus-container-parent)
  "Moves focus to the last-focused child of the focused container."
  (dispatch-command
   (string-append "focus parent")))

(define (sway-focus-output-direction direction)
  "Moves focus to the next output in the specified direction.
  parameters:
    - direction: up|right|down|left"
  (dispatch-command
   (string-append "focus output " direction)))

(define (sway-focus-output-name name)
  "Moves focus to the named output.
  parameters:
    - name: string, output name"
  (dispatch-command
   (string-append "focus output " name)))

(define (sway-focus-container-tiling)
  "Sets focus to the last focused tiling container."
  (dispatch-command
   (string-append "focus tiling")))

(define (sway-focus-container-floating)
  "Sets focus to the last focused floating container."
  (dispatch-command
   (string-append "focus floating")))

(define SWAY-FULLSCREEN-ENABLED "enabled")
(define SWAY-FULLSCREEN-DISABLED "disabled")
(define SWAY-FULLSCREEN-TOGGLE "toggle")

(define (sway-fullscreen option global)
  "Makes focused view fullscreen, non-fullscreen, or the opposite of current."
  (dispatch-command
   (string-append "fullscreen " option " " global)))

(define SWAY-GAPS-OPTION-INNER "inner")
(define SWAY-GAPS-OPTION-OUTER "outer")
(define SWAY-GAPS-OPTION-HORIZONTAL "horizontal")
(define SWAY-GAPS-OPTION-VERTICAL "vertical")
(define SWAY-GAPS-OPTION-TOP "top")
(define SWAY-GAPS-OPTION-RIGHT "right")
(define SWAY-GAPS-OPTION-BOTTOM "bottom")
(define SWAY-GAPS-OPTION-LEFT "left")

(define SWAY-GAPS-WORKSPACE-INNER "all")
(define SWAY-GAPS-WORKSPACE-OUTER "current")
(define SWAY-GAPS-WORKSPACE-HORIZONTAL "set")
(define SWAY-GAPS-WORKSPACE-VERTICAL "plus")
(define SWAY-GAPS-WORKSPACE-TOP "minus")
(define SWAY-GAPS-WORKSPACE-RIGHT "toggle")

(define (sway-gaps option workspace amount)
  "Changes the inner or outer gaps for either all workspaces or the current workspace"
  (dispatch-command
   (string-append "gaps " option " " workspace " " amount)))

(define SWAY-INHIBIT-IDLE-FOCUS "all")
(define SWAY-INHIBIT-IDLE-FULLSCREEN "fullscreen")
(define SWAY-INHIBIT-IDLE-OPEN "open")
(define SWAY-INHIBIT-IDLE-NONE "none")
(define SWAY-INHIBIT-IDLE-VISIBLE "visible")

(define (sway-inhibit-idle option)
  "Set/unset an idle inhibitor for the view."
  (dispatch-command
   (string-append "inhibit_idle " option)))

(define SWAY-LAYOUT-DEFAULT "default")
(define SWAY-LAYOUT-SPLITH "splith")
(define SWAY-LAYOUT-SPLITV "splitv")
(define SWAY-LAYOUT-STACKING "stacking")
(define SWAY-LAYOUT-TABBED "tabbed")

(define (sway-layout option)
  "Set/unset an idle inhibitor for the view."
  (dispatch-command
   (string-append "layout " option)))

(define SWAY-LAYOUT-TOGGLE-ALL "all")
(define SWAY-LAYOUT-TOGGLE-SPLIT "split")

(define (sway-layout-toggle)
  "Cycles the layout mode of the focused container though a preset list of layouts."
  (dispatch-command
   (string-append "layout toggle")))

(define (sway-move-container direction)
  "Moves the focused container in the direction specified."
  (dispatch-command
   (string-append "move " direction)))

(define (sway-move-container-px direction px)
  "Moves the focused container in the direction specified."
  (dispatch-command
   (string-append "move " direction " " px)))

(define (sway-move-container-absolute-position x y)
  "Moves the focused container to the specified position in the workspace."
  (dispatch-command
   (string-append "move absolute position " x " " y)))

(define (sway-move-container-absolute-center)
  "Moves the focused container to be centered on the workspace."
  (dispatch-command
   (string-append "move absolute position center")))

(define (sway-move-container-cursor)
  "Moves the focused container to be centered on the cursor."
  (dispatch-command
   (string-append "move position cursor")))

(define (sway-move-container-to-mark mark)
  "Moves the focused container to the specified mark."
  (dispatch-command
   (string-append "move container to mark " mark)))

(define (sway-move-container-to-workspace workspace)
  "Moves the focused container to the workspace number or (prev|next|current)"
  (dispatch-command
   (string-append "move container to workspace " workspace)))

;;        move [container|window] [to] workspace prev|next|current
;;            Moves  the  focused container to the previous, next or current work‐
;;            space on this output, or if no workspaces remain,  the  previous  or
;;            next output.

;;        move [container|window] [to] workspace prev_on_output|next_on_output
;;            Moves  the  focused  container  to the previous or next workspace on
;;            this output, wrapping around if already at the first or  last  work‐
;;            space.

;;        move [container|window] [to] workspace back_and_forth
;;            Moves the focused container to previously focused workspace.

;;        move [container|window] [to] output <name-or-id>|current
;;            Moves the focused container to the specified output.

;;        move [container|window] [to] output up|right|down|left
;;            Moves  the  focused container to next output in the specified direc‐
;;            tion.

;;        move [container|window] [to] scratchpad
;;            Moves the focused container to the scratchpad.

;;        move workspace [to] output <name-or-id>|current
;;            Moves the focused workspace to the specified output.

;;        move workspace to [output] <name-or-id>|current
;;            Moves the focused workspace to the specified output.

;;        move workspace [to] output up|right|down|left
;;            Moves the focused workspace to next output in the  specified  direc‐
;;            tion.

;;        move workspace to [output] up|right|down|left
;;            Moves  the  focused workspace to next output in the specified direc‐
;;            tion.

;;        nop <comment>
;;            A no operation command that can be used to override  default  behav‐
;;            iour.  The  optional comment argument is ignored, but logged for de‐
;;            bugging purposes.

;;        reload
;;            Reloads the sway config file and applies  any  changes.  The  config
;;            file is located at path specified by the command line arguments when
;;            started, otherwise according to the priority stated in sway(1).

;;        rename workspace [<old_name>] to <new_name>
;;            Rename either <old_name> or the focused workspace to the <new_name>

;;        resize shrink|grow width|height [<amount> [px|ppt]]
;;            Resizes the currently focused container by amount, specified in pix‐
;;            els  or  percentage  points. If the units are omitted, floating con‐
;;            tainers are resized in px and tiled containers by ppt.  amount  will
;;            default to 10 if omitted.

;;        resize set height <height> [px|ppt]
;;            Sets  the  height of the container to height, specified in pixels or
;;            percentage points. If the units are omitted, floating containers are
;;            resized in px and tiled containers by ppt. If height is 0, the  con‐
;;            tainer will not be resized.

;;        resize set [width] <width> [px|ppt]
;;            Sets  the  width  of  the container to width, specified in pixels or
;;            percentage points. If the units are omitted, floating containers are
;;            resized in px and tiled containers by ppt. If width is 0,  the  con‐
;;            tainer will not be resized.

;;        resize set [width] <width> [px|ppt] [height] <height> [px|ppt]
;;            Sets  the  width  and  height  of the container to width and height,
;;            specified in pixels or percentage points. If the units are  omitted,
;;            floating  containers  are resized in px and tiled containers by ppt.
;;            If width or height is 0, the container will not be resized  on  that
;;            axis.

;;        scratchpad show
;;            Shows  a  window  from the scratchpad. Repeatedly using this command
;;            will cycle through the windows in the scratchpad.

;;        shortcuts_inhibitor enable|disable
;;            Enables or disables the  ability  of  clients  to  inhibit  keyboard
;;            shortcuts  for  a  view. This is primarily useful for virtualization
;;            and remote desktop software. It affects either the currently focused
;;            view or a set of views selected by criteria. Subcommand disable  ad‐
;;            ditionally  deactivates any active inhibitors for the given view(s).
;;            Criteria are particularly useful with the for_window command to con‐
;;            figure a class of views differently from the per-seat  defaults  es‐
;;            tablished by the seat subcommand of the same name. See sway-input(5)
;;            for more ways to affect inhibitors.

;;        split vertical|v|horizontal|h|none|n|toggle|t
;;            Splits  the current container, vertically or horizontally. When none
;;            is specified, the effect of a previous split is undone if  the  cur‐
;;            rent  container  is the only child of a split parent. When toggle is
;;            specified, the current container is split  opposite  to  the  parent
;;            container's layout.

;;        splith
;;            Equivalent to split horizontal

;;        splitv
;;            Equivalent to split vertical

;;        splitt
;;            Equivalent to split toggle

;;        sticky enable|disable|toggle
;;            "Sticks" a floating window to the current output so that it shows up
;;            on all workspaces.

;;        swap container with id|con_id|mark <arg>
;;            Swaps  the position, geometry, and fullscreen status of two contain‐
;;            ers. The first container can be selected either by criteria  or  fo‐
;;            cus. The second container can be selected by id, con_id, or mark. id
;;            can only be used with xwayland views. If the first container has fo‐
;;            cus,  it  will  retain focus unless it is moved to a different work‐
;;            space or the second container becomes fullscreen on the  same  work‐
;;            space  as  the first container. In either of those cases, the second
;;            container will gain focus.

;;        title_format <format>
;;            Sets the format of window titles. The following placeholders may  be
;;            used:

;;                %title - The title supplied by the window
;;                          %app_id  -  The  wayland app ID (applicable to wayland
;;                windows only)
;;                          %class - The X11  classname  (applicable  to  xwayland
;;                windows only)
;;                          %instance  -  The X11 instance (applicable to xwayland
;;                windows only)
;;                          %shell - The protocol the window is  using  (typically
;;                xwayland or
;;                    xdg_shell)

;;            This  command  is typically used with for_window criteria. For exam‐
;;            ple:

;;                for_window [title="."] title_format "<b>%title</b> (%app_id)"

;;            Note that markup requires pango to be enabled via the font command.

;;            The default format is "%title".

;;        The following commands may be used either in the configuration  file  or
;;        at runtime.

;;        assign <criteria> [→] [workspace] [number] <workspace>
;;            Assigns  views matching criteria (see CRITERIA for details) to work‐
;;            space. The → (U+2192) is optional  and  cosmetic.  This  command  is
;;            equivalent to:

;;                for_window <criteria> move container to workspace <workspace>

;;        assign <criteria> [→] output left|right|up|down|<name>
;;            Assigns  views  matching  criteria (see CRITERIA for details) to the
;;            specified output. The → (U+2192) is optional and cosmetic. This com‐
;;            mand is equivalent to:

;;                for_window <criteria> move container to output <output>

;;        bindsym  [--whole-window]  [--border]  [--exclude-titlebar]  [--release]
;;        [--locked]  [--to-code]  [--input-device=<device>] [--no-warn] [--no-re‐
;;        peat] [--inhibited] [Group<1-4>+]<key combo> <command>
;;            Binds key combo to execute the sway command  command  when  pressed.
;;            You  may use XKB key names here (wev(1) is a good tool for discover‐
;;            ing these). With the flag --release, the command  is  executed  when
;;            the  key  combo  is  released. If input-device is given, the binding
;;            will only be executed for that input device and will be executed in‐
;;            stead of any binding that is generic to all devices. If a group num‐
;;            ber is given, then the binding  will  only  be  available  for  that
;;            group. By default, if you overwrite a binding, swaynag will give you
;;            a warning. To silence this, use the --no-warn flag.

;;            Unless  the flag --locked is set, the command will not be run when a
;;            screen locking program is active. If there  is  a  matching  binding
;;            with  and  without  --locked,  the  one  with will be preferred when
;;            locked and the one without will be preferred when unlocked. If there
;;            are matching bindings and one has both --input-device  and  --locked
;;            and  the  other  has neither, the former will be preferred even when
;;            unlocked.

;;            Unless the flag --inhibited is set, the command will not be run when
;;            a keyboard shortcuts inhibitor is active for the  currently  focused
;;            window.  Such inhibitors are usually requested by remote desktop and
;;            virtualization software to enable the user to send  keyboard  short‐
;;            cuts  to  the remote or virtual session. The --inhibited flag allows
;;            one to define bindings which will be  exempt  from  pass-through  to
;;            such software. The same preference logic as for --locked applies.

;;            Unless  the flag --no-repeat is set, the command will be run repeat‐
;;            edly when the key is held, according to the repeat  settings  speci‐
;;            fied in the input configuration.

;;            Bindings  to  keysyms are layout-dependent. This can be changed with
;;            the --to-code flag. In this case, the  keysyms  will  be  translated
;;            into the corresponding keycodes in the first configured layout.

;;            Mouse  bindings operate on the container under the cursor instead of
;;            the container that has focus. Mouse buttons can either be  specified
;;            in  the  form button[1-9] or by using the name of the event code (ex
;;            BTN_LEFT or BTN_RIGHT). For the former option, the buttons  will  be
;;            mapped  to  their values in X11 (1=left, 2=middle, 3=right, 4=scroll
;;            up, 5=scroll down, 6=scroll left,  7=scroll  right,  8=back,  9=for‐
;;            ward). For the latter option, you can find the event names using li‐
;;            binput debug-events.

;;            The  priority  for  matching  bindings  is as follows: input device,
;;            group, and locked state.

;;            --whole-window, --border, and --exclude-titlebar are mouse-only  op‐
;;            tions  which  affect  the  region in which the mouse bindings can be
;;            triggered.  By default, mouse bindings are only triggered when  over
;;            the  title  bar.  With the --border option, the border of the window
;;            will be included in this region. With the --whole-window option, the
;;            cursor can be anywhere over a window including  the  title,  border,
;;            and  content. --exclude-titlebar can be used in conjunction with any
;;            other option to specify that the titlebar should  be  excluded  from
;;            the region of consideration.

;;            If  --whole-window  is  given, the command can be triggered when the
;;            cursor is over an empty workspace. Using  a  mouse  binding  over  a
;;            layer surface's exclusive region is not currently possible.

;;            Example:
;;                      # Execute firefox when alt, shift, and f are pressed together
;;                      bindsym Mod1+Shift+f exec firefox

;;            bindcode  [--whole-window]  [--border]  [--exclude-titlebar]  [--re‐
;;            lease] [--locked]  [--input-device=<device>]  [--no-warn]  [--no-re‐
;;            peat]  [--inhibited] [Group<1-4>+]<code> <command> is also available
;;            for binding with key/button codes instead of key/button names.

;;        bindswitch [--locked] [--no-warn] [--reload] <switch>:<state> <command>
;;            Binds <switch> to execute the sway command command on state changes.
;;            Supported switches are lid (laptop lid)  and  tablet  (tablet  mode)
;;            switches.  Valid  values  for  state  are  on, off and toggle. These
;;            switches are on when the device lid is shut and when tablet mode  is
;;            active  respectively. toggle is also supported to run a command both
;;            when the switch is toggled on or off.

;;            Unless the flag --locked is set, the command will not be run when  a
;;            screen  locking  program  is  active. If there is a matching binding
;;            with and without --locked, the  one  with  will  be  preferred  when
;;            locked and the one without will be preferred when unlocked.

;;            If  the  --reload  flag  is given, the binding will also be executed
;;            when the config is reloaded. toggle bindings will not be executed on
;;            reload. The --locked flag will operate as normal so if the config is
;;            reloaded while locked and --locked is not given,  the  binding  will
;;            not be executed.

;;            By  default,  if  you  overwrite  a binding, swaynag will give you a
;;            warning. To silence this, use the --no-warn flag.

;;            Example:
;;                      # Show the virtual keyboard when tablet mode is entered.
;;                      bindswitch tablet:on busctl call --user sm.puri.OSK0 /sm/puri/OSK0 sm.puri.OSK0 SetVisible b true

;;                      # Log a message when the laptop lid is opened or closed.
;;                      bindswitch lid:toggle exec echo "Lid moved"

;;        bindgesture  [--exact]   [--input-device=<device>]   [--no-warn]   <ges‐
;;        ture>[:<fingers>][:directions] <command>
;;            Binds  gesture  to  execute  the sway command command when detected.
;;            Currently supports the hold, pinch or swipe gesture. Optionally  can
;;            be limited to bind to a certain number of fingers or, for a pinch or
;;            swipe gesture, to certain directions.

;;        ┌───────┬─────────┬────────────────────────────────────────────────────┐
;;        │ type  │ fingers │ direction                                          │
;;        ├───────┼─────────┼────────────────────────────────────────────────────┤
;;        │ hold  │  1 - 5  │ none                                               │
;;        ├───────┼─────────┼────────────────────────────────────────────────────┤
;;        │ swipe │  3 - 5  │ up, down, left, right                              │
;;        ├───────┼─────────┼────────────────────────────────────────────────────┤
;;        │ pinch │  2 - 5  │ all  above  + inward, outward, clockwise, counter‐ │
;;        │       │         │ clockwise                                          │
;;        └───────┴─────────┴────────────────────────────────────────────────────┘

;;            The fingers can be limited to any sensible number or left  empty  to
;;            accept  any  finger  counts. Valid directions are up, down, left and
;;            right, as well as inward, outward, clockwise,  counterclockwise  for
;;            the pinch gesture. Multiple directions can be combined by a plus.

;;            If  a  input-device  is given, the binding will only be executed for
;;            that input device and will be executed instead of any  binding  that
;;            is  generic  to all devices. By default, if you overwrite a binding,
;;            swaynag will give you a warning. To silence this, use the  --no-warn
;;            flag.

;;            The  --exact  flag can be used to ensure a binding only matches when
;;            exactly all specified directions are matched and  nothing  more.  If
;;            there is matching binding with --exact, it will be preferred.

;;            The priority for matching bindings is as follows: input device, then
;;            exact  matches followed by matches with the highest number of match‐
;;            ing directions.

;;            Gestures executed while the pointer is above a bar are  not  handled
;;            by sway. See the respective documentation, e.g. bindgesture in sway-
;;            bar(5).

;;            Example:
;;                      # Allow switching between workspaces with left and right swipes
;;                      bindgesture swipe:right workspace prev
;;                      bindgesture swipe:left workspace next

;;                      # Allow container movements by pinching them
;;                      bindgesture pinch:inward+up move up
;;                      bindgesture pinch:inward+down move down
;;                      bindgesture pinch:inward+left move left
;;                      bindgesture pinch:inward+right move right

;;        client.background <color>
;;            This command is ignored and is only present for i3 compatibility.

;;        client.<class>  <border>  <background>  <text> [<indicator> [<child_bor‐
;;        der>]]
;;            Configures the color of window borders and  title  bars.  The  first
;;            three  colors  are  required. When omitted indicator will use a sane
;;            default and child_border will use the color set for background. Col‐
;;            ors may be specified in hex, either as #RRGGBB or #RRGGBBAA.

;;            The available classes are:

;;            client.focused
;;                The window that has focus.

;;            client.focused_inactive
;;                The most recently focused view within a container which  is  not
;;                focused.

;;            client.focused_tab_title
;;                A  view that has focused descendant container. Tab or stack con‐
;;                tainer title that is the parent of the focused container but  is
;;                not directly focused. Defaults to focused_inactive if not speci‐
;;                fied and does not use the indicator and child_border colors.

;;            client.placeholder
;;                Ignored (present for i3 compatibility).

;;            client.unfocused
;;                A view that does not have focus.

;;            client.urgent
;;                A view with an urgency hint. Note: Native Wayland windows do not
;;                support urgency. Urgency only works for Xwayland windows.

;;            The meaning of each color is:

;;            border
;;                The border around the title bar.

;;            background
;;                The background of the title bar.

;;            text
;;                The text color of the title bar.

;;            indicator
;;                The  color  used  to  indicate  where a new view will open. In a
;;                tiled container, this would paint the right border of  the  cur‐
;;                rent view if a new view would be opened to the right.

;;            child_border
;;                The border around the view itself.

;;        The default colors are:

;;        ┌───────────────┬─────────┬────────────┬─────────┬───────────┬────────────┐
;;        │     class     │ border  │ background │ text    │ indicator │ child_bor‐ │
;;        │               │         │            │         │           │ der        │
;;        ├───────────────┼─────────┼────────────┼─────────┼───────────┼────────────┤
;;        │ background    │ n/a     │ #ffffff    │ n/a     │ n/a       │ n/a        │
;;        ├───────────────┼─────────┼────────────┼─────────┼───────────┼────────────┤
;;        │ focused       │ #4c7899 │ #285577    │ #ffffff │ #2e9ef4   │ #285577    │
;;        ├───────────────┼─────────┼────────────┼─────────┼───────────┼────────────┤
;;        │ focused_in‐   │ #333333 │ #5f676a    │ #ffffff │ #484e50   │ #5f676a    │
;;        │ active        │         │            │         │           │            │
;;        ├───────────────┼─────────┼────────────┼─────────┼───────────┼────────────┤
;;        │ fo‐           │ #333333 │ #5f676a    │ #ffffff │ n/a       │ n/a        │
;;        │ cused_tab_ti‐ │         │            │         │           │            │
;;        │ tle           │         │            │         │           │            │
;;        ├───────────────┼─────────┼────────────┼─────────┼───────────┼────────────┤
;;        │ unfocused     │ #333333 │ #222222    │ #888888 │ #292d2e   │ #222222    │
;;        ├───────────────┼─────────┼────────────┼─────────┼───────────┼────────────┤
;;        │ urgent        │ #2f343a │ #900000    │ #ffffff │ #900000   │ #900000    │
;;        ├───────────────┼─────────┼────────────┼─────────┼───────────┼────────────┤
;;        │ placeholder   │ #000000 │ #0c0c0c    │ #ffffff │ #000000   │ #0c0c0c    │
;;        └───────────────┴─────────┴────────────┴─────────┴───────────┴────────────┘

;;        default_border normal|none|pixel [<n>]
;;            Set  default border style for new tiled windows. Config reload won't
;;            affect existing windows, only newly created ones after the reload.

;;        default_floating_border normal|none|pixel [<n>]
;;            Set default border style for new floating windows. This only applies
;;            to windows that are spawned in floating mode, not windows  that  be‐
;;            come floating afterwards.

;;        exec <shell command>
;;            Executes shell command with sh.

;;        exec_always <shell command>
;;            Like  exec,  but  the shell command will be executed again after re‐
;;            load.

;;        floating_maximum_size <width> x <height>
;;            Specifies the maximum size of floating windows. -1 x -1 removes  the
;;            upper  limit.  The  default  is  0 x 0, which will use the width and
;;            height of the entire output layout as the maximums

;;        floating_minimum_size <width> x <height>
;;            Specifies the minimum size of floating windows. The default is 75  x
;;            50.

;;        floating_modifier <modifier> [normal|inverse]
;;            When  the modifier key is held down, you may hold left click to move
;;            windows, and right click to resize them. Setting  modifier  to  none
;;            disables  this  feature. If inverse is specified, left click is used
;;            for resizing and right click for moving.

;;        focus_follows_mouse yes|no|always
;;            If set to yes, moving your mouse over a window will focus that  win‐
;;            dow.  If  set  to always, the window under the cursor will always be
;;            focused, even after switching between workspaces.

;;        focus_on_window_activation smart|urgent|focus|none
;;            This option determines what to do when a client requests window  ac‐
;;            tivation.  If  set  to urgent, the urgent state will be set for that
;;            window. If set to focus, the window will become focused. If  set  to
;;            smart, the window will become focused only if it is already visible,
;;            otherwise the urgent state will be set. Default is urgent.

;;        focus_wrapping yes|no|force|workspace
;;            This  option determines what to do when attempting to focus over the
;;            edge of a container. If set to no, the focused container will retain
;;            focus, if there are no other containers in the direction. If set  to
;;            yes, focus will be wrapped to the opposite edge of the container, if
;;            there are no other containers in the direction. If set to force, fo‐
;;            cus  will  be wrapped to the opposite edge of the container, even if
;;            there are other containers in the direction. If  set  to  workspace,
;;            focus will wrap like in the yes case and additionally wrap when mov‐
;;            ing outside of workspaces boundaries. Default is yes.

;;        font [pango:]<font>
;;            Sets  font  to  use  for the title bars. To enable support for pango
;;            markup, preface the font name with pango:. For example, monospace 10
;;            is the default font. To enable support for pango markup, pango:mono‐
;;            space 10 should be used instead. Regardless of whether pango  markup
;;            is  enabled,  font  should be specified as a pango font description.
;;            For   more   information   on   pango   font    descriptions,    see
;;            https://docs.gtk.org/Pango/type_func.FontDescrip‐
;;            tion.from_string.html#description

;;        force_display_urgency_hint <timeout> [ms]
;;            If an application on another workspace sets an urgency hint, switch‐
;;            ing  to  this  workspace may lead to immediate focus of the applica‐
;;            tion, which also means the window decoration color would be  immedi‐
;;            ately  reset  to client.focused. This may make it unnecessarily hard
;;            to tell which window originally raised the event. This option allows
;;            one to set a timeout in ms to delay the urgency hint reset.

;;        titlebar_border_thickness <thickness>
;;            Thickness of the titlebar border in pixels

;;        titlebar_padding <horizontal> [<vertical>]
;;            Padding of the text in the titlebar. horizontal value affects  hori‐
;;            zontal  padding  of  the  text while vertical value affects vertical
;;            padding (space above and below text). Padding includes titlebar bor‐
;;            ders so their value should be  greater  than  titlebar_border_thick‐
;;            ness. If vertical value is not specified it is set to the horizontal
;;            value.

;;        for_window <criteria> <command>
;;            Whenever  a  window  that matches criteria appears, run list of com‐
;;            mands. See CRITERIA for more details.

;;        gaps inner|outer|horizontal|vertical|top|right|bottom|left <amount>
;;            Sets default amount pixels of inner or outer gap,  where  the  inner
;;            affects  spacing  around  each  view  and  outer affects the spacing
;;            around each workspace. Outer gaps are in addition to inner gaps.  To
;;            reduce  or  remove  outer  gaps, outer gaps can be set to a negative
;;            value. outer gaps can also be specified per side  with  top,  right,
;;            bottom, and left or per direction with horizontal and vertical.

;;            This  affects  new  workspaces  only, and is used when the workspace
;;            doesn't have its own gaps settings (see: workspace <ws> gaps ...).

;;        hide_edge_borders             [--i3]              none|vertical|horizon‐
;;        tal|both|smart|smart_no_gaps
;;            Hides  window borders adjacent to the screen edges. Default is none.
;;            The --i3 option enables i3-compatible behavior to hide the title bar
;;            on   tabbed   and   stacked   containers   with   one   child.   The
;;            smart|smart_no_gaps  options are equivalent to setting smart_borders
;;            smart|no_gaps and hide_edge_borders none.

;;        input <input_device> <input-subcommands...>
;;            For details on input subcommands, see sway-input(5).

;;            * may be used in lieu of a specific device name to configure all in‐
;;            put devices. A list of  input  device  names  may  be  obtained  via
;;            swaymsg -t get_inputs.

;;        seat <seat> <seat-subcommands...>
;;            For details on seat subcommands, see sway-input(5).

;;        kill
;;            Kills  (closes) the currently focused container and all of its chil‐
;;            dren.

;;        smart_borders on|no_gaps|off
;;            If smart_borders are on, borders will only be enabled if  the  work‐
;;            space  has  more  than one visible child. If smart_borders is set to
;;            no_gaps, borders will only be enabled if the workspace has more than
;;            one visible child and gaps equal to zero.

;;        smart_gaps on|off|toggle|inverse_outer
;;            If smart_gaps are on gaps will only be enabled if  a  workspace  has
;;            more than one child. If smart_gaps are inverse_outer outer gaps will
;;            only be enabled if a workspace has exactly one child.

;;        mark --add|--replace [--toggle] <identifier>
;;            Marks are arbitrary labels that can be used to identify certain win‐
;;            dows and then jump to them at a later time. Each identifier can only
;;            be set on a single window at a time since they act as a unique iden‐
;;            tifier.  By default, mark sets identifier as the only mark on a win‐
;;            dow. --add will instead add identifier to the list of current  marks
;;            for  that  window. If --toggle is specified mark will remove identi‐
;;            fier if it is already marked.

;;        mode <mode>
;;            Switches to the specified mode. The default mode is default.

;;        mode [--pango_markup] <mode> <mode-subcommands...>
;;            The  only   valid   mode-subcommands...   are   bindsym,   bindcode,
;;            bindswitch,  and  set. If --pango_markup is given, then mode will be
;;            interpreted as pango markup.

;;        mouse_warping output|container|none
;;            If output is specified, the mouse will be moved to  new  outputs  as
;;            you  move  focus  between them. If container is specified, the mouse
;;            will be moved to the middle of the container on switch.  Default  is
;;            output.

;;        no_focus <criteria>
;;            Prevents  windows  matching  <criteria> from being focused automati‐
;;            cally when they're created. This has no effect on the  first  window
;;            in a workspace.

;;        output <output_name> <output-subcommands...>
;;            For details on output subcommands, see sway-output(5).

;;            *  may  be  used  in lieu of a specific output name to configure all
;;            outputs. A list of output names  may  be  obtained  via  swaymsg  -t
;;            get_outputs.

;;        popup_during_fullscreen smart|ignore|leave_fullscreen
;;            Determines  what  to  do  when  a fullscreen view opens a dialog. If
;;            smart (the default), the dialog will be displayed.  If  ignore,  the
;;            dialog will not be rendered. If leave_fullscreen, the view will exit
;;            fullscreen mode and the dialog will be rendered.

;;        primary_selection enabled|disabled
;;            Enable  or disable the primary selection clipboard. May only be con‐
;;            figured at launch. Default is enabled.

;;        set $<name> <value>
;;            Sets variable $name to value. You can use the new  variable  in  the
;;            arguments  of  future commands. When the variable is used, it can be
;;            escaped with an additional $ (ie $$name)  to  have  the  replacement
;;            happen  at  run time instead of when reading the config. However, it
;;            does not always make sense for the variable to be  replaced  at  run
;;            time since some arguments do need to be known at config time.

;;        show_marks yes|no
;;            If show_marks is yes, marks will be displayed in the window borders.
;;            Any  mark  that  starts with an underscore will not be drawn even if
;;            show_marks is yes. The default is yes.

;;        opacity [set|plus|minus] <value>
;;            Adjusts the opacity of the window between 0 (completely transparent)
;;            and 1 (completely opaque). If the operation is omitted, set will  be
;;            used.

;;        tiling_drag  enable|disable|toggle
;;            Sets whether or not tiling containers can be dragged with the mouse.
;;            If  enabled  (default), the floating_mod can be used to drag tiling,
;;            as well as floating, containers. Using the left mouse button on  ti‐
;;            tle  bars  without the floating_mod will also allow the container to
;;            be dragged. toggle should not be used in the config file.

;;        tiling_drag_threshold <threshold>
;;            Sets the threshold that must be  exceeded  for  a  container  to  be
;;            dragged  by its titlebar. This has no effect if floating_mod is used
;;            or if tiling_drag is set to disable.  Once the  threshold  has  been
;;            exceeded  once,  the drag starts and the cursor can come back inside
;;            the threshold without stopping the drag.  threshold is multiplied by
;;            the scale of the output that the cursor on.  The default is 9.

;;        title_align left|center|right
;;            Sets the title alignment. If right is selected and show_marks is set
;;            to yes, the marks will be shown on the  left  side  instead  of  the
;;            right side.

;;        unbindswitch <switch>:<state>
;;            Removes a binding for when <switch> changes to <state>.

;;        unbindgesture   [--exact]   [--input-device=<device>]   <gesture>[:<fin‐
;;        gers>][:directions]
;;            Removes a binding for the specified gesture, fingers and  directions
;;            combination.

;;        unbindsym  [--whole-window]  [--border] [--exclude-titlebar] [--release]
;;        [--locked] [--to-code] [--input-device=<device>] <key combo>
;;            Removes the binding for key combo that was previously bound with the
;;            given flags.  If input-device is given, only the  binding  for  that
;;            input device will be unbound.

;;            unbindcode  [--whole-window]  [--border] [--exclude-titlebar] [--re‐
;;            lease] [--locked] [--input-device=<device>] <code> is also available
;;            for unbinding with key/button codes instead of key/button names.

;;        unmark [<identifier>]
;;            unmark will remove identifier from the list of current  marks  on  a
;;            window. If identifier is omitted, all marks are removed.

;;        urgent enable|disable|allow|deny
;;            Using  enable or disable manually sets or unsets the window's urgent
;;            state. Using allow or deny controls the window's ability to set  it‐
;;            self as urgent. By default, windows are allowed to set their own ur‐
;;            gency.

;;        workspace [--no-auto-back-and-forth] [number] <[num:]name>
;;            Switches to the specified workspace. The num: portion of the name is
;;            optional  and  will  be  used for ordering. If num: is not given and
;;            name is a number, then it will be also be used for ordering.

;;            If the no-auto-back-and-forth option is  given,  then  this  command
;;            will  not  perform  a back-and-forth operation when the workspace is
;;            already focused and workspace_auto_back_and_forth is enabled.

;;            If the number keyword is specified and a workspace with  the  number
;;            already  exists, then the workspace with the number will be used. If
;;            a workspace with the number does not exist, a new workspace will  be
;;            created with the name name.

;;        workspace prev|next
;;            Switches  to the next workspace on the current output or on the next
;;            output if currently on the last workspace.

;;        workspace prev_on_output|next_on_output
;;            Switches to the next workspace on the current output.

;;        workspace back_and_forth
;;            Switches to the previously focused workspace.

;;        workspace  <name>  gaps   inner|outer|horizontal|vertical|top|right|bot‐
;;        tom|left <amount>
;;            Specifies  that  workspace  name should have the given gaps settings
;;            when it is created.

;;            This command does not affect existing workspaces. To alter the  gaps
;;            of an existing workspace, use the gaps command.

;;        workspace <name> output <outputs...>
;;            Specifies  that workspace name should be shown on the specified out‐
;;            puts. Multiple outputs can be listed and the first available will be
;;            used. If the workspace gets placed on an  output  further  down  the
;;            list and an output that is higher on the list becomes available, the
;;            workspace will be moved to the higher priority output.

;;            This  command does not affect existing workspaces. To move an exist‐
;;            ing workspace, use the move command in combination  with  the  work‐
;;            space  criteria (non-empty workspaces only) or workspace command (to
;;            switch to the workspace before moving).

;;        workspace_auto_back_and_forth yes|no
;;            When yes, repeating a workspace switch command will switch  back  to
;;            the  prior workspace. For example, if you are currently on workspace
;;            1, switch to workspace 2, then invoke the workspace 2 command again,
;;            you will be returned to workspace 1. Default is no.

;; CRITERIA
;;        A criteria is a string in the form of, for example:

;;            [class="[Rr]egex.*" title="some title"]

;;        The string contains one or more (space separated) attribute/value pairs.
;;        They are used by some commands to choose which views to execute  actions
;;        on. All attributes must match for the criteria to match. Criteria is re‐
;;        tained  across  commands  separated by a ,, but will be reset (and allow
;;        for new criteria, if desired) for commands separated by a ;.

;;        Criteria may be used with either the for_window or  assign  commands  to
;;        specify  operations to perform on new views. A criteria may also be used
;;        to perform specific commands (ones that normally act upon one window) on
;;        all views that match that criteria. For example:

;;        Focus on a window with the mark "IRC":

;;            [con_mark="IRC"] focus

;;        Kill all windows with the title "Emacs":

;;            [class="Emacs"] kill

;;        You may like to use swaymsg -t get_tree for finding the values of  these
;;        properties in practice for your applications.

;;        The following attributes may be matched with:

;;        all
;;            Matches all windows.

;;        app_id
;;            Compare  value  against  the app id. Can be a regular expression. If
;;            value is __focused__, then the app id must be the same  as  that  of
;;            the  currently focused window. app_id are specific to Wayland appli‐
;;            cations.

;;        class
;;            Compare value against the window class. Can be a regular expression.
;;            If value is __focused__, then the window class must be the  same  as
;;            that  of the currently focused window. class are specific to X11 ap‐
;;            plications and require XWayland.

;;        con_id
;;            Compare against the internal container ID, which you  can  find  via
;;            IPC.  If  value is __focused__, then the id must be the same as that
;;            of the currently focused window.

;;        con_mark
;;            Compare against the window marks. Can be a regular expression.

;;        floating
;;            Matches floating windows.

;;        id
;;            Compare value against the X11 window ID. Must be numeric. id is spe‐
;;            cific to X11 applications and requires XWayland.

;;        instance
;;            Compare value against the window instance. Can be a regular  expres‐
;;            sion.  If value is __focused__, then the window instance must be the
;;            same as that of the currently focused window. instance  is  specific
;;            to X11 applications and requires XWayland.

;;        pid
;;            Compare value against the window's process ID. Must be numeric.

;;        shell
;;            Compare  value  against  the  window  shell,  such as "xdg_shell" or
;;            "xwayland". Can be a regular expression. If  value  is  __focused__,
;;            then  the  shell  must  be the same as that of the currently focused
;;            window.

;;        tiling
;;            Matches tiling windows.

;;        title
;;            Compare against the window title. Can be a  regular  expression.  If
;;            value is __focused__, then the window title must be the same as that
;;            of the currently focused window.

;;        urgent
;;            Compares the urgent state of the window. Can be first, last, latest,
;;            newest, oldest or recent.

;;        window_role
;;            Compare  against  the window role (WM_WINDOW_ROLE). Can be a regular
;;            expression. If value is __focused__, then the window  role  must  be
;;            the  same  as  that  of the currently focused window. window_role is
;;            specific to X11 applications and requires XWayland.

;;        window_type
;;            Compare against the window type (_NET_WM_WINDOW_TYPE). Possible val‐
;;            ues are  normal,  dialog,  utility,  toolbar,  splash,  menu,  drop‐
;;            down_menu, popup_menu, tooltip and notification. window_type is spe‐
;;            cific to X11 applications and requires XWayland.

;;        workspace
;;            Compare  against  the workspace name for this view. Can be a regular
;;            expression. If the value is __focused__, then all the views  on  the
;;            currently focused workspace matches.

(define (sway-switch-workspace id)
  "switch to the workspace with the provided id.
Parameters:
    -	id
Response:
    An  array of objects corresponding to each command that was parsed. Each
    object has the property success."
  (dispatch-command
   (string-append "workspace number " (number->string id))))

;; (move-to-workspace 2)
;; (send-command 0 "focus left")
;; (send-command 0 "workspace number 3")
;; (send-command 0 "split vertical")
;; (send-command 0 "exec alacaritty")
