"""
MODIFY THIS FILE TO CREATE CUSTOM KEYBINDINGS

Keybindings are configured with tuples, inside Predifined lists Variables

Modifier -> list() -> Ex: [MOD, CONTROL]

Key -> str() -> Ex: 'j'

Command -> str() -> Ex: vscode

(Modifier, Key, Command)
"""

from functions import PWA
from os.path import expanduser

HOME = expanduser("~")

# Keys
MOD = "mod4"
ALT = "mod1"
ALTGR = "mod5"
SHIFT = "shift"
CONTROL = "control"

# Basic wm bindings

# All of these variables include the MOVEMENT_KEYS at the start

# The key which the WM will use to move the layouts
MOVEMENT_KEY = MOD
KILL_KEY = MOD

SWAP_KEY = SHIFT
FLOATING_KEY = SHIFT

############   BINDINGS FOR MONADTALL   ##############
# Move between windows
LEFT = "h"
RIGHT = "l"
DOWN = "j"
UP = "k"

# Swap windows
SWAP_LEFT = "h"
SWAP_RIGHT = "l"
SWAP_DOWN = "j"
SWAP_UP = "k"

SWAP_FLIP = "space"  # Flip the layout

###########         LAYOUTS               ###############
# Change windows lenght
GROW = "i"
SHRINK = "m"
NORMALIZE = "n"
MAXIMIZE = "o"

# Floating layout
TOOGLE_FLOATING = "f"
TOOGLE_FULL = "g"

# Groups key
# Move screen to next and previous group
NEXT = "k"
PREVIOUS = "j"

# Kill Functions
KILL_CURRENT = "w"
KILL_ALL = "x"
KILL_ALL_MINUS_CURRENT = "c"

# Rotates layouts

TOOGLE_LAYOUT = "Tab"


# Define constants here
# TERMINAL = "alacritty"
TERMINAL = "emacsclient -c -e '(+eshell/here nil)')"
# Basic window manager movements
# Qtile shutdown/restart keys
SHUTDOWN_MODIFIER = [MOD, CONTROL]
RESTART = "r"
SHUTDOWN = "q"

# Group movement keys:
SWITCH_GROUP_MODIFIER = [MOD, CONTROL]
MOVE_GROUP_MODIFIER = [MOD, CONTROL, ALT]

NEXT_GROUP = "period"
PREV_GROUP = "comma"

# ------------ Hardware Configs ------------
HARDWARE_KEYS = [
    # (Modifier, Key, Command)
    # Volume
    ([], "XF86AudioLowerVolume", "pactl set-sink-volume @DEFAULT_SINK@ -5%"),
    ([], "XF86AudioRaiseVolume", "pactl set-sink-volume @DEFAULT_SINK@ +5%"),
    ([], "XF86AudioMute", "pactl set-sink-mute @DEFAULT_SINK@ toggle"),
    ([], "XF86AudioNext", "mpc next"),
    ([], "XF86AudioPrev", "mpc prev"),
    ([], "XF86AudioPlay", "mpc toggle"),
    # Brightness
    ([], "XF86MonBrightnessUp", "brightnessctl set +10%"),
    ([], "XF86MonBrightnessDown", "brightnessctl set 10%-"),
]

APPS = [
    ([MOD], "Return", TERMINAL),
    # (Modifier, Key, Command)
    # ([MOD], "e", "thunar"),
    # ([MOD, ALT], "d", "emacs"),
    # ([MOD, ALT], "o", "env LIBGL_ALWAYS_SOFTWARE=1 obs"),
    # ([MOD, ALT], "v", "gvim"),
    # ([MOD, ALT], "b", "brave"),
    # ([MOD, ALT], "c", "code"),
    # ([MOD, ALT], "p", "pycharm"),
    # ([MOD, ALT], "a", "pavucontrol"),
    # ([MOD, ALT], "e", "vim -g .config/qtile/config.py"),
    # ([MOD, ALT], "z", "zoom"),
    # # Media hotkeys
    # ([MOD], "Up", "pulseaudio-ctl up 5"),
    # ([MOD], "Down", "pulseaudio-ctl down 5"),
    # # Makes reference to play-pause script
    # # You can find it in my scripts repository
    # ([ALTGR], "space", "play-pause"),
    # Run "rofi-theme-selector" in terminal to select a theme
    ([MOD], "space", "rofi -show drun"),
    # Screenshots
    ([], "Print", "flameshot gui"),
    ([ALT], "Print", "flameshot gui"),
    # Terminal apps
    ([MOD, ALT], "n", TERMINAL + " -e nvim"),
]

##########################
# Your custom keys here  #
##########################

CUSTOM_SPAWN_KEYS = [
    # PWA keys
    # ([MOD, ALT], "s", PWA.spotify()),
    # ([MOD, ALT], "m", PWA.music()),
    # ([MOD, ALT], "t", PWA.calendar()),
    # ([MOD, ALT], "y", PWA.youtube()),
    # ([MOD, ALT], "l", PWA.notion()),
    # ([MOD, ALT], "h", PWA.habitica()),
]

SPAWN_KEYS = HARDWARE_KEYS + APPS + CUSTOM_SPAWN_KEYS

SPAWN_CMD_KEYS = [
    # Takes full screenshot and creates a file on the screenshot folder
    # ([SHIFT], "Print", f"xfce4-screenshooter -f -s {HOME}/Pictures/Screenshots/"),
    (
        [MOD, CONTROL],
        "space",
        ".config/qtile/scripts/switch_keyboard_layout.sh",
    ),
]
