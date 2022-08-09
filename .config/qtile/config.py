import os
import subprocess

from libqtile import hook, layout
from libqtile.config import Match
from functions import Functions

# Local Files
from keys.keybindings import Mouse, Keybindings

from widgets import MyWidgets
from layouts import Layouts
from groups import CreateGroups

###### MAIN ######
if __name__ in ["config", "__main__"]:
    # Initializes objects

    # Initializes keybindings
    obj_keys = Keybindings()

    # Mouse
    obj_mouse = Mouse()
    obj_widgets = MyWidgets()
    obj_layouts = Layouts()
    obj_groups = CreateGroups()

    # Initializes qtile variables
    keys = obj_keys.init_keys()
    mouse = obj_mouse.init_mouse()
    layouts = obj_layouts.init_layouts()
    groups = obj_groups.init_groups()

    # Append group keys for groups
    # ks = [
    #     "1",
    #     "2",
    #     "3",
    #     "4",
    #     "5",
    #     "6",
    #     "7",
    #     "8",
    #     "9",
    #     "11",
    #     "12",
    #     "13",
    #     "14",
    #     "15",
    #     "16",
    #     "17",
    #     "18",
    #     "19",
    #     "21",
    #     "22",
    #     "23",
    #     "24",
    #     "25",
    #     "26",
    #     "27",
    #     "28",
    #     "29",
    # ]
    # # keys += obj_keys.init_keys_groups(ks)

    ### DISPLAYS WIDGETS IN THE SCREEN ####

    screens = obj_widgets.init_screen()

dgroups_key_binder = None
dgroups_app_rules = []
follow_mouse_focus = False
bring_front_click = True
cursor_warp = False

floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="dialog"),  # Dialogs stuff
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
    ]
)
auto_fullscreen = True
focus_on_window_activation = "smart"
# reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
respect_minimize_requests = True

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
wmname = "LG3D"


@hook.subscribe.startup_once
def start_once():
    home = os.path.expanduser("~")
    subprocess.call([home + "/.config/qtile/scripts/autostart.sh"])


@hook.subscribe.client_new
def dialogs(window):
    if window.window.get_wm_type() == "dialog" or window.window.get_wm_transient_for():
        window.floating = True


@hook.subscribe.changegroup
def group_changed():
    Functions.span_groups()


@hook.subscribe.current_screen_change
def screen_changed():
    Functions.screen_changed()


@hook.subscribe.focus_change
def focus_changed():
    Functions.focus_changed()
