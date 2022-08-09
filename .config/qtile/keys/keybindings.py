from libqtile.config import Click, Drag, Key
from libqtile.lazy import lazy

# Import the function that move the window to the next and prev group
from functions import Functions
import config_keybindings as ck


class Keybindings:
    keys = []
    spawn_keys = ck.SPAWN_KEYS
    cmd_keys = ck.SPAWN_CMD_KEYS

    def create_layout_keys(self):
        ############   BINDINGS FOR MONADTALL   ##############
        modifier = [ck.MOVEMENT_KEY]
        layout_left = Key(modifier, ck.LEFT, Functions.switch_focus(0))
        layout_right = Key(modifier, ck.RIGHT, Functions.switch_focus(3))
        # layout_left = Key(modifier, ck.LEFT, lazy.layout.left())
        # layout_right = Key(modifier, ck.RIGHT, lazy.layout.right())
        layout_down = Key(modifier, ck.DOWN, lazy.layout.down())
        layout_up = Key(modifier, ck.UP, lazy.layout.up())
        toogle_layout = Key(modifier, ck.TOOGLE_LAYOUT, lazy.next_layout())
        self.keys += [layout_left, layout_right, layout_down, layout_up, toogle_layout]

    def create_shuffle_keys(self):
        modifier = [ck.MOVEMENT_KEY, ck.ALT]

        left = Key(modifier, ck.SWAP_LEFT, lazy.layout.shuffle_left())
        right = Key(modifier, ck.SWAP_RIGHT, lazy.layout.shuffle_right())
        down = Key(modifier, ck.SWAP_DOWN, lazy.layout.shuffle_down())
        up = Key(modifier, ck.SWAP_UP, lazy.layout.shuffle_up())

        flip = Key(modifier, ck.SWAP_FLIP, lazy.layout.flip())
        self.keys += [left, right, down, up]

    def create_swap_keys(self):
        modifier = [ck.MOVEMENT_KEY, ck.SWAP_KEY]

        left = Key(modifier, ck.SWAP_LEFT, Functions.move_window_to_screen(0))
        right = Key(modifier, ck.SWAP_RIGHT, Functions.move_window_to_screen(3))
        self.keys += [left, right]

    def create_windows_keys(self):
        modifier = [ck.MOVEMENT_KEY]

        grow = Key(modifier, ck.GROW, lazy.layout.grow())
        shrink = Key(modifier, ck.SHRINK, lazy.layout.shrink())
        normalize = Key(modifier, ck.NORMALIZE, lazy.layout.normalize())
        maximize = Key(modifier, ck.MAXIMIZE, lazy.layout.maximize())

        self.keys += [grow, shrink, normalize, maximize]

    def create_shutdown_keys(self):
        shutdown = Key(ck.SHUTDOWN_MODIFIER, ck.SHUTDOWN, lazy.shutdown())
        restart = Key(ck.SHUTDOWN_MODIFIER, ck.RESTART, lazy.restart())

        self.keys += [shutdown, restart]

    def create_kill_keys(self):
        modifier = [ck.MOVEMENT_KEY, ck.ALTGR]

        all_minus_current = Key(
            modifier,
            ck.KILL_ALL_MINUS_CURRENT,
            Functions.kill_all_windows_minus_current(),
        )
        all_ = Key(modifier, ck.KILL_ALL, Functions.kill_all_windows())
        current = Key([ck.KILL_KEY], ck.KILL_CURRENT, lazy.window.kill())

        self.keys += [all_minus_current, all_, current]

    def create_floating_keys(self):
        modifier = [ck.MOVEMENT_KEY, ck.FLOATING_KEY]

        floating = Key(modifier, ck.TOOGLE_FLOATING, lazy.window.toggle_floating())
        full = Key(modifier, ck.TOOGLE_FULL, lazy.window.toggle_fullscreen())

        self.keys += [floating, full]

    def create_groups_keys(self):
        modifier = ck.SWITCH_GROUP_MODIFIER
        move_modifier = ck.MOVE_GROUP_MODIFIER

        switch_group_left = Key(
            modifier, ck.LEFT, Functions.switch_to_group_direction(0)
        )
        switch_group_down = Key(
            modifier, ck.DOWN, Functions.switch_to_group_direction(1)
        )
        switch_group_up = Key(modifier, ck.UP, Functions.switch_to_group_direction(2))
        switch_group_right = Key(
            modifier, ck.RIGHT, Functions.switch_to_group_direction(3)
        )

        move_group_left = Key(
            move_modifier, ck.LEFT, Functions.move_to_group_direction(0)
        )
        move_group_down = Key(
            move_modifier, ck.DOWN, Functions.move_to_group_direction(1)
        )
        move_group_up = Key(move_modifier, ck.UP, Functions.move_to_group_direction(2))
        move_group_right = Key(
            move_modifier, ck.RIGHT, Functions.move_to_group_direction(3)
        )

        self.keys += [
            switch_group_left,
            switch_group_down,
            switch_group_up,
            switch_group_right,
            move_group_left,
            move_group_down,
            move_group_up,
            move_group_right,
        ]

    def create_spawn_keys(self):
        for spawn_key in self.spawn_keys:
            modifier, key, command = spawn_key
            keybinding = Key(modifier, key, lazy.spawn(command))
            self.keys.append(keybinding)

    def create_cmd_keys(self):
        for cmd_key in self.cmd_keys:
            modifier, key, command = cmd_key
            keybinding = Key(modifier, key, lazy.spawncmd(command))
            self.keys.append(keybinding)

    def init_keys_groups(self, group_names):
        """
        Create bindings to move between groups
        """
        group_keys = []
        for icon in group_names:
            index = (icon[0]).lower()

            group_keys += [
                Key(
                    [ck.MOVEMENT_KEY, ck.GROUPS_KEY], index, lazy.group[icon].toscreen()
                ),
                Key(
                    [ck.MOVEMENT_KEY, ck.SWAP_GROUP_KEY],
                    index,
                    lazy.window.togroup(icon, switch_group=True),
                ),
            ]

        return group_keys

    def init_keys(self):
        self.create_layout_keys()
        self.create_swap_keys()
        self.create_shuffle_keys()
        self.create_windows_keys()
        self.create_shutdown_keys()
        self.create_kill_keys()
        self.create_floating_keys()
        self.create_groups_keys()

        # self.create_cmd_keys()
        self.create_spawn_keys()

        return self.keys


class Mouse:
    def __init__(self, mod_key=ck.MOD):
        self.mod = mod_key

    def init_mouse(self):
        mouse = [
            Drag(
                [self.mod],
                "Button1",
                lazy.window.set_position_floating(),
                start=lazy.window.get_position(),
            ),
            Drag(
                [self.mod],
                "Button3",
                lazy.window.set_size_floating(),
                start=lazy.window.get_size(),
            ),
            Click([self.mod], "Button2", lazy.window.bring_to_front()),
        ]
        return mouse
