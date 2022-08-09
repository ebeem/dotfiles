from libqtile.command import lazy
from groups import ROWS, COLUMNS
from libqtile.core.manager import Qtile
from libqtile.hook import fire
from libqtile import qtile as q
from libqtile.log_utils import logger

# from libqtile.command_client import InteractiveCommandClient


class Functions:
    @staticmethod
    def window_to_prev_group():
        @lazy.function
        def __inner(qtile):
            i = qtile.groups.index(qtile.current_group)

            if qtile.current_window and i != 0:
                group = qtile.groups[i - 1].name
                qtile.current_window.togroup(group, switch_group=True)

        return __inner

    @staticmethod
    def switch_focus(direction: int):
        @lazy.function
        def __inner(qtile: Qtile):
            group_i = qtile.current_group.name.split("-")[0]
            windows = [
                w
                for w in qtile.windows_map.values()
                if w.group.name.split("-")[0] == group_i
            ]
            # 0=left, 1=bottom, 2=top, 3=right
            current_focus = qtile.current_window
            if current_focus is None:
                current_focus = qtile.current_screen

            current_x, current_y = current_focus.x, current_focus.y
            target = None
            for w in windows:
                if w != current_focus:
                    if (
                        direction == 0
                        # and w.y >= current_y
                        and w.x < current_x
                        and (target is None or w.x > target.x)
                    ):
                        target = w
                    if (
                        direction == 3
                        # and w.y >= current_y
                        and w.x > current_x
                        and (target is None or w.x < target.x)
                    ):
                        target = w
                    if (
                        direction == 1
                        and w.x >= current_x
                        and w.y < current_y
                        and (target is None or w.y > target.y)
                    ):
                        target = w
                    if (
                        direction == 2
                        and w.x >= current_x
                        and w.y > current_y
                        and (target is None or w.y < target.y)
                    ):
                        target = w

            if target is not None:
                target.group.focus(target)
                qtile.focus_screen(target.group.screen.index)

        return __inner

    @staticmethod
    def move_window_to_screen(direction: int):
        @lazy.function
        def __inner(qtile: Qtile):
            so = int(qtile.current_group.name.split("-")[1]) - 1
            i = int(qtile.current_group.name.split("-")[0]) - 1

            # 0=left, 1=bottom, 2=top, 3=right
            current_focus = qtile.current_window
            current_x = qtile.current_screen.x
            target_screen = None
            if current_focus is not None:
                if direction == 0:
                    for s in qtile.screens:
                        if s.x < current_x and (
                            target_screen is None or s.x > target_screen.x
                        ):
                            target_screen = s
                if direction == 3:
                    for s in qtile.screens:
                        if s.x > current_x and (
                            target_screen is None or s.x < target_screen.x
                        ):
                            target_screen = s
                if target_screen is not None:
                    target_group = (
                        "{0:0>2}".format(i + 1)
                        + "-"
                        + "{0:0>1}".format(target_screen.index + 1)
                    )
                    current_focus.togroup(target_group)
                    current_focus.group.focus(current_focus)
                    qtile.focus_screen(current_focus.group.screen.index)

        return __inner

    screen_focus = [0 for s in range(ROWS * COLUMNS)]

    @staticmethod
    def span_groups():
        qtile = q
        if not hasattr(qtile, "current_screen"):
            return

        so = int(qtile.current_group.name.split("-")[1]) - 1
        i = int(qtile.current_group.name.split("-")[0]) - 1
        for s in range(len(qtile.screens)):
            target = [
                x
                for x in qtile.groups
                if x.name == "{0:0>2}".format(i + 1) + "-" + "{0:0>1}".format(s + 1)
            ]

            if len(target) > 0:
                qtile.screens[s].set_group(target[0], warp=False)

        # qtile.focus_screen(Functions.screen_focus[i])

    @staticmethod
    def screen_changed():
        qtile = q
        if not hasattr(qtile, "current_screen"):
            return

        so = int(qtile.current_group.name.split("-")[1]) - 1
        i = int(qtile.current_group.name.split("-")[0]) - 1
        # logger.warning("setting focused screen to " + str(so))
        Functions.screen_focus[i] = so

    @staticmethod
    def focus_changed():
        qtile = q
        if not hasattr(qtile, "current_window") or not hasattr(
            qtile.current_window, "name"
        ):
            return
        # logger.warning("focus changed to " + qtile.current_window.name)

    @staticmethod
    def switch_to_group_direction(direction: int, warp=False):
        @lazy.function
        def __inner(qtile: Qtile):
            so = int(qtile.current_group.name.split("-")[1]) - 1
            i = int(qtile.current_group.name.split("-")[0]) - 1
            # 0=left, 1=bottom, 2=top, 3=right
            current_row, current_column = i // COLUMNS, i % COLUMNS
            if direction == 0 or direction == 3:
                current_column += -1 if direction == 0 else 1
                current_column = (
                    current_column + ROWS
                    if current_column < 0
                    else current_column % ROWS
                )
                current_column %= ROWS
            elif direction == 1 or direction == 2:
                current_row += -1 if direction == 2 else 1
                current_row = (
                    current_row + COLUMNS if current_row < 0 else current_row % COLUMNS
                )
            new_index = current_row * COLUMNS + current_column
            target = [
                x
                for x in qtile.groups
                if x.name
                == "{0:0>2}".format(new_index + 1)
                + "-"
                + "{0:0>1}".format(Functions.screen_focus[new_index] + 1)
            ]

            if len(target) > 0:
                qtile.screens[Functions.screen_focus[new_index]].set_group(target[0])
                qtile.focus_screen(Functions.screen_focus[new_index])
                fire("changegroup")

        return __inner

    @staticmethod
    def move_to_group_direction(direction: int):
        @lazy.function
        def __inner(qtile: Qtile):
            s = int(qtile.current_group.name.split("-")[1]) - 1
            i = int(qtile.current_group.name.split("-")[0]) - 1
            # 0=left, 1=bottom, 2=top, 3=right
            current_row, current_column = i // COLUMNS, i % COLUMNS
            if direction == 0 or direction == 3:
                current_column += -1 if direction == 0 else 1
                current_column = (
                    current_column + ROWS
                    if current_column < 0
                    else current_column % ROWS
                )
                current_column %= ROWS
            elif direction == 1 or direction == 2:
                current_row += -1 if direction == 2 else 1
                current_row = (
                    current_row + COLUMNS if current_row < 0 else current_row % COLUMNS
                )
            new_index = current_row * COLUMNS + current_column
            target = [
                x
                for x in qtile.groups
                if x.name
                == "{0:0>2}".format(new_index + 1) + "-" + "{0:0>1}".format(s + 1)
            ]

            if qtile.current_window and len(target) > 0:
                qtile.current_window.togroup(target[0].name, switch_group=True)
                fire("changegroup")

        return __inner

    @staticmethod
    def window_to_next_group():
        @lazy.function
        def __inner(qtile):
            i = qtile.groups.index(qtile.current_group)

            if qtile.current_window and i != len(qtile.groups):
                group = qtile.groups[i + 1].name
                qtile.current_window.togroup(group, switch_group=True)

        return __inner

    ##### KILL ALL WINDOWS #####

    @staticmethod
    def kill_all_windows():
        @lazy.function
        def __inner(qtile):
            for window in qtile.current_group.windows:
                window.kill()

        return __inner

    @staticmethod
    def kill_all_windows_minus_current():
        @lazy.function
        def __inner(qtile):
            for window in qtile.current_group.windows:
                if window != qtile.current_window:
                    window.kill()

        return __inner


class PWA:
    def __init__(self):
        pass

    @staticmethod
    def notion():
        return "brave --profile-directory=Default --app=https://notion.so"

    @staticmethod
    def music():
        return "brave --profile-directory=Default --app=https://music.youtube.com/"

    @staticmethod
    def spotify():
        return "brave --profile-directory=Default --app=https://open.spotify.com/"

    @staticmethod
    def youtube():
        return "brave --user-data-dir=Default --app=https://www.youtube.com"

    @staticmethod
    def calendar():
        return "brave --profile-directory=Default --app=https://calendar.google.com/calendar/"

    @staticmethod
    def habitica():
        return "brave --profile-directory=Default --app=https://habitica.com/"


if __name__ == "__main__":
    print("This is an utilities module")
