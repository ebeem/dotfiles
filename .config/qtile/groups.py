from libqtile.config import Group, Match

ROWS = 3
COLUMNS = 3


class CreateGroups:
    def init_groups(self):
        """
        Return the groups of Qtile
        """
        groups = [
            Group(
                "01-1",
                # matches=[Match(wm_class=["Firefox"])],
                exclusive=False,
                spawn=["firefox"],
                layout="bsp",
                persist=True,
                init=True,
                layout_opts=None,
                screen_affinity=1,
                label="Browsing ",
            ),
            Group(
                "01-2",
                # matches=[],
                exclusive=False,
                spawn=["emacs"],
                layout="bsp",
                persist=True,
                init=True,
                layout_opts=None,
                screen_affinity=2,
                label="Dotfiles ",
            ),
            Group(
                "01-3",
                # matches=[],
                exclusive=False,
                spawn=[],
                layout="bsp",
                persist=True,
                init=True,
                layout_opts=None,
                screen_affinity=3,
                label="Terminal ",
            ),
            Group(
                "02-1",
                # matches=[],
                exclusive=False,
                spawn=[],
                layout="bsp",
                persist=True,
                init=True,
                layout_opts=None,
                screen_affinity=1,
                label="Development ",
            ),
            Group(
                "02-2",
                # matches=[],
                exclusive=False,
                spawn=[],
                layout="bsp",
                persist=True,
                init=True,
                layout_opts=None,
                screen_affinity=2,
                label="Development ",
            ),
            Group(
                "02-3",
                # matches=[],
                exclusive=False,
                spawn=[],
                layout="bsp",
                persist=True,
                init=True,
                layout_opts=None,
                screen_affinity=3,
                label="Development ",
            ),
            Group(
                "03-1",
                # matches=[],
                exclusive=False,
                spawn=[],
                layout="bsp",
                persist=True,
                init=True,
                layout_opts=None,
                screen_affinity=1,
                label="Databases ",
            ),
            Group(
                "03-2",
                # matches=[],
                exclusive=False,
                spawn=[],
                layout="bsp",
                persist=True,
                init=True,
                layout_opts=None,
                screen_affinity=2,
                label="Rest Client ",
            ),
            Group(
                "03-3",
                # matches=[],
                exclusive=False,
                spawn=[],
                layout="bsp",
                persist=True,
                init=True,
                layout_opts=None,
                screen_affinity=3,
                label="Databases ",
            ),
            Group(
                "04-1",
                # matches=[Match(wm_class=["Discord"])],
                exclusive=False,
                spawn=["discord"],
                layout="bsp",
                persist=True,
                init=True,
                layout_opts=None,
                screen_affinity=1,
                label="Discord ",
            ),
            Group(
                "04-2",
                # matches=[],
                exclusive=False,
                spawn=["chromium 'https://web.whatsapp.com/'"],
                layout="bsp",
                persist=True,
                init=True,
                layout_opts=None,
                screen_affinity=2,
                label="Whatsapp ",
            ),
            Group(
                "04-3",
                # matches=[Match(wm_class=["Slack"])],
                exclusive=False,
                spawn=["slack"],
                layout="bsp",
                persist=True,
                init=True,
                layout_opts=None,
                screen_affinity=3,
                label="Slack ",
            ),
            Group(
                "05-1",
                # matches=[],
                exclusive=False,
                spawn=[],
                layout="bsp",
                persist=True,
                init=True,
                layout_opts=None,
                screen_affinity=1,
                label="Development ",
            ),
            Group(
                "05-2",
                # matches=[],
                exclusive=False,
                spawn=[],
                layout="bsp",
                persist=True,
                init=True,
                layout_opts=None,
                screen_affinity=2,
                label="Development ",
            ),
            Group(
                "05-3",
                # matches=[],
                exclusive=False,
                spawn=[],
                layout="bsp",
                persist=True,
                init=True,
                layout_opts=None,
                screen_affinity=3,
                label="Development ",
            ),
            Group(
                "06-1",
                # matches=[],
                exclusive=False,
                spawn=[],
                layout="bsp",
                persist=True,
                init=True,
                layout_opts=None,
                screen_affinity=1,
                label="Gaming ",
            ),
            Group(
                "06-2",
                matches=[Match(wm_class=["Steam"])],
                exclusive=False,
                spawn=["steam-runtime"],
                layout="bsp",
                persist=True,
                init=True,
                layout_opts=None,
                screen_affinity=2,
                label="Steam ",
            ),
            Group(
                "06-3",
                # matches=[],
                exclusive=False,
                spawn=[],
                layout="bsp",
                persist=True,
                init=True,
                layout_opts=None,
                screen_affinity=3,
                label="Steam Social ",
            ),
            Group(
                "07-1",
                # matches=[Match(wm_class=["Liferea"])],
                exclusive=False,
                spawn=["gtk-launch 'elfeed'"],
                layout="bsp",
                persist=True,
                init=True,
                layout_opts=None,
                screen_affinity=1,
                label="RSS ",
            ),
            Group(
                "07-2",
                # matches=[
                #     Match(wm_class=["Thunderbird"]),
                #     Match(wm_class=["Evolution"]),
                # ],
                exclusive=False,
                spawn=["thunderbird"],
                layout="bsp",
                persist=True,
                init=True,
                layout_opts=None,
                screen_affinity=2,
                label="Email ",
            ),
            Group(
                "07-3",
                # matches=[Match(wm_class=["Element"])],
                exclusive=False,
                spawn=["element-desktop"],
                layout="bsp",
                persist=True,
                init=True,
                layout_opts=None,
                screen_affinity=3,
                label="Chat ",
            ),
            Group(
                "08-1",
                # matches=[],
                exclusive=False,
                spawn=[],
                layout="bsp",
                persist=True,
                init=True,
                layout_opts=None,
                screen_affinity=1,
                label="Development ",
            ),
            Group(
                "08-2",
                # matches=[],
                exclusive=False,
                spawn=[],
                layout="bsp",
                persist=True,
                init=True,
                layout_opts=None,
                screen_affinity=2,
                label="Development ",
            ),
            Group(
                "08-3",
                # matches=[],
                exclusive=False,
                spawn=[],
                layout="bsp",
                persist=True,
                init=True,
                layout_opts=None,
                screen_affinity=3,
                label="Development ",
            ),
            Group(
                "09-1",
                # matches=[],
                exclusive=False,
                spawn=[],
                layout="bsp",
                persist=True,
                init=True,
                layout_opts=None,
                screen_affinity=1,
                label="Media ",
            ),
            Group(
                "09-2",
                # matches=[],
                exclusive=False,
                spawn=["ckb-next"],
                layout="bsp",
                persist=True,
                init=True,
                layout_opts=None,
                screen_affinity=2,
                label="Development ",
            ),
            Group(
                "09-3",
                # matches=[],
                exclusive=False,
                spawn=[],
                layout="bsp",
                persist=True,
                init=True,
                layout_opts=None,
                screen_affinity=3,
                label="Development ",
            ),
        ]
        return groups
