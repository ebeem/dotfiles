from libqtile import bar, widget
from libqtile.config import Screen

# widget_defaults = dict(
#     font="Ubuntu Mono",
#     fontsize = 12,
#     padding = 2,
#     background=colors[2]
# )

# extension_defaults = widget_defaults.copy()


class MyWidgets:
    def __init__(self):
        pass

    def init_screen(self):
        """
        Init the widgets in the screen
        """
        return [
            Screen(
                top=bar.Gap(30),
            ),
            Screen(),
            Screen(),
        ]
