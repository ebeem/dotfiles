#!/usr/bin/env python3

import gi
gi.require_version('Gtk', '3.0')
from gi.repository import Gtk, Gdk
import cairo

supports_alpha = True

def screen_changed(widget, old_screen, userdata=None):
    global supports_alpha

    screen = widget.get_screen()
    visual = screen.get_rgba_visual()

    if visual is None:
        visual = screen.get_system_visual()
        supports_alpha = False
    else:
        supports_alpha = True

    widget.set_visual(visual)

def expose_draw(widget, event, userdata=None):
    global supports_alpha

    cr = Gdk.cairo_create(widget.get_window())

    if supports_alpha:
        cr.set_source_rgba(1.0, 1.0, 1.0, 0.0)
    else:
        cr.set_source_rgb(1.0, 1.0, 1.0)

    cr.set_operator(cairo.OPERATOR_SOURCE)
    cr.paint()

    return False

if __name__ == "__main__":
    window = Gtk.Window()
    window.set_title("Sway Transperent Window")
    window.connect("delete-event", Gtk.main_quit)
    window.set_app_paintable(True)
    window.connect("draw", expose_draw)
    window.connect("screen-changed", screen_changed)
    window.set_decorated(False)
    window.add_events(Gdk.EventMask.BUTTON_PRESS_MASK)

    window.show_all()
    Gtk.main()
