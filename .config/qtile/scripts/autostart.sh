#!/usr/bin/env bash
# ---
# Use "run program" to run it only if it is not already running
# Use "program &" to run it regardless
# ---
# NOTE: This script runs with every restart of the window manager
# TODO: run_once


function run {
    if ! pgrep $1 > /dev/null ;
    then
        $@&
    fi
}

# Set keyboard repeat rate.
xset r rate 300 150
run xsettingsd
run picom -b --experimental-backends --dbus
~/.fehbg
# run picom -CGb &
# run nitrogen --restore &
# run xfce4-clipman
# run gammy
# run dunst
xrandr --output DP-1 --mode 1920x1080 --rate 144.00 --output DP-2 --mode 1920x1080 --rate 144.00 --right-of DP-1 --output HDMI-2 --mode 1920x1080 --rate 120.00 --right-of DP-2
~/.config/polybar/launch.sh
