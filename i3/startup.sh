#!/bin/bash

# Additional key mapping for alt gr to also be a super modifer
xmodmap -e "keycode 108 = Super_R"
xmodmap -e "keycode 92 = Super_R"

# Screen resolution and layout
xrandr --output DP-3 --primary --mode 1920x1080 --pos 1280x0 --rotate normal --output DP-1 --mode 1280x1024 --pos 0x232 --rotate normal --output eDP-1 --mode 1920x1080 --pos 3200x312 --rotate normal --output DP-2 --off

# Gnome keyring
gnome-keyring-daemon --start --components=pkcs11,secrets,ssh,gpg

# Wallpaper
feh --randomize --bg-fill ~/Pictures/wallpapers/
