#!/bin/bash

# Additional key mapping for alt gr to also be a super modifer
xmodmap -e "keycode 108 = Super_R"
xmodmap -e "keycode 92 = Super_R"

# Screen resolution and layout
xrandr --output DP-0 --off --output DP-1 --mode 1920x1080 --pos 3200x0 --rotate left --output DP-2 --off --output DP-3 --off --output DP-4 --off --output DP-5 --mode 1920x1080 --pos 1280x0 --rotate normal --output DP-6 --off --output DP-7 --mode 1280x1024 --pos 0x56 --rotate normal

# Gnome keyring
gnome-keyring-daemon --start --components=pkcs11,secrets,ssh,gpg

# Wallpaper
feh --randomize --bg-fill ~/Pictures/planet-wallpapers/

redshift -O 4000 -P
