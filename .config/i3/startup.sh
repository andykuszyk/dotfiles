#!/bin/bash

# Additional key mapping for alt gr to also be a super modifer
xmodmap -e "keycode 108 = Super_R"
xmodmap -e "keycode 92 = Super_R"

# Screen resolution and layout
xrandr --output eDP-1-1 --mode 1920x1080 --pos 0x256 --rotate normal --output DP-1-1 --mode 1920x1080 --pos 3840x0 --rotate left --output DP-1-2 --off --output DP-1-3 --mode 1920x1080 --pos 1920x0 --rotate normal

# Gnome keyring
gnome-keyring-daemon --start --components=pkcs11,secrets,ssh,gpg

# Wallpaper
feh --randomize --bg-fill ~/Pictures/planet-wallpapers/

redshift -O 4000 -P
