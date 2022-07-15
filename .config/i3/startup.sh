#!/bin/bash

# Additional key mapping for alt gr to also be a super modifer
xmodmap -e "keycode 108 = Super_R"
xmodmap -e "keycode 92 = Super_R"

# Screen resolution and layout
xrandr --output DP-0 --off --output DP-1 --mode 1920x1080 --pos 0x221 --rotate normal --output DP-2 --off --output DP-3 --off --output DP-4 --off --output DP-5 --mode 1920x1080 --pos 1920x221 --rotate normal --output DP-6 --off --output DP-7 --mode 1920x1080 --pos 3840x0 --rotate left

# Gnome keyring
gnome-keyring-daemon --start --components=pkcs11,secrets,ssh,gpg

# Wallpaper
feh --randomize --bg-fill ~/Pictures/planet-wallpapers/

# Set screen temperature
redshift -O 4000 -P

# Start ssh-agent
eval $(ssh-agent)

# Start emacs server
emacs --daemon
