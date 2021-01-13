#!/usr/bin/env bash
tmpbg='/tmp/screen.png'
scrot "$tmpbg"
convert "$tmpbg" -scale 20% -scale 500% "$tmpbg"
cd ~/repos/andykuszyk/captains-log
./lg stop
i3lock -n -i "$tmpbg"
./lg start
