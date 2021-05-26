#!/usr/bin/env bash
tmpbg='/tmp/screen.png'
scrot "$tmpbg"
convert "$tmpbg" -scale 20% -scale 500% "$tmpbg"
git track stop &
i3lock -n -i "$tmpbg"
git track start
