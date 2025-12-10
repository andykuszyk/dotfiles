#!/bin/bash
set -euo pipefail

mkdir -p ~/.emacs.d/lisp
mkdir -p ~/wallpaper
mkdir -p ~/.config/i3
mkdir -p ~/.config/gtk-3.0
mkdir -p ~/.config/powerline/themes/wm
mkdir -p ~/.config/powerline/themes/tmux
mkdir -p ~/.config/powerline/themes/shell
mkdir -p ~/.vim
mkdir -p ~/repos
mkdir -p ~/.local/bin
mkdir -p ~/.vim/swapfiles
mkdir -p ~/.ctags.d
mkdir -p ~/.termux/
mkdir -p ~/.gnupg/

for f in $(find . -type f | grep -v git); do
    cp $f $HOME/$(echo $f | sed 's/\.\///g');
done

if [[ -d /usr/share/xsessions ]]; then
    sudo cp emacs.desktop /usr/share/xsessions
fi
