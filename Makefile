dirs:
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

default: dirs
	bash install.sh

install-packages:
	bash install-packages.sh
