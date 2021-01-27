default:
	./install.sh

install-packages:
	sudo apt install i3 vim fonts-powerline redshift zsh tmux dconf-cli uuid-runtime bat fzf maim xclip scrot imagemagick
	pip3 install powerline-status i3ipc
