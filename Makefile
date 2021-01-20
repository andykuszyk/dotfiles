default:
	./install.sh

install-packages:
	sudo apt install i3 vim fonts-powerline redshift zsh tmux dconf-cli uuid-runtime bat fzf
	pip3 install powerline-status i3ipc
