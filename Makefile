default:
	./install.sh

shell:
	chsh -s $$(which zsh)

zsh-kubectl-prompt:
	git clone https://github.com/superbrothers/zsh-kubectl-prompt ~/repos/zsh-kubectl-prompt

bat:
	mkdir -p ~/.local/bin
	ln -s /usr/bin/batcat ~/.local/bin/bat

vim-plug:
	curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

dirs:
	mkdir -p ~/.config/i3
	mkdir -p ~/.config/powerline/themes/wm
	mkdir -p ~/.config/powerline/themes/tmux
	mkdir -p ~/.config/powerline/themes/shell
	mkdir -p ~/.vim
	mkdir -p ~/repos

install-packages:
	sudo apt install i3 vim fonts-powerline redshift zsh tmux dconf-cli uuid-runtime bat fzf maim xclip scrot imagemagick python3-pip automake nodejs npm
	pip3 install powerline-status i3ipc ipython PyGithub==1.54.1

new-machine: install-packages vim-plug shell bat vim-plugin dirs zsh-kubectl-prompt default
