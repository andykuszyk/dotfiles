default:
	./install.sh

shell:
	chsh -s $$(which zsh)

powerline-fonts:
	git clone https://github.com/powerline/fonts ~/repos/fonts
	~/repos/fonts/install.sh

zsh-kubectl-prompt:
	git clone https://github.com/superbrothers/zsh-kubectl-prompt ~/repos/zsh-kubectl-prompt

bat:
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
	mkdir -p ~/.local/bin

install-packages:
	sudo apt install i3 vim fonts-powerline redshift zsh tmux dconf-cli uuid-runtime bat fzf maim xclip scrot imagemagick python3-pip automake nodejs npm vim-gtk3 pavucontrol ruby-dev entr
	pip3 install powerline-status i3ipc ipython PyGithub==1.54.1
	sudo gem install colorls

git-track:
	git clone https://github.com/andykuszyk/git-track ~/repos/git-track
	cp ~/repos/git-track/git* ~/.local/bin

new-machine: install-packages vim-plug shell vim-plugin dirs bat zsh-kubectl-prompt powerline-fonts default git-track
