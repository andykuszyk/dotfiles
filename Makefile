default:
	./install.sh

termux-pkgs:
	pkg update
	pkg upgrade
	pkg in openssl openssh git make vim zsh wget automake pkg-config gcc clang binutils

ctags:
	git clone https://github.com/universal-ctags/ctags.git ~/repos/ctags
	cd ~/repos/ctags && ./autogen.sh && ./configure --prefix $HOME/.local/bin && make && make install

oh-my-zsh:
	sh -c "$$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

termux: termux-pkgs dirs shell powerline-fonts zsh-completions vim-plug oh-my-zsh ctags

git-config:
	git config --global user.email "andy@kuszyk.com"
	git config --global user.name "andykuszyk"

shell:
	chsh -s $$(which zsh)

powerline-fonts:
	git clone https://github.com/powerline/fonts ~/repos/fonts
	~/repos/fonts/install.sh

zsh-kubectl-prompt:
	git clone https://github.com/superbrothers/zsh-kubectl-prompt ~/repos/zsh-kubectl-prompt

zsh-completions:
	git clone https://github.com/zsh-users/zsh-autosuggestions ~/.oh-my-zsh/custom/plugins/zsh-autosuggestions

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

node:
	wget -O ~/node.tar.xz https://nodejs.org/dist/v16.13.1/node-v16.13.1-linux-x64.tar.xz
	cd ~/ && tar xvf node.tar.xz

install-packages:
	sudo apt install i3 vim fonts-powerline redshift zsh tmux dconf-cli uuid-runtime bat fzf maim xclip scrot imagemagick python3-pip automake vim-gtk3 pavucontrol ruby-dev entr
	pip3 install powerline-status i3ipc ipython PyGithub==1.54.1 powerline-swissarmyknife
	sudo gem install colorls

git-track:
	git clone https://github.com/andykuszyk/git-track ~/repos/git-track
	cp ~/repos/git-track/git* ~/.local/bin

new-machine: install-packages vim-plug shell vim-plugin dirs bat zsh-kubectl-prompt powerline-fonts default git-track
