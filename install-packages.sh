#!/bin/bash
set -euo pipefail

os="linux"
package_manager="apt install"
packages="git zsh wget fzf bat automake entr pkg-config imagemagick"
additional_packages=""
if [[ "$(uname)" == "Darwin" ]]; then
    os="darwin"
    package_manager="brew install"
    additional_packages="jansson texinfo libgccjit tree-sitter"
elif command pacman; then
    os="arch-linux"
    package_manager="pacman -Syu"
    additional_packages="which openssh dmenu firefox maim xclip scrot pavucontrol libjansson-dev"
elif uname -r | grep -q android; then
    os="termux"
    package_manager="pkg install"
    additional_packages="which openssl openssh clang binutils zip libvterm emacs"
fi

exec $package_manager $packages $additional_packages

# Install oh-my-zsh.
if [[ ! -d ~/.oh-my-zsh/ ]]; then
    sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
fi

# Set the default shell to be Zsh.
chsh -s $(which zsh)

# Symlink bat to a more convenient command name.
if [[ ! -a ~/.local/bin/bat ]]; then
    ln -s /usr/bin/batcat ~/.local/bin/bat
fi

# Fzf tab-based completion for Zsh.
if [[ ! -d ~/.oh-my-zsh/custom/plugins/fzf-tab/ ]]; then
    git clone https://github.com/Aloxaf/fzf-tab $${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/fzf-tab
fi

# Sdkman java package manager.
if [[ ! -d ~/.sdkman ]]; then
    curl -s "https://get.sdkman.io" | bash
fi

# Powerline fonts
if [[ ! -d ~/repos/fonts && os != "darwin" ]]; then
    git clone https://github.com/powerline/fonts ~/repos/fonts
    ~/repos/fonts/install.sh
fi
