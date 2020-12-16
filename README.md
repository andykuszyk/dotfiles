# Andy Kuszyk's dotfiles
This repo contains the dotfiles that I use for Linux desktop configuration. Most of them relate to my use of i3, at which I am a beginner - this repo is not a primary source of i3 information!

More information about my use of i3 can be found in my [blog post](https://andykuszyk.github.io/2020-02-18-demystifying-i3.html) on the subject.

## Usage
* Clone this repo somewhere.
* Run `make` to copy the files into your home directory.

## Vim
* Vim plugins use [`vim-plug`](https://github.com/junegunn/vim-plug). To install run `curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim`, open Vim and run `:PlugInstall`.

## Dependencies
* Clone https://github.com/superbrothers/zsh-kubectl-prompt into `$HOME/repos/`
* `pip3 install powerline-status`

## Useful utilities
* Adjusting colour balance at night: `redshift`
* Managing network connections: `nmtui`
