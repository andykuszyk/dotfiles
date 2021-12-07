# Andy Kuszyk's dotfiles
This repo contains the dotfiles that I use for Linux desktop configuration. Most of them relate to my use of i3, at which I am a beginner - this repo is not a primary source of i3 information!

More information about my use of i3 can be found in my [blog post](https://andykuszyk.github.io/2020-02-18-demystifying-i3.html) on the subject.

## Usage
* Clone this repo somewhere.
* `make install-packages` to install system dependencies.
* `make new-machine` to set things up.
* After any further changes to files, run `make` to copy the files into your home directory.

Alternatively, run a sandbox environment with `make run`.

## Vim
* Vim plugins use [`vim-plug`](https://github.com/junegunn/vim-plug). To install run `curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim`, open Vim and run `:PlugInstall`.

Tag generation is handled by [`ctags`](https://github.com/universal-ctags/ctags).

After installing the plugins, install these `coc` specific extentions:
- `:CocInstall coc-java`
- `:CocInstall coc-go`

For the Terraform langauge server, install https://github.com/hashicorp/terraform-ls.

## Terminal theme
Terminal themes provided by https://github.com/Mayccoll/Gogh. Current theme is `One Dark`.

## Dependencies
* Clone https://github.com/superbrothers/zsh-kubectl-prompt into `$HOME/repos/`
* `pip3 install powerline-status`

## Useful utilities
* Adjusting colour balance at night: `redshift`
* Managing network connections: `nmtui`
* [`gvm`](https://github.com/moovweb/gvm) for managing Go versions
* [`tfenv`](https://github.com/tfutils/tfenv) for managing Terraform versions
