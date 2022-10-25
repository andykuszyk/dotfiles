# Andy Kuszyk's dotfiles
This repo contains the dotfiles that I use for Emacs, Vim, Zsh, i3wm, and a few other tools. I use these files on Ubuntu, Fedora, Mac OS, and Termux.

## Usage
* Clone this repo somewhere.
* Run `make` to copy the files into your home directory.

> Check out the other Make targets for platform-specific instructions, e.g. for fedora or termux.

## Emacs
Some hacking might be required to get the Emacs config to work first time, since I rarely apply it from the beginning. My Emacs config takes the form of an Org file which is tangled into an init file by org-babel. See the [config](./.emacs.d/emacs.org) for more details.

I use Emacs with EXWM as a window manager on Linux. For this to work, the `emacs.desktop` file needs to be copied to the xsessions directory, e.g:

```sh
$ sudo cp emacs.desktop /usr/share/xsessions
```

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
