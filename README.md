# Andy Kuszyk's dotfiles
These are my dotfiles--primarily for Zsh and Emacs--organised for ease of distribution and installation on new machines.

Most of my Emacs configuration is documented as literate org files in the `.emacs.d` directory. You can see how the files are loaded in [`.emacs.d/init.el`](.emacs.d/init.el).

This repo contains the dotfiles that I use for Emacs, Vim, Zsh, i3wm, and a few other tools. I use these files on Ubuntu, Fedora, Mac OS, and Termux.

## Installation
First, clone this repo. I typically clone it to `~/repos/andykuszyk/dotfiles`:

```sh
mkdir -p ~/repos/andykuszyk
cd ~/repos/andykuszyk
git clone https://github.com/andykuszyk/dotfiles
```

Next, ensure you have `make`.

Then, install the dotfiles by simply running `make`:

```sh
cd ~/repos/andykuszyk/dotfiles
make
```

## Package installation
The instructions above will install the dotfiles stored in this repo. However, some of this configuration may require additional packages to be installed on you system. These can be installed with:

```
make install-packages
```
