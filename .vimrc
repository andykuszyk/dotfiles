set tabstop=4 shiftwidth=4 expandtab
set number
autocmd Filetype go setlocal ts=4 sw=4 sts=0 noexpandtab
autocmd Filetype yaml setlocal ts=2 sw=2 sts=2 expandtab
set nocompatible
filetype off

call plug#begin('~/.vim/plugged')
Plug 'vim-airline/vim-airline'
call plug#end()

set nocompatible              " be iMproved, required
filetype off                  " required

filetype plugin indent on    " required

set cursorline
