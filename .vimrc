set tabstop=4 shiftwidth=4 expandtab
set number
autocmd Filetype go setlocal ts=4 sw=4 sts=0 noexpandtab
autocmd Filetype yaml setlocal ts=2 sw=2 sts=2 expandtab
set nocompatible
filetype off
syntax on
set backspace=indent,eol,start

set runtimepath^=~/.vim/bundle/ctrlp.vim

python3 from powerline.vim import setup as powerline_setup
python3 powerline_setup()
python3 del powerline_setup

set laststatus=2

call plug#begin('~/.vim/plugged')
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'tpope/vim-fugitive'
call plug#end()

let g:go_def_mode='gopls'
let g:go_info_mode='gopls'
au filetype go inoremap <buffer> . .<C-x><C-o>

set nocompatible              " be iMproved, required
filetype off                  " required

filetype plugin indent on    " required

set cursorline
colorscheme delek
