set tabstop=4 shiftwidth=4 expandtab
set number
autocmd Filetype go setlocal ts=4 sw=4 sts=0 noexpandtab
autocmd Filetype yaml setlocal ts=2 sw=2 sts=2 expandtab
syntax on
set backspace=indent,eol,start

python3 from powerline.vim import setup as powerline_setup
python3 powerline_setup()
python3 del powerline_setup

set laststatus=2

call plug#begin('~/.vim/plugged')
Plug 'preservim/nerdtree'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'tpope/vim-fugitive'
Plug 'morhetz/gruvbox'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'kien/ctrlp.vim'
Plug 'PhilRunninger/nerdtree-buffer-ops'
Plug 'ludovicchabant/vim-gutentags'
Plug 'hashivim/vim-terraform'
Plug 'vim-syntastic/syntastic'
Plug 'pprovost/vim-ps1'
call plug#end()

set nocompatible              " be iMproved, required
filetype off                  " required

filetype plugin indent on    " required

set cursorline

let g:gruvbox_contrast_dark = 'hard'
autocmd vimenter * ++nested colorscheme gruvbox
set background=dark

set nofoldenable

let NERDTreeShowHidden=1
map <C-n> :NERDTreeToggle<CR>
let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
let g:ctrlp_show_hidden = 1
tnoremap <Esc><Esc> <C-\><C-n>
autocmd TerminalOpen * setlocal nonumber norelativenumber

let g:go_fmt_command = "goimports"

set guioptions-=m  "menu bar
set guioptions-=T  "toolbar
set guioptions-=r  "scrollbar

let g:terraform_fmt_on_save=1
