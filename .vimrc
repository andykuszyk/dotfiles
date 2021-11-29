" Tab configuration
set tabstop=4 shiftwidth=4 expandtab
autocmd Filetype go setlocal ts=4 sw=4 sts=0 noexpandtab
autocmd Filetype yaml setlocal ts=2 sw=2 sts=2 expandtab

" Set relative line numbers, and the absolute line number
" on the current line.
set number relativenumber

" Turn on syntax highlighting.
syntax on

" Set normal backspace behaviour.
set backspace=indent,eol,start

" Include the python powerline plugin for the status bar.
python3 from powerline.vim import setup as powerline_setup
python3 powerline_setup()
python3 del powerline_setup

" Ensure every window as a status line.
set laststatus=2

" Plugins using vim-plug (https://github.com/junegunn/vim-plug)
call plug#begin('~/.vim/plugged')
Plug 'preservim/nerdtree'                             " file browser
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }    " go support
Plug 'tpope/vim-fugitive'                             " git support
Plug 'morhetz/gruvbox'                                " colour scheme
Plug 'neoclide/coc.nvim', {'branch': 'release'}       " auto-completion
Plug 'kien/ctrlp.vim'                                 " fuzzy file and buffer selector
Plug 'PhilRunninger/nerdtree-buffer-ops'              " highlight open buffers in nerdtree
Plug 'ludovicchabant/vim-gutentags'                   " tag file generation, which supports symbol navigation
Plug 'hashivim/vim-terraform'                         " terraform support
Plug 'vim-syntastic/syntastic'                        " automatic syntax checking
Plug 'pprovost/vim-ps1'                               " powershell support
Plug 'chrisbra/NrrwRgn'                               " narrow region/focus
call plug#end()

" Default vim settings.
set nocompatible              " be iMproved, required
filetype off                  " required
filetype plugin indent on    " required

" Enable a highlighted cursorline.
set cursorline

" Dark colour scheme using gruvbox.
let g:gruvbox_contrast_dark = 'hard'
autocmd vimenter * ++nested colorscheme gruvbox
set background=dark

" Disable code folding
set nofoldenable

" Display hidden files in file browser.
let NERDTreeShowHidden=1

" Ctrl-n to show/hide file browser
map <C-n> :NERDTreeToggle<CR>

" Ignore source control files in ctrlp file finder.
let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'

" Include hidden files in ctrlp file finder.
let g:ctrlp_show_hidden = 1

" Use EscEsc to escape terminal mode.
tnoremap <Esc><Esc> <C-\><C-n>

" Do not display line numbers in terminal mode.
autocmd TerminalOpen * setlocal nonumber norelativenumber

" Run goimports when saving Go files.
let g:go_fmt_command = "goimports"

" Turn off GUI elements in gvim.
set guioptions-=m  "menu bar
set guioptions-=T  "toolbar
set guioptions-=r  "scrollbar

" Format Terraform files when saving.
let g:terraform_fmt_on_save=1
