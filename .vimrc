" Tab configuration
set tabstop=4 shiftwidth=4 expandtab
autocmd Filetype go setlocal ts=4 sw=4 sts=0 noexpandtab
autocmd Filetype yaml setlocal ts=2 sw=2 sts=2 expandtab

" Auto commit/push in notes repo.
autocmd BufWritePost */form3-notes/*.md !make
autocmd BufWritePost */notes/*.md !make

" Write swap files to a specific directory (and not the working
" directory of files being edited).
set directory=$HOME/.vim/swapfiles//

" Set relative line numbers, and the absolute line number
" on the current line.
set number relativenumber

" Turn on syntax highlighting.
syntax on

" Set normal backspace behaviour.
set backspace=indent,eol,start

" Ensure every window as a status line.
set laststatus=2

" Set encoding for powerline fonts.
set encoding=UTF-8

" Enable incremental search.
set incsearch

" Enable powerline fonts
let g:airline_powerline_fonts = 1

" Enable airline tab bar
let g:airline#extensions#tabline#enabled = 1

" Set airline theme.
let g:airline_theme='base16'

" Plugins using vim-plug (https://github.com/junegunn/vim-plug)
call plug#begin('~/.vim/plugged')
Plug 'vim-airline/vim-airline'                        " Powerline style status bar, without Python
Plug 'vim-airline/vim-airline-themes'                 " Colour styles for the status bar
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
Plug 'airblade/vim-gitgutter'                         " Git highlighting by buffer line numbers
Plug 'godlygeek/tabular'                              " Markdown text alignment
Plug 'plasticboy/vim-markdown'                        " Improved markdown support
Plug 'mtdl9/vim-log-highlighting'                     " Log file highlighting for log files
Plug 'dhruvasagar/vim-table-mode'                     " Advanced table management
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

" Disable code folding when Vim starts.
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
set guioptions=

" Set GUI font to make sure gvim uses the right font
set guifont=DejaVu\ Sans\ Mono\ for\ Powerline

" Format Terraform files when saving.
let g:terraform_fmt_on_save=1

" Bind set paste to F10.
map <F10> :set paste<CR>

" Enable syntax concealing for markdown files
autocmd BufRead *.md set conceallevel=2

" Set line length for markdown files
autocmd BufRead *.md set colorcolumn=80

" Allow virtual editing in markdown files
" to allow jumping to columns thst dont exist
" yet with `80|`.
autocmd BufRead *.md set virtualedit=all
