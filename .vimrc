set shiftwidth=4
:set guicursor+=a:blinkon0
set softtabstop=4
set tabstop=4
:set autoindent
:set cindent
set number
set expandtab ts=4 sw=4 ai
set swapfile
set dir=~/tmp
augroup CursorLine
  au!
  au VimEnter,WinEnter,BufWinEnter * setlocal cursorline
  au WinLeave * setlocal nocursorline
augroup END
if has("gui_running")
syntax on
"set guifont=Noto\ Sans\ Mono\ Regular\ 11
"color dracula
"set termguicolors
"colorscheme ceudah
"colorscheme dark_eyes
colorscheme gotham
  if has("gui_gtk2")
    set guifont=DejaVu\ Sans\ Mono\ 11
		"set guifont=Consolas\ 12
  elseif has("gui_macvim")
    set guifont=Menlo\ Regular:h14
  elseif has("gui_win32")
    set guifont=Consolas:h11:cANSI
  endif
endif
execute pathogen#infect()
syntax on
filetype plugin indent on
map <C-n> :NERDTreeToggle<CR>

:set guioptions-=m  "remove menu bar
:set guioptions-=T  "remove toolbar
:set guioptions-=r  "remove right-hand scroll bar
:set guioptions-=L  "remove left-hand scroll bar
call plug#begin('~/.vim/plugged')
Plug 'mhinz/vim-startify'
Plug 'jreybert/vimagit'
Plug 'ajmwagar/vim-deus'
Plug 'rafi/awesome-vim-colorschemes'
Plug 'bf4/vim-dark_eyes'
Plug 'vim-airline/vim-airline'
Plug 'terryma/vim-multiple-cursors'
Plug 'vim-airline/vim-airline-themes'
Plug 'airblade/vim-gitgutter'
Plug 'junegunn/vim-easy-align'
call plug#end()
let g:airline_theme='deus'
let g:airline#extensions#tabline#formatter = 'unique_tail'
let g:gitgutter_override_sign_column_highlight = 0
"let g:airline_section_z = airline#section#create(['windowswap', '%3p%% ', 'linenr', ':%3v'])
"set rtp+=$HOME/.local/lib/python3.6/site-packages/powerline/bindings/vim/
set laststatus=2
set t_Co=256


let g:airline#extensions#tabline#enabled = 1

