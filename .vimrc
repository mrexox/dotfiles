syntax enable
set tabstop=4
set softtabstop=2
set expandtab
set number
set showcmd
"set cursorline
filetype indent on
set wildmenu
set lazyredraw
set showmatch
set smartindent
set autoindent
autocmd BufWritePre * %s/\s\+$//e

set laststatus=2

set tags=tags

set guifont=Hack:h16
set guioptions=

if has('gui_running')
  colorscheme defnoche
endif

silent! nmap § :NERDTreeToggle<CR>


" Plugins
call plug#begin('~/.vim/plugged')

Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }

Plug 'mileszs/ack.vim'
if executable('ag')
  let g:ackprg = 'ag --vimgrep'
endif
cnoreabbrev ag Ack

Plug 'zivyangll/git-blame.vim'
nnoremap <C-b> :call gitblame#echo()<CR>

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
let g:airline_theme='onedark'

call plug#end()
