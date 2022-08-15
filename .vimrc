" Run:
" curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
syntax enable
set tabstop=2
set softtabstop=2
set shiftwidth=2
set expandtab
set smarttab
set number
set showcmd
"set cursorline
filetype indent on
set wildmenu
set lazyredraw
set showmatch
set smartindent
set autoindent
set autoread
set hls
set paste "disable if no identation works
autocmd BufWritePre * %s/\s\+$//e

set laststatus=2

set tags=tags

set guifont=Monoid:h8
set guioptions=

if has('gui_running')
  colorscheme defnoche
endif

silent! map <F2> :NERDTreeToggle<CR>
silent! nmap § :NERDTreeToggle<CR>
silent! nnoremap <S-Left> :bp<CR>
silent! nnoremap <S-Right> :bn<CR>
"silent! nnoremap <C-[> <C-T>

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

Plug 'tpope/vim-fugitive'
cabbrev Gpushf Gpush --force-with-lease

Plug 'romainl/vim-cool'

Plug 'dikiaap/minimalist'
set t_Co=256
let g:airline_theme='onedark'
"let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1

Plug 'sheerun/vim-polyglot'

call plug#end()
