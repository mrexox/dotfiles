"set runtimepath^=~/.vim runtimepath+=~/.vim/after
"let &packpath = &runtimepath
"source ~/.vimrc
" Run:
" curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
syntax enable
set ignorecase
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
set autoindent
set smartindent
set autoread
set hls
set modifiable
set colorcolumn=90
highlight ColorColumn ctermbg=black ctermfg=red
hi Folded ctermbg=black
hi FoldColumn ctermbg=NONE
hi SignColumn ctermbg=NONE
hi DiffDelete ctermbg=160 ctermfg=NONE
hi DiffAdd ctermbg=28 ctermfg=NONE " 114
hi DiffChange ctermbg=black

autocmd BufWritePre * %s/\s\+$//e

set laststatus=2

set tags=tags
nnoremap <leader>ct :silent ! ctags -R --languages=ruby --exclude=.git --exclude=node_modules --exclude=log -f tags<cr>

set guifont=Monoid:h8
"set guifont=Martian\ Mono:h9
set guioptions=

if has('gui_running')
  colorscheme defnoche
	set t_Co=8 t_md=
endif

" useful mappings
silent! nnoremap <leader>o :only<CR>
silent! nnoremap <leader>i :e ~/.config/nvim/init.vim<Cr>
silent! nnoremap <leader>dd :Lexplore %:p:h<Cr>
silent! nnoremap <leader>da :Lexplore<Cr>
silent! nnoremap <S-Left> :bp<CR>
silent! nnoremap <S-Right> :bn<CR>
silent! nnoremap _ :bp<CR>
silent! nnoremap + :bn<CR>
silent! nnoremap \| :bd<CR>

let g:netrw_winsize = 25
let g:netrw_banner = 0
let g:netrw_browse_split = 1

" netrw
function! NetrwMapping()
  nmap <buffer> H u
  nmap <buffer> h -^
  nmap <buffer> l <CR>

  nmap <buffer> . gh
  nmap <buffer> P <C-w>z
  nmap <buffer> o <CR>:only<CR>

  nmap <buffer> L <CR>:Lexplore<CR>
  nmap <buffer> <leader>dd :Lexplore<CR>
endfunction

augroup netrw_mapping
  autocmd!
  autocmd filetype netrw call NetrwMapping()
augroup END

" Plugins
call plug#begin('~/.vim/plugged')

" Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'APZelos/blamer.nvim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'tpope/vim-fugitive'
Plug 'romainl/vim-cool'
Plug 'sheerun/vim-polyglot'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'mhinz/vim-startify'
Plug 'chrisbra/vim-diff-enhanced'
Plug 'tpope/vim-commentary' " For Commenting gcc & gc
" Plug 'https://github.com/terryma/vim-multiple-cursors' " CTRL + N for multiple cursors

call plug#end()

let g:blamer_enabled = 1
let g:blamer_date_format = '%d/%m/%y'
let g:blamer_delay = 500
highlight Blamer ctermfg=darkgray
command! -nargs=0 Blame call BlamerToggle()
silent! nnoremap <leader>b :BlamerToggle<Cr>

set t_Co=256
let g:airline_theme='onedark'
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#formatter = 'unique_tail'

" 'scrooloose/nerdtree'
"silent! map <F2> :NERDTreeToggle<CR>
"silent! nmap § :NERDTreeToggle<CR>
"silent! nnoremap <S-T> :NERDTreeToggle<CR>


" 'tpope/vim-fugitive'
cabbrev Gpushf Gpush --force-with-lease

" junegunn/fzf
let g:fzf_layout = { 'window': { 'width': 0.9, 'height': 0.8 } }
let g:fzf_preview_window = ['right:75%:hidden', 'ctrl-/']

silent! nnoremap <leader>p :Files<CR>
silent! nnoremap <leader>f :GFiles<CR>
silent! nnoremap <leader>s :Ag<Cr>
silent! nnoremap <leader>a :Ag <C-R><C-W><Cr>
silent! nnoremap <leader>l :Buffers<Cr>

