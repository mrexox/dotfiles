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
hi SignColumn ctermbg=NONE

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

silent! nnoremap <C-x>o :only<CR>
silent! nnoremap <leader>i :e ~/.config/nvim/init.vim<Cr>

" Plugins
call plug#begin('~/.vim/plugged')

Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'zivyangll/git-blame.vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'tpope/vim-fugitive'
Plug 'romainl/vim-cool'
Plug 'sheerun/vim-polyglot'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'nvim-lua/plenary.nvim'
Plug 'TimUntersberger/neogit' " depends on plenary.nvim
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'mhinz/vim-startify'

Plug 'neovim/nvim-lspconfig'
lua require'lspconfig'.tsserver.setup{}

call plug#end()

set t_Co=256
let g:airline_theme='onedark'
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#formatter = 'unique_tail'

" 'scrooloose/nerdtree'
silent! map <F2> :NERDTreeToggle<CR>
silent! nmap § :NERDTreeToggle<CR>
silent! nnoremap <S-T> :NERDTreeToggle<CR>
silent! nnoremap <S-Left> :bp<CR>
silent! nnoremap <S-Right> :bn<CR>
silent! nnoremap _ :bp<CR>
silent! nnoremap + :bn<CR>
silent! nnoremap \| :bd<CR>

" 'tpope/vim-fugitive'
cabbrev Gpushf Gpush --force-with-lease

" 'zivyangll/git-blame.vim'
nnoremap <C-b> :call gitblame#echo()<CR>

" junegunn/fzf
let g:fzf_layout = { 'window': { 'width': 0.9, 'height': 0.8 } }
let g:fzf_preview_window = ['right:75%:hidden', 'ctrl-/']

silent! nnoremap <leader>p :Files<CR>
silent! nnoremap <leader>f :GFiles<CR>
silent! nnoremap <leader>s :Ag<Cr>
silent! nnoremap <leader>a :Ag <C-R><C-W><Cr>
silent! nnoremap <leader>l :Buffers<Cr>

" mhinz/vim-startify
let g:startify_bookmarks = systemlist("cut -sd' ' -f 2- ~/.NERDTreeBookmarks")

" 'TimUntersberger/neogit'
silent! nnoremap <C-x>g :Neogit<CR>

if exists("g:neovide")
  let g:neovide_refresh_rate=30
  let g:neovide_refresh_rate_idle=5
  let g:neovide_transparency=0.8
endif
