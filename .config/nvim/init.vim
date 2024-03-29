"set runtimepath^=~/.vim runtimepath+=~/.vim/after
"let &packpath = &runtimepath
"source ~/.vimrc
" Run:
" curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
syntax enable
set clipboard=unnamedplus
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

"Turn on backup option
set backup
set backupdir=~/.vim/backup//
set writebackup
set backupcopy=yes
"Meaningful backup name, ex: filename@2015-04-05.14:59
au BufWritePre * let &bex = '@' . strftime("%F.%H:%M")

highlight ColorColumn ctermbg=black ctermfg=red
hi Folded ctermbg=black
hi FoldColumn ctermbg=NONE
hi SignColumn ctermbg=NONE
hi DiffDelete ctermbg=160 ctermfg=NONE
hi DiffAdd ctermbg=28 ctermfg=NONE " 114
hi DiffChange ctermbg=black
hi Search ctermfg=0 ctermbg=175 guifg=Black guibg=Pink
hi Pmenu ctermbg=black ctermfg=255 guibg=Black

autocmd BufWritePre * %s/\s\+$//e

set laststatus=2

" Leave only the current buffer
command Ob :%bd|edit#|bd#

set tags=tags
nnoremap <leader>ct :silent ! ctags -R --languages=ruby,rust --exclude=.git --exclude=node_modules --exclude=log -f tags<cr>

" set guifont=Monoid:h8
set guifont=Monoid\ Nerd\ Font:h9
"set guifont=Martian\ Mono:h9
set guioptions=


" useful mappings
silent! nnoremap <leader>1 :Ob<CR>
silent! nnoremap <leader>o :only<CR>
silent! nnoremap <leader>i :e ~/.config/nvim/init.vim<Cr>
silent! nnoremap <S-Left> :bp<CR>
silent! nnoremap <S-Right> :bn<CR>
silent! nnoremap _ :bp<CR>
silent! nnoremap + :bn<CR>
silent! nnoremap \| :bd<CR>
silent! nnoremap <leader>gd :Gvdiff!<CR>
silent! nnoremap <leader>gh :diffget //2<CR>
silent! nnoremap <leader>gl :diffget //3<CR>
silent! nnoremap <leader>[ :diffget //2<CR>
silent! nnoremap <leader>] :diffget //3<CR>
silent! nnoremap <leader>t :e ~/Documents/organizational/coveralls.md<CR>

" netrw
let g:netrw_winsize = 25
let g:netrw_banner = 0
let g:netrw_browse_split = 1
" silent! nnoremap <leader>d :Lexplore %:p:h<Cr>
function! NetrwMapping()
  nmap <buffer> H u
  nmap <buffer> h -^
  nmap <buffer> l <CR>

  nmap <buffer> . gh
  nmap <buffer> P <C-w>z
  nmap <buffer> o <CR>:only<CR>
  nmap <buffer> q :q<CR>

  nmap <buffer> L <CR>:Lexplore<CR>
  nmap <buffer> <leader>dd :Lexplore<CR>
  nmap <buffer> cd :cd %<Cr>
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
Plug 'nvim-lua/plenary.nvim'
Plug 'TimUntersberger/neogit' " depends on plenary.nvim
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'mhinz/vim-startify'
Plug 'neovim/nvim-lspconfig'
Plug 'chrisbra/vim-diff-enhanced'
Plug 'tpope/vim-commentary' " For Commenting gcc & gc
" Plug 'https://github.com/terryma/vim-multiple-cursors' " CTRL + N for multiple cursors

Plug 'kyazdani42/nvim-tree.lua'
Plug 'ap/vim-css-color'
Plug 'vim-crystal/vim-crystal'
Plug 'tpope/vim-fugitive'
Plug 'junegunn/gv.vim'
Plug 'editorconfig/editorconfig-vim'
Plug 'nvim-lua/plenary.nvim'
Plug 'sindrets/diffview.nvim'
Plug 'bluz71/vim-moonfly-colors', { 'as': 'moonfly' }
Plug 'rust-lang/rust.vim'
call plug#end()

if has('gui_running')
  set t_Co=8 t_md=
endif

"Rust
let g:rustfmt_autosave = 1

"Crystal
let g:crystal_auto_format = 1

let g:blamer_enabled = 0
let g:blamer_date_format = '%d.%m.%Y'
let g:blamer_delay = 500
highlight Blamer ctermfg=darkgray guifg=darkgray
silent! nnoremap <leader>b :BlamerToggle<Cr>

lua << END

-- Mappings.
-- See `:help vim.diagnostic.*` for documentation on any of the below functions
local opts = { noremap=true, silent=true }
vim.keymap.set('n', '<space>e', vim.diagnostic.open_float, opts)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist, opts)

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
-- Enable completion triggered by <c-x><c-o>
-- vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

-- -- Mappings.
-- -- See `:help vim.lsp.*` for documentation on any of the below functions
-- local bufopts = { noremap=true, silent=true, buffer=bufnr }
-- vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
-- vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
-- vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
-- vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
-- vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, bufopts)
-- vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, bufopts)
-- vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, bufopts)
-- vim.keymap.set('n', '<space>wl', function()
-- print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
-- vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, bufopts)
-- vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, bufopts)
-- vim.keymap.set('n', '<space>ca', vim.lsp.buf.code_action, bufopts)
-- vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)
-- vim.keymap.set('n', '<space>f', vim.lsp.buf.formatting, bufopts)

local lsp_flags = {
  -- This is the default in Nvim 0.7+
  debounce_text_changes = 150,
}

-- require('lspconfig')['pyright'].setup{
--   on_attach = on_attach,
--   flags = lsp_flags,
-- }
-- require('lspconfig')['tsserver'].setup{
--   on_attach = on_attach,
--   flags = lsp_flags,
-- }
-- require('lspconfig')['solargraph'].setup{
--   on_attach = on_attach,
--   flags = lsp_flags,
-- }
END

set t_Co=256
let g:airline_theme='onedark'
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#formatter = 'unique_tail'
let g:airline_powerline_fonts = 1

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

" mhinz/vim-startify
let g:startify_bookmarks = systemlist("cut -sd' ' -f 2- ~/.NERDTreeBookmarks")

" 'TimUntersberger/neogit'
silent! nnoremap <C-x>g :Neogit<CR>
silent! nnoremap <leader>g :Neogit<CR>

silent! nnoremap <leader>d :NvimTreeToggle<Cr>
silent! nnoremap <leader>w :NvimTreeToggle %:p:h<Cr>

if exists("g:neovide")
  let g:neovide_refresh_rate=30
  let g:neovide_refresh_rate_idle=5
  let g:neovide_transparency=0.8
  let g:neovide_hide_mouse_when_typing = v:true
  let g:neovide_cursor_trail_size = 0.2

  lua <<END
    vim.keymap.set('n', '<C-s>', ':w<CR>') -- Save
    vim.keymap.set('v', '<C-C>', '"+y') -- Copy
    vim.keymap.set('n', '<C-V>', '"+P') -- Paste normal mode
    vim.keymap.set('v', '<C-V>', '"+P') -- Paste visual mode
    vim.keymap.set('c', '<C-V>', '<C-R>+') -- Paste command mode
    vim.keymap.set('i', '<C-V>', '<ESC>"+pa') -- Paste insert mode
END

  colorscheme moonfly
endif

" vim-tree
"
lua <<END
local function on_attach(bufnr)
  local api = require "nvim-tree.api"

  local function opts(desc)
    return { desc = "nvim-tree: " .. desc, buffer = bufnr, noremap = true, silent = true, nowait = true }
  end

  -- default mappings
  api.config.mappings.default_on_attach(bufnr)

  -- custom mappings
  vim.keymap.set('n', '?',     api.tree.toggle_help,                opts('Help'))
  vim.keymap.set('n', 'l',     api.node.open.edit,                  opts('Edit'))
  vim.keymap.set('n', 'h',     api.tree.change_root_to_parent,      opts('Up'))

end

require("nvim-tree").setup({
on_attach = on_attach,
view = {
  adaptive_size = true,
  side = "right",
},
actions = {
  change_dir = {
    enable = true,
    global = true,
    },
  },
renderer = {
  indent_markers = {
    enable = false
    },
  icons = {
    webdev_colors =  false,
    show = {
      file = false,
      folder = false,
      folder_arrow = false,
      git = false,
      }
    }
  }
})
END
