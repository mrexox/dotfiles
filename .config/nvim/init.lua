vim.o.autoread = true
vim.o.autoindent = true
vim.o.backup = true
vim.o.backupdir = vim.fn.expand("~/.vim/backup//")
vim.o.expandtab = true
vim.o.hls = true
vim.o.ignorecase = true
vim.o.laststatus = 2
vim.o.lazyredraw = true
vim.o.modifiable = true
vim.o.number = true
vim.o.shiftwidth = 2
vim.o.showcmd = true
vim.o.showmatch = true
vim.o.smartindent = true
vim.o.smarttab = true
vim.o.softtabstop = 2
vim.o.tabstop = 2
vim.o.tags = "tags"
vim.o.wildmenu = true
vim.o.writebackup = true
vim.o.guifont = "Monoid Nerd Font Mono Retina:h15"
vim.o.guioptions = nil
vim.o.t_Co=256

if vim.fn.has("gui_running") then
  vim.o.t_Co = 8
  vim.o.t_md = nil
end

-- highlights

vim.cmd([[
highlight ColorColumn ctermbg=black ctermfg=red
highlight Folded ctermbg=black
highlight FoldColumn ctermbg=NONE
highlight SignColumn ctermbg=NONE
highlight DiffDelete ctermbg=160 ctermfg=NONE
highlight DiffAdd ctermbg=28 ctermfg=NONE
highlight DiffChange ctermbg=black
highlight Search ctermfg=0 ctermbg=175 guifg=Black guibg=Pink
highlight Pmenu ctermbg=black ctermfg=255 guibg=Black
highlight Blamer ctermfg=darkgray guifg=darkgray
]])

-- autocmd

vim.api.nvim_create_autocmd("BufWritePre", {
  command = "%s/\\s\\+$//e"
})

local Plug = vim.fn['plug#']
vim.call('plug#begin', '~/.vim/plugged')
Plug 'APZelos/blamer.nvim'
Plug 'nvim-lualine/lualine.nvim'
Plug 'tpope/vim-fugitive'
Plug 'romainl/vim-cool'
Plug 'sheerun/vim-polyglot'
Plug('junegunn/fzf', {['do'] = vim.fn['fzf#install']})
Plug 'junegunn/fzf.vim'
Plug 'nvim-lua/plenary.nvim'
Plug 'TimUntersberger/neogit'
Plug('fatih/vim-go', {['do'] = 'GoUpdateBinaries'})
Plug 'mhinz/vim-startify'
Plug 'neovim/nvim-lspconfig'
Plug 'chrisbra/vim-diff-enhanced'
Plug 'tpope/vim-commentary'
Plug 'kyazdani42/nvim-tree.lua'
Plug 'ap/vim-css-color'
Plug 'vim-crystal/vim-crystal'
Plug 'tpope/vim-fugitive'
Plug 'junegunn/gv.vim'
Plug 'editorconfig/editorconfig-vim'
Plug 'nvim-lua/plenary.nvim'
Plug 'sindrets/diffview.nvim'
Plug('bluz71/vim-moonfly-colors', {as = 'moonfly'})
Plug 'rust-lang/rust.vim'
Plug('akinsho/bufferline.nvim', {tag = '*' })
Plug 'kazhala/close-buffers.nvim'
vim.call('plug#end')

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

require('lualine').setup({
    options = {
      icons_enabled = true,
      theme = 'onedark',
      tabline = {
        lualine_a = {},
        lualine_b = {'branch'},
        lualine_c = {'filename'},
        lualine_x = {},
        lualine_y = {},
        lualine_z = {},
      },
    }
  })

require("bufferline").setup({
    options = {
      indicator = {
        icon = nil,
        style = 'underline',
      },
      themable = true,
    }
  })

require('close_buffers').setup({
  filetype_ignore = {},  -- Filetype to ignore when running deletions
  file_glob_ignore = {},  -- File name glob pattern to ignore when running deletions (e.g. '*.md')
  file_regex_ignore = {}, -- File name regex pattern to ignore when running deletions (e.g. '.*[.]md')
  preserve_window_layout = { 'this', 'nameless' },  -- Types of deletion that should preserve the window layout
  next_buffer_cmd = nil,  -- Custom function to retrieve the next buffer when preserving window layout
})

-- keymaps

vim.keymap.set('n', '<Leader>ct', function()
  io.popen("/opt/homebrew/bin/ctags -R --exclude=.git --exclude=node_modules --exclude=log -f tags")
end)
vim.keymap.set('n', '<Leader>1', function()
  require("bufferline").close_others()
end)

local opts = { noremap=true, silent=true }
vim.keymap.set('n', '<Leader>o', vim.cmd.only, opts)
vim.keymap.set('n', '<Leader>i', function() vim.cmd.edit("~/.config/nvim/init.lua") end, opts)
vim.keymap.set('n', '<S-Left>', vim.cmd.bp, opts)
vim.keymap.set('n', '_', vim.cmd.bp, opts)
vim.keymap.set('n', '<S-Right>', vim.cmd.bn, opts)
vim.keymap.set('n', '+', vim.cmd.bn, opts)
vim.keymap.set('n', '|', function()
  require('close_buffers').wipe({ type = 'this' })
end, opts)
-- vim.keymap.set('n', '<Leader>gd', vim.cmd.Gvdiff!, opts)
vim.keymap.set('n', '<Leader>gh', function() vim.cmd.diffget('//2') end, opts)
vim.keymap.set('n', '<Leader>gl', function() vim.cmd.diffget('//3') end, opts)
vim.keymap.set('n', '<Leader>[', function() vim.cmd.diffget('//2') end, opts)
vim.keymap.set('n', '<Leader>]', function() vim.cmd.diffget('//3') end, opts)
vim.keymap.set('n', '<Leader>b', vim.cmd.BlamerToggle, opts)
vim.keymap.set('n', '<space>e', vim.diagnostic.open_float, opts)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist, opts)
vim.keymap.set('n', '<Leader>p', vim.cmd.Files, opts)
vim.keymap.set('n', '<Leader>f', vim.cmd.GFiles, opts)
vim.keymap.set('n', '<Leader>s', vim.cmd.Ag, opts)
vim.keymap.set('n', '<Leader>a', function()
  local word = vim.fn.expand("<cword>")
  vim.cmd.Ag(word)
end, opts)
vim.keymap.set('n', '<Leader>l', vim.cmd.Buffers, opts)
vim.keymap.set('n', '<Leader>d', function()
  local pwd = vim.fn.expand("$PWD")
  vim.cmd.NvimTreeToggle(pwd)
end, opts)
vim.keymap.set('n', '<Leader>w', function()
  local curdir = vim.fn.expand("%:p:h")
  vim.cmd.NvimTreeToggle(curdir)
end, opts)

if vim.g.neovide then
  vim.g.neovide_refresh_rate = 30
  vim.g.neovide_refresh_rate_idle = 5
  vim.g.neovide_transparency = 0.95
  vim.g.neovide_hide_mouse_when_typing = true
  vim.g.neovide_cursor_trail_size = 0.2

  if vim.fn.has('macunix') then
    vim.keymap.set('n', '<D-s>', ':w<CR>') -- Save
    vim.keymap.set('v', '<D-c>', '"+y') -- Copy
    vim.keymap.set('n', '<D-v>', '"+P') -- Paste normal mode
    vim.keymap.set('v', '<D-v>', '"+P') -- Paste visual mode
    vim.keymap.set('c', '<D-v>', '<C-R>+') -- Paste command mode
    vim.keymap.set('i', '<D-v>', '<ESC>"+pa') -- Paste insert mode
  else
    vim.keymap.set('n', '<C-s>', ':w<CR>') -- Save
    vim.keymap.set('v', '<C-C>', '"+y') -- Copy
    vim.keymap.set('n', '<C-V>', '"+P') -- Paste normal mode
    vim.keymap.set('v', '<C-V>', '"+P') -- Paste visual mode
    vim.keymap.set('c', '<C-V>', '<C-R>+') -- Paste command mode
    vim.keymap.set('i', '<C-V>', '<ESC>"+pa') -- Paste insert mode
  end

  vim.cmd.colorscheme("moonfly")
end

-- globals

vim.g.blamer_date_format = '%d.%m.%Y'
vim.g.blamer_delay = 500
vim.g.blamer_enabled = 0
vim.g.crystal_auto_format = 1
vim.g.netrw_banner = 0
vim.g.netrw_browse_split = 1
vim.g.netrw_winsize = 25
vim.g.rustfmt_autosave = 1
vim.g.fzf_layout = { window = { width = 0.9, height = 0.8 } }
vim.g.fzf_preview_window = {'right:75%:hidden', 'ctrl-/'}
