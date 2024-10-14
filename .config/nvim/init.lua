vim.o.autoread = true
vim.o.autoindent = true
vim.o.backup = true
vim.o.backupdir = vim.fn.expand("~/.vim/backup//")
vim.o.expandtab = true
vim.o.hls = true
vim.o.ignorecase = true
vim.o.laststatus = 2
vim.o.lazyredraw = true
vim.o.linebreak = true
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

vim.g.bigfile_mode = false
local open_ok, fd = pcall(vim.uv.fs_open, vim.api.nvim_buf_get_name(0), "r", 438)
local stat_ok, stat = pcall(vim.uv.fs_fstat, fd)
if open_ok and stat_ok and stat.size > 1000 * 1000 then
  vim.g.bigfile_mode = true
  vim.uv.fs_close(fd)
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
-- install manually: ripgrep, fzf
Plug('ibhagwan/fzf-lua', {['branch'] = 'main'})
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-tree/nvim-web-devicons'
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
Plug 'sindrets/diffview.nvim'
Plug 'Shatur/neovim-ayu'
Plug('bluz71/vim-moonfly-colors', {as = 'moonfly'})
Plug 'rust-lang/rust.vim'
Plug('akinsho/bufferline.nvim', {tag = '*' })
Plug 'kazhala/close-buffers.nvim'
Plug 'sindrets/diffview.nvim'
Plug 'ejrichards/mise.nvim'
Plug 'mrexox/github-open.nvim'
Plug 'kdheepak/lazygit.nvim'
Plug 'ollykel/v-vim'
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

vim.api.nvim_create_user_command('ToHex', '%!xxd', {bang = true})

vim.api.nvim_create_user_command('ToBin', '%!xxd -r', {bang = true})

vim.env.PATH = vim.env.HOME .. '/.local/bin' .. ':' .. vim.env.HOME .. '/go/bin' .. ':' .. vim.env.HOME .. '/bin' .. ':' .. vim.env.PATH
require('mise').setup({})

-- TypeScript and JavaScript
require('lspconfig').ts_ls.setup({})

-- Ruby
-- require('lspconfig').solargraph.setup({})

-- Rust
require('lspconfig').rust_analyzer.setup({})

vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('UserLspConfig', {}),
  callback = function(ev)
    -- Enable completion triggered by <c-x><c-o>
    vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

    -- Buffer local mappings.
    -- See `:help vim.lsp.*` for documentation on any of the below functions
    local opts = { buffer = ev.buf }
    vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
    vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
    vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
    vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, opts)
    vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, opts)
    vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, opts)
    vim.keymap.set('n', '<space>wl', function()
      print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
    end, opts)
    vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, opts)
    vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, opts)
    vim.keymap.set({ 'n', 'v' }, '<space>ca', vim.lsp.buf.code_action, opts)
    vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
    vim.keymap.set('n', '<space>f', function()
      vim.lsp.buf.format { async = true }
    end, opts)
  end,
})


require('fzf-lua').setup({
    defaults = {
      file_icons = false,
    },
  })

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
      theme = 'ayu',
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
      show_close_icons = false,
      show_buffer_close_icons = false,
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
vim.keymap.set('n', '<Leader>lg', vim.cmd.LazyGit, opts)
vim.keymap.set('n', '<Leader>gh', require('github-open').open_file, opts)
vim.keymap.set('n', '<Leader>gl', require('github-open').open_line, opts)
vim.keymap.set('n', '<Leader>[', function() vim.cmd.diffget('//2') end, opts)
vim.keymap.set('n', '<Leader>]', function() vim.cmd.diffget('//3') end, opts)
vim.keymap.set('n', '<Leader>b', vim.cmd.BlamerToggle, opts)
vim.keymap.set('n', '<space>e', vim.diagnostic.open_float, opts)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist, opts)
vim.keymap.set('n', '<Leader>p', vim.cmd.Files, opts)

vim.keymap.set('n', '<Leader>f', require('fzf-lua').git_files, opts)
vim.keymap.set('n', '<Leader>s', require('fzf-lua').live_grep, opts)
vim.keymap.set('n', '<Leader>l', require('fzf-lua').buffers, opts)
vim.keymap.set('n', '<Leader>/', require('fzf-lua').lgrep_curbuf, opts)
vim.keymap.set('n', '<Leader>a', require('fzf-lua').grep_cword, opts)
vim.keymap.set('v', '<Leader>a', require('fzf-lua').grep_visual, opts)
vim.keymap.set('n', '<Leader>A', function()
  local curdir = vim.fn.expand("%:p:h")
  require('fzf-lua').grep_cword({ cwd = curdir })
end, opts)
vim.keymap.set('v', '<Leader>A', function()
  local curdir = vim.fn.expand("%:p:h")
  require('fzf-lua').grep_visual({ cwd = curdir })
end, opts)

vim.keymap.set('n', '<Leader>d', function()
  local cwd = vim.fn.getcwd()
  vim.cmd.NvimTreeToggle(cwd)
end, opts)
vim.keymap.set('n', '<Leader>w', function()
  local curdir = vim.fn.expand("%:p:h")
  vim.cmd.NvimTreeToggle(curdir)
end, opts)
vim.keymap.set('n', '<Leader>gd', vim.cmd.GoDocBrowser, opts)

if vim.g.neovide then
  vim.g.neovide_refresh_rate = 30
  vim.g.neovide_refresh_rate_idle = 5
  -- vim.g.neovide_transparency = 0.95
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
end

vim.cmd.colorscheme("moonfly")

-- globals

vim.g.blamer_date_format = '%d.%m.%Y'
vim.g.blamer_delay = 500
vim.g.blamer_enabled = 0
vim.g.crystal_auto_format = 1
vim.g.netrw_banner = 0
vim.g.netrw_browse_split = 1
vim.g.netrw_winsize = 25
vim.g.rustfmt_autosave = 1
