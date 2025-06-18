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
vim.o.guifont = "Fira Code:h17"

-- Big file mode
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
]])

-- autocmd

vim.api.nvim_create_autocmd("BufWritePre", {
  command = "%s/\\s\\+$//e"
})


require("config.lazy")

vim.api.nvim_create_user_command('ToHex', '%!xxd', {bang = true})
vim.api.nvim_create_user_command('ToBin', '%!xxd -r', {bang = true})
vim.env.PATH = vim.env.HOME .. '/.local/bin' .. ':' .. vim.env.HOME .. '/go/bin' .. ':' .. vim.env.HOME .. '/bin' .. ':' .. vim.env.PATH

-- Keymaps
vim.keymap.set('n', '<Leader>ct', function()
  io.popen("/opt/homebrew/bin/ctags -R --exclude=.git --exclude=node_modules --exclude=log -f tags")
end)
vim.keymap.set('n', '<Leader>tt', ":%! typos -w -<cr>")
local opts = { noremap=true, silent=true }
vim.keymap.set('n', '<Leader>o', vim.cmd.only, opts)
vim.keymap.set('n', '<Leader>i', function() vim.cmd.edit("~/.config/nvim/init.lua") end, opts)
vim.keymap.set('n', '<S-Left>', vim.cmd.bp, opts)
vim.keymap.set('n', '_', vim.cmd.bp, opts)
vim.keymap.set('n', '<S-Right>', vim.cmd.bn, opts)
vim.keymap.set('n', '+', vim.cmd.bn, opts)
vim.keymap.set('n', '<Leader>lg', vim.cmd.LazyGit, opts)
vim.keymap.set('n', '<Leader>[', function() vim.cmd.diffget('//2') end, opts)
vim.keymap.set('n', '<Leader>]', function() vim.cmd.diffget('//3') end, opts)
vim.keymap.set('n', '<space>e', vim.diagnostic.open_float, opts)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist, opts)
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

vim.g.netrw_banner = 0
vim.g.netrw_browse_split = 1
vim.g.netrw_winsize = 25

-- Sonic Pi
vim.g.sonic_pi_command = 'sonic-pi-tool'
vim.g.sonic_pi_stop = 'stop'
vim.g.sonic_pi_logs = 'logs'
vim.g.sonic_pi_record = 'record'
