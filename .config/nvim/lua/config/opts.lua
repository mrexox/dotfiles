-- opts.lua
--
-- Configuration of nvim global options.

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
vim.o.guifont = "FiraMono Nerd Font:h18"
vim.opt.colorcolumn = "120"
vim.opt.swapfile = false

-- Faster load of large Ruby files
vim.g.polyglot_disabled = {'ruby'}

-- Speedup
vim.g.lazyredraw = true
vim.g.ttyfast = true
vim.g.regexpengine = 1

-- Big file mode
vim.g.bigfile_mode = false
local open_ok, fd = pcall(vim.uv.fs_open, vim.api.nvim_buf_get_name(0), "r", 438)
local stat_ok, stat = pcall(vim.uv.fs_fstat, fd)
if open_ok and stat_ok and stat.size > 1000 * 1000 then
  vim.g.bigfile_mode = true
  vim.uv.fs_close(fd)
end
