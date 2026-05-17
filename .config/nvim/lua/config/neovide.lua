-- neovide.lua

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

