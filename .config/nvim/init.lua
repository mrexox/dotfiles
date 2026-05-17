-- global options
require("config.opts")
-- keymaps
require("config.keymaps")
-- Neovide GUI options
require("config.neovide")
-- Plugins
require("config.lazy")

-- Delete whitespaces on :w
vim.api.nvim_create_autocmd("BufWritePre", { command = "%s/\\s\\+$//e" })

-- Custom commands
vim.api.nvim_create_user_command("ToHex", "%!xxd", {bang = true})
vim.api.nvim_create_user_command("FromHex", "%!xxd -r", {bang = true})

-- Extend PATH
vim.env.PATH = table.concat({
  vim.env.HOME .. "/.local/bin",
  vim.env.HOME .. "/go/bin",
  vim.env.HOME .. "/bin",
  vim.env.PATH
}, ":")

-- Highlights tweaks
vim.cmd([[
highlight ColorColumn ctermbg=black ctermfg=red guibg=Black
highlight Folded ctermbg=black
highlight FoldColumn ctermbg=NONE
highlight SignColumn ctermbg=NONE
highlight DiffDelete ctermbg=160 ctermfg=NONE
highlight DiffAdd ctermbg=28 ctermfg=NONE
highlight DiffChange ctermbg=black
highlight SignColumn ctermbg=black guibg=Black
highlight LineNr ctermbg=NONE guibg=NONE
highlight Search ctermfg=0 ctermbg=175 guifg=Black guibg='#FF52A0'
highlight CurSearch guifg=Black guibg='#FFA47F'
highlight Pmenu ctermbg=black ctermfg=255 guibg=Black
highlight link MiniPickMatchCurrent FloatTitle
]])
