-- keymaps.lua

local keymaps = {
  { "<leader>ct", function() io.popen("/opt/homebrew/bin/ctags --languages=ruby -R --exclude=.git --exclude=node_modules --exclude=log -f tags") end, },
  { "<leader>tt", ":%! typos -w -<cr>" },
  { "<leader>tt", ":%! typos -w -<cr>" },
  { "<leader>o", vim.cmd.only },
  { "<leader>i", function() vim.cmd.edit("~/.config/nvim/init.lua") end },
  { "<S-Left>", vim.cmd.bp },
  { "_", vim.cmd.bp },
  { "<S-Right>", vim.cmd.bn },
  { "+", vim.cmd.bn },
  { "<leader>lg", vim.cmd.LazyGit },
  { "<leader>[", function() vim.cmd.diffget('//2') end },
  { "<leader>]", function() vim.cmd.diffget('//3') end },
  { "<space>e", vim.diagnostic.open_float },
  { "[d", vim.diagnostic.goto_prev },
  { "]d", vim.diagnostic.goto_next },
  { "<space>q", vim.diagnostic.setloclist },
}

for _, keymap in ipairs(keymaps) do
  vim.keymap.set("n", keymap[1], keymap[2], { noremap = true, silent = true })
end

