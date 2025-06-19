local is_gui = not not vim.g.neovide

return {
  { "echasnovski/mini.ai", lazy = false },
  {
    "echasnovski/mini.files",
    dependencies = { "echasnovski/mini.icons" },
    opts = is_gui and {} or {
      content = {
        prefix = function() end,
      }
    },
    keys = {
      { '<leader>d', function() require('mini.files').open() end, },
    },
  },
}
