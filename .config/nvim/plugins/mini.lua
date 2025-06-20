local is_gui = vim.fn.has("gui_running")

return {
  { "echasnovski/mini.ai", lazy = false },
  {
    "echasnovski/mini.files",
    dependencies = { "echasnovski/mini.icons" },
    opts = {
      content = {
        prefix = (not is_gui) and function() end or nil,
        filter = function(fs_entry)
          if fs_entry.name == ".DS_Store" then return false else return true end
        end,
      }
    },
    keys = {
      {
        '<leader>d',
        function()
          local MiniFiles = require('mini.files')
          if not MiniFiles.close() then MiniFiles.open(nil, false) end
        end },
      {
        '<leader>w',
        function()
          local MiniFiles = require('mini.files')
          if not MiniFiles.close() then
            require('mini.files').open(vim.api.nvim_buf_get_name(0), false)
          end
        end },
    },
  },
  { "echasnovski/mini.comment", lazy = false },
}
