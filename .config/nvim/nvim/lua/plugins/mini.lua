local is_gui = vim.fn.has("gui_running") == 1

return {
  -- { "lewis6991/gitsigns.nvim", opts = {} },
  -- { "nvim-mini/mini.statusline", version = false, opts = {} },
  { "echasnovski/mini.ai", lazy = false },
  { "echasnovski/mini.jump", lazy = false, opts = {}, },
  { "echasnovski/mini.comment", lazy = false },
  { "nvim-mini/mini.pick",
    lazy = false,
    version = "*",
    opts = {
      mappings = {
        choose_marked = "<C-q>",
      },
    },
    keys = {
      { "<leader>f", "<cmd>Pick files<cr>", noremap = true, silent = true },
      { "<leader>s", "<cmd>Pick grep_live<cr>", noremap = true, silent = true },
    },
  },
  {
    "echasnovski/mini.snippets",
    config = function()
      local gen_loader = require('mini.snippets').gen_loader
      require('mini.snippets').setup({
          snippets = {
            gen_loader.from_file('~/.config/nvim/snippets/global.lua'),
          },
        })
    end,
  },
  {
    "echasnovski/mini.move",
    lazy = false,
    opts = {
      mappings = {
        -- Move visual selection in Visual mode. Defaults are Alt (Meta) + hjkl.
        left = 'ħ', -- meta-h
        right = 'ł', -- meta-l
        down = '˝', -- meta-j
        up = '˚', -- meta-k

        -- -- Move current line in Normal mode
        line_left = 'ħ', -- meta-h
        line_right = 'ł', -- meta-l
        line_down = '˝', -- meta-j
        line_up = '˚', -- meta-k
      },
    },
      },
  {
    "echasnovski/mini.files",
    dependencies = { "echasnovski/mini.icons" },
    opts = {
      content = {
        prefix = (not is_gui) and (function() end) or nil,
        filter = function(fs_entry)
          if fs_entry.name == ".DS_Store" then return false else return true end
        end,
      },
      mappings = {
        go_in = 'L',
        go_in_plus = 'l',
      },
    },
    keys = {
      {
        '<leader>d',
        function()
          local MiniFiles = require('mini.files')
          if not MiniFiles.close() then MiniFiles.open() end
        end
      },
      {
        '<leader>D',
        function()
          local MiniFiles = require('mini.files')
          if not MiniFiles.close() then MiniFiles.open(nil, false) end
        end
      },
      {
        '<leader>w',
        function()
          local MiniFiles = require('mini.files')
          if not MiniFiles.close() then
            require('mini.files').open(vim.api.nvim_buf_get_name(0), false)
          end
        end
      },
    },
  },
}
