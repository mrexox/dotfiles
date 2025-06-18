return {
  -- Mise CLI
  { "ejrichards/mise.nvim" },
  -- Better search results
  {
    'romainl/vim-cool', init = function()
      vim.g.cool_total_matches = 1
    end,
  },
  {
    'nvim-lualine/lualine.nvim',
    dependencies = {
      "nvim-tree/nvim-web-devicons",
      "Shatur/neovim-ayu",
    },
    opts =  {
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
      },
    },
  },
  -- gcc to comment
  { "tpope/vim-commentary" },
  { "mhinz/vim-startify" },
  { "ap/vim-css-color" },
  { "editorconfig/editorconfig-vim" },
  { "bluz71/vim-moonfly-colors", init = function()
      vim.cmd.colorscheme("moonfly")
    end,
  },
  { "akinsho/bufferline.nvim", version = "*", dependencies = "nvim-tree/nvim-web-devicons", opts = {
      options = {
        indicator = {
          icon = nil,
          style = 'underline',
        },
        themable = true,
        show_close_icons = false,
        show_buffer_close_icons = false,
      }
    },
    keys = {
      { "<leader>1", "<cmd>BufferLineCloseOthers<cr>" },
    }
  },
  {
    "kazhala/close-buffers.nvim",
    name = "close_buffers",
    opts = {
      filetype_ignore = {},
      file_glob_ignore = {},
      file_regex_ignore = {},
      preserve_window_layout = { 'this', 'nameless' },
      next_buffer_cmd = nil,
    },
    keys = {
      { "|", function() require("close_buffers").wipe({ type = "this" }) end }
    },
  },
}
