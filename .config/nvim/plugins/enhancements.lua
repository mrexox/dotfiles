local is_gui = vim.fn.has("gui_running")

return {
  -- Mise CLI
  { "ejrichards/mise.nvim", opts = {} },
  -- Better search results
  {
    "romainl/vim-cool", init = function()
      vim.g.cool_total_matches = 1
    end,
  },
  {
    "nvim-lualine/lualine.nvim",
    dependencies = {
      "Shatur/neovim-ayu",
    },
    opts =  {
      options = {
        icons_enabled = is_gui,
        theme = "ayu",
        tabline = {
          lualine_a = {},
          lualine_b = {"branch"},
          lualine_c = {"filename"},
          lualine_x = {},
          lualine_y = {},
          lualine_z = {},
        },
      },
    },
  },
  { "mhinz/vim-startify" },
  { "ap/vim-css-color" },
  { "editorconfig/editorconfig-vim" },
  { "bluz71/vim-moonfly-colors", init = function()
      vim.cmd.colorscheme("moonfly")
    end,
  },
  {
    "akinsho/bufferline.nvim",
    name = "bufferline",
    version = "*",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    init = function()
      vim.opt.termguicolors = true
      vim.keymap.set("n", "<leader>1", "<cmd>BufferLineCloseOthers<cr>")
    end,
    opts = {
      options = {
        indicator = {
          icon = nil,
          style = "underline",
        },
        themable = true,
        show_close_icons = false,
        show_buffer_close_icons = false,
        show_buffer_icons = is_gui,
      }
    },
  },
  {
    "kazhala/close-buffers.nvim",
    name = "close_buffers",
    opts = {
      filetype_ignore = {},
      file_glob_ignore = {},
      file_regex_ignore = {},
      preserve_window_layout = { "this", "nameless" },
      next_buffer_cmd = nil,
    },
    keys = {
      { "|", function() require("close_buffers").wipe({ type = "this" }) end }
    },
  },
}
