local function on_attach(bufnr)
  local api = require "nvim-tree.api"

  local function opts(desc)
    return { desc = "nvim-tree: " .. desc, buffer = bufnr, noremap = true, silent = true, nowait = true }
  end

  -- default mappings
  api.config.mappings.default_on_attach(bufnr)

  -- custom mappings
  vim.keymap.set("n", "?",     api.tree.toggle_help,                opts("Help"))
  vim.keymap.set("n", "l",     api.node.open.edit,                  opts("Edit"))
  vim.keymap.set("n", "h",     api.tree.change_root_to_parent,      opts("Up"))
end

return {
  {
    "kyazdani42/nvim-tree.lua",
    dependencies = "nvim-lua/plenary.nvim",
    opts = {
      on_attach = on_attach,
      view = {
        adaptive_size = true,
        side = "right",
      },
      actions = {
        change_dir = {
          enable = true,
          global = true,
        },
      },
      renderer = {
        indent_markers = {
          enable = false
        },
        icons = {
          webdev_colors =  false,
          show = {
            file = false,
            folder = false,
            folder_arrow = false,
            git = false,
          }
        }
      }
    },
    keys = {
      {
        "<leader>t",
        function()
          local cwd = vim.fn.getcwd()
          vim.cmd.NvimTreeToggle(cwd)
        end,
        noremap = true,
        silent = true,
      },
      {
        "<leader>c",
        function()
          local curdir = vim.fn.expand("%:p:h")
          vim.cmd.NvimTreeToggle(curdir)
        end,
        noremap = true,
        silent = true,
      },
    },
  },
}
