return {
  -- View git blame on a line
  {
    "APZelos/blamer.nvim",
    init = function()
      vim.g.blamer_date_format = "%d.%m.%Y"
      vim.g.blamer_delay = 500
      vim.g.blamer_enabled = 0
      vim.cmd([[
        highlight Blamer ctermfg=darkgray guifg=darkgray
        ]])
    end,
    keys = {
      { "<leader>b", "<cmd>BlamerToggle<cr>", noremap = true, silent = true },
    },
  },
  -- Quick searching content and files
  {
    "ibhagwan/fzf-lua", opts = {
      defaults = {
        file_icons = false,
      },
    },
    keys = {
      { "<leader>f", "<cmd>FzfLua git_files<cr>", noremap = true, silent = true },
      { "<leader>p", "<cmd>FzfLua files<cr>", noremap = true, silent = true },
      { "<leader>s", "<cmd>FzfLua live_grep<cr>", noremap = true, silent = true },
      { "<leader>l", "<cmd>FzfLua buffers<cr>", noremap = true, silent = true },
      { "<leader>/", "<cmd>FzfLua lgrep_curbuf<cr>", noremap = true, silent = true },
      { "<leader>a", "<cmd>FzfLua grep_cword<cr>", noremap = true, silent = true },
      { "<leader>a", "<cmd>FzfLua grep_visual<cr>", mode = "v", noremap = true, silent = true },
      { "<leader>A", function()
          local curdir = vim.fn.expand("%:p:h")
          require("fzf-lua").grep_cword({ cwd = curdir })
      end, noremap = true, silent = true },
      { "<leader>A", function()
          local curdir = vim.fn.expand("%:p:h")
          require("fzf-lua").grep_visual({ cwd = curdir })
      end, mode = "v", noremap = true, silent = true },
    },
  },
  -- Open file on line
  {
    "mrexox/github-open.nvim",
    keys = {
      { "<leader>gh", function() require("github-open").open_file() end, },
      { "<leader>gl", function() require("github-open").open_line() end, },
      { "<leader>gb", function() require("github-open").open_blame_line() end, },
      { "<leader>gc", function() require("github-open").open_commit() end, },
    },
    dev = true,
  },
  {
    "kdheepak/lazygit.nvim",
    lazy = true,
    cmd = {
      "LazyGit",
      "LazyGitConfig",
      "LazyGitCurrentFile",
      "LazyGitFilter",
      "LazyGitFilterCurrentFile",
    },
    -- optional for floating window border decoration
    dependencies = {
      "nvim-lua/plenary.nvim",
    },
    -- setting the keybinding for LazyGit with "keys" is recommended in
    -- order to load the plugin when the command is run for the first time
    keys = {
      { "<leader>lg", "<cmd>LazyGit<cr>", desc = "LazyGit" }
    }
  }
}
