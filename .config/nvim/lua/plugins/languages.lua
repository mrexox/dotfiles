return {
  {
    "sheerun/vim-polyglot",
    dependencies = { "ejrichards/mise.nvim" },
    init = function()
      vim.g.ruby_path = os.execute("mise which ruby")
    end,
  },
  { "ollykel/v-vim" },
  {
    "keith/rspec.vim" ,
    ft = "ruby",
    lazy = true,
  },
  {
    "vim-crystal/vim-crystal",
    ft = "crystal",
    lazy = true,
    init = function()
      vim.g.crystal_auto_format = 1
    end,
  },
  {
    "rust-lang/rust.vim",
    ft = "rust",
    lazy = true,
    init = function()
      vim.g.rustfmt_autosave = 1
    end,
  },
  {
    "mrcjkb/rustaceanvim",
    version = "^6",
    lazy = true,
    config = function()
      vim.keymap.set(
        "n",
        "K",
        function()
          vim.cmd.RustLsp({'hover', 'actions'})
        end,
        { silent = true }
        )
    end,
  },
  {
      "fatih/vim-go",
      lazy = true,
      ft = "go",
      build = "GoUpdateBinaries",
      dependencies = { "ejrichards/mise.nvim" },
      keys = {
          { "<Leader>gd", "<cmd>GoDocBrowser<cr>" },
      },
  },
}
