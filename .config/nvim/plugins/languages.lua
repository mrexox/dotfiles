return {
  { "sheerun/vim-polyglot" },
  {
    "vim-crystal/vim-crystal",
    ft = "crystal",
    init = function()
      vim.g.crystal_auto_format = 1
    end,
  },
  {
    "rust-lang/rust.vim",
    ft = "rust",
    init = function()
      vim.g.rustfmt_autosave = 1
    end,
  },
  {
    "mrcjkb/rustaceanvim",
    version = "^6",
    lazy = false,
  },
  { "ollykel/v-vim" },
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
