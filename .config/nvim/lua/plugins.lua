vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function(use)
  use {
    'tanvirtin/vgit.nvim',
    requires = {
      'nvim-lua/plenary.nvim'
    }
  }
end)

require('vgit').setup()
