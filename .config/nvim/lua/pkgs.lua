local gh = function(x) return "https://github.com/".. x end

vim.pack.add {
  { src = gh("vague-theme/vague.nvim"), version = "v2.1.4"},
  { src = gh("stevearc/conform.nvim")},
  { src = gh("stevearc/oil.nvim")},
  { src = gh("nvim-treesitter/nvim-treesitter")},
  { src = gh("nvim-mini/mini.nvim")},
  { src = gh("ibhagwan/fzf-lua")},
  { src = gh("saghen/blink.cmp"), version = "v1.8.0"},
  { src = gh("shortcuts/no-neck-pain.nvim")},
  { src = gh("FylerOrg/fyler.nvim")},
  { src = gh("dlyongemallo/diffview-plus.nvim")},
  { src = gh("noisesfromspace/touchup.nvim")},
}
