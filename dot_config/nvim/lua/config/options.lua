-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here
--
local opt = vim.opt

opt.clipboard = ""
opt.scrolloff = 10
opt.signcolumn = "yes"
opt.isfname:append("@-@")
opt.colorcolumn = "120"
opt.laststatus = 3
-- opt.textwidth = 120
opt.linebreak = true
opt.wrap = true
opt.breakindent = false

-- -- smart 4 space indents
opt.tabstop = 4
opt.softtabstop = 4
opt.shiftwidth = 4
opt.expandtab = true
opt.smartindent = true

-- avoid vim creating swap files and rely on undotree instead
opt.swapfile = false
opt.backup = false
vim.opt.undodir = os.getenv("HOME") .. "/.local/share/nvim/undodir"
opt.undofile = true
