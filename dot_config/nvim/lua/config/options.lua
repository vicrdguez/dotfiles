local utils = require("custom.utils")

local opt = vim.opt
-- Line numbers
opt.nu = true
opt.relativenumber = true

-- smart 4 space indents
opt.tabstop = 4
opt.softtabstop = 4
opt.shiftwidth = 4
opt.expandtab = true
opt.smartindent = true
opt.autoindent = true
opt.expandtab = true -- Use spaces instead of tabs
opt.wrap = true
opt.conceallevel = 1
opt.textwidth = 0

-- avoid vim creating swap files and rely on undotree instead
opt.swapfile = false
opt.backup = false
-- vim.opt.undodir = os.getenv("HOME") .. "/.vim/undodir"
opt.undolevels = 10000
opt.undofile = true

-- highlight in-file search results as you type using '/'
opt.hlsearch = true
opt.incsearch = true

opt.termguicolors = true

-- always keep 10 lines up and down as I scroll
opt.scrolloff = 10
opt.isfname:append("@-@")

vim.updatetime = 50

opt.colorcolumn = "100"
---@diagnostic disable-next-line: inject-field
opt.colorcolumn.guifg = "Grey"
opt.colorcolumn.guibg = "1"
opt.signcolumn = "number"
opt.laststatus = 3
opt.cmdheight = 0
opt.cursorline = true
opt.wildmode = "longest:full,full"
opt.ignorecase = true
opt.inccommand = "nosplit" -- preview incremental substitute
opt.grepformat = "%f:%l:%c:%m"


vim.diagnostic.config({
    virtual_text = true,
})

--vim.notify = require("notify")

vim.g.mapleader = " "

-- Resolving nix-profile paths, which are symlinks to the nix-store that are constantly changing
package.path = package.path .. ";" .. utils.real_path("~/.nix-profile/share/lua/5.1") .. "/?/init.lua;"
package.path = package.path .. ";" .. utils.real_path("~/.nix-profile/share/lua/5.1") .. "/?.lua;"
package.path = package.path .. ";" .. vim.fn.expand("$HOME") .. "/.luarocks/share/lua/5.1/?/init.lua;"
package.path = package.path .. ";" .. vim.fn.expand("$HOME") .. "/.luarocks/share/lua/5.1/?.lua;"

