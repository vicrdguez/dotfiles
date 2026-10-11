-- Set <space> as the leader key
-- This must happen before plugins are loaded
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Should be true only if you have a Nerd Font installed
vim.g.have_nerd_font = false

-- [[ Options ]]
-- :help vim.opt
-- :help option-list
local opt = vim.opt

-- I hate files folding just because
opt.foldenable = false

-- Relative line numbers
opt.number = true
opt.relativenumber = true

-- Mouse mode
opt.mouse = "a"

-- Don't show the mode below the statusline, already in there
opt.showmode = false

-- Search is case-insensitive unless \C or one or more capitals in the search
opt.ignorecase = true
opt.smartcase = true

-- Sign  column always on
opt.signcolumn = "yes"

-- Shorter update time
opt.updatetime = 250

-- Decrease mapped sequence wait time
-- which-key pops up sooner
opt.timeoutlen = 300

-- Always split in these directions
opt.splitright = true
opt.splitbelow = true

-- Sets how certain whitespace characters should be displayed
opt.list = true
opt.listchars = { tab = "» ", trail = "·", nbsp = "␣" }

-- Live substitutions preview
opt.inccommand = "split"

-- Enable cursorline
opt.cursorline = true

-- Smaller number of lines bellow cursor
opt.scrolloff = 10

-- Smart 4 space indents
opt.tabstop = 4
opt.softtabstop = 4
opt.shiftwidth = 4
opt.smartindent = true
opt.autoindent = true

--
-- Spaces instead of tabs
opt.expandtab = true

-- Wrap lines
opt.wrap = true
-- Break indent so wrapped lines stay visually indented
opt.breakindent = true

-- concealing
-- 3 hides concealed text even when the query defines a replacement, so &lt;
-- &gt; &amp; vanish instead of rendering as < > &
-- opt.conceallevel = 3
opt.conceallevel = 2
opt.concealcursor = "nvic"
opt.textwidth = 0

-- @class
opt.colorcolumn = "100"
-- opt.colorcolumn.guifg = "Grey"
-- opt.colorcolumn.guibg = "1"
--
--
-- Treesitter Folds
vim.wo[0][0].foldexpr = "v:lua.vim.treesitter.foldexpr()"
vim.wo[0][0].foldmethod = "expr"

-- Treesitter indents
vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"

-- Window borders
vim.o.winborder = "rounded"

-- No swapfiles or backup files but use native unotree
opt.swapfile = false
opt.backup = false
opt.undodir = vim.fn.stdpath("data") .. "/undodir"
-- Save undo history
opt.undofile = true

-- Diagnostic config
vim.diagnostic.config({
    signs = {
        text = {
            [vim.diagnostic.severity.ERROR] = "󰅙",
            [vim.diagnostic.severity.WARN] = "󰀨",
            [vim.diagnostic.severity.INFO] = "󰋼",
            [vim.diagnostic.severity.HINT] = "󰋗",
        },
    },
})
