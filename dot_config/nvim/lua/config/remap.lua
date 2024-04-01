-- From ThePrimeagen: https://github.com/ThePrimeagen/init.lua/blob/master/lua/theprimeagen/remap.lua
--

local utils = require("custom.utils")

local nmap = utils.nmap
local map = utils.map

vim.g.mapleader = " "
nmap("<leader>fv", vim.cmd.Ex)

map("t", "<esc><esc>", "<C-\\><C-n>")

map("i", "jk", "<esc>")

map("v", "J", ":m '>+1<CR>gv=gv")
map("v", "K", ":m '<-2<CR>gv=gv")

-- Joining lines keeps cursor in its position
nmap("J", "mzJ`z")

-- Jumping through the buffer and within search terms keeps cursor in the
-- middle of the screen
nmap("<C-d>", "<C-d>zz", { desc = "Jump down with cursor in the middle of the screen" })
nmap("<C-u>", "<C-u>zz", { desc = "Jump up with cursor in the middle of the screen" })
nmap("n", "nzzzv")
nmap("N", "Nzzzv")

-- Better window moving
nmap("<leader>wl", "<C-w>l")
nmap("<leader>wh", "<C-w>h")
nmap("<leader>wk", "<C-w>k")
nmap("<leader>wj", "<C-w>j")

nmap("<C-l>", "<C-w>l")
nmap("<C-h>", "<C-w>h")
nmap("<C-k>", "<C-w>k")
nmap("C-j>", "<C-w>j")

nmap("<leader>wv", "<C-w>v<C-w>l")
nmap("<leader>ws", "<C-w>s<C-w>j")
nmap("<leader>wc", "<C-w>c")
nmap("<leader>w=", "<C-w>=")
nmap("<leader>wf", "<C-w>|")

-- nmap("<C-h>", "<cmd>ZellijNavigateLeft<cr>")
-- nmap("<C-j>", "<cmd>ZellijNavigateDown<cr>")
-- nmap("<C-k>", "<cmd>ZellijNavigateUp<cr>")
-- nmap("<C-l>", "<cmd>ZellijNavigateUp<cr>")
--
-- Paste over selection without overriding the vim paste register with the
-- substituted string. Doing so by yanking into the void register
map("x", "<leader>p", [["_dP]])

-- Yanks on the clipboard instead on vim's paste register
map({ "n", "v" }, "<leader>y", [["+y]])
nmap("<leader>Y", [["+Y]])

-- Deletes to the void register (does not yank into the main register)
map({ "n", "v" }, "<leader>d", [["_d]])

nmap("Q", "<nop>")
-- nmap("<C-f>", "<cmd>silent !tmux neww tmux-sessionizer<CR>")

-- formats the file
nmap("<leader>F", vim.lsp.buf.format)

-- Quick fix navigation with j and k
nmap("[d", "<cmd>cnext<CR>zz")
nmap("]d", "<cmd>cprev<CR>zz")
nmap("<leader>k", "<cmd>lnext<CR>zz")
nmap("<leader>j", "<cmd>lprev<CR>zz")

-- Lets you replace the word you have the cursor on
nmap("<leader>s", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]])


-- Makes a file executagle without leaving vim
-- nmap("<leader>x", "<cmd>!chmod +x %<CR>", { silent = true })

-- nmap("<leader>xs", "<cmd>source ~/.config/nvim/lua/custom/snippets.lua<cr>")
nmap("<leader>xs", function ()
    require("luasnip").cleanup()
    R("custom.snippets")
end)

nmap("gx", ":silent !open <cWord><cr>")


-- nmap("<leader>wr", utils.resetConfig)
-- nmap("<leader>vpp", "<cmd>e ~/.dotfiles/nvim/.config/nvim/lua/theprimeagen/packer.lua<CR>");
-- nmap("<leader>mr", "<cmd>CellularAutomaton make_it_rain<CR>");
