-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here
local utils = require("utils")
local map = utils.map
local nmap = utils.nmap
map("v", "J", ":m '>+1<CR>gv=gv")
map("v", "K", ":m '<-2<CR>gv=gv")

-- Joining lines keeps cursor in its position
nmap("J", "mzJ`z")
nmap("jk", "<esc>")
nmap("<leader>wv", "<C-w>v<C-w>l")
nmap("<leader>ws", "<C-w>s<C-w>j")

-- Jumping through the buffer and within search terms keeps cursor in the
-- middle of the screen
nmap("<C-d>", "<C-d>zz", { desc = "Jump down with cursor in the middle of the screen" })
nmap("<C-u>", "<C-u>zz", { desc = "Jump up with cursor in the middle of the screen" })
nmap("n", "nzzzv")
nmap("N", "Nzzzv")
--
-- Quick fix navigation with j and k
nmap("<C-j>", "<cmd>cnext<CR>zz")
nmap("<C-k>", "<cmd>cprev<CR>zz")
nmap("<leader>k", "<cmd>lnext<CR>zz")
nmap("<leader>j", "<cmd>lprev<CR>zz")
-- Make current dile executable
nmap("<leader>x", "<cmd>!chmod +x %<CR>", { silent = true })
--
-- Yanks on the clipboard instead on vim's paste register
map({ "n", "v" }, "<leader>y", [["+y]])
nmap("<leader>Y", [["+Y]])
-- Deletes to the void register (does not yank into the main register)
-- map({ "n", "v" }, "<leader>d", [["_d]])

-- Better window moving
nmap("<leader>wl", "<C-w>l")
nmap("<leader>wh", "<C-w>h")
nmap("<leader>wk", "<C-w>k")
nmap("<leader>wj", "<C-w>j")
-- nmap("<leader>wv", "<C-w>v<C-w>l")
-- nmap("<leader>ws", "<C-w>s<C-w>j")
nmap("<leader>wv", "<C-w>v")
nmap("<leader>ws", "<C-w>s")
nmap("<leader>wc", "<C-w>c")

nmap("<leader>by", function()
    vim.notify(vim.fn.expand("%"))
    -- vim.fn.setreg('"+', "hello")
    vim.fn.setreg('"+', vim.fn.expand("%"))
end, { desc = "Copy buf relative path" })

nmap("<leader>bY", function()
    vim.notify(vim.fn.expand("%"))
    -- vim.fn.setreg('"+', "hello")
    vim.fn.setreg("*", vim.fn.expand("%"))
end, { desc = "Copy buf relative path to clipboard" })
