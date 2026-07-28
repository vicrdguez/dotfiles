-- [[ General keymaps, not specific to any plugin ]] 
local nmap = require("lib.util").nmap
local map = require('lib.util').map

-- Confy buffer movements
nmap('<C-h>', '<C-w><C-h>', { desc = 'Ez move to the left buffer' })
nmap('<C-l>', '<C-w><C-l>', { desc = 'Ez move to the right buffer' })
nmap('<C-j>', '<C-w><C-j>', { desc = 'Ez move to the lower buffer' })
nmap('<C-k>', '<C-w><C-k>', { desc = 'Ez move to the upper buffer' })
nmap("<leader>wl", "<C-w>l", { desc = 'Ez move to the right buffer' })
nmap("<leader>wh", "<C-w>h", { desc = 'Ez move to the left buffer' })
nmap("<leader>wk", "<C-w>k", { desc = 'Ez move to the upper buffer' })
nmap("<leader>wj", "<C-w>j", { desc = 'Ez move to the lower buffer' })
nmap("<leader>wv", "<C-w>v<C-w>l", { desc = 'Split vertical' })
nmap("<leader>ws", "<C-w>s<C-w>j", { desc = 'Split horizontal' })
nmap("<leader>wc", "<C-w>c", { desc = 'Close buffer' })
nmap("<leader>w=", "<C-w>=", { desc = 'Balance buffer sizes' })
nmap("<leader>wf", "<C-w>|", { desc = '' })

-- Diagnostic keymaps
nmap('[d', vim.diagnostic.goto_prev, { desc = 'Go to previous [D]iagnostic message' })
nmap(']d', vim.diagnostic.goto_next, { desc = 'Go to next [D]iagnostic message' })
nmap('<leader>e', vim.diagnostic.open_float, { desc = 'Show diagnostic [E]rror messages' })
nmap('<leader>q', vim.diagnostic.setloclist, { desc = 'Open diagnostic [Q]uickfix list' })
-- Paste over selection without overriding the vim paste register with the
-- substituted string. Doing so by yanking into the void register
map("x", "<leader>p", [["_dP]])

-- Yanks on the clipboard instead on vim's paste register
map({ "n", "v" }, "<leader>y", [["+y]])
nmap("<leader>Y", [["+Y]])
-- Deletes to the void register (does not yank into the main register)
-- So deleting does not mess up what you yanked before
map({ "n", "v" }, "<leader>d", [["_d]])
-- Alternative to escape, if the keyboard one is not easy to reach
map("i", "jk", "<esc>")
-- In visual mode, use J,K to move selection down/up the buffer
map("v", "J", ":m '>+1<CR>gv=gv")
map("v", "K", ":m '<-2<CR>gv=gv")

-- Joining lines keeps cursor in its position
nmap("J", "mzJ`z")

-- Jumping through the buffer and within search terms keeps cursor in the
-- middle of the screen. Less confusing
nmap("<C-d>", "<C-d>zz", { desc = "Jump down with cursor in the middle of the screen" })
nmap("<C-u>", "<C-u>zz", { desc = "Jump up with cursor in the middle of the screen" })
nmap("n", "nzzzv")
nmap("N", "Nzzzv")

