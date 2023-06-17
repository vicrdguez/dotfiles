local augroup = require("custom.utils").augroup

-- resize splits if window got resized
vim.api.nvim_create_autocmd({ "VimResized" }, {
    group = augroup("resize_splits"),
    callback = function()
        vim.cmd("tabdo wincmd =")
    end,
})


vim.api.nvim_create_autocmd("FileType", {
    group = augroup("md_fo"),
    pattern = "markdown",
    callback = function()
        vim.notify("Seting format options for markdown")
        vim.opt.textwidth = 100
        vim.opt.formatoptions = "jtcroql"
        vim.opt.wrap = false
    end
})

vim.api.nvim_create_autocmd("FileType", {
    group = augroup("md-no-neck-pain"),
    pattern = "markdown",
    callback = function()
        vim.cmd("NoNeckPain")
    end
})


-- [[ Highlight on yank ]]
-- See `:help vim.highlight.on_yank()`
-- local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
    callback = function()
        vim.highlight.on_yank()
    end,
    group = augroup("YankHighlight"),
    pattern = '*',
})
