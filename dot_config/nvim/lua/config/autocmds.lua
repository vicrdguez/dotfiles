-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua
-- Add any additional autocmds here

local function augroup(name)
    return vim.api.nvim_create_augroup("vic" .. name, { clear = true })
end

-- resize splits if window got resized
vim.api.nvim_create_autocmd({ "VimResized" }, {
    group = augroup("resize_splits"),
    callback = function()
        vim.cmd("tabdo wincmd =")
    end,
})

-- vim.api.nvim_create_autocmd({ "FileType" }, {
--     group = augroup("gradleFt"),
--     pattern = { "*.gradle" },
--     callback = function()
--         vim.bo.filetype = "groovy"
--     end,
-- })

vim.api.nvim_create_autocmd({ "FileType" }, {
    group = augroup("markdownFt"),
    pattern = { "*.md" },
    callback = function()
        -- vim.bo.filetype = "groovy"
        vim.cmd("NoNeckPain")
    end,
})
