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
        vim.opt.textwidth = 100
        vim.opt.formatoptions = "jtcroql"
        vim.opt.wrap = true
        vim.opt.linebreak = true
        vim.opt.breakindent = true
    end
})

vim.api.nvim_create_autocmd("FileType", {
    group = augroup("md-no-neck-pain"),
    pattern = "markdown",
    callback = function()
        -- vim.cmd("NoNeckPain")
        -- vim.schedule(function ()
        --     _G.NoNeckPain.enable()
        -- end)
        _G.NoNeckPain.enable()
        -- vim.schedule(function ()
        --     vim.cmd("wincmd l")
        -- end)
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



-- updates managed dotfiles with chezmoi
-- Using nested autocmds here because otherwise it was executed once per pattern for the same file
vim.api.nvim_create_autocmd("BufWritePost", {
    callback = function(ev)
        require("custom.chezmoi").chezmoi_add_if_changed(ev)
        -- if not added then
        --     vim.api.nvim_create_autocmd("BufWritePost", {
        --         callback = function(ev2)
        --             chezmoi_add_if_changed(ev2)
        --         end,
        --         group = augroup("chezmoi_add"),
        --         pattern = { "*.*" }
        --     })
        -- end
    end,
    group = augroup("chezmoi_add"),
    pattern = { "*.config/*" }
})

-- applies chezmoi source state to target
vim.api.nvim_create_autocmd("BufWritePost", {
    callback = function(ev)
        require("custom.chezmoi").chezmoi_apply_if_changed(ev)
    end,
    group = augroup("chezmoi_apply"),
    pattern = require("custom.chezmoi").chezmoi_get_source_path() .. "/*"
    -- pattern = "/Users/vrodriguez/.local/share/chezmoi".."/*"
})
--
--
