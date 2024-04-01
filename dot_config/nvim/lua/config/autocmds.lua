local augroup = require("custom.utils").augroup
local nmap = require("custom.utils").nmap
-- resize splits if window got resized
-- vim.api.nvim_create_autocmd({ "VimResized" }, {
--     group = augroup("resize_splits"),
--     callback = function()
--         vim.cmd("tabdo wincmd =")
--     end,
-- })


-- for some reason, nvim is able to detect main.tf as a terraform file, but not other files such as
-- variables.tf. This solves that
vim.api.nvim_create_autocmd({ "BufEnter" }, {
    group = augroup("terraform"),
    pattern = { "*.tf", "*.tfvars" },
    callback = function()
        vim.opt.filetype = "terraform"
        -- also comment string is not detected
        vim.opt.commentstring = "# %s"
    end
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
        vim.opt.colorcolumn = ""
    end
})

-- vim.api.nvim_create_autocmd("FileType", {
--     group = augroup("md-no-neck-pain"),
--     pattern = "markdown",
--     callback = function(p)
--                 vim.schedule(function()
--                     if _G.NoNeckPain.state ~= nil and _G.NoNeckPain.state.enabled then
--                         return
--                     end
--
--                     if NoNeckPain.enable() ~= nil then
--                         vim.api.nvim_del_autocmd(p.id)
--                     end
--                 end)
--             end,
--     -- callback = function()
--     --     -- vim.cmd("NoNeckPain")
--     --     -- vim.schedule(function ()
--     --     --     _G.NoNeckPain.enable()
--     --     --
--     --     -- vim.cmd(":NoNeckPain")
--     --     -- end)
--     --     _G.NoNeckPain.enable()
--     --     -- vim.schedule(function ()
--     --     --     vim.cmd("wincmd l")
--     --     -- end)
--     -- end
-- })

vim.api.nvim_create_autocmd("FileType", {
    group = augroup("nix"),
    pattern = "nix",
    callback = function()
        vim.opt.tabstop = 4
        vim.opt.softtabstop = 4
        vim.opt.shiftwidth = 4
        vim.opt.commentstring = "# %s"
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

vim.api.nvim_create_autocmd({
    "FileChangedShellPost",
    "Syntax",
    "TextChanged",
    "InsertLeave",
    -- "FileType",
    "WinScrolled"
}, {
    group = require("custom.utils").augroup("md_prettify"),
    pattern = "*",
    callback = function ()
        local md = require("custom.markdown")
        md.md_prettify()
    end
})
--
--
