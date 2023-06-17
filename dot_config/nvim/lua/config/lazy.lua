local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
        "git",
        "clone",
        "--filter=blob:none",
        "https://github.com/folke/lazy.nvim.git",
        "--branch=stable", -- latest stable release
        lazypath,
    })
end
vim.opt.rtp:prepend(lazypath)

vim.g.mapleader = " "

require('lazy').setup({
    spec = {
        -- Load plugins from the plugins folder
        { import = 'plugins' },
    },
    defaults = {
        version = false
    },
    dev = {
        path = "~/dev/neovim"
    },
    install = {
        colorscheme = { "kanagawa-wave" }
    },
    performance = {
        cache = {
            enabled = true,
            ttl = 1000
        },
        rtp = {
            disabled_plugins = {
                "gzip",
                -- "matchit",
                -- "matchparen",
                -- "netrwPlugin",
                "tarPlugin",
                "tohtml",
                "tutor",
                "zipPlugin",
            }
        }
    }
})
