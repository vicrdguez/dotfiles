return {
    {
        "rebelot/kanagawa.nvim",
        opts = {
            transparent = false,
        },
        init = function()
            -- vim.api.nvim_set_hl(0, "NormalFloat", { bg = "#1F1F28", blend = 0 })
            -- vim.api.nvim_set_hl(0, "FloatBorder", { bg = "#1F1F28" })
            -- vim.api.nvim_set_hl(0, "FloatTitle", { bg = "none" })
            -- vim.api.nvim_set_hl(0, "TelescopeNormal", { bg = "none" })
            -- vim.api.nvim_set_hl(0, "TelescopeBorder", { bg = "none" })
        end,
    },

    -- Configure LazyVim to load gruvbox
    {
        "LazyVim/LazyVim",
        opts = {
            colorscheme = "kanagawa-wave",
            -- colorscheme = "tokyonight-night",
        },
    },
    {
        "akinsho/bufferline.nvim",
        enabled = false,
    },
    {
        "echasnovski/mini.indentscope",
        enabled = false,
    },
    {
        "rcarriga/nvim-notify",
        enabled = false,
        config = function()
            require("notify").setup({
                background_colour = "#000000",
            })
        end,
    },
    {
        "stevearc/dressing.nvim",
        enabled = true,
    },
    {
        "folke/noice.nvim",
        config = function()
            require("noice").setup({
                views = {
                    cmdline_popup = {
                        border = {
                            style = "none",
                            padding = { 2, 3 },
                        },
                        filter_options = {},
                        win_options = {
                            winhighlight = "NormalFloat:NormalFloat,FloatBorder:FloatBorder",
                        },
                    },
                },
            })
        end,
    },
}
