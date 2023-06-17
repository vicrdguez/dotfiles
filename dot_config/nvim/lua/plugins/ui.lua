return {
    -- THEMES
    -- -----
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
    {
        "catppuccin/nvim",
        name = "catppuccin",
        opts = {
            flavour = "mocha",
            dim_inactive = {
                enabled = true,
            },
            integrations = {
                cmp = true,
                gitsigns = true,
                nvimtree = true,
                telescope = true,
                notify = false,
                mini = false,
                alpha = true,
                lsp_trouble = true,
                treesitter = true,
                markdown = true,
            },
        },
    },

    -- Configure LazyVim to load gruvbox
    {
        "LazyVim/LazyVim",
        opts = {
            colorscheme = "catppuccin",
            -- colorscheme = "kanagawa-wave",
            -- colorscheme = "tokyonight-night",
        },
    },
    -- UI IMPROVEMENTS
    {
        "nvim-lualine/lualine.nvim",
        enabled = false,
        event = "VeryLazy",
        opts = function(_, opts)
            opts.theme = "catppuccin"
            -- table.insert(opts.theme = "catpuccin")
        end,
    },
    {
        "freddiehaddad/feline.nvim",
        lazy = false,
        -- dependencies = { "catppuccin/nvim" },
        init = function()
            require("feline").setup({
                components = require("catppuccin.groups.integrations.feline").get(),
            })
        end,
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
        enabled = false, -- creates annoying cursot blink when notifications are shown
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
        "Jxstxs/conceal.nvim",
        dependencies = { "nvim-treesitter/nvim-treesitter" },
        enabled = false,
    },
    {
        "folke/noice.nvim",
        enabled = true,
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
