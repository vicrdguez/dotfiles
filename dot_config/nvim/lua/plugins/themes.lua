return {
    {
        "catppuccin/nvim",
        name = "catppuccin",
        priority = 1000,
        opts = {
            flavour = "mocha",
            dim_inactive = {
                enabled = false,
                percentage = 0.05
            },
            transparent_background = true,
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
                -- headlines = true,
                noice = true
            },
            custom_highlights = function(colors)
                return {
                    FzfLuaTitle             = { fg = colors.mauve, bold = true },
                    FzfLuaNormal            = { bg = colors.mantle },
                    -- FzfLuaBorder            = { fg = colors.teal, bg = colors.crust },
                    -- FzfLuaCursor            = { fg = colors.maroon, bg = colors.yellow },
                    -- FzfLuaHelpNormal        = { bg = colors.overlay2 },
                    -- FzfLuaHelpBorder        = { bg = colors.overlay2, fg = colors.lavender },
                    -- FzfLuaCursorLine        = { bg = colors.maroon },
                    -- FzfLuaSearch            = { bg = colors.maroon },
                    -- FzfLuaScrollBorderEmpty = { bg = colors.maroon },
                    -- FzfLuaScrollBorderFull  = { bg = colors.maroon },
                    -- FzfLuaScrollFloatEmpty  = { bg = colors.maroon },
                    -- FzfLuaScrollFloatFull   = { bg = colors.maroon },
                    -- FzfLuaCursorLineNr      = { bg = colors.maroon },
                    FzfLuaPreviewNormal     = { bg = colors.crust },
                    FzfLuaPreviewBorder     = { bg = colors.mantle, fg = colors.lavender },
                }
            end
        },
    },

    {
        'rebelot/kanagawa.nvim',
        lazy = false,
        opts = {
            transparent = false,

            overrides = function(colors)
                local theme = colors.theme
                return {
                    -- NormalFloat = { bg = "none" },
                    FloatBorder = { bg = "none" },
                    FloatTitle = { bg = "none" },
                    -- Save an hlgroup with dark background and dimmed foreground
                    -- so that you can use it where your still want darker windows.
                    -- E.g.: autocmd TermOpen * setlocal winhighlight=Normal:NormalDark
                    -- NormalDark = { fg = theme.ui.fg_dim, bg = theme.ui.bg_m3 },

                    -- more uniform colors for the popup menu
                    -- Pmenu = { fg = theme.ui.shade0, bg = theme.ui.bg_p1 }, -- add `blend = vim.o.pumblend` to enable transparency
                    -- PmenuSel = { fg = "NONE", bg = theme.ui.bg_p2 },
                    -- PmenuSbar = { bg = theme.ui.bg_m1 },
                    -- PmenuThumb = { bg = theme.ui.bg_p2 },
                    -- Popular plugins that open floats will link to NormalFloat by default;
                    -- set their background accordingly if you wish to keep them dark and borderless
                    NormalFloat = { bg = theme.ui.bg_m3, fg = theme.ui.fg_dim, blend = 0 },
                    LazyNormal = { bg = theme.ui.bg_m3, fg = theme.ui.fg_dim },
                    MasonNormal = { bg = theme.ui.bg_m3, fg = theme.ui.fg_dim },
                    TelescopeTitle = { fg = theme.ui.special, bold = true },
                    TelescopePromptNormal = { bg = theme.ui.bg_p1 },
                    TelescopeResultsNormal = { fg = theme.ui.fg_dim, bg = theme.ui.bg_m1 },
                    TelescopePromptBorder = { fg = theme.ui.bg_p1, bg = theme.ui.bg_p1 },
                    TelescopeResultsBorder = { fg = theme.ui.bg_m1, bg = theme.ui.bg_m1 },
                    TelescopePreviewNormal = { bg = theme.ui.bg_dim },
                    TelescopePreviewBorder = { bg = theme.ui.bg_dim, fg = theme.ui.bg_dim },

                    FzfLuaTitle = { fg = theme.ui.special, bold = true },
                    FzfLuaNormal = { fg = theme.ui.fg_dim, bg = theme.ui.bg_m1 },
                    FzfLuaBorder = { fg = theme.ui.bg_m1, bg = theme.ui.bg_m1 },
                    FzfLuaCursor = { bg = theme.ui.bg_p1 },
                    FzfLuaHelpNormal = { bg = theme.ui.bg_dim },
                    FzfLuaHelpBorder = { bg = theme.ui.bg_dim, fg = theme.ui.bg_dim },
                }
            end
        }
    },
    {
        'rose-pine/neovim',
        lazy = true,
        name = 'rose-pine',
        config = function()
            require("rose-pine").setup()
            vim.cmd('colorscheme rose-pine')
        end,
        opts = {
            disable_background = false,
            disble_float_background = true
        },
    },
    {
        'folke/tokyonight.nvim',
        opts = {
            style = "storm",
            transparent = true
        }
    },
}
