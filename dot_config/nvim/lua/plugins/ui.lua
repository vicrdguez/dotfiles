-- vim.opt.laststatus = 3
--
-- vim.opt.fillchars:append({
--     horiz = '━',
--     horizup = '┻',
--     horizdown = '┳',
--     vert = '┃',
--     vertleft = '┨',
--     vertright = '┣',
--     verthoriz = '╋',
-- })

return {
    {
        'lukas-reineke/indent-blankline.nvim',
        main = "ibl",
        opts = {
            indent = {
                char = "┊"
            }
            -- whitespace = {
            --     char = '┊',
            -- },
            -- show_trailing_blankline_indent = false,
        }
    },
    {
        'nvim-lualine/lualine.nvim',
        enabled = true,
        event = 'VeryLazy',
        dependencies = {
            'nvim-tree/nvim-web-devicons',
        },
        opts = function(plugin)
            local icons = require("config").icons
            local theme = require("custom.lualine")
            return {
                options = {
                    icons_enabled = true,
                    theme = theme.kanagawa_dragon(),
                    -- component_separators = '|',
                    -- section_separators = '',
                    -- component_separators = { left = '', right = '' },
                    -- section_separators = { left = '', right = '' },
                    -- globalstatus = false,
                },
                -- sections = {
                --     lualine_b = {
                --         'branch',
                --         {
                --             'diff',
                --             symbols = {
                --                 added = icons.git.added,
                --                 modified = icons.git.modified,
                --                 removed = icons.git.removed
                --             }
                --         }
                --     },
                --     lualine_c = {
                --         {
                --             'diagnostics',
                --             -- sources = {'nvim_lsp', 'nvim_diagnostic'},
                --             -- sections = { 'error', 'warn', 'info', 'hint'},
                --             -- symbols = {error = 'E', warn = 'W', info = 'I', hint = 'H'},
                --             symbols = {
                --                 error = icons.diagnostics.Error .. " ",
                --                 warn = icons.diagnostics.Warn .. " ",
                --                 info = icons.diagnostics.Info .. " ",
                --                 hint = icons.diagnostics.Hint .. " "
                --             },
                --             colored = true,
                --         },
                --         {
                --             'filename',
                --             symbols = { modified = icons.git.modified, readonly = ' [ro] ' },
                --             path = 3,
                --
                --         },
                --     }
                --
                -- }
            }
        end
    },
    {
        "freddiehaddad/feline.nvim",
        lazy = false,
        enabled = false,
        -- dependencies = { "catppuccin/nvim" },
        init = function()
            require("custom.feline").catppuccin()
        end
    },
    {
        "goolord/alpha-nvim",
        event = "VimEnter",
        opts = function()
            local dashboard = require("alpha.themes.dashboard")
            dashboard.section.footer.opts.hl = "Type"
            dashboard.section.header.opts.hl = "AlphaHeader"
            dashboard.section.buttons.opts.hl = "AlphaButtons"

            return dashboard
        end,
        config = function(_, dashboard)
            -- close lazy and reopen when the dashboard is ready to
            -- display stats
            if vim.o.filetype == "lazy" then
                vim.cmd.close()
                vim.api.nvim_create_autocmd("User", {
                    pattern = "AlphaReady",
                    callback = function()
                        require("lazy").show()
                    end
                })
            end
            -- now load the dashboard
            require("alpha").setup(dashboard.opts)

            -- finally laod lazy to display stats
            vim.api.nvim_create_autocmd("User", {
                pattern = "LazyVimStarted",
                callback = function()
                    local stats = require("lazy").stats()
                    local ms = (math.floor(stats.startuptime * 100 + 0.5) / 100)
                    dashboard.section.footer.val = "Neovim loaded " .. stats.count .. "plugins in " .. ms .. " ms"
                    pcall(vim.cmd.AlphaRedraw)
                end
            })
        end
    },
    -- {
    --     "SmiteshP/nvim-navic", -- attached on nvim-lspconfig. See lsp/init.lua
    --     opts = {
    --         lsp = {
    --             auto_attach = true,
    --         },
    --         highligh = true,
    --     }
    --     -- lazy = true,
    --     -- opts = function()
    --     --     return {
    --     --         separator = " ",
    --     --         highlight = true,
    --     --         depth_limit = 5,
    --     --         icons = require("config").icons.kinds
    --     --     }
    --     -- end
    -- },
}
