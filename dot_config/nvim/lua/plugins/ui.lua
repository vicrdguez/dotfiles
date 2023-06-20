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
        opts = {
            char = '┊',
            show_trailing_blankline_indent = false,
        }
    },
    -- {
    --     'nvim-lualine/lualine.nvim',
    --     event = 'VeryLazy',
    --     dependencies = {
    --         'nvim-tree/nvim-web-devicons',
    --     },
    --     opts = function(plugin)
    --         local icons = require("config").icons
    --         return {
    --             options = {
    --                 icons_enabled = true,
    --                 theme = 'auto',
    --                 component_separators = '|',
    --                 section_separators = '',
    --                 -- component_separators = { left = '', right = '' },
    --                 -- section_separators = { left = '', right = '' },
    --                 globalstatus = false,
    --             }, sections = {
    --                 lualine_b = {
    --                     'branch',
    --                     {
    --                         'diff',
    --                         symbols = {
    --                             added = icons.git.added,
    --                             modified = icons.git.modified,
    --                             removed = icons.git.removed
    --                         }
    --                     }
    --                 },
    --                 lualine_c = {
    --                     {
    --                         'diagnostics',
    --                         -- sources = {'nvim_lsp', 'nvim_diagnostic'},
    --                         -- sections = { 'error', 'warn', 'info', 'hint'},
    --                         -- symbols = {error = 'E', warn = 'W', info = 'I', hint = 'H'},
    --                         symbols = {
    --                             error = icons.diagnostics.Error,
    --                             warn = icons.diagnostics.Warn,
    --                             info = icons.diagnostics.Info,
    --                             hint = icons.diagnostics.Hint
    --                         },
    --                         colored = true,
    --                     },
    --                     { 'filename', symbols = { modified = " ", readonly = ' [ro] ' } }
    --                 }
    --
    --             }
    --         }
    --     end
    -- },
    {
        "freddiehaddad/feline.nvim",
        lazy = false,
        -- dependencies = { "catppuccin/nvim" },
        init = function()
            vim.opt.termguicolors = true
            local show_modified = true
            local filename_icon = "󰈙"
            local components = require("catppuccin.groups.integrations.feline").get()
            local colors = require("catppuccin.palettes").get_palette()
            local U = require "catppuccin.utils.colors"
            components.active[3][3].provider = function()
                local filename = vim.fn.pathshorten(vim.fn.expand("%:p"))
                local extension = vim.fn.expand "%:e"
                local present, icons = pcall(require, "nvim-web-devicons")
                local icon = present and icons.get_icon(filename, extension) or filename_icon
                return (show_modified and "%m" or "") .. " " .. icon .. " " .. filename .. " "
            end
            components.active[3][3].hl = {
                bg = colors.mantle,
                fg = colors.blue
            }
            components.active[3][3].left_sep.hl = {
                fg = colors.mantle,
                bg = colors.base
            }
            components.active[3][4].hl = {
                bg = colors.blue,
                fg = colors.mantle
            }
            components.active[3][4].left_sep.hl = {
                bg = colors.mantle,
                fg = colors.blue
            }

            require("feline").setup({
                components = components,
                sett = {
                    curr_file = colors.mantle
                },
            })
        end,
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
    {
        "SmiteshP/nvim-navic", -- attached on nvim-lspconfig. See lsp/init.lua
        opts = {
            lsp = {
                auto_attach = true,
            },
            highligh = true,
        }
        -- lazy = true,
        -- opts = function()
        --     return {
        --         separator = " ",
        --         highlight = true,
        --         depth_limit = 5,
        --         icons = require("config").icons.kinds
        --     }
        -- end
    },
}
