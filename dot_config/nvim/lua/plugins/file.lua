return {
    {
        'stevearc/oil.nvim',
        -- Optional dependencies
        dependencies = { "nvim-tree/nvim-web-devicons" },
        lazy = false,
        opts = {
            default_file_explorer = true,
            columns = {
                "permissions",
                "size",
                "icon"
            },
            view_options = {
                show_hidden = true
            }
        },
        keys = {
            {
                "<leader>f.",
                function() require("oil").open() end,
                desc = "Open Oil browser"
            },
            {
                "<leader>ff",
                function() require("oil").open_float() end,
                desc = "Open Oil browser"
            }
        },
    },
    {
        "nvim-neo-tree/neo-tree.nvim",
        branch = "v2.x",
        cmd = "Neotree",
        enabled = false,
        dependencies = {
            "MunifTanjim/nui.nvim",
            'nvim-lua/plenary.nvim'
        },
        keys = {
            {
                "<leader>fE",
                function()
                    require("neo-tree.command").execute({
                        toggle = true,
                        dir = require("custom.utils").get_root(),
                        position = "right"
                    })
                end,
                desc = "Open neotree (root dir)"
            },
            {
                "<leader>fe",
                function()
                    require("neo-tree.command").execute({ toggle = true, dir = vim.loop.cwd(), position = "right" })
                end,
                desc = "Open neotree (cwd)"
            },
            {
                "<leader>e",
                function()
                    require("neo-tree.command").execute({ action = "focus", dir = vim.loop.cwd(), position = "right" })
                end,
                desc = "Focus on neotree (cwd)"
            }
        },
        opts = {
            filesystem = {
                bind_to_cwd = false,
                follow_current_file = true
            },
            default_component_configs = {
                icon = {
                    folder_empty = "󰜌",
                    folder_empty_open = "󰜌",
                },
                git_status = {
                    symbols = {
                        -- Change type
                        added     = "✚", -- or "✚", but this is redundant info if you use git_status_colors on the name
                        modified  = "", -- or "", but this is redundant info if you use git_status_colors on the name
                        deleted   = "✖", -- this can only be used in the git_status source
                        renamed   = "", -- this can only be used in the git_status source
                        -- Status type
                        untracked = "",
                        ignored   = "",
                        unstaged  = "",
                        staged    = "",
                        conflict  = "",
                    },
                },
            },
        }

    }

}
