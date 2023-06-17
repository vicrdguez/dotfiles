return {
    {
        'stevearc/oil.nvim',
        opts = {},
        -- Optional dependencies
        dependencies = { "nvim-tree/nvim-web-devicons" },
        keys = {
                {"<leader>f.", "<cmd>Oil<cr>", desc = "Open Oil browser"}
            }
        },
        {
            "nvim-neo-tree/neo-tree.nvim",
            branch = "v2.x",
            cmd = "Neotree",
            dependencies = {
                "MunifTanjim/nui.nvim"
            },
            keys = {
                {
                    "<leader>fE",
                    function()
                        require("neo-tree.command").execute({toggle = true, dir = require("custom.utils").get_root(), position = "right" })
                    end,
                    desc = "Open neotree (root dir)"
                },
                {
                    "<leader>fe",
                    function()
                        require("neo-tree.command").execute({toggle = true, dir = vim.loop.cwd(), position = "right"})
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
                            deleted   = "✖",-- this can only be used in the git_status source
                            renamed   = "",-- this can only be used in the git_status source
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
