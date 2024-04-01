return {
    {
        "folke/noice.nvim",
        enabled = true,
        event = "VeryLazy",
        version = "1.14.2",
        dependencies = { -- if you lazy-load any plugin below, make sure to add proper `module="..."` entries
            "MunifTanjim/nui.nvim",
            -- OPTIONAL:
            --   `nvim-notify` is only needed, if you want to use the notification view.
            --   If not available, we use `mini` as the fallback
            -- "rcarriga/nvim-notify",
        },
        opts = {
            cmdline = {
                view = "cmdline"
            },
            popupmenu = {
                enabled = true
            },
            messages = {
                enabled = true,
                view_history = "split"
            },
            views = {
                cmdline_popup = {
                    border = {
                        style = "none",
                        padding = { 2, 3 },
                    },
                    filter_options = {},
                    win_options = {
                        winhighlight = "Normal:Normal,FloatBorder:FloatBorder",
                    },
                    position = {
                        row = 25,
                        col = "50%",
                    },
                    size = {
                        width = 60,
                        height = "auto",
                    },
                },
                split = {
                    win_options = { wrap = false },
                    size = "30%",
                    close = { keys = { "q", "<CR>", "<Esc>" } },
                    enter = true
                },
            },
            lsp = {
                progress = {
                    enabled = true,
                    throttle = 10000/30,
                },
                -- override markdown rendering so that **cmp** and other plugins use **Treesitter**
                override = {
                    ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
                    ["vim.lsp.util.stylize_markdown"] = true,
                    ["cmp.entry.get_documentation"] = true,
                },
            },
            presets = {
                long_message_to_split = true,
                cmdline_output_to_split = true,
            },
            routes = {
                -- skip messages when you write to a file
                {
                    filter = {
                        event = "msg_show",
                        kind = "",
                        find = "written"
                    },
                    opts = { skip = true },
                },
                -- skip messages when you undo
                {
                    filter = {
                        event = "msg_show",
                        kind = "",
                        find = "; before"
                    },
                    opts = { skip = true },
                },
                -- skip messages when you redo
                {
                    filter = {
                        event = "msg_show",
                        kind = "",
                        find = "; after"
                    },
                    opts = { skip = true },
                },
                {
                    filter = {
                        event = "msg_show",
                        kind = "",
                        find = "fewer lines"
                    },
                    opts = { skip = true },
                },
                -- route long messages to split
                {
                    filter = {
                        event = "msg_show",
                        any = { { min_height = 2 }, { min_length = 50 } },
                    },
                    view = "split",
                    opts = { stop = true },
                },
            },
        }
    }
}
