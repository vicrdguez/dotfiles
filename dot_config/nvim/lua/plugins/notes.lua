return {
    {
        "vicrdguez/zk-nvim",
        dev = true,
        config = function()
            require("zk").setup({
                -- can be "telescope", "fzf" or "select" (`vim.ui.select`)
                -- it's recommended to use "telescope" or "fzf"
                picker = "fzf_lua",
                lsp = {
                    -- `config` is passed to `vim.lsp.start_client(config)`
                    config = {
                        cmd = { "zk", "lsp" },
                        name = "zk",
                        -- on_attach = ...
                        -- etc, see `:h vim.lsp.start_client()`
                    },

                    -- automatically attach buffers in a zk notebook that match the given filetypes
                    auto_attach = {
                        enabled = true,
                        filetypes = { "markdown" },
                    },
                },
            })
        end,
        keys = {
            { "<leader>rf", "<Cmd>ZkNotes<CR>" },
            { "<leader>ri", "<Cmd>ZkInsertLink<CR>" },
            { "<C-c>i",     "<Cmd>ZkInsertLink<CR>", mode = "i" },
        },
    },
    { dir = "/usr/local/opt/fzf" },
    {
        "ekickx/clipboard-image.nvim",
        opts = {
            -- default = {
            --     img_dir = "img",
            --     img_name = function() return os.date('%Y-%m-%d-%H-%M-%S') end, -- Example result: "2021-04-13-10-04-18"
            --     affix = "<\n  %s\n>"                                           -- Multi lines affix
            -- },
            markdown = {
                img_dir = { "assets" },
                img_dir_txt = "../assets",
                img_name = function() return os.date('%Y-%m-%d-%H-%M-%S') end, -- Example result: "2021-04-13-10-04-18"
                affix = "![](%s)"
            }
        }
    },
    {
        "shortcuts/no-neck-pain.nvim",
        -- this plugin gets automatically enabled on markdown files, check autocmds.lua file
        version = "*",
        opts = {
            width = 120
        }
    },
    {
        "gaoDean/autolist.nvim",
        ft = {
            "markdown",
            "text",
            "tex",
            "plaintex",
        },
        config = function()
            local autolist = require("autolist")
            autolist.setup()
            autolist.create_mapping_hook("i", "<CR>", autolist.new)
            autolist.create_mapping_hook("i", "<Tab>", autolist.indent)
            autolist.create_mapping_hook("i", "<S-Tab>", autolist.indent, "<C-D>")
            autolist.create_mapping_hook("n", "o", autolist.new)
            autolist.create_mapping_hook("n", "O", autolist.new_before)
            autolist.create_mapping_hook("n", ">>", autolist.indent)
            autolist.create_mapping_hook("n", "<<", autolist.indent)
            autolist.create_mapping_hook("n", "<C-r>", autolist.force_recalculate)
            autolist.create_mapping_hook("n", "<leader>x", autolist.invert_entry, "")
        end,
    },
    {
        "lukas-reineke/headlines.nvim",
        dependencies = "nvim-treesitter/nvim-treesitter",
        config = true, -- or `opts = {}`
    }
}
