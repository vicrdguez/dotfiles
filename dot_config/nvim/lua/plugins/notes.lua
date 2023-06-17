-- All related to notes

local home_dir = vim.fn.expand("~/dev/braindump")

local function setHighlights()
    local cmd = vim.cmd

    -- just blue and gray links
    -- cmd("tkLink ctermfg=Blue cterm=bold,underline guifg=blue gui=bold,underline")
    -- cmd("tkBrackets ctermfg=gray guifg=gray")

    -- for gruvbox
    cmd("hi tklink ctermfg=72 guifg=#689d6a cterm=bold,underline gui=bold,underline")
    cmd("hi tkBrackets ctermfg=gray guifg=gray")

    -- real yellow
    -- cmd("tkHighlight ctermbg=yellow ctermfg=darkred cterm=bold guibg=yellow guifg=darkred gui=bold")
    -- gruvbox
    cmd("hi tkHighlight ctermbg=214 ctermfg=124 cterm=bold guibg=#fabd2f guifg=#9d0006 gui=bold")

    cmd("hi link CalNavi CalRuler")
    cmd("hi tkTagSep ctermfg=gray guifg=gray")
    cmd("hi tkTag ctermfg=175 guifg=#d3869B")
end

return {
    {
        "renerocksai/telekasten.nvim",
        dependencies = { "nvim-telescope/telescope.nvim" },
        enabled = false,
        init = function()
            setHighlights()
        end,
        opts = {
            home = home_dir,
            take_over_my_home = true,
            auto_set_filetype = false,
            dailies = home_dir .. "/log/daily",
            weeklies = home_dir .. "/log/weeklies",
            templates = home_dir .. "/_meta/templates",
            image_subdir = home_dir .. "/_meta/images",
        },
    },
    {
        "ixru/nvim-markdown",
        enabled = false,
    },
    {
        "mickael-menu/zk-nvim",
        config = function()
            require("zk").setup({
                -- can be "telescope", "fzf" or "select" (`vim.ui.select`)
                -- it's recommended to use "telescope" or "fzf"
                picker = "telescope",
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
            { "<C-c>i", "<Cmd>ZkInsertLink<CR>", mode = "i" },
        },
    },
    {
        "andrewferrier/wrapping.nvim",
        config = function()
            require("wrapping").setup()
        end,
    },
    {
        "gaoDean/autolist.nvim",
        ft = {
            "markdown",
            "telekasten",
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
        "shortcuts/no-neck-pain.nvim",
        version = "*",
        opts = {
            width = 120,
            -- buffers = {
            --     colors = {
            --         blend = -0.2,
            --     },
            -- },
        },
    },
}
