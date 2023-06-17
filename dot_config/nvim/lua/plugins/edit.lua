-- sets my preferred telescope theme as default
local telescope = function(builtin, opts, theme)
    local Util = require("lazyvim.util")
    local th = theme or "ivy"
    local final_opts = require("telescope.themes")["get_" .. th](opts)

    return Util.telescope(builtin, final_opts)
end

return {
    {
        "nvim-telescope/telescope.nvim",
        keys = {
            --   -- mode defaults to "n"
            --   -- special
            {
                "<leader><space>",
                telescope("buffers", { show_all_buffers = true }),
                desc = "Switch buffer",
            },
            {
                "<leader>.",
                telescope("find_files"),
                desc = "Find files (root dir)",
            },
            {
                "<leader>,",
                telescope("buffers"),
                desc = "Search commands",
            },
            {
                "<leader>/",
                telescope("current_buffer_fuzzy_find", { previewer = false }),
                desc = "Search in buffer",
            },
            {
                "<leader>;",
                telescope("commands"),
                desc = "Search commands",
            },
            --   { "<leader>:", "<cmd>Telescope command_history<cr>", desc = "Command History" },
            --   { "<leader>?", telescope("keymaps"), desc = "Seach keymap" },
            --
            --   -- files/find
            {
                "<leader>ff",
                telescope("find_files", { cwd = false }),
                desc = "Find files (cwd)",
            },
            {
                "<leader>fr",
                telescope("old_files"),
                desc = "Find recent files",
            },
            {
                "<leader>fg",
                telescope("git_files"),
                desc = "Find git files",
            },
            --
            --   -- search
            {
                "<leader>sg",
                telescope("live_grep", { cwd = false }),
                desc = "Search in files (cwd)",
            },
            {
                "<leader>sG",
                telescope("live_grep"),
                desc = "Search in files (root dir)",
            },
            {
                "<leader>sh",
                telescope("help_tags"),
                desc = "Search help pages",
            },
            --   {
            --     "<leader>sc",
            --     telescope("colorscheme", { enable_preview = true, previewer = false }),
            --     desc = "Search colorscheme",
            --   },
            --   { "<leader>sm", telescope("man_pages"), desc = "Search man pages" },
            --
            -- --dev
            { "<leader>dd", telescope("diagnostics", { previewer = false }), desc = "Diagnostics" },
            { "<leader>ds", telescope("lsp_document_symbols"), desc = "Goto symbol" },
            { "<leader>dws", telescope("lsp_wokspace_symbols"), desc = "Goto symbol" },
            {
                "gd",
                telescope("lsp_definitions"),
                desc = "Goto definition",
            },
            {
                "gr",
                telescope("lsp_references"),
                desc = "Goto references",
            },
        },

        -- change some options
        --
        opts = {
            defaults = {
                layout_strategy = "horizontal",
                layout_config = { prompt_position = "top" },
                sorting_strategy = "ascending",
                winblend = 0,
                themes = "ivy",
                mappings = {
                    i = {
                        ["<C-j>"] = "move_selection_next",
                        ["<C-k>"] = "move_selection_previous",
                    },
                },
            },
            extensions = {
                file_browser = {
                    theme = "ivy",
                    hijack_netrw = true,
                },
            },
        },
    },
    {
        "nvim-telescope/telescope-file-browser.nvim",
        dependencies = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" },
        config = function()
            require("telescope").load_extension("file_browser")
        end,
    },
    {
        "hrsh7th/nvim-cmp",
        opts = function(_, opts)
            vim.notify("From nvim-cmp")
            local cmp = require("cmp")
            -- local cmp_select = { behavior = cmp.SelectBehavior.Select }
            -- vim.pretty_print(opts.mapping)
            -- opts.mapping = vim.tbl_extend(
            --     "force",
            --     opts.mapping,
            --     cmp.mapping.preset.insert({
            --         ["C-k"] = cmp.mapping.select_prev_item(cmp_select),
            --         ["C-j"] = cmp.mapping.select_next_item(cmp_select),
            --         ["C-y"] = cmp.mapping.confirm(cmp_select),
            --         ---@diagnostic disable-next-line: missing-parameter
            --         ["C-Space"] = cmp.mapping.complete(),
            --     })
            -- )
            opts.mapping = {}
            vim.notify("Should be empty")
            -- vim.pretty_print(opts.mapping)
            opts.mapping = cmp.mapping.preset.insert({
                ["<C-k>"] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select }),
                ["<C-j>"] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Select }),
                ["<C-u>"] = cmp.mapping.scroll_docs(-4),
                ["<C-d>"] = cmp.mapping.scroll_docs(4),
                ["<C-Space>"] = cmp.mapping.complete(),
                ["<C-e>"] = cmp.mapping.abort(),
                ["<CR>"] = cmp.mapping.confirm({ behavior = cmp.SelectBehavior.Replace, select = true }),
                ["<Tab>"] = cmp.mapping.confirm({ behavior = cmp.SelectBehavior.Replace, select = true }),
                -- [" "] = function()
                --     if vim.fn.pumvisible() == 1 and vim.fn.complete_info().selected == -1 then
                --         return vim.api.nvim_replace_termcodes("<Space>", true, true, true)
                --     end
                --     return " "
                -- end,
            })
            vim.notify("cmp after new keymaps")
            -- vim.pretty_print(opts.mapping)
        end,
    },
    { "tpope/vim-fugitive" },
    {
        "nvim-treesitter/nvim-treesitter",
        enabled = true,
        opts = {
            highlight = {
                enable = true,
                -- additional_vim_regex_highlighting = { "markdown" },
            },
        },
        -- opts = {
        --     ensure_installed = {
        --         "bash",
        --         "help",
        --         "html",
        --         "json",
        --         "lua",
        -- disable = { "markdown" },
        --         "markdown",
        --         "markdown_inline",
        --         "query",
        --         "regex",
        --         "yaml",
        --     },
        --     -- highlight = {
        --     --     additional_vim_regex_highlighting = { "markdown" },
        --     -- },
        -- },
        config = function()
            require("nvim-treesitter.configs").setup({
                highlight = {
                    enable = true,
                    -- Setting this to true or a list of languages will run `:h syntax` and tree-sitter at the same time.
                    -- additional_vim_regex_highlighting = { "markdown" },
                    -- additional_vim_regex_highlighting = false,
                    -- disable = { "markdown" },
                },
            })
        end,
    },
    {
        "nvim-treesitter/playground",
    },
}
