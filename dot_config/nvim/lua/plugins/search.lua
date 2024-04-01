local fzf = require("custom.search").fzf
local fzf_act = require("custom.search").fzf_act
local tel = require("custom.search").tel
local tel_act = require("custom.search").tel_act

return {
    {
        "ibhagwan/fzf-lua",
        -- lazy = false,
        enabled = false,
        keys = {
            { "<leader><space>", fzf("buffers"),               { desc = "Switch buffers" } },
            -- {"<leader>,",  fzf(""), {desc = "Switch buffers"}},
            { "<leader>.",       fzf("files"),                 { desc = "Find files" } },
            { "<leader>/",       fzf("grep_curbuf"),           { desc = "Search in buffer" } },
            { "<leader>;",       fzf("commands"),              { desc = "Command list" } },
            { "<leader>:",       fzf("command_history"),       { desc = "Command history" } },
            { "<leader>'",       fzf("resume"),                { desc = "Resume last search" } },
            { "<leader>'",       fzf("resume"),                { desc = "Resume last search" } },
            { "<leader>fg",      fzf("git_files"),             { desc = "Resume last search" } },
            { "<leader>fr",      fzf("oldfiles"),              { desc = "Resume last search" } },
            { "<leader>sg",      fzf("live_grep_glob"),        { desc = "Live grep project" } },
            { "<leader>sG",      fzf("grep_project"),          { desc = "Fuzzy grep project" } },
            { "<leader>ht",      fzf("colorschemes"),          { desc = "Search colorschemes" } },
            { "<leader>hh",      fzf("help_tags"),             { desc = "Search help tags" } },
            { "<leader>hm",      fzf("man_pages"),             { desc = "Search man pages" } },
            { "<leader>hk",      fzf("keymaps"),               { desc = "Search keymaps" } },
            { "<leader>ha",      fzf("autocmds"),              { desc = "Search autocommands" } },
            -- { "<leader>dd",      fzf("diagnostics_document"),  { desc = "Search diagnostics in document" } },
            -- { "<leader>dD",      fzf("diagnostics_workspace"), { desc = "Search diagnostics in ws" } },
            { "<leader>ds",      fzf("lsp_document_symbols"),  { desc = "Document symbols" } },
            { "<leader>dS",      fzf("lsp_workspace_symbols"), { desc = "Document symbols" } },
            { "<leader>da",      fzf("lsp_code_actions"),      { desc = "Code actions" } },
            {
                "gd",
                fzf("lsp_definitions", { sync = true, jump_to_single_result = true }),
                { desc = "LSP definitions" }
            },
            { "gD", fzf("lsp_declarations"), { desc = "LSP declarations" } },
            {
                "gr",
                fzf("lsp_references", { sync = true, jumpt_to_single_result = true }),
                { desc = "LSP references" }
            },

        },
        opts = {
            winopts = {
                height = 0.3,
                width = 1,
                row = 1,
                border = false,
                preview = {
                    horizontal = 'right:50%', -- right|left:size
                    wrap = "wrap"
                    -- default = "Fuzzy grep in project"
                }
            },
            keymap = {
                builtin = {
                    ["<C-n>"] = "preview-page-down",
                    ["<C-p>"] = "preview-page-up",
                    -- ["<S-left>"] = "preview-page-reset",
                }
            },
            file_icon_padding = " ",
            file_ignore_patterns = { "lazy-%.json$" },
            previewers = {
                builtin = {
                    extensions = {
                        ["png"] = { "chafa" },
                        ["jpg"] = { "chafa" },
                        ["svg"] = { "chafa" }
                    }
                }
            },
            fzf_opts = {
                ['--history'] = vim.fn.stdpath("data") .. '/fzf-lua-history',
            },
            actions = {
                files = {
                    ["default"] = fzf_act("file_edit"),
                    ["ctrl-s"]  = fzf_act("file_split"),
                    ["ctrl-v"]  = fzf_act("file_vsplit"),
                    ["ctrl-l"]  = fzf_act("file_sel_to_qf"),
                }
            }
        },
        config = function(_, opts)
            local fzf_lua = require("fzf-lua")
            fzf_lua.setup(opts)
            fzf_lua.register_ui_select()
        end
    },
    {
        'nvim-telescope/telescope.nvim',
        tag = '0.1.5',
        dependencies = {
            { 'nvim-lua/plenary.nvim' },
            { 'nvim-telescope/telescope-fzf-native.nvim', build = 'make' },
            { "nvim-telescope/telescope-ui-select.nvim" },
            { 'nvim-telescope/telescope-media-files.nvim' },
        },
        lazy = false,
        cmd = "Telescope",
        version = false, -- Using repo HEAD
        enabled = true,
        keys = {
            -- special
            { "<leader><space>", tel("buffers", { show_all_buffers = true }), desc = "Switch buffer" },
            -- {"<leader>,", tel("find_files"), desc = "Find files (root dir)"},
            -- {"<leader>.", "<cmd>Telescope file_browser path=%:p:h select_buffer=true<cr>", desc = "File browser"},
            { "<leader>.",       tel("find_files", { cwd = true }),           desc = "File browser" },
            {
                "<leader>/",
                tel('current_buffer_fuzzy_find', { previewer = false }),
                desc =
                "Search in buffer"
            },
            {
                "<leader>;",
                tel("commands"),
                desc =
                "Search commands"
            },
            {
                "<leader>:",
                tel("command_history"),
                desc =
                "Command History"
            },
            {
                "<leader>'",
                tel("resume"),
                desc =
                "Command History"
            },
            -- files/find
            -- {"<leader>ff", tel("find_files", { cwd = false }), desc = "Find files (cwd)"},
            {
                "<leader>fr",
                tel("oldfiles"),
                desc =
                "Find recent files"
            },
            {
                "<leader>fg",
                tel("git_files"),
                desc =
                "Find git files"
            },
            -- search
            {
                "<leader>sg",
                tel("live_grep", { cwd = false }),
                desc =
                "Search in files (cwd)"
            },
            {
                "<leader>sG",
                tel("live_grep"),
                desc =
                "Search in files (root dir)"
            },
            -- help
            {
                "<leader>hh",
                tel("help_tags"),
                desc =
                "Search help pages"
            },
            {
                "<leader>ht",
                tel("colorscheme", { enable_preview = true, previewer = false }),
                desc =
                "Search colorscheme"
            },
            {
                "<leader>hm",
                tel("man_pages"),
                desc =
                "Search man pages"
            },
            { "<leader>hk",  tel("keymaps"),                            desc = "Keymaps" },
            -- --dev
            -- { "<leader>dd",  tel("diagnostics", { previewer = false }), desc = "Diagnostics" },
            -- { "<leader>da",  tel("actions", { previewer = false }),     desc = "Code actions" },
            { "<leader>ds",  tel("lsp_document_symbols"),               desc = "Goto symbol" },
            { "<leader>dws", tel("lsp_wokspace_symbols"),               desc = "Goto symbol" },
            {
                "gd",
                tel("lsp_definitions"),
                desc =
                "Goto definition"
            },
            {
                "gr",
                tel("lsp_references"),
                desc =
                "Goto references"
            },
        },
        opts = {
            defaults = {
                file_ignore_patterns = {
                    "^lazy-lock"
                },
                prompt_prefix = "::: ",
                selection_carret = ' ',
                mappings = {
                    i = {
                        ["<C-j>"] = "move_selection_next",
                        ["<C-k>"] = "move_selection_previous",
                        ["<C-l>"] = tel_act({ "send_to_qflist", "open_qflist" }),
                        ["<C-c>"] = tel_act("close")
                    }
                },
                layout_strategy = "bottom_pane",
                layout_config = {
                    height = 20,
                },
                border = true,
                sorting_strategy = "ascending",
            },
            extensions = {
                fzf = {
                    fuzzy = true,                   -- false will only do exact matching
                    override_generic_sorter = true, -- override the generic sorter
                    override_file_sorter = true,    -- override the file sorter
                    case_mode = "smart_case",       -- or "ignore_case" or "respect_case"
                    -- the default case_mode is "smart_case"
                }
            }
        },
        config = function(_, opts)
            local telescope = require("telescope")
            -- setup telescope
            telescope.setup(opts)
            -- Load all extensions
            telescope.load_extension("ui-select")
            telescope.load_extension('fzf')
            telescope.load_extension('media_files')
            -- telescope.load_extension("file_browser")
        end
    },
    -- extensions
    -- {
    --     "nvim-telescope/telescope-file-browser.nvim",
    --     dependencies = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" }
    -- },
}
