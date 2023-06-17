-- This file just contains some configs that I'm not using anymore, but that I don't want to lose.

local specs = {
    {
        "rcarriga/nvim-notify",
        enabled = false,
        init = function()
            local notify = require("notify")
            notify.setup({
                background_colour = "#000000",
            })
            vim.notify = notify
        end,
        opts = {
            timeout = 3000,
            max_height = function()
                return math.floor(vim.o.lines * 0.75)
            end,
            max_width = function()
                return math.floor(vim.o.columns * 0.75)
            end,
        },
        keys = {
            {
                "<leader>un",
                function()
                    require("notify").dismiss({ silent = true, pending = true })
                end,
                desc = "delete all notifications",
            },
        },
    },
    -- better vim.ui
    {
        "stevearc/dressing.nvim",
        enabled = false,
        lazy = true,
        opts = {
            input = {
                enabled = false,
                win_options = {
                    winblend = 10,
                }
            },
            nui = {
                win_options = {
                    winblend = 10,
                }
            },
            builtin = {
                win_options = {
                    winblend = 10,
                }
            },
            select = {
                win_options = {
                    winblend = 0,
                }
            }
        },
        init = function()
            ---@diagnostic disable-next-line: duplicate-set-field
            vim.ui.select = function(...)
                require("lazy").load({ plugins = { "dressing.nvim" } })
                return vim.ui.select(...)
            end
            ---@diagnostic disable-next-line: duplicate-set-field
            vim.ui.input = function(...)
                require("lazy").load({ plugins = { "dressing.nvim" } })
                return vim.ui.input(...)
            end
        end,
    },
    {
        "VonHeikemen/lsp-zero.nvim",
        branch = "v2.x",
        event = { "BufReadPre", "BufNewFile" },
        dependencies = {
            -- LSP Support
            { "neovim/nvim-lspconfig" },             -- Required
            { "williamboman/mason.nvim" },           -- Optional
            { "williamboman/mason-lspconfig.nvim" }, -- Optional

            -- Autocompletion
            { "hrsh7th/nvim-cmp" },         -- Required
            { "hrsh7th/cmp-nvim-lsp" },     -- Required
            { "hrsh7th/cmp-buffer" },       -- Optional
            { "hrsh7th/cmp-path" },         -- Optional
            { "saadparwaiz1/cmp_luasnip" }, -- Optional
            { "hrsh7th/cmp-nvim-lua" },     -- Optional

            -- Snippets
            { "L3MON4D3/LuaSnip" },             -- Required
            { "rafamadriz/friendly-snippets" }, -- Optional
        },
        lazy = true,
        -- init = function()
        -- end,
        config = function(_, opts)
            local lsp = require("lsp-zero").preset({
                name = "recommended",
                set_lsp_keymaps = {
                    omit = { "gd", "gr", "K", "<C-k" }
                }
            })
            setupLsp(lsp)
            lsp.setup()
        end,
        keys = common_lsp_maps
    },
    {
        'nvim-telescope/telescope.nvim',
        tag = '0.1.1',
        dependencies = { { 'nvim-lua/plenary.nvim' } },
        lazy = false,
        cmd = "Telescope",
        version = false, -- Using repo HEAD
        keys = {
            -- special
            {
                "<leader><space>",
                custom.tel("buffers", { show_all_buffers = true }),
                desc =
                "Switch buffer"
            },
            {
                "<leader>,",
                custom.tel("find_files"),
                desc =
                "Find files (root dir)"
            },
            {
                "<leader>.",
                "<cmd>Telescope file_browser path=%:p:h select_buffer=true<cr>",
                desc =
                "File browser"
            },
            {
                "<leader>/",
                custom.tel('current_buffer_fuzzy_find', { previewer = false }),
                desc =
                "Search in buffer"
            },
            {
                "<leader>;",
                custom.tel("commands"),
                desc =
                "Search commands"
            },
            {
                "<leader>:",
                custom.tel("command_history"),
                desc =
                "Command History"
            },
            {
                "<leader>'",
                custom.tel("resume"),
                desc =
                "Command History"
            },
            -- files/find
            {
                "<leader>ff",
                custom.tel("find_files", { cwd = false }),
                desc =
                "Find files (cwd)"
            },
            {
                "<leader>fr",
                custom.tel("oldfiles"),
                desc =
                "Find recent files"
            },
            {
                "<leader>fg",
                custom.tel("git_files"),
                desc =
                "Find git files"
            },
            -- search
            {
                "<leader>sg",
                custom.tel("live_grep", { cwd = false }),
                desc =
                "Search in files (cwd)"
            },
            {
                "<leader>sG",
                custom.tel("live_grep"),
                desc =
                "Search in files (root dir)"
            },
            -- help
            {
                "<leader>hh",
                custom.tel("help_tags"),
                desc =
                "Search help pages"
            },
            {
                "<leader>ht",
                custom.tel("colorscheme", { enable_preview = true, previewer = false }),
                desc =
                "Search colorscheme"
            },
            {
                "<leader>hm",
                custom.tel("man_pages"),
                desc =
                "Search man pages"
            },
            {
                "<leader>hk",
                custom.tel("keymaps"),
                desc =
                "Keymaps"
            },
            -- --dev
            {
                "<leader>dd",
                custom.tel("diagnostics", { previewer = false }),
                desc =
                "Diagnostics"
            },
            {
                "<leader>ds",
                custom.tel("lsp_document_symbols"),
                desc =
                "Goto symbol"
            },
            {
                "<leader>dws",
                custom.tel("lsp_wokspace_symbols"),
                desc =
                "Goto symbol"
            },
            {
                "gd",
                custom.tel("lsp_definitions"),
                desc =
                "Goto definition"
            },
            {
                "gr",
                custom.tel("lsp_references"),
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
                        ["<C-l>"] = custom.tel_actions({ "send_to_qflist", "open_qflist" })
                    }
                },
                layout_strategy = "bottom_pane",
                layout_config = {
                    height = 20,
                },
                border = true,
                sorting_strategy = "ascending",
            }
        },
        config = function(_, opts)
            local telescope = require("telescope")
            -- setup telescope
            telescope.setup(opts)
            -- Load all extensions
            telescope.load_extension("ui-select")
            telescope.load_extension("file_browser")
        end
    },
    -- extensions
    {
        "nvim-telescope/telescope-file-browser.nvim",
        dependencies = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" }
    },
    { "nvim-telescope/telescope-ui-select.nvim" },

}



local setupLsp = function(lsp)
    -- (Optional) Configure lua language server for neovim
    lsp.nvim_workspace()
    -- lsp.setup()
    lsp.ensure_installed({
        "tsserver",
        "eslint",
        "lua_ls",
        "rust_analyzer",
        "jdtls"
    })

    -- local cmp = require("cmp")
    local cmp_select = { behavior = cmp.SelectBehavior.Select }
    local cmp_mappings = lsp.defaults.cmp_mappings({
        ["<C-k>"] = cmp.mapping.select_prev_item(cmp_select),
        ["<C-j>"] = cmp.mapping.select_next_item(cmp_select),
        ["<C-y>"] = cmp.mapping.confirm({ select = true }),
        ["<C-Space>"] = cmp.mapping.complete(),
    })

    lsp.setup_nvim_cmp({
        mapping = cmp_mappings
    })

    -- lsp.set_preferences({
    -- 	sign_icons = {}
    -- })

    -- this one will be configured using nvim-jdtls
    lsp.skip_server_setup({ "jdtls" })

    lsp.on_attach(function(client, _)
        -- vim.notify("Lsp client attached: [" .. client.name .. "]")
        -- vim.keymap.set("n", "K", vim.lsp.buf.hover)
        -- vim.keymap.set("n", "<C-k>", vim.lsp.buf.signature_help)
        -- vim.keymap.set("n", "<leader>da", vim.lsp.buf.code_action)
        -- vim.keymap.set("n", "<leader>dr",  vim.lsp.buf.rename)
        -- { "<leader>da", function() vim.lsp.buf.code_action() end, desc = "Code actions"},
        -- { "<leader>dr", function() vim.lsp.buf.rename() end, desc = "Rename symbol"},
    end)
end
