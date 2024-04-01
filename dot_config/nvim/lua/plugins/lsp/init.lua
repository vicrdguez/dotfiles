local nmap = require("custom.utils").nmap
-- local augroup = require("custom.utils").augroup

-- Learn the keybindings, see :help lsp-zero-keybindings
-- Learn to configure LSP servers, see :help lsp-zero-api-showcase

return {
    {
        "VonHeikemen/lsp-zero.nvim",
        branch = "v3.x",
        lazy = true,
        config = false,
        init = function()
            -- simple setup, disabling borders
            -- require("lsp-zero").preset({ float_border = "none" })
            -- Disabling automatic setup to do it manually
            vim.g.lsp_zero_extend_cmp = 0
            vim.g.lsp_zero_extend_lspconfig = 0
        end
    },
    -- needed so mason-lspconfig can be configured bello on lspconfig section
    {
        'williamboman/mason.nvim',
        lazy = false,
        config = true,
    },
    {
        "neovim/nvim-lspconfig",
        cmd = { "LspInfo", "LspInstall", "LspStart" },
        event = { "BufReadPre", "BufNewFile" },
        dependencies = {
            { "williamboman/mason-lspconfig.nvim" }, -- Optional
            { "hrsh7th/cmp-nvim-lsp" },
        },
        config = function()
            local lspz = require("lsp-zero")
            lspz.extend_lspconfig()

            -- LSP attach
            lspz.on_attach(function(_, bufnr) -- client, bufnr
                -- Keymaps
                lspz.default_keymaps({ buffer = bufnr })
                nmap("<C-k>", vim.lsp.buf.signature_help, { desc = "LSP signature help", buffer = bufnr })
                nmap("<leader>da", vim.lsp.buf.code_action, { desc = "Code actions" })
                nmap("<leader>df", function() require("conform").format({ async = true, lsp_fallback = true }) end,
                    { desc = "LSP format buffer" })
                nmap("<leader>dr", vim.lsp.buf.rename, { desc = "Rename symbol" })
                nmap("<leader>do", vim.diagnostic.open_float, { desc = "Diagnostics: open float" })
                nmap("<leader>dt", require("custom.java").mvn_test, { desc = "Maven test"})
                nmap("<leader>dp", require("custom.java").mvn_package, { desc = "Maven package"})
            end)

            -- Diagnostic icons
            lspz.set_sign_icons({
                error = '✘',
                warn = '▲',
                hint = '⚑',
                info = '»'
            })

            require("mason-lspconfig").setup({
                ensure_installed = {},
                handlers = {
                    -- This is the default handler, delegated to lsp-zero. It will be used for each
                    -- server that does not have a dedicated handler
                    lspz.default_setup,
                    lua_ls = function()
                        -- Explicitly setup neovim's lua LSP stuff using lsp-zero defaults
                        require("lspconfig").lua_ls.setup(lspz.nvim_lua_ls())
                    end,
                    jdtls = function ()
                        require("java").setup()
                        require("lspconfig").jdtls.setup({
                            settings = {
                                configuration = {
                                    runtimes = require("custom.utils").get_java_versions()
                                }
                            }
                        })
                    end
                }
            })
            -- lspz.skip_server_setup({ "jdtls", "gopls" }) -- This servers is configured independently.
        end
    },
    {
        'nvim-java/nvim-java',
        dependencies = {
            'nvim-java/lua-async-await',
            'nvim-java/nvim-java-core',
            'nvim-java/nvim-java-test',
            'nvim-java/nvim-java-dap',
            'MunifTanjim/nui.nvim',
            'neovim/nvim-lspconfig',
            'mfussenegger/nvim-dap',
            {
                'williamboman/mason.nvim',
                opts = {
                    registries = {
                        'github:nvim-java/mason-registry',
                        'github:mason-org/mason-registry',
                    },
                },
            }
        },
    },
    {
        "ray-x/go.nvim",
        dependencies = { -- optional packages
            "ray-x/guihua.lua",
            "neovim/nvim-lspconfig",
            "nvim-treesitter/nvim-treesitter",
        },
        opts = {
            lsp_cfg = true,
            lsp_keymaps = false
        },
        config = function(_, opts)
            require("go").setup(opts)
        end,
        event = { "CmdlineEnter" },
        ft = { "go", 'gomod' },
        -- build = ':lua require("go.install").update_all_sync()' -- if you need to install/update all binaries
    },
    -- {
    --     "mfussenegger/nvim-jdtls",
    -- lazy = false,
    -- -- event = { "BufReadPre", "BufNewFile" },
    -- ft = "java",
    -- config = function()
    --     -- local jdtls = require("jdtls")
    --     -- local opts = require("plugins.lsp.java").get_opts(jdtls)
    --     -- jdtls.start_or_attach(opts)
    -- end,
    -- keys = common_lsp_maps
    -- },
    -- { "mfussenegger/nvim-dap" },
    {
        "rcarriga/nvim-dap-ui",
        config = function()
            local dap, dapui = require("dap"), require("dapui")
            dapui.setup()
            -- open dap ui automatically when debug starts
            dap.listeners.after.event_initialized["dapui_config"] = function()
                dapui.open()
            end
            -- dap.listeners.before.event_terminated["dapui_config"] = function()
            --     dapui.close()
            -- end
            dap.listeners.before.event_exited["dapui_config"] = function()
                dapui.close()
            end
        end,
        enabled = true,
    },
}
