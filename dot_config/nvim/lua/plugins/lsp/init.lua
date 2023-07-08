local nmap = require("custom.utils").nmap
-- local augroup = require("custom.utils").augroup

-- Learn the keybindings, see :help lsp-zero-keybindings
-- Learn to configure LSP servers, see :help lsp-zero-api-showcase

return {
    {
        "neovim/nvim-lspconfig",
        cmd = "LspInfo",
        event = { "BufReadPre", "BufNewFile" },
        dependencies = {
            { "williamboman/mason-lspconfig.nvim" }, -- Optional
            {
                "williamboman/mason.nvim",
                build = function()
                    ---@diagnostic disable-next-line: param-type-mismatch
                    pcall(vim.cmd, "MasonUpdate")
                end,
            },
        },
        config = function()
            local lspz = require("lsp-zero")

            -- LSP attach
            lspz.on_attach(function(client, bufnr)
                -- Keymaps
                lspz.default_keymaps({ buffer = bufnr })
                nmap("<C-k>", vim.lsp.buf.signature_help, { desc = "LSP signature help", buffer = bufnr })
                nmap("<leader>da", vim.lsp.buf.code_action, { desc = "Code actions" })
                nmap("<leader>df", vim.lsp.buf.format, { desc = "LSP format buffer" })
                nmap("<leader>dr", vim.lsp.buf.rename, { desc = "Rename symbol" })
                nmap("<leader>do", vim.diagnostic.open_float, { desc = "Diagnostics: open float" })

                -- nvim-navic. See ui.lua
                if client.server_capabilities.documentSymbolProvider then
                    require("nvim-navic").attach(client, bufnr)
                end
            end)

            -- Diagnostic icons
            lspz.set_sign_icons({
                error = '✘',
                warn = '▲',
                hint = '⚑',
                info = '»'
            })

            -- Explicitly setup neovim's lua LSP stuff
            require("lspconfig").lua_ls.setup(lspz.nvim_lua_ls())

            lspz.skip_server_setup({ "jdtls", "gopls" }) -- This servers is configured independently. 
            -- vim.api.nvim_create_autocmd("FileType", {
            --     group = augroup("jdtls"),
            --     pattern = { "java" },
            --     desc = "Setup jdtls",
            --     callback = require("plugins.lsp.jdtls").jdtls_setup
            -- })

            lspz.setup()
        end

    },
    {
        "mfussenegger/nvim-jdtls",
        -- lazy = false,
        -- -- event = { "BufReadPre", "BufNewFile" },
        -- ft = "java",
        -- config = function()
        --     -- local jdtls = require("jdtls")
        --     -- local opts = require("plugins.lsp.java").get_opts(jdtls)
        --     -- jdtls.start_or_attach(opts)
        -- end,
        -- keys = common_lsp_maps
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
    {
        "VonHeikemen/lsp-zero.nvim",
        branch = "v2.x",
        lazy = true,
        config = function()
            -- simple setup, disabling borders
            require("lsp-zero").preset({ float_border = "none" })
        end
    },
    { "mfussenegger/nvim-dap" },
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
