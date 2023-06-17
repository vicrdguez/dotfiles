local common_lsp_maps = {
    { "K", vim.lsp.buf.hover, desc = "Hover symbol" },
    { "<C-k>", vim.lsp.buf.signature_help, desc = "Signature help" },
    { "<leader>da", vim.lsp.buf.code_action, desc = "Code actions" },
    { "<leader>dr", vim.lsp.buf.rename, desc = "Rename symbol" },
    { "<leader>df", vim.lsp.buf.format, desc = "Format buffer" },
    { "<leader>do", vim.diagnostic.open_float, desc = "Open float diagnostic" },
}

return {
    {
        "mfussenegger/nvim-jdtls",
        lazy = true,
        -- event = { "BufReadPre", "BufNewFile" },
        ft = "java",
        keys = common_lsp_maps,
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
    -- todo revamp server config to have something similar to this: https://github.com/number5/dot_vim/blob/main/lua/plugins/mason-lspconfig.lua#L106-L108
    -- found in reddit: https://www.reddit.com/r/neovim/comments/12kxaa7/yamllint_configuration_when_using_neovimlazyvim/
    {
        "neovim/nvim-lspconfig",
        init = function()
            local keys = require("lazyvim.plugins.lsp.keymaps").get()
            keys[#keys + 1] = { "<leader>da", vim.lsp.buf.code_action, desc = "Code actions" }
            keys[#keys + 1] = { "<leader>dr", vim.lsp.buf.rename, desc = "Rename symbol" }
            keys[#keys + 1] = { "<leader>df", vim.lsp.buf.format, desc = "Format buffer" }
            keys[#keys + 1] = { "<leader>do", vim.diagnostic.open_float, desc = "Open float diagnostic" }

            require("lspconfig").yamlls.setup({
                settings = {
                    yaml = { keyOrdering = false },
                    redhat = { telemetry = { enabled = false } },
                },
            })
            -- change a keymap
            -- keys[#keys + 1] = { "K", "<cmd>echo 'hello'<cr>" }
            -- disable a keymap
            -- keys[#keys + 1] = { "K", false }
            -- add a keymap
            -- keys[#keys + 1] = { "H", "<cmd>echo 'hello'<cr>" }
        end,
        opts = {
            setup = {
                -- yamlls = function()
                --     require("lspconfig").yamlls.setup({
                --         settings = {
                --             yaml = {
                --                 keyOrdering = false,
                --             },
                --             redhat = {
                --                 telemetry = {
                --                     enabled = false,
                --                 },
                --             },
                --         },
                --     })
                -- end,
                jdtls = function()
                    return true
                end,
                -- jdtls = function()
                --     local jdtls = require("jdtls")
                --     local opts = require("plugins.lsp.java").get_opts(jdtls)
                --     jdtls.start_or_attach(opts)
                --     return true
                -- end,
            },
            severs = {
                -- yamlls = {
                --     yaml = {
                --         keyOrdering = false,
                --     },
                -- settings = {
                --     redhat = {
                --         telemetry = {
                --             enabled = false,
                --         },
                --     },
                --     yaml = {
                --         keyOrdering = false,
                --     },
                -- },
                -- },
            },
        },
    },
    -- {
    --     "jose-elias-alvarez/null-ls.nvim",
    --     -- Having a weird behavior with shiftwidth=4 indenting different that this formatter
    --     -- opts = function(_, opts)
    --     --     local nls = require("null-ls")
    --     --     opts.sources = vim.tbl_extend("force", opts.sources, {
    --     --         nls.builtins.formatting.google_java_format,
    --     --     })
    --     -- end,
    -- },
}
