return {

    { "tpope/vim-fugitive", },
    { "tpope/vim-rhubarb", },
    {
        'lewis6991/gitsigns.nvim',
        -- opts = {
        --     signscolun = true,
        -- }
    },
    {
        "folke/trouble.nvim",
        dependencies = { "nvim-tree/nvim-web-devicons" },
        keys = {
            { "<leader>dd", function() require("trouble").toggle("document_diagnostics") end },
            { "<leader>dd", function() require("trouble").toggle("workspace_diagnostics") end },
            -- { "gr",         function() require("trouble").toggle("lsp_references") end },
            -- { "gd",         function() require("trouble").toggle("lsp_definitions") end }
        }
    },
    {
        'stevearc/conform.nvim',
        opts = {
            formatters_by_ft = {
                nix = { "nixpkgs_fmt" }
            }
        },
    }
    -- {
    --     "Lilja/zellij.nvim",
    --     lazy = false,
    --     config = true,
    -- }

}
