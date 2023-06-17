local fzf = require("custom.search").fzf

return {
    {
        "ibhagwan/fzf-lua",
        -- lazy = false,
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
            { "<leader>fr",      fzf("oldfiles"),             { desc = "Resume last search" } },
            { "<leader>sg",      fzf("grep_project"),          { desc = "Fuzzy grep project" } },
            { "<leader>sG",      fzf("live_grep"),             { desc = "Live grep project" } },
            { "<leader>ht",      fzf("colorschemes"),          { desc = "Search colorschemes" } },
            { "<leader>hh",      fzf("help_tags"),             { desc = "Search help tags" } },
            { "<leader>hm",      fzf("man_pages"),             { desc = "Search man pages" } },
            { "<leader>hk",      fzf("keymaps"),               { desc = "Search keymaps" } },
            { "<leader>ha",      fzf("autocmds"),              { desc = "Search autocommands" } },
            { "<leader>dd",      fzf("diagnostics_document"),  { desc = "Search diagnostics in document" } },
            { "<leader>dD",      fzf("diagnostics_workspace"), { desc = "Search diagnostics in ws" } },
            { "<leader>ds",      fzf("lsp_document_symbols"),  { desc = "Document symbols" } },
            { "<leader>dS",      fzf("lsp_workspace_symbols"), { desc = "Document symbols" } },
            { "<leader>da",      fzf("lsp_code_actions"),      { desc = "Code actions" } },
            { "gd",              fzf("lsp_definitions"),       { desc = "LSP definitions" } },
            { "gD",              fzf("lsp_declarations"),      { desc = "LSP declarations" } },
            { "gr",              fzf("lsp_references"),        { desc = "LSP references" } },

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
            file_icon_padding = " ",
            file_ignore_patterns = { "lazy-*.json$" }
        },
        config = function(_, opts)
            local fzf_lua = require("fzf-lua")
            fzf_lua.setup(opts)
            fzf_lua.register_ui_select()
        end
    },
}
