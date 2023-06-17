return {
    {
        "L3MON4D3/LuaSnip",
        -- follow latest release.
        version = "1.*",
        -- dependencies = {
        --     "rafamadriz/friendly-snippets",
        --     config = function()
        --         require("luasnip.loaders.from_vscode").lazy_load()
        --     end,
        -- },
        event = "VeryLazy",
        build = "make install_jsregexp",
        -- keys = {
        --     {
        --         "<c-f>",
        --         function()
        --             local ls = require "luasnip"
        --             if ls.expand_or_jumpable() then
        --                 ls.expand_or_jump()
        --             end
        --         end,
        --         mode = { "i", "s" },
        --         silent = true
        --     }
        -- },
        opts = {
            -- history = true,
            -- updateevents = { "TextChanged", "TextChangedI" },
            update_events = "TextChanged,TextChangedI",
            -- delete_check_events = "TextChanged",
        },
    }
}

