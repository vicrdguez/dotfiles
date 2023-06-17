-- Editor experience
--

local augroup = require("custom.utils").augroup

local cursorword_blocklist = {
    all = {
        "if",
        "for"
    },
    lua = {
        "local",
        "require",
        "end",
        "then",
        "function"
    },
    netrw = false
}

return {
    {
        'echasnovski/mini.comment',
        version = false,
        config = function() require("mini.comment").setup() end
    },
    {
        'echasnovski/mini.cursorword',
        version = false,
        config = function()
            _G.cursorword_blocklist = function()
                local cword = vim.fn.expand('<cword>')
                local ft = vim.api.nvim_buf_get_option(0, 'filetype')

                -- if the config for ft is set to false explicitly, we don't want cursorword at all
                if cursorword_blocklist[ft] == false then
                    if vim.b.minicursorword_disable then
                        -- avoid setting trying to disable if it is disabled already
                        return
                    end
                    vim.b.minicursorword_disable = true
                    return
                end
                -- concat both tables to get a final blocklist
                if cursorword_blocklist.all then
                    local blocklist = require("custom.utils").list_concat(
                        cursorword_blocklist[ft],
                        cursorword_blocklist.all,
                        true
                    )
                    vim.b.minicursorword_disable = vim.tbl_contains(blocklist, cword)
                end
            end

            vim.api.nvim_create_autocmd({ "CursorMoved" }, {
                group = augroup("cursorword_blocklist"),
                pattern = { "*" },
                callback = _G.cursorword_blocklist
            })
            -- vim.cmd('au CursorMoved * lua _G.cursorword_blocklist()')

            require("mini.cursorword").setup()
        end
    },
    {
        'echasnovski/mini.splitjoin',
        version = false,
        config = function() require("mini.splitjoin").setup() end
    },
    {
        'echasnovski/mini.pairs',
        version = false,
        config = function() require("mini.pairs").setup() end
    },
    {
        'echasnovski/mini.jump',
        version = false,
        config = function() require("mini.jump").setup() end
    },
    {
        'echasnovski/mini.surround',
        version = false,
        config = function() require("mini.surround").setup() end
    },
    {
        'echasnovski/mini.ai',
        version = false,
        config = function() require("mini.ai").setup() end
    },
    {
        'echasnovski/mini.hipatterns',
        version = false,
        config = function()
            local hipatterns = require("mini.hipatterns")
            hipatterns.setup({
                highlighters = {
                    -- Highlight standalone 'FIXME', 'HACK', 'TODO', 'NOTE'
                    fixme     = {
                        pattern = '%f[%w]()FIXME()%f[%W]',
                        group = 'MiniHipatternsFixme'
                    },
                    hack      = {
                        pattern = '%f[%w]()HACK()%f[%W]',
                        group = 'MiniHipatternsHack'
                    },
                    todo      = {
                        pattern = '%f[%w]()TODO()%f[%W]',
                        group = 'MiniHipatternsTodo'
                    },
                    note      = {
                        pattern = '%f[%w]()NOTE()%f[%W]',
                        group = 'MiniHipatternsNote'
                    },
                    -- Highlight hex color strings (`#rrggbb`) using that color
                    hex_color = hipatterns.gen_highlighter.hex_color(),
                },
            })
        end
    },

    -- {
    --     'windwp/nvim-autopairs',
    --     opts = function()
    --         local cmp_autopairs = require('nvim-autopairs.completion.cmp')
    --         local cmp = require('cmp')
    --
    --         -- adds autopair
    --         cmp.event:on('confirm_done', cmp_autopairs.on_confirm_done())
    --     end
    --
    -- },
    -- {
    --     'numToStr/Comment.nvim',
    --     config = function ()
    --         require('Comment').setup()
    --     end
    -- },
}
