return {
    {
        "hrsh7th/nvim-cmp",
        -- event = "InsertEnter",
        dependencies = {
            { "L3MON4D3/LuaSnip" },
            { "onsails/lspkind.nvim" },
            -- completion sources
            { "hrsh7th/cmp-nvim-lsp" }, -- Required for nvim-lspconfig
            { "saadparwaiz1/cmp_luasnip" },
            { "hrsh7th/cmp-nvim-lua" },
            { "hrsh7th/cmp-buffer" },
            { "hrsh7th/cmp-path" },
        },
        config = function()
            -- This function allows light-weight configuration of `nim-cmp` with `lsp-zero`. It is
            -- similar to the 'minimal' preset. This allows for lazy loading.
            -- The arguments for .extend() have the same shape as `manage_nvim_cmp`:
            -- https://github.com/VonHeikemen/lsp-zero.nvim/blob/v2.x/doc/md/api-reference.md#manage_nvim_cmp
            require("lsp-zero").extend_cmp()

            -- And you can configure cmp even more, if you want to.
            local cmp = require("cmp")
            local ls = require("luasnip")
            -- local cmp_action = require("lsp-zero").cmp_action()
            local cmp_select = { behavior = cmp.SelectBehavior.Select }

            local function expand_or_jump(select_opts)
                return cmp.mapping(function(fallback)
                    if ls.expand_or_jumpable() then
                        ls.expand_or_jump()
                    elseif cmp.visible() then
                        cmp.select_prev_item(select_opts)
                    else
                        fallback()
                    end
                end, { "i", "s" })
            end

            local function jump_prev(select_opts)
                return cmp.mapping(function(fallback)
                    if ls.jumpable(-1) then
                        ls.jump(-1)
                    elseif cmp.visible() then
                        cmp.select_next_item(select_opts)
                    else
                        fallback()
                    end
                end, { "i", "s" })
            end

            local function cycle_choice(_)
                return cmp.mapping(function(fallback)
                    if ls.choice_active() then
                        ls.change_choice(1)
                    else
                        fallback()
                    end
                end, { "i" })
            end
            cmp.setup({
                snippet = {
                    expand = function(args)
                        ls.lsp_expand(args.body)
                    end
                },
                -- Sources for completion items
                sources = {
                    { name = "nvim_lua" },
                    { name = "nvim_lsp" },
                    { name = "buffer",  keyword_length = 3 },
                    { name = "luasnip", keyword_length = 2 },
                    { name = "path" },

                },
                -- Having a lot of entries clutters the screen, I prefer limiting the number of entries
                performance = {
                    max_view_entries = 20,
                },
                -- Keymaps for completion related stuff
                mapping = {
                    -- ["<C-f>"] = cmp_action.luasnip_next_or_expand(),
                    -- ["<C-b>"] = cmp_action.luasnip_jump_backward(),
                    -- ["<Tab>"] = cmp_action.luasnip_supertab(),
                    -- ["<S-Tab>"] = cmp_action.luasnip_shift_supertab(),
                    ["<C-f>"] = expand_or_jump(cmp_select),
                    ["<C-b>"] = jump_prev(cmp_select),
                    ["<C-m>"] = cycle_choice(cmp_select),
                    ["<C-k>"] = cmp.mapping.select_prev_item(cmp_select),
                    ["<C-j>"] = cmp.mapping.select_next_item(cmp_select),
                    ["<C-y>"] = cmp.mapping.confirm({ select = true }),
                    ["<CR>"] = cmp.mapping.confirm({ select = true }),
                    ["<C-Space>"] = cmp.mapping.complete(),
                },
                -- Adds some icons to each kind of completions
                formatting = {
                    fields = { "abbr", "kind", "menu" },
                    format = require("lspkind").cmp_format({
                        mode = "symbol_text",
                        -- maxwidth = 50,
                        -- ellipsis_char = "...",
                        -- show just the function name without the signature. we rely on the docs
                        -- popoup for that
                        before = function(_, vim_item)
                            vim_item.abbr = vim_item.abbr:match("[^()]+")
                            -- vim.notify("In kind", vim.log.levels.WARN)
                            return vim_item
                        end,
                        menu = {
                            buffer = "[buf]",
                            nvim_lsp = "[LSP]",
                            nvim_lua = "[api]",
                            path = "[path]",
                            luasnip = "[snip]",
                        }
                    })
                },
                experimental = {
                    ghost_text = false
                },
            })
        end
    },
}
