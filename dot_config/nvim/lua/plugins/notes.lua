local zk = require("custom.zk").zk
local map = require("custom.utils").map
local nmap = require("custom.utils").nmap

local function get_picker()
    if require("lazy.core.config").plugins["telescope.nvim"] then
        if require("lazy.core.config").plugins["telescope.nvim"].enabled then
            return "telescope"
        end
    end
    return "fzf_lua"
end

local function peek()
    local p = require("peek")
    p.setup()
    local is_markdown_file = vim.bo[vim.api.nvim_get_current_buf()].filetype == 'markdown'

    if p.is_open() and is_markdown_file then
        p.close()
    elseif is_markdown_file then
        p.open()
    end
end


return {
    {
        "vicrdguez/zk-nvim",
        dev = true,
        config = function()
            require("zk").setup({
                -- can be "telescope", "fzf" or "select" (`vim.ui.select`)
                -- it's recommended to use "telescope" or "fzf"
                -- picker = "telescope",
                picker = get_picker(),
                lsp = {
                    -- `config` is passed to `vim.lsp.start_client(config)`
                    config = {
                        cmd = { "zk", "lsp" },
                        name = "zk",
                        -- on_attach = ...
                        -- etc, see `:h vim.lsp.start_client()`
                    },

                    -- automatically attach buffers in a zk notebook that match the given filetypes
                    auto_attach = {
                        enabled = true,
                        filetypes = { "markdown" },
                    },
                },
            })
        end,
        keys = {
            -- { "<leader>rf", zk("ZkNotes", {}) },
            { "<leader>rf", zk("find_or_create", { tags = { "-meeting", "-customer" }, excludeHrefs = { "log" } }) },
            { "<leader>rn", zk("prompt_new", {}) },
            { "<leader>rt", zk("ZkTags", { sort = { "note-count" } }) },
            { "<leader>rb", zk("ZkBacklinks", {}) },
            { "<leader>rp", zk("capture_today", {}) },
            { "<leader>ro", require("custom.markdown").quick_look },
            { "<leader>ri", zk("ZkInsertLink", {
                matchSelected = true,
                select = {
                    "title",
                    "absPath",
                    "path",
                    "metadata"
                }
            }) },
            { "<leader>rI", zk("ZkInsertLinkAtSelection", { matchSelected = true }),         mode = "x" },
            -- { "<leader>rm", zk("ZkMatch", {}), mode = "v" },
            -- { "<leader>rc", zk("ZkNewFromTitleSelection", { dir = "main" }), mode = {"v"} },
            -- { "<leader>rC", zk("ZkNewFromContentSelection", { dir = "main" }), mode = {"v"}},
            --
            -- This mappings are a workaround, since neovim does not update the visual selection
            -- until you leave visual mode. This makes that the first time you select text there is
            -- no text the command can detect, after the first, the next time you try to visual select
            -- the command will be executed with the last selection and not the current one.
            -- https://github.com/neovim/neovim/pull/13896 should improve this
            -- I can probably get the current functionality of the PR  if I add the following to my
            -- config: https://github.com/jonatan-branting/dotfiles/commit/245d5fd65a3177f88f929afe7baf9fe3434d866f
            { "<leader>rm", "<esc><cmd>'<,'>ZkMatch<cr>",                                    mode = "x" },
            { "<leader>rc", "<esc><cmd>'<,'>ZkNewFromTitleSelection { dir = 'main' }<cr>",   mode = "x" },
            { "<leader>rC", "<esc><cmd>'<,'>ZkNewFromContentSelection { dir = 'main' }<cr>", mode = "x" },
            { "<leader>r.", "<cmd>PasteImg<cr>" },
            { "<leader>r,", "<cmd>Screenshot<cr>" },
            { "<C-i>l",     require("custom.markdown").insert_md_link,                       mode = "i" },
            { "<C-i>k",     "<cmd>TSPlaygroundToggle<cr>",                                   mode = "i" },
            {
                "<C-i>i",
                zk("ZkInsertLink", {
                    matchSelected = true,
                    select = {
                        "title",
                        "absPath",
                        "path",
                        "metadata"
                    }
                }),
                mode = "i"
            },
        },
    },
    {
        "shortcuts/no-neck-pain.nvim",
        -- this plugin gets automatically enabled on markdown files, check autocmds.lua file
        version = "*",
        enabled = true,
        opts = {
            width = 130,
            minSideBufferWidth = 15,
        }
    },
    {
        'TobinPalmer/BetterGX.nvim',
        keys = {
            { 'gx', '<CMD>lua require("better-gx").BetterGx()<CR>' },
        }
    },
    {
        "gaoDean/autolist.nvim",
        enabled = true,
        ft = {
            "markdown",
            "text",
            "tex",
            "plaintex",
        },
        opts = {
            colon = {
                indent = false,
                indent_raw = false,
            }
        },
        config = function(_, opts)
            require("autolist").setup(opts)

            map("i", "<tab>", "<cmd>AutolistTab<cr>")
            map("i", "<s-tab>", "<cmd>AutolistShiftTab<cr>")
            map("i", "<CR>", "<CR><cmd>AutolistNewBullet<cr>")
            nmap("o", "o<cmd>AutolistNewBullet<cr>")
            nmap("O", "O<cmd>AutolistNewBulletBefore<cr>")
            nmap("<CR>", "<cmd>AutolistToggleCheckbox<cr><CR>")
            nmap("<C-r>", "<cmd>AutolistRecalculate<cr>")

            -- if you don't want dot-repeat
            -- vim.keymap.set("n", "<leader>cn", "<cmd>AutolistCycleNext<cr>")
            -- vim.keymap.set("n", "<leader>cp", "<cmd>AutolistCycleNext<cr>")

            -- functions to recalculate list on edit
            nmap(">>", ">><cmd>AutolistRecalculate<cr>")
            nmap("<<", "<<<cmd>AutolistRecalculate<cr>")
            nmap("dd", "dd<cmd>AutolistRecalculate<cr>")
            map("v", "d", "d<cmd>AutolistRecalculate<cr>")
        end,
    },
    {
        "lukas-reineke/headlines.nvim",
        dependencies = "nvim-treesitter/nvim-treesitter",
        enabled = false,
        opts = {
            -- markdown = {
            --     dash_string = "•"
            -- }
        }
    },
    -- {
    --     'edluffy/hologram.nvim',
    --     enabled = false,
    --     -- otps = {
    --     --     auto_display = true
    --     -- },
    --     config = function()
    --         require("hologram").setup({
    --             auto_display = true
    --         })
    --     end
    -- },
    { "nvim-lua/plenary.nvim" },
    {
        "3rd/image.nvim",
        enabled = false,
        event = "VeryLazy",
        opts = {
            backend = "ueberzug",
            integrations = {
                markdown = {
                    enabled = true,
                    clear_in_insert_mode = false,
                    download_remote_images = true,
                    only_render_image_at_cursor = false,
                    filetypes = { "markdown", "vimwiki" }, -- markdown extensions (ie. quarto) can go here
                },
                -- neorg = {
                --     enabled = true,
                --     clear_in_insert_mode = false,
                --     download_remote_images = true,
                --     only_render_image_at_cursor = false,
                --     filetypes = { "norg" },
                -- },
            },
            max_width = nil,
            max_height = nil,
            max_width_window_percentage = nil,
            max_height_window_percentage = 50,
            kitty_method = "normal",
        },
    },
    {
        'toppair/peek.nvim',
        build = 'deno task --quiet build:fast',
        config = function()
            vim.api.nvim_create_user_command("Peek", peek , {})
        end,
        keys = {
            {"<leader>rw", ft = "markdown", peek, desc = "Peek - Markdown preview"}
        }
    }
}
