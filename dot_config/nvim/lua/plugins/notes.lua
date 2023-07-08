local zk = require("custom.zk").zk

local function get_picker()
    if require("lazy.core.config").plugins["telescope.nvim"] then
        if require("lazy.core.config").plugins["telescope.nvim"].enabled then
            return "telescope"
        end
    end
    return "fzf_lua"
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
            { "<leader>rf", zk("find_or_create", { tags = { "-meeting" }, excludeHrefs = {"log"} }) },
            { "<leader>rn", zk("prompt_new", {}) },
            { "<leader>rt", zk("ZkTags", { sort = { "note-count" } }) },
            { "<leader>rb", zk("ZkBacklinks", {}) },
            { "<leader>ri", zk("ZkInsertLink", { matchSelected = true }) },
            { "<leader>rI", zk("ZkInsertLinkAtSelection", { matchSelected = true }) },
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
            { "<leader>rm", "<esc><cmd>'<,'>ZkMatch<cr>", mode = "x" },
            { "<leader>rc", "<esc><cmd>'<,'>ZkNewFromTitleSelection { dir = 'main' }<cr>", mode = "x" },
            { "<leader>rC", "<esc><cmd>'<,'>ZkNewFromContentSelection { dir = 'main' }<cr>", mode = "x" },
            { "<C-i>i",     zk("ZkInsertLink", { matchSelected = true }), mode = "i" },
        },
    },
    -- { dir = "/usr/local/opt/fzf" },
    {
        "ekickx/clipboard-image.nvim",
        opts = {
            -- default = {
            --     img_dir = "img",
            --     img_name = function() return os.date('%Y-%m-%d-%H-%M-%S') end, -- Example result: "2021-04-13-10-04-18"
            --     affix = "<\n  %s\n>"                                           -- Multi lines affix
            -- },
            markdown = {
                img_dir = { "assets" },
                img_dir_txt = "../assets",
                img_name = function() return os.date('%Y-%m-%d-%H-%M-%S') end, -- Example result: "2021-04-13-10-04-18"
                affix = "![](%s)"
            }
        }
    },
    {
        "shortcuts/no-neck-pain.nvim",
        -- this plugin gets automatically enabled on markdown files, check autocmds.lua file
        version = "*",
        opts = {
            width = 150
        }
    },
    {
        "gaoDean/autolist.nvim",
        enabled = false,
        ft = {
            "markdown",
            "text",
            "tex",
            "plaintex",
        },
        config = function()
            local autolist = require("autolist")
            autolist.setup()
            autolist.create_mapping_hook("i", "<CR>", autolist.new)
            autolist.create_mapping_hook("i", "<Tab>", autolist.indent)
            autolist.create_mapping_hook("i", "<S-Tab>", autolist.indent, "<C-D>")
            autolist.create_mapping_hook("n", "o", autolist.new)
            autolist.create_mapping_hook("n", "O", autolist.new_before)
            autolist.create_mapping_hook("n", ">>", autolist.indent)
            autolist.create_mapping_hook("n", "<<", autolist.indent)
            autolist.create_mapping_hook("n", "<C-r>", autolist.force_recalculate)
            autolist.create_mapping_hook("n", "<leader>x", autolist.invert_entry, "")
        end,
    },
    {
        "lukas-reineke/headlines.nvim",
        dependencies = "nvim-treesitter/nvim-treesitter",
        config = true, -- or `opts = {}`
    },
    {
        'edluffy/hologram.nvim',
        enabled = false,
        otps = {
            auto_display = true
        }
    },
    {
        'toppair/peek.nvim',
        event = { "BufRead", "BufNewFile" },
        -- build = 'deno task --quiet build:fast',
        config = function()
            vim.api.nvim_create_user_command("Peek", function()
                local peek = require("peek")
                peek.setup()
                local is_markdown_file = vim.bo[vim.api.nvim_get_current_buf()].filetype == 'markdown'

                if peek.is_open() and is_markdown_file then
                    peek.close()
                elseif is_markdown_file then
                    peek.open()
                end
            end, {})
        end
    }
}
