-- Heavy inspiration from linkarzu:
-- https://github.com/linkarzu/dotfiles-latest/blob/main/neovim/neobean/lua/plugins/render-markdown.lua
--
-- Colors for every RenderMarkdown* group live in lua/themes/vague.lua, so they
-- track the colorscheme instead of falling back to the plugin's default links.
require("render-markdown").setup({
    -- env.lua:138 is an exact tbl_contains against vim.fn.mode(true), so "v" only
    -- matched charwise visual and missed V, ctrl-v and select mode (s/S), which
    -- is where snippet placeholders land
    -- render_modes = { "n", "v", "i", "c", "t" },
    render_modes = true,
    heading = {
        -- custom heading icons
        icons = { "󰎤 ", "󰎧 ", "󰎪 ", "󰎭 ", "󰎱 ", "󰎳 " },
        -- forced to 100 with no window clamp (heading.lua:207 is a plain math.max),
        -- so blocks overflow anything narrower than that
        -- width = "block",
        -- min_width = 100,
        width = "full",
        -- must list all six: the plugin clamps the index to the last element,
        -- so { true } would border every level
        border = { true, false, false, false, false, false },
        sign = false,
    },
    code = {
        -- see heading above, same overflow problem
        -- width = "block",
        -- min_width = 100,
        width = "full",
        border = "thick",
        language_icon = true,
        language_name = true,
        -- on the right it slides under the NNP side windows when room is tight
        -- position = "right",
        position = "left",
        sign = false,
        left_pad = 2,
        right_pad = 2,
    },
    bullet = {
        enabled = true,
        icons = { "•", "◦", "‣", "⁃" },
    },
    checkbox = {
        enabled = true,
        right_pad = 1,
        unchecked = {
            icon = "󰄱 ",
            highlight = "RenderMarkdownUnchecked",
            -- hl for item assoc. with unchecked checkobx
            -- scope_highlight = nil
        },
        checked = {
            icon = "󰱒 ",
            highlight = "RenderMarkdownChecked",
            -- hl for item assoc. with unchecked checkobx
            -- scope_highlight = nil
        },
    },
    -- indents by *heading* depth, not list nesting: gives body content a ▎ rail
    -- down the left of its section. Forces pipe_table.border_virtual on.
    indent = {
        enabled = false,
        icon = "▎",
        per_level = 2,
        skip_level = 1,
        skip_heading = true,
    },
    pipe_table = { preset = "heavy" },
    quote = { repeat_linebreak = true },
    html = {
        enabled = true,
        comment = {
            -- set to true to conceal html comments
            conceal = false,
        },
    },
    link = {
        image = "󰥶 ",
        custom = {
            youtu = { pattern = "youtu%.be", icon = "󰗃 " },
        },
    },
})
