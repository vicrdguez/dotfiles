-- Two markdown looks, one toggle:
--   render-markdown  heading blocks, boxed code, indent rails   (default)
--   touchup          flat backgrounds, # stays put, obsidian checkboxes
-- Colors for every Touchup* group live in lua/themes/vague.lua, same as the
-- RenderMarkdown* ones.

local ns = vim.api.nvim_create_namespace("touchup")

-- read at startup so lua/options.lua stays the only place conceallevel is set
-- (init.lua requires options before plugins)
local default = vim.o.conceallevel

-- conceallevel touchup mode runs at. keeping it means links and wikilinks stay
-- concealed by nvim's own markdown_inline queries. drop to 0 for touchup as
-- designed, nothing hidden and the ** ` markers dimmed instead
-- local conceal = 0
local conceal = default

local opts = {
    -- same icons as render-markdown so only the treatment around them changes
    -- bullets = { icons = { "✸", "✿", "✦", "✧" } },
    bullets = { icons = { "•", "◦", "‣", "⁃" } },

    -- bullets.vim already owns <CR>, and enter.lua sets a buffer-local map in
    -- markdown, which wins over the global one
    -- enter = { enabled = true },
    enter = { enabled = false },

    -- these only dim what conceal already hides. init.lua:100 skips them above
    -- conceallevel 0 anyway, and leaving them on there fires a one-time warning
    links = { enabled = conceal == 0 },
    markers = { enabled = conceal == 0 },
}

local M = {}

M.on = false

function M.set(on)
    M.on = on

    local level = on and conceal or default
    vim.o.conceallevel = level
    for _, w in ipairs(vim.api.nvim_list_wins()) do
        vim.wo[w].conceallevel = level
    end

    require("render-markdown.api").set(not on)

    -- no enable/disable in touchup: setup() registers the decoration provider
    -- and an empty table unregisters it. every extmark it draws is ephemeral,
    -- so there is nothing left behind
    if on then
        require("touchup").setup(opts)
    else
        vim.api.nvim_set_decoration_provider(ns, {})
    end

    -- no heading blocks in touchup mode, so H1/H2 get an underline instead.
    -- read the color back rather than repeat the palette
    for i = 1, 2 do
        local name = ("@markup.heading.%d.markdown"):format(i)
        local h = vim.api.nvim_get_hl(0, { name = name })
        h.underline = on or nil
        vim.api.nvim_set_hl(0, name, h)
    end
end

function M.toggle()
    M.set(not M.on)
end

return M
