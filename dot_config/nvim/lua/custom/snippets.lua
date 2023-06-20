local ls = require("luasnip")
local s = ls.s
local fmt = require("luasnip.extras.fmt").fmt
local i = ls.insert_node
local t = ls.text_node
local f = ls.function_node
-- local rep = require("luasnip.extras").rep

local function rep_last(index)
    return f(function(text)
        local parts = vim.split(text[1][1], ".", { plain = true })
        return parts[#parts] or ""
    end, { index }, {})
end

ls.add_snippets("all", {
    s({trig = "ct", desc = "Resolves current time"},
        f(function()
            return os.date("%H:%M")
        end)),

    s({trig = "cd", desc = "Resolves current date"},
        f(function()
            return os.date("%Y-%m-%d")
        end))
})

ls.add_snippets("lua", {
    s("req", fmt('local {} = require("{}")', { rep_last(1), i(1, "package") }))
})

ls.add_snippets("markdown", {
    s("<s",
        fmt([[
    ```{}
    {}
    ```
    ]], { i(1, "lang"), i(2) }))
})

