local ls = require("luasnip")
local s = ls.s
local fmt = require("luasnip.extras.fmt").fmt
local i = ls.insert_node
local t = ls.text_node
local rep = require("luasnip.extras").rep

-- ls.add_snippets = {
--     all = {
--         s("<s", fmt([[
--         ```{}
--         {}
--         ```
--         ]], { i(1, "lang"), i(1) }))
--
--     },
--     lua = {
--         s("req", fmt("local {} = require('{}')", { i(i), rep(1)}))
--     },
--     markdown = {
--         s("hey", t("how are you doing?"))
--     }
-- }

-- ls.add_snippets("lua", {
--     s("req", fmt("local {} = require('{}')", { i(i), rep(1)}))
-- })

ls.add_snippets("all", {
})


ls.add_snippets("lua", {
    s("req", fmt('local {} = require("{}")', { rep(1), i(1, "package") }))
})

ls.add_snippets("markdown", {
    s("<s",
    fmt([[
    ```{}
    {}
    ```
    ]], { i(1, "lang"), i(2) }))
})
