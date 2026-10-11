-- Default colors
--
local colors = {
    ---@type string
    bg = "#141415",
    ---@type string
    inactiveBg = "#1c1c24",
    ---@type string
    fg = "#cdcdcd",
    ---@type string
    floatBorder = "#878787",
    ---@type string
    line = "#252530",
    ---@type string
    comment = "#606079",
    ---@type string
    builtin = "#b4d4cf",
    ---@type string
    func = "#c48282",
    ---@type string
    string = "#e8b589",
    ---@type string
    number = "#e0a363",
    ---@type string
    property = "#c3c3d5",
    ---@type string
    constant = "#aeaed1",
    ---@type string
    parameter = "#bb9dbd",
    ---@type string
    visual = "#333738",
    ---@type string
    error = "#d8647e",
    ---@type string
    warning = "#f3be7c",
    ---@type string
    hint = "#7e98e8",
    ---@type string
    operator = "#90a0b5",
    ---@type string
    keyword = "#6e94b2",
    ---@type string
    type = "#9bb4bc",
    ---@type string
    search = "#405065",
    ---@type string
    plus = "#7fa563",
    ---@type string
    delta = "#f3be7c",
}
--
-- CUSTOM HIGHLIGHTS
--
-- local hl = function(name, data)
--   vim.api.nvim_set_hl(0, name, data)
-- end
-- [[Mini statusline]]
-- hl('MiniStatuslineModeNormal', { bg = colors.delta, fg = colors.bg, bold = true })
-- hl('MiniStatuslineModeInsert', { bg = colors.plus, fg = colors.bg, bold = true })
-- hl('MiniStatuslineModeReplace', { bg = colors.error, fg = colors.bg, bold = true })
-- hl('MiniStatuslineModeCommand', { bg = colors.comment, fg = colors.bg, bold = true })
-- vim.notify('VAGUE')

-- Blend `fg` over `bg` at alpha `a`, returns "#rrggbb".
local function tint(fg, bg, a)
    local function ch(h, i)
        return tonumber(h:sub(i, i + 1), 16)
    end
    local function mix(i)
        return math.floor(ch(fg, i) * a + ch(bg, i) * (1 - a) + 0.5)
    end
    return ("#%02x%02x%02x"):format(mix(2), mix(4), mix(6))
end

require("vague").setup({
    bold = true,

    on_highlights = function(hl, col)
        -- [[Markdown]]
        -- Nothing defines RenderMarkdown* groups, so they fall back to `default`
        -- links pointing at unrelated groups (heading backgrounds land on the Diff
        -- colors, every heading level resolves to the same blue). Pin them here:
        -- this table is fed straight to nvim_set_hl at `colorscheme vague` time,
        -- which is the last thing init.lua does.
        local heads = { col.func, col.string, col.plus, col.type, col.hint, col.parameter }
        for i, c in ipairs(heads) do
            hl["@markup.heading." .. i .. ".markdown"] = { fg = c, bold = true }
            hl["RenderMarkdownH" .. i .. "Bg"] = { bg = tint(c, col.bg, 0.14) }
            hl["RenderMarkdownQuote" .. i] = { fg = c }
        end

        hl.RenderMarkdownCode = { bg = col.inactiveBg }
        -- the ▄/▀ border glyphs are painted using this group's *bg* as their fg
        hl.RenderMarkdownCodeBorder = { bg = col.line }
        hl.RenderMarkdownCodeInfo = { fg = col.comment, italic = true }
        hl.RenderMarkdownCodeInline = { fg = col.number, bg = col.line }
        hl.RenderMarkdownDash = { fg = col.comment }

        hl.RenderMarkdownBullet = { fg = col.operator }
        hl.RenderMarkdownUnchecked = { fg = col.operator }
        hl.RenderMarkdownChecked = { fg = col.plus }
        hl.RenderMarkdownTodo = { fg = col.warning }
        hl.RenderMarkdownLink = { fg = col.hint }
        hl.RenderMarkdownWikiLink = { fg = col.parameter }
        hl.RenderMarkdownLinkTitle = { fg = col.comment, underline = true }
        hl.RenderMarkdownTableHead = { fg = col.keyword, bold = true }
        hl.RenderMarkdownTableRow = { fg = col.line }
        hl.RenderMarkdownIndent = { fg = col.line }

        -- vague leaves these unset, so bold/italic prose renders flat
        hl["@markup.strong"] = { fg = col.fg, bold = true }
        hl["@markup.italic"] = { fg = col.fg, italic = true }
        hl["@markup.strikethrough"] = { fg = col.comment, strikethrough = true }
        hl.Conceal = { fg = col.comment }

        -- [[Touchup]] the other markdown renderer, see lua/plugins/touchup.lua.
        -- same story as above: hl.lua registers these as `default` links onto
        -- NonText/CursorLine/Diagnostic*, and Important/Warning both land on
        -- DiagnosticWarn so they come out identical
        hl.TouchupBulletDash = { fg = col.operator }
        hl.TouchupBulletPlus = { fg = col.operator }
        hl.TouchupBulletStar = { fg = col.operator }
        hl.TouchupCodeBlock = { bg = col.inactiveBg }
        hl.TouchupQuote = { bg = col.line }

        hl.TouchupCheckboxUnchecked = { fg = col.operator }
        hl.TouchupCheckboxChecked = { fg = col.plus }
        hl.TouchupCheckboxPending = { fg = col.warning }
        hl.TouchupCheckboxCancelled = { fg = col.comment }
        hl.TouchupCheckboxImportant = { fg = col.error }
        hl.TouchupCheckboxProgress = { fg = col.hint }
        hl.TouchupCheckboxBracket = { fg = col.comment }

        hl.TouchupAdmonitionNote = { fg = col.hint, bold = true }
        hl.TouchupAdmonitionTip = { fg = col.plus, bold = true }
        hl.TouchupAdmonitionImportant = { fg = col.parameter, bold = true }
        hl.TouchupAdmonitionWarning = { fg = col.warning, bold = true }
        hl.TouchupAdmonitionCaution = { fg = col.error, bold = true }

        -- only visible at conceal = 0, where they dim the ** ~~ ` markers and the
        -- link brackets, parens and URLs
        hl.TouchupDim = { fg = col.comment }
        hl.TouchupLinkLabel = { fg = col.hint, underdotted = true }
        hl.TouchupLinkLabelBold = { fg = col.hint, bold = true, underdotted = true }
        hl.TouchupLinkLabelItalic = { fg = col.hint, italic = true, underdotted = true }
        hl.TouchupLinkLabelStrikethrough = { fg = col.hint, strikethrough = true, underdotted = true }
        hl.TouchupLinkLabelCode = { fg = col.number, bg = col.line, underdotted = true }

        -- touchup renders quotes in cursive, reads fine in both modes
        hl["@markup.quote"] = { italic = true }

        -- [[Mini statusline]]
        hl.MiniStatuslineModeNormal = { bg = col.search, fg = col.bg, bold = true }
        hl.MiniStatuslineModeReplace = { bg = col.error, fg = col.bg, bold = true }
        hl.MiniStatuslineModeVisual = { bg = col.hint, fg = col.bg, bold = true }
        hl.MiniStatuslineModeInsert = { bg = col.string, fg = col.bg, bold = true }
        hl.MiniStatuslineModeCommand = { bg = col.type, fg = col.bg, bold = true }
        hl.VicMiniStatuslineGitBranch = { bg = col.func, fg = col.bg, bold = true }
        hl.VicMiniStatuslineLSP = { fg = col.delta, bold = true }
        hl.DiagnosticHintBold = { fg = col.hint, bold = true }
        hl.DiagnosticOkBold = { fg = col.plus, bold = true }
        hl.DiagnosticInfoNonItalic = { fg = col.constant, bold = true }
        hl.DiagnosticUnnecessary = {}
    end,
})
