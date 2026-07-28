-- Default colors
--
local colors = {
  ---@type string
  bg = '#141415',
  ---@type string
  inactiveBg = '#1c1c24',
  ---@type string
  fg = '#cdcdcd',
  ---@type string
  floatBorder = '#878787',
  ---@type string
  line = '#252530',
  ---@type string
  comment = '#606079',
  ---@type string
  builtin = '#b4d4cf',
  ---@type string
  func = '#c48282',
  ---@type string
  string = '#e8b589',
  ---@type string
  number = '#e0a363',
  ---@type string
  property = '#c3c3d5',
  ---@type string
  constant = '#aeaed1',
  ---@type string
  parameter = '#bb9dbd',
  ---@type string
  visual = '#333738',
  ---@type string
  error = '#d8647e',
  ---@type string
  warning = '#f3be7c',
  ---@type string
  hint = '#7e98e8',
  ---@type string
  operator = '#90a0b5',
  ---@type string
  keyword = '#6e94b2',
  ---@type string
  type = '#9bb4bc',
  ---@type string
  search = '#405065',
  ---@type string
  plus = '#7fa563',
  ---@type string
  delta = '#f3be7c',
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

require('vague').setup {
  style = {
    keywords = 'bold',
  },

  on_highlights = function(hl, col)
    -- [[Mini statusline]]
    hl.MiniStatuslineModeNormal = { bg = col.search, fg = col.bg, gui = 'bold' }
    hl.MiniStatuslineModeReplace = { bg = col.error, fg = col.bg, gui = 'bold' }
    hl.MiniStatuslineModeVisual = { bg = col.hint, fg = col.bg, gui = 'bold' }
    hl.MiniStatuslineModeInsert = { bg = col.string, fg = col.bg, gui = 'bold' }
    hl.MiniStatuslineModeCommand = { bg = col.type, fg = col.bg, gui = 'bold' }
    hl.VicMiniStatuslineGitBranch = { bg = col.func, fg = col.bg, gui = 'bold' }
    hl.VicMiniStatuslineLSP = { fg = col.delta, gui = 'bold' }
    hl.DiagnosticHintBold = { fg = col.hint, gui = 'bold' }
    hl.DiagnosticOkBold = { fg = col.plus, gui = 'bold' }
    hl.DiagnosticInfoNonItalic = { fg = col.constant, gui = 'bold' }
  end,
}
