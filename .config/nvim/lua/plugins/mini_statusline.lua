-- Keeping the statusline separated from the other mini modules as it contains much more
-- customization. Keeping the other file more clean
require('lib.util')
local statusline = require('mini.statusline')
local icons = require('mini.icons')

M = {}
-- [[LSP section]]
M.attached_lsp = {}
-- Get Filetype icon using mini.icons
function get_ft_icon()
  local ft = vim.bo.filetype
  return require('mini.icons').get('filetype', ft), ft
end

local function hl(group, text)
  -- Formats the string as: %#group#text%*
  return string.format('%%#%s#%s%%*', group, text)
end
-- Track LSP changes and add the statusline string into the global table
--
local track_lsp = vim.schedule_wrap(function(data)
  local valid = vim.api.nvim_buf_is_valid(data.buf)
  local clients = vim.lsp.get_clients { bufnr = data.buf }
  local content = ''
  local separator = ''
  for _, v in pairs(clients) do
    content = v.name .. separator .. content
    separator = ' '
  end

  M.attached_lsp[data.buf] = valid and content
  vim.cmd('redrawstatus')
end)

-- Add an autocmd to run track_lsp on attach/dettach
local gr = vim.api.nvim_create_augroup('MiniStatuslineCustom', {})
vim.api.nvim_create_autocmd({ 'LspAttach', 'LspDetach' }, {
  group = gr,
  pattern = '*',
  callback = track_lsp,
  desc = 'Track Lsp clients',
})
-- Build up the section final content
function lsp_sect(args)
  if statusline.is_truncated(args.trunc_width) then
    return ''
  end

  local attached = M.attached_lsp[vim.api.nvim_get_current_buf()] or ''

  if attached == '' then
    return ''
  end

  return ' [' .. attached .. ']'
end

local function fileinfo_sect()
  local icon, ft = get_ft_icon()
  -- return icon .. ' ' .. ft
  return icon .. ' '
end

-- [[ Git section]]
--
local git_summary = function(data)
  local summary = vim.b[data.buf].minigit_summary
  vim.b[data.buf].minigit_summary_string = summary.head_name
end

vim.api.nvim_create_autocmd('User', {
  group = gr,
  pattern = 'MiniGitUpdated',
  callback = git_summary,
  desc = 'Format statusline git section',
})
-- [[Diff section]]

local diff_summary = function(data)
  local summary = vim.b[data.buf].minidiff_summary
  local t = {}
  if summary.add > 0 then
    table.insert(t, hl('DiagnosticOkBold', '+' .. summary.add))
  end
  if summary.change > 0 then
    table.insert(t, hl('DiagnosticWarn', '~' .. summary.change))
  end
  if summary.change > 0 then
    table.insert(t, hl('DiagnosticError', '-' .. summary.delete))
  end
  vim.b[data.buf].minidiff_summary_string = table.concat(t, ' ')
end
vim.api.nvim_create_autocmd('User', {
  group = gr,
  pattern = 'MiniDiffUpdated',
  callback = diff_summary,
  desc = 'Format statusline Diff section',
})

-- [[Final Statusline content]]
function statusline_content()
  local mode, mode_hl = statusline.section_mode { trunc_width = 10000 } -- always short form
  local diff = statusline.section_diff { trunc_width = 75, icon = '' }
  local diagnostics = statusline.section_diagnostics {
    trunc_width = 75,
    icon = '',
    signs = {
      ERROR = hl('DiagnosticError', 'E'),
      WARN = hl('DiagnosticWarn', 'W'),
      INFO = hl('DiagnosticInfoNonItalic', 'I'),
      HINT = hl('DiagnosticHintBold', 'H'),

      -- ERROR = hl('DiagnosticError', ' : '),
      -- WARN = hl('DiagnosticWarn', ' : '),
      -- HINT = hl('DiagnosticInfoNonItalic', '󰰂 : '),
      -- INFO = hl('DiagnosticHint', ' : '),
    },
  }
  local filename = statusline.section_filename { trunc_width = 140 }
  local location = statusline.section_location { trunc_width = 75 }

  local lsp = lsp_sect { trunc_width = 75 }
  local fileinfo = fileinfo_sect()
  local git = statusline.section_git { trunc_width = 40 }

  return statusline.combine_groups {
    { strings = { ' ' } },
    { hl = mode_hl, strings = { '[' .. mode .. ']' } },
    { hl = 'VicMiniStatuslineGitBranch', strings = { git } },
    { hl = 'MiniStatuslineDevinfo', strings = { diff } },
    '%<', -- Mark general truncate point
    { hl = 'MiniStatuslineFilename', strings = { fileinfo, filename } },
    '%=', -- End left alignment
    { hl = 'MiniStatuslineFileinfo', strings = { diagnostics } },
    { hl = 'VicMiniStatuslineLSP', strings = { lsp } },
    { hl = mode_hl, strings = { location } },
  }
end

statusline.setup {
  content = {
    inactive = nil,
    active = statusline_content,
  },
  use_icons = true,
}

-- Error:   or ✘
-- Warning:   or 
-- Hint:  or i
-- Info:  or i
return M
