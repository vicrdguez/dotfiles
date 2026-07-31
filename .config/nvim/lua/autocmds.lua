-- [[ General Autocmds ]]
-- This autocmds apply generally to all files
--
local au = require('lib.util').au
local augroup = require('lib.util').augroup

-- Highlight when yanking text
au('TextYankPost', {
  desc = 'Highlight when yanking (copying) text',
  group = augroup('kickstart-highlight-yank'),
  callback = function()
    vim.highlight.on_yank()
  end,
})
