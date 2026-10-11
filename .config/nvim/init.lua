-- New core UI
require('vim._core.ui2').enable()

require('pkgs')
require('options')
require('keymaps')
require('servers')
require('plugins')
require('themes')

-- Set colorscheme
vim.cmd([[colorscheme vague]])
