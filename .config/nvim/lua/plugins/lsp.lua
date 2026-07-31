-- [[ LSP configs ]] 
-- Loading all server configs
require('servers')

local augroup = require('lib.util').augroup
local au = require('lib.util').au
local default_on_attach = require('lib.lsp').default_on_attach

-- LSP default keybindings and config
au('LspAttach', {
    group = augroup('default-lsp-conf'),
    callback = default_on_attach
})
