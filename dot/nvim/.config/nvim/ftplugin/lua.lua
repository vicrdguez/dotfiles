vim.bo.comments = ':---,:--'
vim.opt.tabstop = 2
vim.opt.softtabstop = 2
vim.opt.shiftwidth = 2
vim.opt.smartindent = true
vim.opt.autoindent = true

vim.treesitter.start()
-- local lua_ls_cmd = 'lua-language-server'
-- 
-- -- Check if lua-language-server is available
-- if vim.fn.executable(lua_ls_cmd) ~= 1 then
--   return
-- end
-- 
-- local root_files = {
--   '.luarc.json',
--   '.luarc.jsonc',
--   '.luacheckrc',
--   '.stylua.toml',
--   'stylua.toml',
--   'selene.toml',
--   'selene.yml',
--   '.git',
-- }
-- 
-- vim.lsp.start {
--   name = 'luals',
--   cmd = { lua_ls_cmd },
--   root_dir = vim.fs.dirname(vim.fs.find(root_files, { upward = true })[1]),
--   capabilities = require('lib.lsp').make_client_capabilities(),
--   settings = {
--     Lua = {
--       runtime = {
--         version = 'LuaJIT',
--       },
--       diagnostics = {
--         -- Get the language server to recognize the `vim` global, etc.
--         globals = {
--           'vim',
--           'describe',
--           'it',
--           'assert',
--           'stub',
--         },
--         disable = {
--           'duplicate-set-field',
--         },
--       },
--       workspace = {
--         checkThirdParty = false,
--       },
--       telemetry = {
--         enable = false,
--       },
--       hint = { -- inlay hints (supported in Neovim >= 0.10)
--         enable = true,
--       },
--     },
--   },
-- }
