---@mod lib.lsp
---
---@brief [[
---LSP related functions
---@brief ]]
local nmap = require('lib.util').nmap
local tel = require('lib.search').tel

local M = {}

function M.is_ls_present(server_bin)
    if vim.fn.executable(server_bin) ~= 1 then
        -- vim.notify('Language Sever ['..server_bin..'] is not present in the system', vim.log.levels.WARN)
        return false
    end
    return true
end

---Gets a 'ClientCapabilities' object, describing the LSP client capabilities
---Extends the object with capabilities provided by plugins.
---@return lsp.ClientCapabilities
function M.make_client_capabilities()
  local capabilities = vim.lsp.protocol.make_client_capabilities()
  -- Add com_nvim_lsp capabilities
  local cmp_lsp = require('cmp_nvim_lsp')
  local cmp_lsp_capabilities = cmp_lsp.default_capabilities()
  capabilities = vim.tbl_deep_extend('keep', capabilities, cmp_lsp_capabilities)
  -- Add any additional plugin capabilities here.
  -- Make sure to follow the instructions provided in the plugin's docs.
  return capabilities
end

-- Default LSP on_attach function to run on the designed autocmd
function M.default_on_attach(event)
    nmap('<leader>da', vim.lsp.buf.code_action , { desc = 'Search Code actions' })
    --nmap('<leader>df', function() vim.lsp.buf.format({async = true}) end, { desc = 'Search Diagnostics' })
    nmap('<leader>dr', vim.lsp.buf.rename, { desc = 'Rename symbol under cursor' })
    nmap('K', vim.lsp.buf.hover, { desc = 'Hover documentation' })
    nmap('gD', vim.lsp.buf.declaration, { desc = 'Goto declaration' })
    nmap('gk', vim.diagnostic.open_float, { desc = 'Diagnostics float' })

    -- Highlight references of the word under the cursor after some time.
    -- Highlights clear when moving the cursor again. 
    -- - :help CursorHold
    -- - https://github.com/nvim-lua/kickstart.nvim/blob/93fde0556e82ead2a5392ccb678359fa59437b98/init.lua#L509
    local client = vim.lsp.get_client_by_id(event.data.client_id)
    if client and client.server_capabilities.documentHighlightProvider then
        vim.api.nvim_create_autocmd({'CursorHold', 'CursorHoldI'}, {
            buffer = event.buf,
            callback = vim.lsp.buf.document_highlight
        })
        vim.api.nvim_create_autocmd({'CursorMoved', 'CursorMovedI'}, {
            buffer = event.buf,
            callback = vim.lsp.buf.clear_references,
        })
    end
end

return M
