---@brief
--- https://github.com/Feel-ix-343/markdown-oxide
---
--- Editor Agnostic PKM: you bring the text editor and we
--- bring the PKM.
---
--- Inspired by and compatible with Obsidian.
---
--- Check the readme to see how to properly setup.
---
--- Shamelessly stollen from nvim-lspconfig

local augroup = require("lib.util").augroup
local au = require("lib.util").au

---@param client vim.lsp.Client
---@param bufnr integer
local function on_attach(client, bufnr)
    -- Daily notes commands:
    -- :Daily
    -- :Daily yesterday
    -- :Daily tomorrow
    -- :Daily two days ago
    -- :Daily next monday
    -- :Daily +7
    -- :Daily -3
    vim.api.nvim_buf_create_user_command(bufnr, "Daily", function(args)
        local target = args.args == "" and "today" or args.args

        client:exec_cmd({
            title = ("Markdown Oxide: %s"):format(target),
            command = "jump",
            arguments = { target },
        }, { bufnr = bufnr })
    end, {
        nargs = "*",
        desc = "Open a Markdown Oxide daily notes",
    })

    -- codelens support
    if client.server_capabilities.codeLensProvider then
        vim.lsp.codelens.enable(true, {
            bufnr = bufnr,
            client_id = client.id,
        })
    end
end

---@type vim.lsp.Config
return {
    root_markers = { ".git", ".obsidian", ".moxide.toml" },
    filetypes = { "markdown" },
    cmd = { "markdown-oxide" },
    capabilities = {
        workspace = {
            didChangeWatchedFiles = {
                dynamicRegistration = true,
            },
        },
    },
    on_attach = on_attach,
}
