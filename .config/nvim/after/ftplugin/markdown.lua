-- Custom options for writing markdown to my liking.
-- lives in after/ftplugin so $VIMRUNTIME/ftplugin/markdown.vim can't overwrite
-- it, see :h ftplugin-overrule. opt_local, not opt, so nothing leaks to other
-- buffers. conceallevel/concealcursor are already global in lua/options.lua.

-- gq/gw target and colorcolumn match, does not wrap on its own without "t"
vim.opt_local.textwidth = 0

-- hard wraps at 100 no matter how narrow the window is. also dead where it
-- used to live: $VIMRUNTIME/ftplugin/markdown.vim runs after ftplugin/ and
-- rewrites it to jtcqln. from after/ it sticks, so swapping these two works
-- vim.opt_local.formatoptions = "jtcroql"
vim.opt_local.formatoptions:remove("t")

vim.opt_local.wrap = true
vim.opt_local.linebreak = true
vim.opt_local.breakindent = true

-- swap if list:-1 pushes continuations 2 columns too far right
vim.opt_local.showbreak = ""
-- If using heading indenting this seems to work best
-- vim.opt_local.showbreak = "  "

-- lines wrapped list items up under their own text
-- vim.opt_local.breakindentopt = ""
vim.opt_local.breakindentopt = "list:-1"

vim.treesitter.start()

local util = require("lib.util")
local img = require("lib.images")
local mdlink = require("custom.mdlink")

local M = {}

M.namespace = vim.api.nvim_create_namespace("list_markers_ns")

-- function M.get_url_name(name)
--     local curl = require "plenary.curl"
--     local res = curl.get("https://stackoverflow.com/questions/8286677/https-request-in-lua", {
--         accept = "application/json",
--     })
--     local title = string.match(res.body, "<title>(.*)</title>")
--     -- P(title)
--     return title
--     -- https://vi.stackexchange.com/questions/39681/how-to-insert-text-from-lua-function-at-cursor-position-insert-mode
-- end

--- Inserts a markdown link from URL, querying the title from it first and use is as the prompt
--- suggestion. The link format is markdown reflink
--- @param buffnr number the buffer number, defaults to 0
function M.insert_md_link(buffnr)
    buffnr = 0
    local curl = require("plenary.curl")

    local crow, ccol = table.unpack(vim.api.nvim_win_get_cursor(buffnr))
    local mode = vim.fn.mode()
    local url = vim.fn.input("url: ")
    local response = curl.get({
        url = url,
        accept = "text/html",
    })
    local url_title = string.match(response.body, "<title>(.*)</title>")
    ---@diagnostic disable-next-line: redundant-parameter
    local link_title = vim.fn.input("Link title: ", url_title)
    local ref = string.format("[%s]", link_title)
    local link = string.format("[%s]: %s", link_title, url)
    local insert_col = ccol

    if mode == "n" and ccol ~= 0 then
        insert_col = ccol + 1
    end
    local end_col = string.len(link)

    vim.api.nvim_buf_set_text(buffnr, crow - 1, insert_col, crow - 1, insert_col, { ref })
    vim.api.nvim_buf_set_lines(buffnr, -1, -1, false, { link })
    vim.api.nvim_win_set_cursor(0, { crow, end_col })

    -- To insert normal md links
    -- local md_link = string.format("[%s](%s)", link_title, url)
    -- vim.api.nvim_buf_set_text(buffnr, crow - 1, ccol + 1, crow - 1, ccol + 1, { md_link })
    vim.cmd("normal a ")
end

local image_callback = function(title, filepath)
    -- plenary on_stdout callback
    ---@diagnostic disable-next-line: unused-local
    return function(jobid, data, err)
        local rel_path = require("lib.files").relative_to_current_file(filepath)
        local markdown_img_str = "![" .. title .. "](" .. rel_path .. ")"
        vim.cmd("normal A" .. markdown_img_str)
    end
end

---
-- Commands
---
vim.api.nvim_buf_create_user_command(
    0,
    "Screenshot",
    function(opts)
        local title = vim.fn.input("Image title: ")
        local out_dir = nil
        if title ~= "" then
            if opts.args and opts.args ~= "" then
                out_dir = opts.args
            end
            img.screencapture(title, out_dir, image_callback)
        else
            vim.notify("You need to provide a title for the image")
        end
    end,
    { desc = "Invokes screenshot tool and paste img in clipboard into the specified dir or one of a default dir list" }
)

vim.api.nvim_buf_create_user_command(0, "PasteImg", function(opts)
    local title = vim.fn.input("Image title: ")
    local out_dir = nil
    if title ~= "" then
        if opts.args and opts.args ~= "" then
            out_dir = opts.args
        end
        img.pngpaste(title, out_dir, image_callback)
    else
        vim.notify("You need to provide a title for the image")
    end
end, { desc = "Paste img in clipboard into the specified dir or one of a default dir list" })

---
-- Autocmds
---

---
-- maps
---
util.nmap("<leader>ro", mdlink.quick_look, { buffer = true, desc = "quicklook link under cursor" })
util.nmap("gx", mdlink.open, { buffer = true, desc = "open link under cursor" })
util.nmap("<leader>r.", "<cmd>PasteImg<cr>", { buffer = true })
util.nmap("<leader>r,", "<cmd>Screenshot<cr>", { buffer = true })
util.map("i", "<C-i>l", M.insert_md_link, { buffer = true })
util.nmap("<leader>mm", require("plugins.touchup").toggle, { buffer = true, desc = "toggle markdown style" })

-- NoNeckPain is a toggle and this file runs for every markdown buffer, lsp
-- hover floats included, so without these guards the 2nd buffer turns it off
local nnp_on = _G.NoNeckPain and _G.NoNeckPain.state and _G.NoNeckPain.state.enabled
if vim.bo.buftype == "" and not nnp_on then
    vim.cmd("NoNeckPain")
end

return M
