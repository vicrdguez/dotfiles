--- Follow the markdown link under the cursor, whatever shape it takes:
---   [text](path)   ![alt](path)   <https://url>   [[wikilink|alias]]
--- quick_look() previews it with macos QuickLook, open() hands it to the OS.
--- Maps live in after/ftplugin/markdown.lua.

local util = require("lib.util")

local M = {}

local function is_url(s)
    return s:find("^%a[%w+.-]*:") ~= nil
end

--- Link target under the cursor, or nil
---@return string?
function M.target()
    -- injections are ignored by default, which stops at the markdown-level
    -- `inline` node and never reaches the link
    local node = vim.treesitter.get_node({ ignore_injections = false })
    while node do
        local t = node:type()
        if t == "inline_link" or t == "image" then
            for child in node:iter_children() do
                if child:type() == "link_destination" then
                    return vim.treesitter.get_node_text(child, 0)
                end
            end
        elseif t == "shortcut_link" then
            -- [[foo]] parses as a shortcut_link inside a bare [ ] pair, so its
            -- own text is the target. drop the |alias and the #heading
            local target = vim.treesitter.get_node_text(node, 0):sub(2, -2):match("^[^|#]*")
            return target ~= "" and target or nil
        elseif t == "uri_autolink" then
            return vim.treesitter.get_node_text(node, 0):sub(2, -2)
        end
        node = node:parent()
    end
end

local function under(path, root)
    return vim.startswith(path, (root:gsub("/$", "")) .. "/")
end

--- Absolute path for @target, nil if nothing matches inside the project root.
--- note's dir, then the root, then a search under the root for a bare name.
--- an absolute (or ~) target is taken at face value, that one is explicit
---@param target string
---@return string?
function M.resolve(target)
    -- obsidian leaves the extension off wikilinks to notes
    if not target:find("%.[%w]+$") then
        target = target .. ".md"
    end
    -- expands ~ and resolves the .. components
    target = vim.fs.normalize(target)

    if target:sub(1, 1) == "/" then
        return vim.uv.fs_stat(target) and target or nil
    end

    -- lsp workspace folder first, so this is the vault even when nvim was
    -- opened in a subfolder of it
    local note = vim.fn.expand("%:p:h")
    local root = util.get_root()

    -- a scratch note opened outside the project gets its own dir as the
    -- ceiling, or none of its relative links would resolve
    if not under(note, root) then
        root = note
    end

    -- relative to the note and then to the vault, never to nvim's cwd. cwd is
    -- what gx gets wrong
    for _, dir in ipairs({ note, root }) do
        local path = vim.fs.normalize(vim.fs.joinpath(dir, target))
        if under(path, root) and vim.uv.fs_stat(path) then
            return path
        end
    end

    -- only a bare wikilink name is worth searching for, and a search rooted at
    -- $HOME or / is a filesystem crawl rather than a lookup
    if target:find("/") or root == "/" or root == vim.uv.os_homedir() then
        return nil
    end
    return vim.fs.find(target, { path = root, type = "file", limit = 1, follow = false })[1]
end

--- Opens macos quicklook on the link under the cursor. images, PDFs, anything
--- qlmanage has a generator for
function M.quick_look()
    local target = M.target()
    if not target or is_url(target) then
        vim.notify("quicklook wants a file link, got: " .. (target or "nothing"), vim.log.levels.WARN)
        return
    end
    local path = M.resolve(target)
    if not path then
        vim.notify("no such file: " .. target, vim.log.levels.WARN)
        return
    end
    -- vim.fn.system blocks until the preview closes and breaks on spaces
    vim.system({ "qlmanage", "-p", path })
end

--- Same resolution, handed to the OS instead. built-in gx only reads the `url`
--- metadata the highlights query puts on inline_link and image, so wikilinks
--- miss it, and it opens relative destinations against nvim's cwd
function M.open()
    local target = M.target()
    if not target then
        vim.ui.open(vim.fn.expand("<cfile>"))
    elseif is_url(target) then
        vim.ui.open(target)
    else
        local path = M.resolve(target)
        if path then
            vim.ui.open(path)
        else
            vim.notify("no such file: " .. target, vim.log.levels.WARN)
        end
    end
end

return M
