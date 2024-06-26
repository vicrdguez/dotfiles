local M = {}


--- Nice little tricks from our frind TJ
P = function(val, comment)
    local info = debug.getinfo(2, "Sn")
    -- print(caller_func.."\n=====\n" .. vim.inspect(val) .. "\n=====")
    print(string.format("%s\n%s: %s\n=====\n%s\n=====", comment, info.short_src, info.name, vim.inspect(val)))
    return val
end

-- RELOAD = function(mod)
--     -- return require("pleanary.reload").reload_module(...)
--     package.loaded[mod] = nil
--
-- end

R = function(mod)
    -- RELOAD(name)
    package.loaded[mod] = nil
    return require(mod)
end

M.root_patterns = { ".git", "lua" }
-- returns the root directory based on:
-- * lsp workspace folders
-- * lsp root_dir
-- * root pattern of filename of the current buffer
-- * root pattern of cwd
-- Borrowed from: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/util/init.lua
---@return string
function M.get_root()
    ---@type string?
    local path = vim.api.nvim_buf_get_name(0)
    path = path ~= "" and vim.loop.fs_realpath(path) or nil
    ---@type string[]
    local roots = {}
    if path then
        for _, client in pairs(vim.lsp.get_active_clients({ bufnr = 0 })) do
            local workspace = client.config.workspace_folders
            local paths = workspace and vim.tbl_map(function(ws)
                return vim.uri_to_fname(ws.uri)
            end, workspace) or client.config.root_dir and { client.config.root_dir } or {}
            for _, p in ipairs(paths) do
                local r = vim.loop.fs_realpath(p)
                if path:find(r, 1, true) then
                    roots[#roots + 1] = r
                end
            end
        end
    end
    table.sort(roots, function(a, b)
        return #a > #b
    end)
    ---@type string?
    local root = roots[1]
    if not root then
        path = path and vim.fs.dirname(path) or vim.loop.cwd()
        ---@type string?
        root = vim.fs.find(M.root_patterns, { path = path, upward = true })[1]
        root = root and vim.fs.dirname(root) or vim.loop.cwd()
    end
    ---@cast root string
    return root
end

function M.setColor(color)
    color = color or "kanagawa-wave"
    vim.cmd.colorscheme(color)
end

function M.map(mode, lhs, rhs, opts)
    local keys = require("lazy.core.handler").handlers.keys
    ---@cast keys LazyKeysHandler
    -- do not create the keymap if a lazy keys handler exists
    if not keys.active[keys.parse({ lhs, mode = mode }).id] then
        opts = opts or {}
        opts.silent = opts.silent ~= false
        vim.keymap.set(mode, lhs, rhs, opts)
    end
end

function M.nmap(lhs, rhs, opts)
    M.map("n", lhs, rhs, opts)
end

function M.augroup(name)
    return vim.api.nvim_create_augroup("vic-" .. name, { clear = true })
end

-- Concats 2 lists together, appending values of t2 at the end of t1
-- if @copy is set to true, the returned table is a brand new copy, so the original t1 is not modified
-- if any of both tables is nil or empty, the other or an empty table is returned instead
-- See: https://stackoverflow.com/a/15278426
function M.list_append(t1, t2, copy)
    if not t1 or next(t1) == nil then
        return t2 or {}
    elseif not t2 or next(t2) == nil then
        return t1 or {}
    end

    local res = copy and { unpack(t1) } or t1
    for i = 1, #t2 do
        res[#res + 1] = t2[i]
    end

    return res
end

--- Tries to run a function for a module and returns true if succeeds, if this function does not
--- exist, it just returns false instead of failing with an error. It is expected that the caller
--- manages the error
---
--- @param mod_name string of the module to use for the execution
--- @param func string name of the function to be executed
--- @param opts table|string? to be passed to @func
function M.run(mod_name, func, opts)
    local mod     = require(mod_name)
    local ok, res = pcall(function()
        return mod[func](opts)
    end)
    if ok then
        return res
    end
    return nil
end

function M.slug(str)
    return str:gsub("(%u)(%u)", "%1-%2"):lower():gsub(" ", "-")
end

--- Creates a folder if it does not exsist. It assumes happy path now and does not check on any
--- errors like creating a folder in a read only fs.
---
---@param dir string the directory to create
---@return string|boolean mkdir_out created directory or false if it already existed
function M.maybe_mkdir(dir)
    local output = vim.fn.system("ls " .. dir)
    if string.find(output, "No such file") then
        local mkdir_out = vim.fn.system("mkdir -v " .. dir)
        if string.find(mkdir_out, "File exists") then
            return dir
        else
            return mkdir_out
        end
    end
    return true
end

function M.real_path(path, from)
    local expanded_path = vim.fn.expand(path)
    local cmd = "realpath " .. expanded_path
    if from then
        cmd = "cd " .. from .. ";" .. cmd
    end
    local result = vim.fn.systemlist(cmd)
    if result ~= nil then
        return result[1]
    end
    return nil
end

function M.resolve_nix_store_path(path)

end

function M.relative_path(filenames, dir)
    local cmd = "realpath --relative-to " .. dir

    if type(filenames) == table then
        for filename in pairs(filenames) do
            cmd = cmd .. " " .. filename
        end
    else
        cmd = cmd .. " " .. filenames
    end

    local result = vim.fn.systemlist(cmd)
    if result ~= nil then
        if #result > 1 then
            return result
        end
        return result[1]
    end
    return nil
end

function M.relative_to_current(filenames)
    return M.relative_path(filenames, vim.fn.expand("%:h"))
end

-- using rtx to multi-verion java
--/Users/vrodriguez/.local/share/rtx/installs/java/temurin-17.0.6+10/bin/java

function M.get_java_versions()
    local home = os.getenv("HOME")
    local java_install_path = home .. "/.local/share/mise/installs/java/"
    local java_8_version
    local java_11_version
    local java_17_version
    local java_21_version
    local output = vim.fn.json_decode(vim.fn.systemlist({ "mise", "list", "java", "--json" }))
    for _, java in pairs(output) do
        if string.find(java.version, "-8.") then
            java_8_version = java.version
        elseif string.find(java.version, "-11.") then
            java_11_version = java.version
        elseif string.find(java.version, "-17.") then
            java_17_version = java.version
        elseif string.find(java.version, "-21.") then
            java_21_version = java.version
        end
    end
    -- print("java 8: " .. java_8_version)
    -- print("java 11: " .. java_11_version)
    -- print("java 17: " .. java_17_version)
    -- print("java 21: " .. java_21_version)
    return {
        {
            name = "JavaSE-1.8",
            path = java_install_path .. java_8_version,
        },
        {
            name = "JavaSE-11",
            path = java_install_path .. java_11_version,
        },
        {
            name = "JavaSE-17",
            path = java_install_path .. java_17_version,
            default = true,
        },
        {
            name = "JavaSE-21",
            path = java_install_path .. java_21_version,
        },
    }
end

function M.test_tables()
    local test = {
        hei = "word",
        hooo = "word",
    }

    for k, v in ipairs(test) do
        P(k)
        P(v)
    end
end

return M
