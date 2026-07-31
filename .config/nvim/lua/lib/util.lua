local M = {}

--- Nice little tricks from our frind TJ
P = function(val)
  local info = debug.getinfo(2, 'Sn')
  -- print(caller_func.."\n=====\n" .. vim.inspect(val) .. "\n=====")
  print(string.format('%s: %s\n=====\n%s\n=====', info.short_src, info.name, vim.inspect(val)))
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

M.root_patterns = { '.git', 'lua' }
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
  path = path ~= '' and vim.loop.fs_realpath(path) or nil
  ---@type string[]
  local roots = {}
  if path then
    for _, client in pairs(vim.lsp.get_active_clients { bufnr = 0 }) do
      local workspace = client.config.workspace_folders
      local paths = workspace
          and vim.tbl_map(function(ws)
            return vim.uri_to_fname(ws.uri)
          end, workspace)
        or client.config.root_dir and { client.config.root_dir }
        or {}
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
  color = color or 'kanagawa-wave'
  vim.cmd.colorscheme(color)
end

function M.map(mode, lhs, rhs, opts)
  opts = opts or {}
  if not opts.silent then
    opts.silent = false
  end
  vim.keymap.set(mode, lhs, rhs, opts)
end

function M.nmap(lhs, rhs, opts)
  M.map('n', lhs, rhs, opts)
end

function M.augroup(name)
  return vim.api.nvim_create_augroup('vic-' .. name, { clear = true })
end

function M.au(events, opts)
  vim.api.nvim_create_autocmd(events, opts)
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
  local mod = require(mod_name)
  local ok, _ = pcall(function() -- ok, result
    return mod[func](opts)
  end)
  return ok
end

function M.slug(str)
  return str:gsub('(%u)(%u)', '%1-%2'):lower():gsub(' ', '-')
end

return M
