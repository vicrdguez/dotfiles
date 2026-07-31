local M = {}

function M.relative_path(filenames, dir)
  local cmd = 'realpath --relative-to ' .. dir

  if type(filenames) == table then
    for filename in pairs(filenames) do
      cmd = cmd .. ' ' .. filename
    end
  else
    cmd = cmd .. ' ' .. filenames
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

function M.relative_to_current_file(filenames)
  return M.relative_path(filenames, vim.fn.expand('%:h'))
end

--- Creates a folder if it does not exsist. It assumes happy path now and does not check on any
--- errors like creating a folder in a read only fs.
---
---@param dir string the directory to create
---@return string|boolean mkdir_out created directory or false if it already existed
function M.maybe_mkdir(dir)
  local output = vim.fn.system('ls ' .. dir)
  if string.find(output, 'No such file') then
    local mkdir_out = vim.fn.system('mkdir -v ' .. dir)
    if string.find(mkdir_out, 'File exists') then
      return dir
    else
      return mkdir_out
    end
  end
  return true
end

function M.get_own_dir()
  -- debug.getinfo(1, "S") gives info about the current function (= this file)
  local source = debug.getinfo(2, 'S').source -- usually starts with @

  -- Remove @ prefix and drop the filename itself
  local path = source:gsub('^@', ''):gsub('/[^/]+%.lua$', '')

  return path
end

function M.require_all(dir)
  -- Simple & robust version using vim.loop
  local root = dir:match('/lua/(.*)$')
  local scandir = vim.loop.fs_scandir(dir)
  if not scandir then
    vim.notify('Cannot open directory: ' .. dir, vim.log.levels.ERROR)
    return
  end

  while true do
    local name, typ = vim.loop.fs_scandir_next(scandir)
    if not name then
      break
    end

    -- We only want .lua files, not directories, and init.lua itslef
    if typ == 'file' and name:match('%.lua$') and name ~= 'init.lua' then
      -- Convert to module path
      -- examples:
      --   options.lua     →  yourfolder.options
      --   lsp/config.lua  →  yourfolder.lsp.config
      local mod = root .. '/' .. name
      mod = mod:gsub('%.lua$', ''):gsub('/', '.')

      local ok, err = pcall(require, mod)
      if not ok then
        vim.notify('Failed to load module ' .. mod .. ':\n' .. tostring(err), vim.log.levels.ERROR)
      end
    end
  end

  -- local config_path = vim.fn.stdpath('config')
  -- -- Get all .lua files in the specified directory relative to the config path
  -- local pattern = config_path .. '/' .. directory .. '/*.lua'
  -- local files = vim.fn.glob(pattern, false, true)
  -- vim.notify(pattern)
  -- for _, file_path in ipairs(files) do
  --   -- Extract the module name: remove the base path and the .lua extension, then replace slashes with dots
  --   local module_name = file_path:gsub('^' .. config_path .. '/lua/', ''):gsub('%.lua$', ''):gsub('/', '.')
  --
  --   -- Only require non-init modules
  --   if vim.fn.match(module_name, [[\.init]]) == -1 then
  --     -- vim.notify(module_name)
  --     -- Require the module
  --     -- pcall is used here to safely handle potential errors in required files
  --     local ok, err = pcall(require, module_name)
  --     if not ok then
  --       vim.notify('Error requiring ' .. module_name .. ': ' .. tostring(err), vim.log.levels.ERROR)
  --     end
  --   end
  -- end
end

-- Example usage to load all files in the 'user/options' directory:
-- require_all("user/options")

return M
