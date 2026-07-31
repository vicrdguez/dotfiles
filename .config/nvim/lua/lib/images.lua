local util = require('lib.util')

local M = {}

local function screencapture_cmd(output_file)
  local executable = 'screencapture'
  return executable .. ' -i ' .. output_file
end

local function pngpaste_cmd(output_file)
  local executable = 'pngpaste'
  return executable .. ' ' .. output_file
end

local function fd_cmd()
  local executable = 'fd'
  return { executable, '--full-path', vim.loop.cwd(), '--type', 'd' }
  -- return executable .. " --type d"
end

function M.find_img_dir()
  local img_dirs = { 'assets', 'images', 'img' }
  local result = vim.fn.systemlist(fd_cmd())
  for _, img_dir in ipairs(img_dirs) do
    for _, dir in ipairs(result) do
      if dir:match(img_dir) then
        -- Remove the trailing `/`
        return string.sub(dir, 1, -2)
      end
    end
  end
  print('no assets folder found')
  return require('lib.files').maybe_mkdir('assets')
end

function M.screencapture(title, output_dir, callback)
  title = util.slug(title)
  output_dir = output_dir or M.find_img_dir()
  local filename = title .. '_' .. os.date('%Y%m%d%H%M%S') .. '.png'
  local filepath = output_dir .. '/' .. filename
  print(filepath)
  vim.fn.jobstart(screencapture_cmd(filepath), {
    stdout_buffered = true,
    on_stdout = callback(title, filepath),
  })
end

function M.pngpaste(title, output_dir, callback)
  title = util.slug(title)
  output_dir = output_dir or M.find_img_dir()
  local filename = title .. '_' .. os.date('%Y%m%d%H%M%S') .. '.png'
  local filepath = output_dir .. '/' .. filename
  vim.fn.jobstart(pngpaste_cmd(filepath), {
    stdout_buffered = true,
    on_stdout = callback(title, filepath),
  })
end

return M
