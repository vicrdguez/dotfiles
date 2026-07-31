---@diagnostic disable: undefined-field, need-check-nil
---
--- Some git utility functions that I got from mini.git
M = {}

local function stream_to_str(stream)
  return (table.concat(stream):gsub('\n+$', ''))
end

local function read_stream(stream, feed)
  local cb = function(err, data)
    if err then
      return table.insert(feed, 1, 'ERROR: ' .. err)
    end
    if data ~= nil then
      return table.insert(feed, data)
    end
    stream:close()
  end
  stream:read_start(cb)
end

function M.git(args, on_done, opts)
  local ex = 'git'
  local timeout = 30000
  local spawn_opts = opts or {}
  local proc, stdout, stderr = nil, vim.loop.new_pipe(), vim.loop.new_pipe()
  spawn_opts.args, spawn_opts.cwd, spawn_opts.stdio = args, vim.fn.getcwd(), { proc, stdout, stderr }

  local is_sync, res = false, nil

  if on_done == nil then
    is_sync = true
    on_done = function(code, out, err)
      res = { code = code, out = out, err = err }
    end
  end

  local out, err, done = {}, {}, false
  local on_exit = function(code)
    if done then
      return
    end
    done = true

    if proc:is_closing() then
      return
    end
    proc:close()
    -- Convert to strings
    local out_str = stream_to_str(out)
    local err_str = stream_to_str(err):gsub('\r+', '\n'):gsub('\n%S+\n', '\n\n')
    on_done(code, out_str, err_str)
  end

  proc = vim.loop.spawn(ex, spawn_opts, on_exit)
  read_stream(stdout, out)
  read_stream(stderr, err)
  -- Handle timeout
  vim.defer_fn(function()
    if not proc:is_active() then
      return
    end
    vim.notify('(Git) ' .. 'Process timeout', vim.log.levels.WARN)
  end, timeout)
  -- Wait for result
  if is_sync then
    vim.wait(timeout + 10, function()
      return done
    end, 1)
  end

  return res
end

return M
