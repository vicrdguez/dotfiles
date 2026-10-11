-- Custom options for writing typst to my liking
vim.opt_local.textwidth = 100
vim.opt_local.formatoptions = 'jtcroql'
vim.opt_local.wrap = true
vim.opt_local.linebreak = true
vim.opt_local.breakindent = true

local img = require('lib.images')

local image_callback = function(title, filepath)
  -- plenary on_stdout callback
  ---@diagnostic disable-next-line: unused-local
  return function(jobid, data, err)
    local rel_path = require('lib.files').relative_to_current_file(filepath)
    local typst_img_str = 'image("' .. rel_path .. '")'
    vim.cmd('normal A' .. typst_img_str)
  end
end

vim.api.nvim_create_user_command('PasteImg', function(opts)
  local title = vim.fn.input('Image title: ')
  local out_dir = nil
  if title ~= '' then
    if opts.args and opts.args ~= '' then
      out_dir = opts.args
    end
    img.pngpaste(title, out_dir, image_callback)
  else
    vim.notify('You need to provide a title for the image')
  end
end, { desc = 'Paste img in clipboard into the specified dir or one of a default dir list' })

