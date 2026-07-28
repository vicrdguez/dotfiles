-- Custom options for writing markdown to my liking
vim.opt.textwidth = 100
vim.opt.formatoptions = 'jtcroql'
vim.opt.wrap = true
vim.opt.linebreak = true
vim.opt.breakindent = true
vim.treesitter.start()

local util = require('lib.util')
local img = require('lib.images')

local M = {}

M.namespace = vim.api.nvim_create_namespace('list_markers_ns')

local function get_root_node(buffnr, parser)
  local p = vim.treesitter.get_parser(buffnr, parser, {})
  local tree = p:parse()[1]
  return tree:root()
end

--- Conceals markdown lists to be bullet points
function M.md_prettify(buffnr)
  buffnr = buffnr or 0
  vim.api.nvim_buf_clear_namespace(buffnr, M.namespace, 0, -1)

  if vim.bo.filetype ~= 'markdown' then
    return
  end

  local list_marker_q = vim.treesitter.query.parse(
    'markdown',
    [[
    [(list_marker_minus)
     (list_marker_star)] @list_marker
    ]]
  )
  -- P(M.namespace)

  local root = get_root_node(buffnr, 'markdown')
  for id, node in list_marker_q:iter_captures(root, buffnr, 0, -1) do
    local start_row, start_col, end_row, end_col = node:range()
    -- local marker_col = col ~= 0 and col +2
    local list_marker_text = vim.treesitter.get_node_text(node, buffnr)
    local new_marker_text = list_marker_text:gsub('[-*]', '•')
    -- local count = 0
    -- if count <= 0 then
    --     P(new_marker_text)
    --     count = count + 1
    -- end
    -- local marker_pos = string.find(list_marker_text, "-") or string.find(list_marker_text, "*")
    -- local extmark_col

    -- if marker_pos then
    -- extmark_col = start_col + marker_pos - 1
    vim.api.nvim_buf_set_extmark(buffnr, M.namespace, start_row, start_col, {
      virt_text = { { new_marker_text, 'prettyMdListMarker' } },
      virt_text_pos = 'overlay',
      hl_mode = 'combine',
      end_col = end_col,
      end_row = end_row,
    })
    -- end
  end
end

--- Opens macos quicklook with the image under the cursor
function M.quick_look(buffnr)
  buffnr = buffnr or 0
  local crow, _ = unpack(vim.api.nvim_win_get_cursor(buffnr))
  local img_link_q = vim.treesitter.query.parse(
    'markdown_inline',
    [[
    (inline
        (image
        (link_destination) @img_link))
    ]]
  )

  local root = get_root_node(buffnr, 'markdown_inline')
  -- store cwd
  local cwd = vim.fn.getcwd()
  -- get note dir
  local buff_dir = vim.fn.expand('%:h')
  -- change cwd to be the note dir, so we can get the real path from the same dir the note is located
  -- using the relative path
  vim.fn.chdir(buff_dir)
  for _, node in img_link_q:iter_captures(root, buffnr, crow - 1, crow) do
    -- I don't care about columns here
    local row1, _, row2, _ = node:range()
    if row1 == crow - 1 and row2 == crow - 1 then
      local img_target = vim.treesitter.get_node_text(node, buffnr)
      vim.fn.system('qlmanage -p ' .. img_target)
    end
  end
  -- Restore cwd
  vim.fn.chdir(cwd)
end

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
  local curl = require('plenary.curl')

  local crow, ccol = unpack(vim.api.nvim_win_get_cursor(buffnr))
  local mode = vim.fn.mode()
  local url = vim.fn.input('Url: ')
  local response = curl.get {
    url = url,
    accept = 'text/html',
  }
  local url_title = string.match(response.body, '<title>(.*)</title>')
  ---@diagnostic disable-next-line: redundant-parameter
  local link_title = vim.fn.input('Link title: ', url_title)
  local ref = string.format('[%s]', link_title)
  local link = string.format('[%s]: %s', link_title, url)
  local insert_col = ccol

  if mode == 'n' and ccol ~= 0 then
    insert_col = ccol + 1
  end
  local end_col = string.len(link)

  vim.api.nvim_buf_set_text(buffnr, crow - 1, insert_col, crow - 1, insert_col, { ref })
  vim.api.nvim_buf_set_lines(buffnr, -1, -1, false, { link })
  vim.api.nvim_win_set_cursor(0, { crow, end_col })

  -- To insert normal md links
  -- local md_link = string.format("[%s](%s)", link_title, url)
  -- vim.api.nvim_buf_set_text(buffnr, crow - 1, ccol + 1, crow - 1, ccol + 1, { md_link })
  vim.cmd('normal a ')
end

local image_callback = function(title, filepath)
  -- plenary on_stdout callback
  ---@diagnostic disable-next-line: unused-local
  return function(jobid, data, err)
    local rel_path = require('lib.files').relative_to_current_file(filepath)
    local markdown_img_str = '![' .. title .. '](' .. rel_path .. ')'
    vim.cmd('normal A' .. markdown_img_str)
  end
end

---
-- Commands
---
vim.api.nvim_create_user_command(
  'Screenshot',
  function(opts)
    local title = vim.fn.input('Image title: ')
    local out_dir = nil
    if title ~= '' then
      if opts.args and opts.args ~= '' then
        out_dir = opts.args
      end
      img.screencapture(title, out_dir, image_callback)
    else
      vim.notify('You need to provide a title for the image')
    end
  end,
  { desc = 'Invokes screenshot tool and paste img in clipboard into the specified dir or one of a default dir list' }
)

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

---
-- Autocmds
---
vim.api.nvim_create_autocmd({
    "FileChangedShellPost",
    "Syntax",
    "TextChanged",
    "InsertLeave",
    -- "FileType",
    "WinScrolled"
}, {
    group = util.augroup("md_prettify"),
    pattern = "*",
    callback = function ()
        M.md_prettify()
    end
})

---
-- maps
---
util.nmap('<leader>ro', M.quick_look)
util.nmap('<leader>r.', '<cmd>PasteImg<cr>')
util.nmap('<leader>r,', '<cmd>Screenshot<cr>')
util.map('i', '<C-i>l', M.insert_md_link)

return M
