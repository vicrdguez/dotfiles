local nmap = require("lib.util").nmap

-- [[Mini Files]]
--
local MiniFiles = require("mini.files")
MiniFiles.setup({
	windows = {
		preview = false,
		width_nofocus = 20,
		-- width_preview = 100,
	},
})

nmap("<leader>f.", MiniFiles.open, { desc = "Toggle Mini files" })
nmap("-", MiniFiles.open, { desc = "Toggle Mini files" })
nmap("<leader>-", function()
	MiniFiles.open(vim.api.nvim_buf_get_name(0), false)
	MiniFiles.reveal_cwd()
end, { desc = "Toggle Mini files" })

-- toggle hidden files
local show_dotfiles = true

local filter_show = function(_)
	return true
end

local filter_hide = function(fs_entry)
	return not vim.startswith(fs_entry.name, ".")
end

local toggle_dotfiles = function()
	show_dotfiles = not show_dotfiles
	local new_filter = show_dotfiles and filter_show or filter_hide
	MiniFiles.refresh({ content = { filter = new_filter } })
end

-- open in a split
local map_split = function(buf_id, lhs, direction)
	local rhs = function()
		-- Make new window and set it as target
		local cur_target = MiniFiles.get_explorer_state().target_window
		local new_target = vim.api.nvim_win_call(cur_target, function()
			vim.cmd(direction .. " split")
			return vim.api.nvim_get_current_win()
		end)

		MiniFiles.set_target_window(new_target)
		MiniFiles.go_in({ close_on_file = true })
	end

	-- Adding `desc` will result into `show_help` entries
	local desc = "Split " .. direction
	nmap(lhs, rhs, { buffer = buf_id, desc = desc })
end
--
-- Yank in register full path of entry under cursor
local yank_path = function()
	local path = (MiniFiles.get_fs_entry() or {}).path
	if path == nil then
		return vim.notify("Cursor is not on valid entry")
	end
	vim.fn.setreg(vim.v.register, path)
end
--
-- Open path with system default handler (useful for non-text files)
local ui_open = function()
	vim.ui.open(MiniFiles.get_fs_entry().path)
end

vim.api.nvim_create_autocmd("User", {
	pattern = "MiniFilesBufferCreate",
	callback = function(args)
		local buf_id = args.data.buf_id
		nmap("g.", toggle_dotfiles, { buffer = buf_id })
		nmap("gy", yank_path, { buffer = buf_id, desc = "Yank path" })
		nmap("gX", ui_open, { buffer = buf_id, desc = "OS open" })
		-- Tweak keys to your liking
		map_split(buf_id, "<C-s>", "belowright horizontal")
		map_split(buf_id, "<C-v>", "belowright vertical")
	end,
})
