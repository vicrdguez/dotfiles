-- ## Mini.nvim configuration
-- Since mini is made of multiple little modules that act as independent plugins, I'm keeping
-- in this file only the ones with the most concise configuration. Once the config start growing
-- beyond what feels right to me, I refactor into an independent file.
-- - see mini_statusline.lue and mini_files.lua

-- [[ Mini AI ]]
-- Better arround/inside navigation
require("mini.ai").setup({ n_lines = 500 })

-- [[ Mini surround ]]
-- Manipulate surround characters on selected text (quotes, brackets, etc...)
require("mini.surround").setup({})

-- [[ Mini comment ]]
-- gc(c) for commenting code
require("mini.comment").setup({})

-- [[ Mini Notify]]
-- Better notifications
require("mini.notify").setup({})

-- [[ Mini jump ]]
-- Improves f,F,t and T jumps
require("mini.jump").setup({})

-- [[ Mini splitjoin ]]
-- Join args and lists, or split them with gS
require("mini.splitjoin").setup({})

-- [[ Mini pairs ]]
-- Auto-match charachter pairs
require("mini.pairs").setup({})

-- [[ Mini hipatterns ]]
-- Highlight standalone 'FIXME', 'HACK', 'TODO', 'NOTE' comments
require("mini.hipatterns").setup({
	highlighters = {
		fixme = {
			pattern = "%f[%w]()FIXME()%f[%W]",
			group = "MiniHipatternsFixme",
		},
		hack = {
			pattern = "%f[%w]()HACK()%f[%W]",
			group = "MiniHipatternsHack",
		},
		todo = {
			pattern = "%f[%w]()TODO()%f[%W]",
			group = "MiniHipatternsTodo",
		},
		note = {
			pattern = "%f[%w]()NOTE()%f[%W]",
			group = "MiniHipatternsNote",
		},
		-- Highlight hex color strings (`#rrggbb`) using that color
		hex_color = require("mini.hipatterns").gen_highlighter.hex_color(),
	},
})

-- [[Mini Icons]]
require("mini.icons").setup()

-- [[Mini cmdline]]
-- Better completion for the cmdline
-- require("mini.cmdline").setup()

-- [[Mini Diff]]
require("mini.diff").setup()
require("mini.git").setup()

local lang_patterns = {
	markdown_inline = { "markdown.lua" },
}
local MiniSnippets = require("mini.snippets")
MiniSnippets.setup({
	snippets = {
		-- load from /snippets based on lang name
		MiniSnippets.gen_loader.from_lang({
			lang_patterns = lang_patterns,
		}),
	},
})
-- MiniSnippets.start_lsp_server({ match = false })
