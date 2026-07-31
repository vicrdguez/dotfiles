
-- [[ Mini AI ]]
-- Better arround/inside navigation
require('mini.ai').setup { n_lines = 500 }

-- [[ Mini surround ]]
-- Manipulate surround characters on selected text (quotes, brackets, etc...)
require('mini.surround').setup {}

-- [[ Mini comment ]]
-- gc(c) for commenting code
require('mini.comment').setup {}

-- [[ Mini jump ]]
-- Improves f,F,t and T jumps
require('mini.jump').setup {}

-- [[ Mini splitjoin ]]
-- Join args and lists, or split them with gS
require('mini.splitjoin').setup {}

-- [[ Mini pairs ]]
-- Auto-match charachter pairs
require('mini.pairs').setup {}

-- [[ Mini hipatterns ]]
-- Highlight standalone 'FIXME', 'HACK', 'TODO', 'NOTE' comments
require('mini.hipatterns').setup {
  highlighters = {
    fixme = {
      pattern = '%f[%w]()FIXME()%f[%W]',
      group = 'MiniHipatternsFixme',
    },
    hack = {
      pattern = '%f[%w]()HACK()%f[%W]',
      group = 'MiniHipatternsHack',
    },
    todo = {
      pattern = '%f[%w]()TODO()%f[%W]',
      group = 'MiniHipatternsTodo',
    },
    note = {
      pattern = '%f[%w]()NOTE()%f[%W]',
      group = 'MiniHipatternsNote',
    },
    -- Highlight hex color strings (`#rrggbb`) using that color
    hex_color = require('mini.hipatterns').gen_highlighter.hex_color(),
  },
}

-- [[Mini Icons]]
require('mini.icons').setup()

-- [[Mini Diff]]
require('mini.diff').setup()
require('mini.git').setup()
