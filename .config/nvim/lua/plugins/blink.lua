require('blink.cmp').setup {
  fuzzy = { implementation = 'prefer_rust_with_warning' },
  completion = {
    keyword = { range = 'full' },
    list = { selection = { preselect = true, auto_insert = true } },
    documentation = { auto_show = true, auto_show_delay_ms = 500 },
    ghost_text = { enabled = true },
    menu = {
      draw = {
        padding = 2,
        gap = 2,
        treesitter = { 'lsp' },
        columns = {
          { 'label', 'label_description', gap = 2 },
          { 'kind_icon' },

          -- { 'kind_icon', 'source_name', gap = 3 },
        },
      },
    },
  },
  keymap = {
    preset = 'default',
    ['<C-j>'] = { 'select_next', 'fallback_to_mappings' },
    ['<C-k>'] = { 'select_prev', 'fallback_to_mappings' },
    ['<C-f>'] = { 'select_and_accept', 'fallback' },
    ['<C-n>'] = { 'scroll_documentation_down', 'fallback' },
    ['<C-p>'] = { 'scroll_documentation_up', 'fallback' },
  },
}
