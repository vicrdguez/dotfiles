local nmap = require('lib.util').nmap
local fzf = require('lib.search').fzf
local fzf_act = require('lib.search').fzf_act

-- {
--     { "<leader><space>", fzf("buffers"),               { desc = "Switch buffers" } },
--     -- {"<leader>,",  fzf(""), {desc = "Switch buffers"}},
--     { "<leader>.",       fzf("files"),                 { desc = "Find files" } },
--     { "<leader>/",       fzf("grep_curbuf"),           { desc = "Search in buffer" } },
--     { "<leader>;",       fzf("commands"),              { desc = "Command list" } },
--     { "<leader>:",       fzf("command_history"),       { desc = "Command history" } },
--     { "<leader>'",       fzf("resume"),                { desc = "Resume last search" } },
--     { "<leader>'",       fzf("resume"),                { desc = "Resume last search" } },
--     { "<leader>fg",      fzf("git_files"),             { desc = "Resume last search" } },
--     { "<leader>fr",      fzf("oldfiles"),              { desc = "Resume last search" } },
--     { "<leader>sg",      fzf("live_grep_glob"),        { desc = "Live grep project" } },
--     { "<leader>sG",      fzf("grep_project"),          { desc = "Fuzzy grep project" } },
--     { "<leader>ht",      fzf("colorschemes"),          { desc = "Search colorschemes" } },
--     { "<leader>hh",      fzf("help_tags"),             { desc = "Search help tags" } },
--     { "<leader>hm",      fzf("man_pages"),             { desc = "Search man pages" } },
--     { "<leader>hk",      fzf("keymaps"),               { desc = "Search keymaps" } },
--     { "<leader>ha",      fzf("autocmds"),              { desc = "Search autocommands" } },
--     -- { "<leader>dd",      fzf("diagnostics_document"),  { desc = "Search diagnostics in document" } },
--     -- { "<leader>dD",      fzf("diagnostics_workspace"), { desc = "Search diagnostics in ws" } },
--     { "<leader>ds",      fzf("lsp_document_symbols"),  { desc = "Document symbols" } },
--     { "<leader>dS",      fzf("lsp_workspace_symbols"), { desc = "Document symbols" } },
--     { "<leader>da",      fzf("lsp_code_actions"),      { desc = "Code actions" } },
--     {
--         "gd",
--         fzf("lsp_definitions", { sync = true, jump_to_single_result = true }),
--         { desc = "LSP definitions" }
--     },
--     { "gD", fzf("lsp_declarations"), { desc = "LSP declarations" } },
--     {
--         "gr",
--         fzf("lsp_references", { sync = true, jumpt_to_single_result = true }),
--         { desc = "LSP references" }
--     },
--
-- }
--
-- [[ Fzf lua keymaps ]]
--
nmap('<leader><leader>', fzf('buffers'), { desc = 'Switch buffers' })
-- nmap('<leader>,', fzf('files'), { desc = 'Find files (root dir)' })
nmap('<leader>.', fzf('files'), { desc = 'Find files' })
nmap('<leader>fr', fzf('old_files', { cwd = true }), { desc = 'Find recent files' })
nmap('<leader>fg', fzf('git_files', { cwd = true }), { desc = 'Find git files' })
-- Search
nmap('<leader>/', fzf('grep_curbuf'), { desc = 'Search in buffer' })
nmap('<leader>sg', fzf('live_grep'), { desc = 'Live Grep project' })
nmap('<leader>sG', fzf('grep_project'), { desc = 'Fuzzy grep project' })
--help
nmap('<leader>hh', fzf('help_tags'), { desc = 'Seach help pages' })
nmap('<leader>ht', fzf('colorschemes'), { desc = 'Search colorschemes' })
nmap('<leader>hm', fzf('man_pages'), { desc = 'Search man pages' })
nmap('<leader>hk', fzf('keymaps'), { desc = 'Search keymaps' })
nmap('<leader>ha', fzf('autocmds'), { desc = 'Search autocommands' })
-- commands
nmap('<leader>;', fzf('commands'), { desc = 'Search commands' })
nmap('<leader>:', fzf('command_history'), { desc = 'Command history' })
-- others
nmap("<leader>'", fzf('resume'), { desc = 'Resume last search' })
-- lsp
nmap('gd', fzf('lsp_definitions', { sync = true, jump1 = true }), { desc = 'Go to definition' })
nmap('gr', fzf('lsp_references', { sync = true, jump1 = true }), { desc = 'Go to reference' })
nmap('gi', fzf('lsp_implementations'), { desc = 'Go to implementation' })
nmap('<leader>ds', fzf('lsp_document_symbols'), { desc = 'Search symbol in file' })
nmap('<leader>dws', fzf('lsp_workspace_symbols'), { desc = 'Search symbol in workspace' })
nmap('<leader>dd', fzf('diagnostics_workspace'), { desc = 'Search Diagnostics' })
nmap('<leader>dD', fzf('diagnostics_document'), { desc = 'Search Diagnostics' })
nmap('<leader>dA', fzf('lsp_code_actions'), { desc = 'Search Diagnostics' })

--
--
local opts = {
  winopts = {
    height = 0.3,
    width = 1,
    row = 1,
    border = false,
    preview = {
      horizontal = 'right:50%', -- right|left:size
      wrap = 'wrap',
      -- default = "Fuzzy grep in project"
    },
  },
  keymap = {
    builtin = {
      ['<C-n>'] = 'preview-page-down',
      ['<C-p>'] = 'preview-page-up',
      -- ["<S-left>"] = "preview-page-reset",
    },
  },
  file_icon_padding = ' ',
  file_ignore_patterns = { 'lazy-%.json$' },
  previewers = {
    builtin = {
      extensions = {
        ['png'] = { 'chafa' },
        ['jpg'] = { 'chafa' },
        ['svg'] = { 'chafa' },
      },
    },
  },
  fzf_opts = {
    ['--history'] = vim.fn.stdpath('data') .. '/fzf-lua-history',
  },
  actions = {
    files = {
      ['default'] = fzf_act('file_edit'),
      ['ctrl-s'] = fzf_act('file_split'),
      ['ctrl-v'] = fzf_act('file_vsplit'),
      ['ctrl-l'] = fzf_act('file_sel_to_qf'),
    },
  },
}

local fzf_lua = require('fzf-lua')
fzf_lua.setup(opts)
fzf_lua.register_ui_select()
