local nmap = require('lib.util').nmap

local opts = {
    default_file_explorer = true,
    columns = {
        'permissions',
        'size',
        'icon'
    },
    view_options = { show_hidden = true }
}

-- [[ keymaps ]]
--

nmap('<leader>f.', require('oil').open, { desc = 'Open Oil file browser' })
nmap('<leader>ff', require('oil').open_float, { desc = 'Open Oil file browser in a floating window' })

require('oil').setup(opts)
