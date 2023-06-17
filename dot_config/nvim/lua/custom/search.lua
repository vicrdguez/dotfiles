local run = require("custom.utils").run

local M = {}
-- returns a function calling telescope. And configures with theme if specified
--
function M.tel(builtin, opts, theme)
    local params = { builtin = builtin, opts = opts, theme = theme }
    return function()
        local final_opts

        if params.theme then
        final_opts = require('telescope.themes')['get_'..theme](params.opts)
        end
        -- require('telescope.builtin')[builtin](opts)
        require('telescope.builtin')[params.builtin](final_opts)
    end
end

-- Little helper for telescope actions
-- actions is a list
function M.tel_actions(actions)
    return function(prompt_bufnr)
        local telescope_actions = require("telescope.actions")
        for _, action in pairs(actions) do
            vim.notify(action)
            telescope_actions[action](prompt_bufnr)
        end
    end
end


function M.fzf(builtin, opts)
    return function ()
        if not run("fzf-lua", builtin, opts) then
            vim.notify("Function does not exist in fzf-lua")
        end
    end
end


function M.fzfe()

end

return M
