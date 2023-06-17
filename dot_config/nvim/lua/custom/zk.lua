local run = require("custom.utils").run

local M = {}

function M.zk(command, opts)
    if not run("zk.commands", command, opts) then
        vim.notify("Function does not exist in zk", vim.log.levels.ERROR)
    end
    
end

return M
