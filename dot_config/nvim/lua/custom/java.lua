local M = {}

function M.mvn_test()
    local cmd = "zellij run"
    vim.fn.system(cmd .. " -f -- mvnd test")
end


function M.mvn_package()
    local cmd = "zellij run"
    vim.fn.system(cmd .. " -f -- mvnd clean package -DskipTests")
end

return M
