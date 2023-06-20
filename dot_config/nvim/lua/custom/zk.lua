local M = {}

function M.zk(command, opts)
    return function()
        local zk_cmd = require("zk.commands").get(command)
        if zk_cmd then
            zk_cmd(opts)
        else
            M[command](opts)
        end
        -- local zk_cmd = function(_opts)
        --     run("zk.commands", "get", command)(_opts)
        -- end
        -- if zk_cmd then
        --     zk_cmd(opts)
        -- end
    end
    -- if not run("zk.commands", command, opts) then
    --     vim.notify("Function does not exist in zk", vim.log.levels.ERROR)
    -- end
end

function M.prompt_new(opts)
    local dir = opts.dir or "main"
    local title = vim.fn.input("Title: ")
    require("zk.commands").get("ZkNew")({ title = title, dir = dir })
end


function M.remove(opts)
    local fname = vim.fn.expand("%")
    local fpath = vim.fn.expand("%:p")
    local input = vim.fn.input("Are you sure you want to remove ".. fname, "y/n")
    if input == "y" then
        vim.cmd("!rm "..fpath)
    end
end





function M.create_meeting(opts)
    local zk_pick = require("zk").pick_notes
    local zkopts = { tags = {"customer"} }
    vim.tbl_extend("force", opts or {}, zkopts)
    P(zkopts)
    zk_pick(
        zkopts,
        {title = "Pick a customer", multi_select = false },
        function (note)
            P(note)
        end
    )

end

function M.create_customer(opts)
    local zk_new = require("zk.commands").get("ZkNew")
    local cust_name = vim.fn.input("Customer name: ")
    local slug_name = cust_name:gsub("(%u)(%u)", "%1-%2"):lower():gsub(" ", "-")

    local notebook_path = require("zk.util").resolve_notebook_path(0).."/"
    local dir_name = "confluent/"..slug_name
    local zkopts = {
        dir = dir_name,
        title = cust_name,
        template = "customer.md",
        edit = true
    }

    local output = vim.fn.system("ls "..notebook_path..dir_name)
    if string.find(output, "No such file") then
        local mkdir_out = vim.fn.system("mkdir "..notebook_path..dir_name)
        print(mkdir_out)
    end

    vim.tbl_extend("force", opts or {}, zkopts)

    zk_new(zkopts)
end

vim.api.nvim_create_user_command("ZkNewCustomer", function (opts)
    M.create_customer(opts)
end, {desc = "Creates a new customer note in Zk"})

return M
