local M = {}

--- starts a job that runs `chezmoi diff` on a file. returns
---
--- @param file string file to check for changes using chezmoi diff
--- @param source_path boolean if true, the file will be checked as if it is a chezmoi source file
--- param success_cb function a callback called on success of the command. removed for blocking version
---@diagnostic disable-next-line: unused-local
function M.chezmoi_has_changes(file, source_path, success_cb)
    local source_flag = source_path and "--source-path" or ""
    local cmd = { "chezmoi", "diff", source_flag, file }
    local diff_out = vim.fn.systemlist(cmd)
    if diff_out then
        return true
    end
    return false
    -- NOTE This is the non-blocking version more complex, but should be more performant
    --
    -- vim.fn.jobstart(cmd, {
    --     stdout_buffered = true,
    --     detach = true,
    --     on_stdout = function(_, diff_out)
    --         if diff_out and diff_out[1] ~= "" then
    --             success_cb()
    --         end
    --     end,
    --     on_stderr = function(_, err)
    --         if err and err[1] ~= "" then
    --             -- vim.notify(
    --             --     "Could not get chezmoi diff for file: " .. file,
    --             --     vim.log.levels.ERROR
    --             -- )
    --             -- vim.notify(vim.inspect(err), vim.log.levels.ERROR)
    --             vim.notify(table.concat(err, " "))
    --         end
    --     end
    -- })
end

--- Starts a job that runs `chezmoi managed -i files` and check if a file is part of chezmoi managed
--- files
---
--- @param file string filename of the file that wants to be checked if it is managed
--- @param use_abs_path boolean indicates if the check should be done using absolute paths
--- @param success_cb function a callback called on success of the command
function M.chezmoi_file_is_managed(file, use_abs_path, success_cb)
    local cmd = { "chezmoi", "managed", "-i", "files" }
    if use_abs_path then
        vim.list_extend(cmd, { "-p", "absolute" })
    end
    vim.fn.jobstart(cmd, {
        stdout_buffered = true,
        on_stdout = function(_, managed_files)
            if vim.tbl_contains(managed_files, file) then
                success_cb()
            end
        end,
        on_stderr = function(_, err)
            if err and err[1] ~= "" then
                -- vim.notify(
                --     "Could not check if file: " .. file .. "is managed by chezmoi",
                --     vim.log.levels.ERROR
                -- )
                -- vim.notify(vim.inspect(err), vim.log.levels.ERROR)
                vim.notify(table.concat(err, " "))
            end
        end
    })
end

--- Starts a job that runs `chezmoi add` to make a file managed by chezmoi. If the file is already
--- managed it just adds it again to upadate with its changes
---
--- @param file string file to add to chezmoi
--- @param success_cb function a callback called on success of the command
function M.chezmoi_add(file, success_cb)
    local cmd = { "chezmoi", "add", file }
    vim.fn.jobstart(cmd, {
        stdout_buffered = true,
        on_stdout = function(_, output)
            if output then
                vim.notify("Chezmoi File: " .. file .. " added to source")
                success_cb() -- calling success callback
            end
        end,
        on_stderr = function(_, err)
            if err and err[1] ~= "" then
                vim.notify(
                    "Chezmoi: could not add file: " .. file .. " to chezmoi",
                    vim.log.levels.ERROR
                )
                vim.notify(vim.inspect(err), vim.log.levels.ERROR)
            end
        end
    })
end

function M.chezmoi_get_target_path(file)
    local cmd = { "chezmoi", "target-path", file }
    local target_path = vim.fn.system(cmd):gsub("[\n\r]", "")
    return target_path
end

function M.chezmoi_get_source_path(file)
    local cmd = { "chezmoi", "source-path", file }
    local source_path = vim.fn.system(cmd):gsub("[\n\r]", "")
    return source_path
end

--- Starts a job that runs `chezmoi apply` to update the state of a target file from a source file
--- in the chezmoi source path, thus applying changes done in the source dir, to the target dir
---
--- @param file string file to add to chezmoi
--- @param source_path boolean if true, the file will be checked as if it is a chezmoi source file
--- param success_cb function a callback called on success of the command. Removed for blocking version
function M.chezmoi_apply(file, source_path)
    local source_flag = source_path and "--source-path" or ""
    local cmd = { "chezmoi", "apply", source_flag, file, "--force", "--verbose" }

    local output = vim.fn.systemlist(cmd)
    if output then
        vim.notify("Chezmoi: file: " .. file .. " applied to target")
    end
    -- NOTE This is the non-blocking version more complex, but should be more performant
    --
    -- vim.fn.jobstart(cmd, {
    --     stdout_buffered = true,
    --     detach = true,
    --     on_stdout = function(_, output)
    --         if output then
    --             vim.notify("Chezmoi: file: " .. file .. " applied to target")
    --             success_cb() -- calling success callback
    --         end
    --     end,
    --     on_stderr = function(_, err)
    --         if err and err[1] ~= "" then
    --             vim.notify(
    --                 "Chezmoi: could not apply file: " .. file .. " to target",
    --                 vim.log.levels.ERROR
    --             )
    --             vim.notify(vim.inspect(err), vim.log.levels.ERROR)
    --         end
    --     end
    -- })
end

--- Checks if there is changes between chezmoi source and target, and (re)adds the file to chezmoi
--- source dir. This function is meant to be executed within a autocommand callback
---
--- @param ev table is the matched buffer/file table passed to the autocmd callback.
function M.chezmoi_add_if_changed(ev)
    M.chezmoi_has_changes(ev.file, false, function()
        M.chezmoi_file_is_managed(ev.file, true, function()
            M.chezmoi_add(ev.file, function()
                -- vim.notify("general success")
            end)
        end)
    end)
end

--- Checks if there is changes between chezmoi source and target, and applies the file to the
--- target. This function is meant to be executed within a autocommand callback
---
--- @param ev table is the matched buffer/file table passed to the autocmd callback.
function M.chezmoi_apply_if_changed(ev)
    if M.chezmoi_has_changes(ev.file, true) then
        M.chezmoi_apply(ev.file, true)
    end

    -- NOTE This is the non-blocking version more complex, but should be more performant
    --
    -- M.chezmoi_has_changes(ev.file, true, function()
    --     M.chezmoi_apply(ev.file, true, function()
    --     end)
    -- end)
end

vim.api.nvim_create_user_command("CmAdd", function(opts)
    local file = opts.args ~= "" and opts.args or vim.fn.expand("%:p")
    M.chezmoi_add(file, function()
    end)
end, {
    desc = "Adds file to chezmoi source state. If it is already added, it updates it",
})

vim.api.nvim_create_user_command("CmSource", function (opts)
    local source_file = opts.args ~= "" and opts.args or vim.fn.expand("%:p")
    local file = M.chezmoi_get_target_path(source_file)
    vim.notify("Target file: ".. file .. " sourced")
    vim.cmd("source "..file)
end, {
desc = "Sources a chezmoi source file, as it was the original in the target state"
})
return M
