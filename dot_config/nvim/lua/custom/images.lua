local utils = require("custom.utils")

local M = {}

local function screencapture_cmd(output_file)
    local executable = "screencapture"
    return executable .. " -i " .. output_file
end

local function pngpaste_cmd(output_file)
    local executable = "pngpaste"
    return executable .. " " .. output_file
end

local function fd_cmd()
    local executable = "fd"
    return { executable, "--full-path", vim.loop.cwd(), "--type", "d" }
    -- return executable .. " --type d"
end

function M.find_img_dir()
    local img_dirs = { "assets", "images", "img" }
    local result = vim.fn.systemlist(fd_cmd())
    for _, img_dir in ipairs(img_dirs) do
        for _, dir in ipairs(result) do
            if dir:match(img_dir) then
                -- Remove the trailing `/`
                return string.sub(dir, 1, -2)
            end
        end
    end
end

function M.screencapture_markdown(title, output_dir)
    title = utils.slug(title)
    output_dir = output_dir or M.find_img_dir()
    local filename = title .. "_" .. os.date("%Y%m%d%H%M%S") .. ".png"
    local filepath = output_dir .. "/" .. filename
    vim.fn.jobstart(screencapture_cmd(filepath), {
        stdout_buffered = true,
        on_stdout = function(_, _)
            local rel_path = utils.relative_to_current(filepath)
            local markdown_img = "![" .. title .. "](" .. rel_path .. ")"
            vim.cmd("normal A" .. markdown_img)
        end
    })
end

function M.pngpaste_markdown(title, output_dir)
    title = utils.slug(title)
    output_dir = output_dir or M.find_img_dir()
    local filename = title .. "_" .. os.date("%Y%m%d%H%M%S") .. ".png"
    local filepath = output_dir .. "/" .. filename
    vim.fn.jobstart(pngpaste_cmd(filepath), {
        stdout_buffered = true,
        on_stdout = function(_, _)
            local rel_path = utils.relative_to_current(filepath)
            local markdown_img = "![" .. title .. "](" .. rel_path .. ")"
            vim.cmd("normal A" .. markdown_img)
        end
    })
end

vim.api.nvim_create_user_command("PasteImg", function(opts)
    local out_dir
    if not opts.args or opts.args ~= "" then out_dir = nil end
    local title = vim.fn.input("Image title: ")
    M.pngpaste_markdown(title, out_dir)
end, { desc = "Paste img in clipboard into the specified dir or one of a default dir list" })

vim.api.nvim_create_user_command("Screenshot", function(opts)
    local title = vim.fn.input("Image title: ")
    if title ~= "" then
        if not opts.args or opts.args ~= "" then
            M.screencapture_markdown(title, opts.args)
        else
            M.screencapture_markdown(title)
        end
    end
end, { desc = "Paste img in clipboard into the specified dir or one of a default dir list" })

return M
