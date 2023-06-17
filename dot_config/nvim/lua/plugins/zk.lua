local zk = require("zk")
local api = require("zk.api")
local util = require("zk.util")
local commands = require("zk.commands")

local function insert_link(selected, opts)
    opts = vim.tbl_extend("force", {}, opts or {})

    local location = util.get_lsp_location_from_selection()
    local selected_text = util.get_text_in_range(util.get_selected_range())

    if not selected then
        location = util.get_lsp_location_from_caret()
    else
        if opts["matchSelected"] then
            opts = vim.tbl_extend("force", { match = { selected_text } }, opts or {})
        end
    end

    local picker_opts = require("telescope.themes").get_ivy()

    zk.pick_notes(opts, { multi_select = false, telescope = picker_opts }, function(note)
        assert(note ~= nil, "Picker failed before link insertion: note is nil")

        local link_opts = {}

        if selected and selected_text ~= nil then
            link_opts.title = selected_text
        end

        api.link(note.path, location, nil, link_opts, function(err, res)
            if not res then
                error(err)
            end
        end)
    end)
end

commands.add("ZkInsertLinkIvy", function(opts)
    insert_link(false, opts)
end, { title = "Insert Zk link" })
