-- customizations to feline status line based on catppuccin's integration
-- require("feline")
local M = {}

-- Config when using catppuccin theme
function M.catppuccin()
    vim.opt.termguicolors = true
    local show_modified = true
    local filename_icon = "󰈙"
    local ctp_feline = require("catppuccin.groups.integrations.feline")
    local components = ctp_feline.get()
    -- local colors = require("catppuccin.palettes").get_palette()
    local colors = require("catppuccin.palettes").get_palette()

    components.active[2][1].enabled = false
    components.active[3][3].provider = function()
        local filename = vim.fn.pathshorten(vim.fn.expand("%:p"))
        local extension = vim.fn.expand "%:e"
        local present, icons = pcall(require, "nvim-web-devicons")
        local icon = present and icons.get_icon(filename, extension) or filename_icon
        return (show_modified and "%m" or "") .. " " .. icon .. " " .. filename .. " "
    end
    components.active[3][3].hl = {
        bg = colors.mantle,
        fg = colors.blue
    }
    components.active[3][3].left_sep.hl = {
        fg = colors.mantle,
        bg = colors.base
    }
    components.active[3][4].hl = {
        bg = colors.blue,
        fg = colors.mantle
    }
    components.active[3][4].left_sep.hl = {
        bg = colors.mantle,
        fg = colors.blue
    }

    require("feline").setup({
        components = components,
        sett = {
            curr_file = colors.mantle
        },
    })
end

function M.kanagawa(variant)
    return function()
        vim.opt.termguicolors = true
        local show_modified = true
        local filename_icon = "󰈙"
        local vim_icon = ""
        local ctp_feline = require("catppuccin.groups.integrations.feline")
        -- local colors = require("catppuccin.palettes").get_palette()
        local kanagawa = require("kanagawa.colors").setup({ theme = variant or "wave" })
        local colors = kanagawa.palette
        local mode_colors = {
            ["n"] = { "NORMAL", colors.dragonBlue },
            ["no"] = { "N-PENDING", colors.oniViolet },
            ["i"] = { "INSERT", colors.autumnGreen },
            ["ic"] = { "INSERT", colors.autumnGreen },
            ["t"] = { "TERMINAL", colors.autumnGreen },
            ["v"] = { "VISUAL", colors.autumnYellow },
            ["V"] = { "V-LINE", colors.autumnYellow },
            [""] = { "V-BLOCK", colors.autumnYellow },
            ["R"] = { "REPLACE", colors.autumnRed },
            ["Rv"] = { "V-REPLACE", colors.autumnRed },
            ["s"] = { "SELECT", colors.autumnRed },
            ["S"] = { "S-LINE", colors.autumnRed },
            [""] = { "S-BLOCK", colors.autumnRed },
            ["c"] = { "COMMAND", colors.surimiOrange },
            ["cv"] = { "COMMAND", colors.surimiOrange },
            ["ce"] = { "COMMAND", colors.surimiOrange },
            ["r"] = { "PROMPT", colors.waveAqua2 },
            ["rm"] = { "MORE", colors.waveAqua2 },
            ["r?"] = { "CONFIRM", colors.springViolet1 },
            ["!"] = { "SHELL", colors.autumnGreen },
        }
        ctp_feline.setup({
            sett = {
                curr_file = colors.sumiInk0,
                bkg = colors.sumiInk3,
                text = colors.dragonBlue,
                curr_dir = colors.dragonBlue,
                extras = colors.oniViolet
            },
            mode_colors = mode_colors
        })
        local components = ctp_feline.get()

        components.active[1][1] = {
            provider = " " .. vim_icon .. "  ",
            hl = function()
                return {
                    fg = colors.sumiInk0,
                    bg = mode_colors[vim.fn.mode()][2],
                    style = "bold"
                }
            end
        }

        components.active[1][2].hl = function()
            return {
                fg = colors.sumiInk0,
                bg = mode_colors[vim.fn.mode()][2],
                style = "bold"
            }
        end
        components.active[2][1].hl.fg = colors.springGreen -- Lsp progress
        components.active[2][1].enabled = false
        components.active[2][2].hl.fg = colors.autumnRed   -- Errors
        components.active[2][2].icon = components.active[2][2].icon .. " "
        components.active[2][3].hl.fg = colors.roninYellow -- Warnings
        components.active[2][3].icon = components.active[2][3].icon .. " "
        components.active[2][4].hl.fg = colors.springBlue  -- Infos
        components.active[2][4].icon = components.active[2][4].icon .. " "
        components.active[2][5].hl.fg = colors.crystalBlue -- Hints
        components.active[2][5].icon = components.active[2][5].icon .. " "

        components.active[3][3].provider = function()
            local filename = vim.fn.pathshorten(vim.fn.expand("%:p"))
            local extension = vim.fn.expand "%:e"
            local present, icons = pcall(require, "nvim-web-devicons")
            local icon = present and icons.get_icon(filename, extension) or filename_icon
            return (show_modified and "%m" or "") .. " " .. icon .. " " .. filename .. " "
        end
        -- components.active[3][3].hl = {
        --     bg = colors.sumiInk0,
        --     fg = colors.dragonBlue
        -- }
        -- components.active[3][3].left_sep.hl = {
        --     fg = colors.sumiInk0,
        --     bg = colors.sumiInk3
        -- }
        components.active[3][4].hl = {
            bg = colors.dragonBlue,
            fg = colors.sumiInk0
        }
        -- components.active[3][4].left_sep.hl = {
        --     bg = colors.sumiInk0,
        --     fg = colors.dragonBlue
        -- }

        require("feline").setup({
            components = components,
        })
    end
end

function M.oxocarbon()
    local base00 = "#161616"
    local base06 = "#ffffff"
    local base09 = "#78a9ff"
    local oxocarbon = (((vim.o.background == "dark") and { base00 = base00, base01 = blend_hex(base00, base06, 0.085), base02 =
    blend_hex(base00, base06, 0.18), base03 = blend_hex(base00, base06, 0.3), base04 = blend_hex(base00, base06, 0.82), base05 =
    blend_hex(base00, base06, 0.95), base06 = base06, base07 = "#08bdba", base08 = "#3ddbd9", base09 = base09, base10 =
    "#ee5396", base11 = "#33b1ff", base12 = "#ff7eb6", base13 = "#42be65", base14 = "#be95ff", base15 = "#82cfff", blend =
    "#131313", none = "NONE" }) or { base00 = base06, base01 = blend_hex(base00, base06, 0.95), base02 = blend_hex(
    base00, base06, 0.82), base03 = base00, base04 = "#37474F", base05 = "#90A4AE", base06 = "#525252", base07 =
    "#08bdba", base08 = "#ff7eb6", base09 = "#ee5396", base10 = "#FF6F00", base11 = "#0f62fe", base12 = "#673AB7", base13 =
    "#42be65", base14 = "#be95ff", base15 = "#FFAB91", blend = "#FAFAFA", none = "NONE" })
end

return M
