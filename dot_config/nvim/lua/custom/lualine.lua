local M = {}

function M.kanagawa_dragon()
    local colors = {
        black = '#0d0c0c',
        darkGrey = '#181616',
        grey = '#282727',
        ash = '#737c73',
        lightGrey = '#a6a69c',
        blue = '#8ba4b0',
        cyan = '#8ea4a2',
        green = '#87a987',
        magenta = '#a292a3',
        red = '#c4746e',
        orange = '#b6927b',
        white = '#c5c9c5',
        yellow = '#c4b28a',
    }
    local theme = require("lualine.themes.horizon")
    theme.normal.a.bg = colors.yellow
    theme.normal.a.fg = colors.darkGrey
    theme.normal.b.bg = colors.grey
    theme.normal.b.fg = colors.lightGrey
    theme.normal.c.bg = colors.black
    theme.normal.c.fg = colors.lightGrey

    theme.insert.a.bg = colors.green
    theme.insert.a.fg = colors.darkGrey
    theme.insert.b.bg = colors.grey
    theme.insert.b.fg = colors.lightGrey
    theme.insert.c.bg = colors.grey
    theme.insert.c.fg = colors.lightGrey

    theme.visual.a.bg = colors.red
    theme.visual.a.fg = colors.darkGrey
    theme.visual.b.bg = colors.grey
    theme.visual.b.fg = colors.lightGrey
    theme.visual.c.bg = colors.grey
    theme.visual.c.fg = colors.lightGrey

    theme.replace.a.bg = colors.magenta
    theme.replace.a.fg = colors.darkGrey
    theme.replace.b.bg = colors.grey
    theme.replace.b.fg = colors.lightGrey
    theme.replace.c.bg = colors.grey
    theme.replace.c.fg = colors.lightGrey

    theme.command.a.bg = colors.blue
    theme.command.a.fg = colors.darkGrey
    theme.command.b.bg = colors.grey
    theme.command.b.fg = colors.lightGrey
    theme.command.c.bg = colors.grey
    theme.command.c.fg = colors.lightGrey

    theme.inactive.a.bg = colors.grey
    theme.inactive.a.fg = colors.white
    theme.inactive.b.bg = colors.lightGrey
    theme.inactive.b.fg = colors.grey
    theme.inactive.c.bg = colors.grey
    theme.inactive.c.fg = colors.lightGrey

    return theme
end

return M
