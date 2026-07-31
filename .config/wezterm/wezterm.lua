-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This table will hold the configuration.
local config = {}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
    config = wezterm.config_builder()
end

local function scheme_for_appearance(appearance)
    if appearance:find "Dark" then
        local scheme = wezterm.get_builtin_color_schemes()["Catppuccin Mocha"]
        scheme.background = "#1f1f28" -- kanagawa background
        config.color_schemes = {
            ['Catppuccin Custom Mocha'] = scheme
        }

        return "Catppuccin Custom Mocha"
    else
        return "Catppuccin Latte"
    end
end

-- This is where you actually apply your config choices

-- [[appearance
-- config.color_scheme = scheme_for_appearance(wezterm.gui.get_appearance())
-- config.color_scheme = 'Gruvbox Dark (Gogh)'
-- Kanagawa theme
config.colors = require("theme").colors
-- config.color_scheme = "Oxocarbon Dark"
-- config.color_scheme = "Flexoki Dark"

config.force_reverse_video_cursor = true

config.enable_tab_bar = false
config.window_decorations = "RESIZE"
config.window_padding = {
    top = 4,
    bottom = 4,
    left = 4,
    right = 4
}
config.window_background_opacity = 1

-- Fonts
-- config.font = wezterm.font("Iosevka Term")
--config.font = wezterm.font("Iosevka", {
--    weight = "Heavy",
--    -- stretch = "SemiExpanded"
--})
config.font = wezterm.font 'Iosevka Medium'
--config.font = wezterm.font( "Iosevka", {
--	weight = "Black",
--	stretch = "Normal",
--	style = "Normal"
--})
config.font_rules = {
  {
	  intensity = 'Bold',
	  italic = true,
	  font = wezterm.font {
		  family = "Iosevka",
		  weight = "Black",
		  style = 'Oblique'
	  }
  },
  {
	  intensity = 'Bold',
	  italic = false,
	  font = wezterm.font("Iosevka Heavy")
  }
}
config.font_size = 16
-- ]]

-- https://wezfurlong.org/wezterm/config/lua/keyassignment/OpenLinkAtMouseCursor.html
config.mouse_bindings = {
    -- Ctrl-click will open the link under the mouse cursor
    {
        event = { Up = { streak = 1, button = 'Left' } },
        mods = 'CTRL',
        action = wezterm.action.OpenLinkAtMouseCursor,
    },
}

-- Using fish shell
-- Spawn a fish shell in login mode
config.default_prog = { '/Users/vrod/.nix-profile/bin/fish', '-l' }

-- and finally, return the configuration to wezterm
return config
