#!/usr/bin/env bash

lxsession &
picom --experimental-backends --config ./picom.conf &
#nitrogen --restore &
xfce4-power-manager &
feh --randomize --bg-fill ~/.wallpapers/* &
volumeicon &
nm-applet &
variety &
