#!/bin/bash

display=$(yabai -m query --displays --display | jq -re ".index")

target_display="$((display % 2 + 1))"

yabai -m window --display $target_display
yabai -m display --focus $target_display
