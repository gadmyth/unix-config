#!/bin/bash

# dvorak keymapping, z/x/c/v/b move one left step to shift/z/x/c/v
declare -A key_mappings=(
    [24]="apostrophe quotedbl"
    [25]="comma less"
    [26]="period greater"
    [27]="p"
    [28]="y"
    [29]="f"
    [30]="g"
    [31]="c"
    [32]="r"
    [33]="l"
    [34]="slash question"
    [35]="equal plus"
    [51]="backslash bar"
    [38]="a"
    [39]="o"
    [40]="e"
    [41]="u"
    [42]="i"
    [43]="d"
    [44]="h"
    [45]="t"
    [46]="n"
    [47]="s"
    [48]="minus underscore"
    [50]="Shift_L NoSymbol Shift_L"
    [52]="q"
    [53]="j"
    [54]="k"
    [55]="x"
    [56]="x"
    [57]="b"
    [58]="m"
    [59]="w"
    [60]="v"
    [61]="z"
    [62]="Shift_R NoSymbol Shift_R"
)

current_mappings=$(xmodmap -pke)
for keycode in "${!key_mappings[@]}"; do
    if echo "$current_mappings" | grep -Eq "keycode *$keycode = ${key_mappings[$keycode]}"; then
        echo "keycode $keycode is already mapped to ${key_mappings[$keycode]}, skipping."
    else
        echo "Mapping keycode $keycode to ${key_mappings[$keycode]}."
        xmodmap -e "keycode $keycode = ${key_mappings[$keycode]}"
    fi
done

# https://github.com/alols/xcape
xmodmap -e 'keycode 183 = semicolon' \
        -e 'keycode 184 = colon'

pkill xcape
echo "xcape Shift_L to semicolon..."
xcape -t 300 -e 'Shift_L=semicolon'
echo "xcape Shift_R to colon..."
xcape -t 300 -e 'Shift_R=colon'

current_modifier_mappings=$(xmodmap -pm)

if [[ $(echo "$current_mappings" | grep -E "keycode *127 = " | grep Caps_Lock | wc -l) > 0 ]]; then
    echo "Caps_Lock is already mapped to keycode 127 (original Break), skipping."
else
    echo "Mapping Caps_Lock to keycode 127 (original Break)..."
    xmodmap -e "keycode 127 = Caps_Lock"
    xmodmap -e "add Lock = Caps_Lock"
fi

if [[ $(echo "$current_mappings" | grep -E "keycode *66 = " | grep Alt_L | wc -l) > 0 ]]; then
    echo "Alt_L is already mapped to keycode 66 (original CapsLock), skipping."
else
    echo "Mapping Alt_L to keycode 66 (original CapsLock)..."
    xmodmap -e "keycode 66 = Alt_L"
fi

if [[ $(echo "$current_modifier_mappings" | grep mod1 | grep "Alt_L (0x42)" | wc -l) > 0 ]]; then
    echo "Alt_L (66) is already mapped to mod1, skipping."
else
    old_mod=$(echo "$current_modifier_mappings" | grep "Alt_L (0x42)" | cut -d ' ' -f 1)
    if [[ ! -z ${old_mod} ]]; then
        xmodmap -e "remove ${old_mod} = Alt_L"
    fi

    echo "Adding ALt_L to mod1..."
    xmodmap -e "add mod1 = Alt_L"
fi

if [[ $(echo "$current_mappings" | grep -E "keycode *64 = " | grep Control_L | wc -l) > 0 ]]; then
    echo "Control_L is already mapped to keycode 64 (original Alt_L), skipping."
else
    echo "Mapping Control_L to keycode 64 (original Alt_L)..."
    xmodmap -e "keycode 64 = Control_L"
fi

if [[ $(echo "$current_mappings" | grep -E "keycode *37 = " | grep Hyper_L | wc -l) > 0 ]]; then
    echo "Hyper_L is already mapped to keycode 37 (original Left Control), skipping."
else
    echo "Mapping Alt_L to keycode 66 (original Left Control)..."
    xmodmap -e "keycode 37 = Hyper_L"
fi

if [[ $(echo "$current_modifier_mappings" | grep control | grep Control_L | wc -l) > 0 ]]; then
    echo "Control_L is already mapped to control, skipping."
else
    old_mod=$(echo "$current_modifier_mappings" | grep Control_L | cut -d ' ' -f 1)
    if [[ ! -z ${old_mod} ]]; then
        xmodmap -e "remove ${old_mod} = Control_L"
    fi

    echo "Adding Control_L to control..."
    xmodmap -e "add control = Control_L"
fi

if [[ $(echo "$current_modifier_mappings" | grep mod2 | grep Control_R | wc -l) > 0 ]]; then
    echo "Control_R is already mapped to Mod2, skipping."
else
    echo "Removing Control_R from Control..."
    xmodmap -e "remove Control = Control_R"
    echo "Adding Control_R to Mod5..."
    xmodmap -e "add Mod2 = Control_R"
fi

old_mod=$(echo "$current_modifier_mappings" | grep Num_Lock | cut -d ' ' -f 1)
if [[ ! -z ${old_mod} ]]; then
    xmodmap -e "remove ${old_mod} = Num_Lock"
fi
xmodmap -e "add Mod3 = Hyper_L"

if [[ $(echo "$current_modifier_mappings" | grep mod3 | grep Hyper_L | wc -l) > 0 ]]; then
    echo "Hyper_L is already mapped to Mod3, skipping."
else
    echo "Adding Hyper_L to Mod3..."
    old_mod=$(echo "$current_modifier_mappings" | grep Hyper_L | cut -d ' ' -f 1)
    if [[ ! -z ${old_mod} ]]; then
        xmodmap -e "remove ${old_mod} = Hyper_L"
    fi
    xmodmap -e "add Mod3 = Hyper_L"
fi

if [[ $(echo "$current_modifier_mappings" | grep mod4 | grep Super_L | grep Super_R | wc -l) > 0 ]]; then
    echo "SuperL and Super_R are already mapped to Mod4, skipping."
else
    echo "Adding Super_L and Super_R to Mod4..."
    xmodmap -e "add Mod4 = Super_L Super_R"
fi

if [[ $(echo "$current_modifier_mappings" | grep mod5 | grep Alt_R | wc -l) > 0 ]]; then
    echo "Alt_R is already mapped to Mod5, skipping."
else
    echo "Removing Alt_R from Mod2..."
    old_mod=$(echo "$current_modifier_mappings" | grep Alt_R | cut -d ' ' -f 1)
    if [[ ! -z ${old_mod} ]]; then
        xmodmap -e "remove ${old_mod} = Alt_R"
    fi
    echo "Adding Alt_R to Mod5..."
    xmodmap -e "add Mod5 = Alt_R"
fi
