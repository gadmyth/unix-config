#!/bin/bash

# dvorak keymapping, z/x/c/v/b move one left step to shift/z/x/c/v
declare -A keycode_mappings=(
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
    [37]="Alt_L"
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
    [64]="Control_L"
    [66]="Hyper_L"
    [127]="Caps_Lock"
    [183]="semicolon"
    [184]="colon"
)

function log_info() {
    echo '####' $*
}

function log_attension() {
    echo '!!!!' $*
}

function remap-current-keycode-mappings() {
    current_keycode_mappings=$(xmodmap -pke)
    for keycode in "${!keycode_mappings[@]}"; do
        if echo "$current_keycode_mappings" | grep -Eq "keycode *$keycode = ${keycode_mappings[$keycode]}"; then
            log_info "keycode $keycode is already mapped to ${keycode_mappings[$keycode]}, skipping."
        else
            log_attension "Mapping keycode $keycode to ${keycode_mappings[$keycode]}."
            xmodmap -e "keycode $keycode = ${keycode_mappings[$keycode]}"
        fi
    done
}

declare -A current_modifier_mappings=()

function reset-current-modifier-mappings() {
    clear-current-modifier-mappings
    IFS=$'\n'
    log_info "Reset current modifier mappigs..."
    for line in $(xmodmap -pm | grep 0x | tr -s ' '); do
        if [[ "$line" =~ ([a-z0-9]*)\s*(.*) ]]; then
            modifier=${BASH_REMATCH[1]}
            modifiers=${BASH_REMATCH[2]}
            for str in $(echo "$modifiers" | tr ',' '\n'); do
                if [[ "$str" =~ ([a-zA-Z0-9_ ]*)\s*\((.*)\) ]]; then
                    keysym=$(echo ${BASH_REMATCH[1]} | trim)
                    keycode=$((${BASH_REMATCH[2]}))
                    current_modifier_mappings[$keysym]=$modifier
                    echo -e "\t${keysym}:\t${modifier}"
                fi
            done
        fi
    done
}

function clear-current-modifier-mappings {
    for key in "${!current_modifier_mappings[@]}"; do
        unset current_modifier_mappings[$key]
    done
}

function show-current-modifier-mappings() {
    for k in "${!current_modifier_mappings[@]}"; do
        echo "$k: ${current_modifier_mappings[$k]}"
    done
}

function remove-modifier-mapping() {
    local keysym=$1
    old_mod=${current_modifier_mappings["$keysym"]}
    if [[ ! -z ${old_mod} ]]; then
        log_attension "Removing $keysym from ${old_mod}..."
        xmodmap -e "remove ${old_mod} = $keysym"
    fi
}

function check-and-remap-keysym() {
    local keysym=$1
    local modifier=$2
    if [[ "${current_modifier_mappings[$keysym]}" = "$modifier" ]]; then
        log_info "$keysym is already mapped to $modifier, skipping."
    else
        remove-modifier-mapping $keysym
        
        log_attension "Adding $keysym to ${modifier}..."
        xmodmap -e "add $modifier = $keysym"
    fi
}

function remap-rich-modmap() {
    # https://github.com/alols/xcape
    pkill xcape
    log_attension "xcape Shift_L to semicolon..."
    xcape -t 300 -e 'Shift_L=semicolon'
    log_attension "xcape Shift_R to colon..."
    xcape -t 300 -e 'Shift_R=colon'
}

remap-current-keycode-mappings
reset-current-modifier-mappings

remap-rich-modmap

check-and-remap-keysym 'Caps_Lock' 'lock'
check-and-remap-keysym 'Control_L' 'control'
check-and-remap-keysym 'Alt_L' 'mod1'
check-and-remap-keysym 'Control_R' 'mod2'
check-and-remap-keysym 'Hyper_L' 'mod3'
check-and-remap-keysym 'Super_L' 'mod4'
check-and-remap-keysym 'Super_R' 'mod4'
check-and-remap-keysym 'Alt_R' 'mod5'

remove-modifier-mapping 'Number_Lock'
