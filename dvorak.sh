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
    [37]="Hyper_L"
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
    [50]="Shift_L"
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
    [62]="Alt_R"
    [64]="Control_L"
    [66]="Alt_L"
    [108]="Shift_R"
    [127]="Caps_Lock"
    [183]="semicolon"
    [184]="colon"
)

declare -A keysym_modifier_mappings=(
    ['Caps_Lock']='lock'
    ['Shift_L']='shift'
    ['Shift_R']='shift'
    ['Control_L']='control'
    ['Alt_L']='mod1'
    ['Control_R']='mod2'
    ['Hyper_L']='mod3'
    ['Super_L']='mod4'
    ['Super_R']='mod4'
    ['Alt_R']='mod5'
)

declare -A keysym_keycode_mappings=(
    ['Alt_L']='66'
    ['Alt_R']='62'
    ['Control_L']='64'
    ['Control_R']='105'
    ['Shift_L']='50'
    ['Shift_R']='108'
    ['Super_L']='133'
    ['Super_R']='134'
    ['Hyper_L']='37'
    ['Caps_Lock']='127'
)

function log_info() {
    echo '####' $*
}

function log_attension() {
    echo '!!!!' $*
}

function explain-to-hex() {
    printf "%d(0x%x)" $1 $1
}

function explain-to-dec() {
    printf "0x%x(%d)" $1 $1
}

function remap-current-keycode-mappings() {
    current_keycode_mappings=$(xmodmap -pke)
    for keycode in "${!keycode_mappings[@]}"; do
        if echo "$current_keycode_mappings" | grep -Eq "keycode *$keycode = ${keycode_mappings[$keycode]}"; then
            log_info "keycode $(explain-to-hex $keycode) is already mapped to ${keycode_mappings[$keycode]}, skipping."
        else
            log_attension "Mapping keycode $(explain-to-hex $keycode) to ${keycode_mappings[$keycode]}."
            xmodmap -e "keycode $keycode = ${keycode_mappings[$keycode]}"
        fi
    done
}

declare -A current_keysym_modifier_mappings=()

declare -A current_keycode_keysym_mappings=()

function reset-current-keysym-modifier-mappings() {
    clear-current-keysym-modifier-mappings
    clear-current-keycode-keysym-mappings
    IFS=$'\n'
    log_info "Reset current modifier mappings..."
    for line in $(xmodmap -pm | grep 0x | tr -s ' '); do
        if [[ "$line" =~ ([a-z0-9]*)\s*(.*) ]]; then
            local modifier=${BASH_REMATCH[1]}
            local keysyms=${BASH_REMATCH[2]}
            for str in $(echo "$keysyms" | tr ',' '\n'); do
                if [[ "$str" =~ ([a-zA-Z0-9_ ]*)\s*\((.*)\) ]]; then
                    local keysym=$(echo ${BASH_REMATCH[1]} | trim)
                    local keycode=$((${BASH_REMATCH[2]}))
                    current_keycode_keysym_mappings[$keycode]=$keysym
                    current_keysym_modifier_mappings[$keysym]=$modifier
                    local target_modifier=${keysym_modifier_mappings[$keysym]}
                    if [[ "$modifier" != "$target_modifier" ]]; then
                        log_attension "$keysym is mapping to the wrong $modifier"
                        xmodmap -e "remove $modifier = $keysym"
                        remove-modifier-mapping $keysym
                    fi
                fi
            done
        fi
    done

    for target_keysym in ${!keysym_keycode_mappings[@]}; do
        local target_keycode=${keysym_keycode_mappings[$target_keysym]}
        local keysym=${current_keycode_keysym_mappings[$target_keycode]}
        if [[ -z "$keysym" ]]; then
            log_attension "$target_keycode is not mapping to any keysym"
            check-and-remap-keysym $target_keysym $modifier
        elif [[ "$keysym" == "$target_keysym" ]]; then
            local modifier=${current_keysym_modifier_mappings[$keysym]}
            log_info "$keysym is already mapping to $modifier"
            echo -e "\t$(explain-to-hex ${target_keycode}) --> ${keysym} --> ${modifier}"
        else
            log_attension "$target_keycode is mapping to the wrong $keysym"
            xmodmap -e "remove $modifier = $keysym"
            remove-modifier-mapping $keysym
        fi
    done
}

function clear-current-keysym-modifier-mappings {
    for key in "${!current_keysym_modifier_mappings[@]}"; do
        unset current_keysym_modifier_mappings[$key]
    done
}

function show-current-keysym-modifier-mappings() {
    for keysym in "${!current_keysym_modifier_mappings[@]}"; do
        local modifier=${current_keysym_modifier_mappings[$keysym]}
        echo "$keysym --> $modifier"
    done
}

function clear-current-keycode-keysym-mappings {
    for key in "${!current_keycode_keysym_mappings[@]}"; do
        unset current_keycode_keysym_mappings[$key]
    done
}

function show-current-keycode-keysym-mappings() {
    for keycode in "${!current_keycode_keysym_mappings[@]}"; do
        local keysym=${current_keycode_keysym_mappings[$keycode]}
        local modifier=${current_keysym_modifier_mappings[$keysym]}
        echo "$(explain-to-hex $keycode) --> $keysym --> $modifier"
    done
}

function remove-modifier-mapping() {
    local keysym=$1
    old_mod=${current_keysym_modifier_mappings["$keysym"]}
    if [[ ! -z ${old_mod} ]]; then
        log_attension "Removing $keysym from ${old_mod}..."
        xmodmap -e "remove ${old_mod} = $keysym"
        unset current_keysym_modifier_mappings[$keysym]
    fi
}

function check-and-remap-keysym() {
    local keysym=$1
    local modifier=$2
    if [[ "${current_keysym_modifier_mappings[$keysym]}" = "$modifier" ]]; then
        log_info "$keysym is already mapped to $modifier, skipping."
    else
        remove-modifier-mapping $keysym
        
        log_attension "Adding $keysym to ${modifier}..."
        xmodmap -e "add $modifier = $keysym"
        current_keysym_modifier_mappings[$keysym]=$modifier
    fi
}

function remap-keysyms() {
    for keysym in ${!keysym_modifier_mappings[@]}; do
        local modifier=${keysym_modifier_mappings[$keysym]}
        check-and-remap-keysym $keysym $modifier
    done
}


function remap-rich-modmap() {
    # https://github.com/alols/xcape
    pkill xcape
    log_attension "xcape Shift_L to semicolon..."
    xcape -t 300 -e 'Shift_L=semicolon'
    log_attension "xcape Alt_R to colon..."
    xcape -t 300 -e 'Alt_R=colon'
}

remap-current-keycode-mappings
reset-current-keysym-modifier-mappings
remap-keysyms

remap-rich-modmap

remove-modifier-mapping 'Number_Lock'
