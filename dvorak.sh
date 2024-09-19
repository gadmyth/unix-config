#!/bin/bash

xmodmap -e 'keycode 24 = apostrophe quotedbl' \
        -e 'keycode 25 = comma less' \
        -e 'keycode 26 = period greater' \
        -e 'keycode 27 = p' \
        -e 'keycode 28 = y' \
        -e 'keycode 29 = f' \
        -e 'keycode 30 = g' \
        -e 'keycode 31 = c' \
        -e 'keycode 32 = r' \
        -e 'keycode 33 = l' \
        -e 'keycode 34 = slash question' \
        -e 'keycode 35 = equal plus' \
        -e 'keycode 51 = backslash bar'

xmodmap -e 'keycode 38 = a' \
        -e 'keycode 39 = o' \
        -e 'keycode 40 = e' \
        -e 'keycode 41 = u' \
        -e 'keycode 42 = i' \
        -e 'keycode 43 = d' \
        -e 'keycode 44 = h' \
        -e 'keycode 45 = t' \
        -e 'keycode 46 = n' \
        -e 'keycode 47 = s' \
        -e 'keycode 48 = minus underscore'

xmodmap -e 'keycode 50 = Shift_L NoSymbol Shift_L NoSymbol' \
        -e 'keycode 52 = q' \
        -e 'keycode 53 = j' \
        -e 'keycode 54 = k' \
        -e 'keycode 55 = x' \
        -e 'keycode 56 = x' \
        -e 'keycode 57 = b' \
        -e 'keycode 58 = m' \
        -e 'keycode 59 = w' \
        -e 'keycode 60 = v' \
        -e 'keycode 61 = z' \
        -e 'keycode 62 = Shift_R NoSymbol Shift_R NoSymbol'

# https://github.com/alols/xcape
xmodmap -e 'keycode 183 = semicolon' \
        -e 'keycode 184 = colon'

pkill xcape
echo "xcape Shift_L to semicolon..."
xcape -t 300 -e 'Shift_L=semicolon'
echo "xcape Shift_R to colon..."
xcape -t 300 -e 'Shift_R=colon'

if [[ $(xmodmap -pke | grep -E "keycode 127 = " | grep Caps_Lock | wc -l) > 0 ]]; then
    echo "Caps_Lock is mapping to keycode 127 (original Break)"
else
    echo "Mapping Caps_Lock to keycode 127 (original Break)..."
    xmodmap -e "keycode 127 = Caps_Lock"
    xmodmap -e "add Lock = Caps_Lock"
fi

if [[ $(xmodmap -pke | grep -E "keycode  66 = " | grep Alt_L | wc -l) > 0 ]]; then
    echo "Alt_L is mapping to keycode 66 (original CapsLock)"
else
    echo "Mapping Alt_L to keycode 66 (original CapsLock)..."
    xmodmap -e "keycode 66 = Alt_L"
fi

if [[ $(xmodmap -pm | grep mod1 | grep "Alt_L (0x42)" | wc -l) > 0 ]]; then
    echo "Alt_L (66) is mapping to mod1"
else
    old_mod=$(xmodmap -pm | grep "Alt_L (0x42)" | cut -d ' ' -f 1)
    if [[ ! -z ${old_mod} ]]; then
        xmodmap -e "remove ${old_mod} = Alt_L"
    fi

    echo "Adding ALt_L to mod1..."
    xmodmap -e "add mod1 = Alt_L"
fi

if [[ $(xmodmap -pke | grep -E "keycode  64 = " | grep Control_L | wc -l) > 0 ]]; then
    echo "Control_L is mapping to keycode 64 (original Alt_L)"
else
    echo "Mapping Control_L to keycode 64 (original Alt_L)..."
    xmodmap -e "keycode 64 = Control_L"
fi

if [[ $(xmodmap -pke | grep -E "keycode  37 = " | grep Hyper_L | wc -l) > 0 ]]; then
    echo "Hyper_L is mapping to keycode 37 (original Left Control)"
else
    echo "Mapping Alt_L to keycode 66 (original Left Control)..."
    xmodmap -e "keycode 37 = Hyper_L"
fi

if [[ $(xmodmap -pm | grep control | grep Control_L | wc -l) > 0 ]]; then
    echo "Control_L is mapping to control"
else
    old_mod=$(xmodmap -pm | grep Control_L | cut -d ' ' -f 1)
    if [[ ! -z ${old_mod} ]]; then
        xmodmap -e "remove ${old_mod} = Control_L"
    fi

    echo "Adding Control_L to control..."
    xmodmap -e "add control = Control_L"
fi

if [[ $(xmodmap -pm | grep mod2 | grep Control_R | wc -l) > 0 ]]; then
    echo "Control_R is mapping to Mod2"
else
    echo "Removing Control_R from Control..."
    xmodmap -e "remove Control = Control_R"
    echo "Adding Control_R to Mod5..."
    xmodmap -e "add Mod2 = Control_R"
fi

old_mod=$(xmodmap -pm | grep Num_Lock | cut -d ' ' -f 1)
if [[ ! -z ${old_mod} ]]; then
    xmodmap -e "remove ${old_mod} = Num_Lock"
fi
xmodmap -e "add Mod3 = Hyper_L"

if [[ $(xmodmap -pm | grep mod3 | grep Hyper_L | wc -l) > 0 ]]; then
    echo "Hyper_L is mapping to Mod3"
else
    echo "Adding Hyper_L to Mod3..."
    old_mod=$(xmodmap -pm | grep Hyper_L | cut -d ' ' -f 1)
    if [[ ! -z ${old_mod} ]]; then
        xmodmap -e "remove ${old_mod} = Hyper_L"
    fi
    xmodmap -e "add Mod3 = Hyper_L"
fi

if [[ $(xmodmap -pm | grep mod4 | grep Super_L | grep Super_R | wc -l) > 0 ]]; then
    echo "SuperL and Super_R are mapping to Mod4"
else
    echo "Adding Super_L and Super_R to Mod4..."
    xmodmap -e "add Mod4 = Super_L Super_R"
fi

if [[ $(xmodmap -pm | grep mod5 | grep Alt_R | wc -l) > 0 ]]; then
    echo "Alt_R is mapping to Mod5"
else
    echo "Removing Alt_R from Mod2..."
    old_mod=$(xmodmap -pm | grep Alt_R | cut -d ' ' -f 1)
    if [[ ! -z ${old_mod} ]]; then
        xmodmap -e "remove ${old_mod} = Alt_R"
    fi
    echo "Adding Alt_R to Mod5..."
    xmodmap -e "add Mod5 = Alt_R"
fi
