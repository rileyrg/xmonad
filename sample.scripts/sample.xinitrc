#!/bin/sh
# email:rileyrg@gmail.com
# redirect all shell output
rm ${HOME}/.xsession-errors
exec > ${HOME}//.xsession-errors 2>&1

xrdb -merge ${HOME}/.Xresources
xsetroot -cursor_name left_ptr
setxkbmap -layout de
setxkbmap -layout de -variant nodeadkeys -option terminate:ctrl_alt_bksp -option ctrl:nocaps
xhost +
xmonad

