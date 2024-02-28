#!/bin/bash

ln -s $PWD/i3/i3 $HOME/.config/i3
ln -s $PWD/i3/i3status $HOME/.config/i3status

ln -s $PWD/vim/vim $HOME/.vim
ln -s $PWD/vim/vimrc $HOME/.vimrc

ln -s $PWD/nvim $HOME/.config/nvim

ln -s $PWD/xmonad/xmonad $HOME/.xmonad
ln -s $PWD/xmonad/xmobar $HOME/.config/xmobar

ln -s $PWD/compton/compton.conf $HOME/.config/picom.conf

ln -s $PWD/tmux.conf $HOME/.tmux.conf

ln -s $PWD/dunst $HOME/.config/dunst

if [ ! -d "$HOME/.local/bin" ];
then
    mkdir $HOME/.local/bin
fi

ln -s $PWD/scripts $HOME/.local/bin/scripts
