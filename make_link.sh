#!/bin/sh

cd ~
ln -sf unix-config/.bash_alias
ln -sf unix-config/.bashrc
ln -sf unix-config/.gitconfig
ln -sf unix-config/.gvimrc
ln -sf unix-config/.profile
ln -sf unix-config/.Xmodmap
ln -sf unix-config/.tmux.conf
rm -f .vimrc
ln -s .gvimrc .vimrc
ln -sf unix-config/.vumrc
ln -sf unix-config/mybash_profile
ln -sf unix-config/sh-extends
# xmonad
ln -sf ~/unix-config/.xmonad

if [ ! -d ~/.vim ]; then
    mkdir ~/.vim
fi

cd ~/.vim
ln -sf ~/unix-config/vim/syntax

