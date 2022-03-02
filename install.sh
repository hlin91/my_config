#!/bin/bash

ln -sf $(pwd)/emacs_config.el ~/.emacs
ln -sf $(pwd)/profile.sh ~/.profile
ln -sf $(pwd)/zsh_config.sh ~/.zshrc

cp -rf ./emacs-color-theme-solarized ~/.emacs.d/emacs-color-theme-solarized
