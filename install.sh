#!/bin/bash

ln -sf ./emacs_config.el ~/.emacs
ln -sf ./profile.sh ~/.profile
ln -sf ./zsh_config.sh ~/.zshrc

cp -rf ./emacs-color-theme-solarized ~/.emacs.d/emacs-color-theme-solarized
