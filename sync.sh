#!/bin/sh

get() {
  local source=$1
  local dest="./${source#~/}"
  if [ -d $source ]; then
    (set -x; rsync -d $source -d $dest --delete)
  elif [ -f $source ]; then
    (set -x; cp $source $dest)
  fi
}

get_vim() {
  get ~/.config/nvim
  get ~/.vimrc
}

get_alacritty() {
  get ~/.alacritty.toml
}

get_emacs() {
  get ~/.emacs
}

get_rc() {
  get ~/.psqlrc
  get ~/.inputrc
  get ~/.zshrc
  get ~/.bashrc
}

get_git() {
  get ~/.gitignore
  get ~/.gitconfig
}

get_tmux() {
  get ~/.tmux.conf
}

get_wm() {
  get ~/.config/sxhkd/
  get ~/.config/bspwm/
}

get_bin() {
  get ~/bin/git-pr
}

case $1 in
  vim | nvim | vi)
    get_vim
    ;;
  git)
    get_git
    ;;
  tmux)
    get_tmux
    ;;
  wm | bspwm | sxhkd)
    get_wm
    ;;
  bin)
    get_bin
    ;;
  zsh | zshrc | bash | bashrc | rc)
    get_rc
    ;;
  alacritty)
    get_alacritty
    ;;
  emacs)
    get_emacs
    ;;
  *)
    get_vim
    get_git
    get_tmux
    get_wm
    get_bin
    get_rc
    get_alacritty
    get_emacs
    ;;
esac
