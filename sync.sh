#!/bin/sh

usage() {
  cat << EOF
sync.sh - a script for syncing dotfiles, configs, useful scripts.
          Syncs the source files only if they exist.

Usage:
  ./sync [OPTION] TARGET

Options:
  -r | --restore     Put files from current folder to the system.
  -h | --help        Print usage.
  -f | --file        Sync the specific file, e.g.: -f ~/.zshrc

Targets:
  rc                 .zshrc, .bashrc, .inputrc, and other .*rc files.
  vim                Vim and Neovim configuration.
  emacs              Emacs configuration.
  alacritty          Alacritty terminal configuration.
  bin                Scripts in ~/bin.
  tmux               Tmux configuration.
  wm                 Spectrwm and other window managers configuration.
  all                All targets.
EOF
}

# Sync file from or to this folder.
sync_file() {
  local source=$1
  local dest="./${source#~/}"

  if [ "$ACTION" == "put" ]; then
    local tmp=$source
    source=$dest
    dest=$tmp
  fi

  if [ -d $source ]; then
    (set -x; rsync -d $source -d $dest --delete)
  elif [ -f $source ]; then
    [ ! -f $dest ] && mkdir -p $(dirname $dest)
    (set -x; cp $source $dest)
  fi
}

sync_vim() {
  sync_file ~/.config/nvim
  sync_file ~/.vimrc
}

sync_alacritty() {
  sync_file ~/.alacritty.toml
}

sync_emacs() {
  sync_file ~/.emacs
}

sync_rc() {
  sync_file ~/.psqlrc
  sync_file ~/.inputrc
  sync_file ~/.zshrc
  sync_file ~/.bashrc
  sync_file ~/.config/htop/htoprc
}

sync_git() {
  sync_file ~/.gitignore
  sync_file ~/.gitconfig
}

sync_tmux() {
  sync_file ~/.tmux.conf
}

sync_wm() {
  sync_file ~/.config/sxhkd/
  sync_file ~/.config/bspwm/
}

sync_bin() {
  sync_file ~/bin/git-pr
  sync_file ~/bin/commit-msg
}

if [ "$#" -eq 0 ]; then
  usage
  exit 0
fi

while [ "$#" -gt 0 ]; do
  case $1 in
    -r | --restore)
      ACTION=put
      shift
      ;;
    -h | --help)
      usage
      exit 0
      ;;
    -f | --file)
      sync_file $2
      shift 2
      ;;
    *)
      ACTION=get
      break
      ;;
  esac
done

case $1 in
  vim | nvim | vi)
    sync_vim
    ;;
  git)
    sync_git
    ;;
  tmux)
    sync_tmux
    ;;
  wm | bspwm | sxhkd)
    sync_wm
    ;;
  bin)
    sync_bin
    ;;
  zsh | zshrc | bash | bashrc | rc)
    sync_rc
    ;;
  alacritty | terminal)
    sync_alacritty
    ;;
  emacs)
    sync_emacs
    ;;
  all)
    sync_vim
    sync_git
    sync_tmux
    sync_wm
    sync_bin
    sync_rc
    sync_alacritty
    sync_emacs
    ;;
  *)
    ;;
esac
