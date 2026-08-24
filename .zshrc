# .zshrc

autoload -Uz compinit && compinit
[ -z "$HISTFILE" ] && HISTFILE="$HOME/.zsh_history"
[ "$HISTSIZE" -lt 50000 ] && HISTSIZE=50000
[ "$SAVEHIST" -lt 10000 ] && SAVEHIST=10000

bindkey -e
setopt braceccl               # expand num ranges {1..4}
setopt extended_history       # record timestamp of command in HISTFILE
setopt inc_append_history
setopt hist_expire_dups_first # delete duplicates first when HISTFILE size exceeds HISTSIZE
setopt hist_ignore_all_dups   # ignore duplicated commands history list
setopt hist_ignore_space      # ignore commands that start with space
setopt hist_verify            # show command with history expansion to user before running it
setopt hist_reduce_blanks
setopt autocd

## Prompt

autoload -Uz vcs_info
precmd() { vcs_info }
zstyle ':vcs_info:git:*' formats ' %F{242}%b%f'
setopt prompt_subst
PROMPT_MULTILINE=$'\n%F{blue}%~%f${vcs_info_msg_0_}\n%(?.%F{magenta}.%F{red})$%f '
PROMPT_ONELINE='%(?.%F{magenta}.%F{red})$%f '
PROMPT=$PROMPT_MULTILINE
ps1-oneline() { PROMPT=$PROMPT_ONELINE }
ps1-reset()   { PROMPT=$PROMPT_MULTILINE }

## Use arrows to search by entering parts of the words

bindkey "^[[B" history-beginning-search-forward # up
bindkey "^[[A" history-beginning-search-backward # down
bindkey "^[OB" history-beginning-search-forward
bindkey "^[OA" history-beginning-search-backward
bindkey "\e[1;5C": forward-word   # ctrl + right
bindkey "\e[1;5D": backward-word  # ctrl + left
bindkey "\e[C" forward-char
bindkey "\e[D" backward-char
bindkey '^[[3~' delete-char
my-backward-delete-word() {
    local WORDCHARS=$WORDCHARS
    # stop at ':'
    WORDCHARS="${WORDCHARS//:}"
    # stop at '/'
    WORDCHARS="${WORDCHARS//\/}"
    # stop at '.'
    WORDCHARS="${WORDCHARS//.}"
    # zle <widget-name> will run an existing widget.
    zle backward-delete-word
}
# create a new widget
zle -N my-backward-delete-word
bindkey "^W" my-backward-delete-word
bindkey "^[^?" my-backward-delete-word
bindkey "^?" backward-delete-char

## Aliases

[ -f ~/.aliases ] && source ~/.aliases

alias j='jobs'
alias s='git status --short'
alias ls='ls --color'
alias ll='ls -GlaF'
alias ..='cd ..'
alias vv="NVIM_APPNAME=nvim-clean nvim"

# Git aliases. See: https://github.com/ohmyzsh/ohmyzsh/wiki/Cheatsheet
alias ggp="git push origin"
alias ggpf="git push origin --force-with-lease"
alias ggl="git pull origin"
alias gfa="git fetch --all --prune"
alias gcb="git switch -c" # deprecate
alias gsc="git switch -c"
alias gco="git switch" # deprecate
alias gs="git switch"
alias gr="git restore"
alias gcam="git commit -am"

# Tools aliases
alias be='bundle exec'
alias batn='bat --style=plain' # no line numbers

## ENVs
paths=(
  "$HOME/bin"
  "$HOME/go/bin"
  "/opt/homebrew/bin"
	"/opt/homebrew/opt/make/libexec/gnubin"
  "/opt/homebrew/opt/gnu-sed/libexec/gnubin"
#	"$(go env GOPATH)/bin"
)
for p in ${paths[@]}; do
  if [[ -d "$p" ]]; then
    PATH="$p:$PATH"
  fi
done

EDITOR="vi"
if command -v nvim >/dev/null 2>&1; then
  EDITOR="nvim"
  alias v=nvim
fi
export EDITOR PATH

export PAGER=less
export LIBRARY_PATH="${LIBRARY_PATH}:/opt/homebrew/lib"
export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:$(brew --prefix openssl)/lib/pkgconfig/

## gcloud
source "$(brew --prefix)/share/google-cloud-sdk/path.zsh.inc"
source "$(brew --prefix)/share/google-cloud-sdk/completion.zsh.inc"

## Tools
init_tool() {
  local tool=$1
  shift

  if command -v "$tool" >/dev/null 2>&1; then
    eval "$($tool "$@")"
  fi
}

init_tool ~/.local/bin/mise activate zsh
init_tool ~/.rakubrew/bin/rakubrew init Zsh
init_tool fzf --zsh

# Useful API keys
[ -f ~/.api_keys.env ] && source ~/.api_keys.env

alias nz="$EDITOR ~/.zshrc"
alias rz="source ~/.zshrc"
