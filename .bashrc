# .bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return
export BASH_SILENCE_DEPRECATION_WARNING=1

# Aliases

[ -f ~/.aliases ] && source ~/.aliases
alias ls='ls --color'
alias s='git status --short'
alias dc='docker-compose'
alias de='docker-compose exec'
alias ll='nnn -de'
alias ..='cd ..'
alias e='emacsclient -nw'
alias ec='emacsclient'

# Git aliases. See: https://github.com/ohmyzsh/ohmyzsh/wiki/Cheatsheet

alias ggp="git push origin \$(current_git_branch)"
alias ggpf="git push origin \$(current_git_branch) --force-with-lease"
alias ggl="git pull origin \$(current_git_branch)"
alias gfa="git fetch --all --prune"
alias gcb="git checkout -b"
alias gco="git checkout"
alias gcam="git commit -am"

alias be='bundle exec'

# PS1

current_git_branch() {
  res=$(git rev-parse --abbrev-ref HEAD 2>/dev/null)
  [ "$res" != "" ] && echo " $res"
}

beautiful_git_branch() {
    current_git_branch | sed -e 's|fix/|🛠️  |' \
                             -e 's|feature/|⚡|' \
                             -e 's|chore/|🏠|' \
			     -Ee 's|([A-Z]+-[0-9]+)-.*|[\1]|'
}

git-msg() {
    issue=$(beautiful_git_branch | sed -E 's|[^A-Z]*([A-Z]+-[0-9]+)-(.*)|\1|')
    msg=$(beautiful_git_branch   | sed -E 's|.*([A-Z]+-[0-9]+)-(.*)|\2|' | sed -e 's|-| |g')
    echo "[${issue}] ${msg^}"
}

export PS1="\[\033[01;36m\][\u: \w]\[\033[01;33m\]\$(beautiful_git_branch)\[\033[00m\]\[\033[01;36m\] \$ \[\033[00m\]"
export PS2='🏃‍ '

# envs

paths=(
  ${HOME}/go/bin
  ${HOME}/bin
  /opt/homebrew/bin
)
for path in ${paths[*]}; do
	export PATH="${PATH}:${path}"
done
export PATH="/opt/homebrew/opt/gnu-sed/libexec/gnubin:$PATH"

export EDITOR=nvim
export PAGER=less

gpgconf --launch gpg-agent
export SSH_AUTH_SOCK=$HOME/.gnupg/S.gpg-agent.ssh

# export LDFLAGS="-L/opt/homebrew/lib $LDFLAGS"
# export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:/opt/homebrew/lib"
export LIBRARY_PATH="${LIBRARY_PATH}:/opt/homebrew/lib"
export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:$(brew --prefix openssl)/lib/pkgconfig/

eval "$(/usr/local/bin/rtx activate bash)"
