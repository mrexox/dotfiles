# .bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Helpful functions

current_git_branch() {
  res=$(git rev-parse --abbrev-ref HEAD 2>/dev/null)
  [ "$res" != "" ] && echo " $res"
}

beautiful_git_branch() {
    current_git_branch | sed -e 's|fix/|🛠️ |' \
                             -e 's|feature/|⚡|' \
                             -e 's|chore/|🏠|' \
			     -Ee 's|([A-Z]+-[0-9]+)-.*|[\1]|'
}

git-msg() {
    issue=$(beautiful_git_branch | sed -E 's|[^A-Z]*([A-Z]+-[0-9]+)-(.*)|\1|')
    msg=$(beautiful_git_branch   | sed -E 's|.*([A-Z]+-[0-9]+)-(.*)|\2|' | sed -e 's|-| |g')
    echo "[${issue}] ${msg^}"
}

# Aliases

alias ls='ls --color'
alias s='git status --short'
alias dc='docker-compose'
alias ll='nnn -de'
alias ..='cd ..'
alias ec='emacsclient'
alias k='kubectl'

# Git aliases. See: https://github.com/ohmyzsh/ohmyzsh/wiki/Cheatsheet

alias ggp="git push origin \$(current_git_branch)"
alias ggpf="git push origin \$(current_git_branch) --force-with-lease"
alias ggl="git pull origin \$(current_git_branch)"
alias gfa="git fetch --all --prune"
alias gcb="git checkout -b"
alias gco="git checkout"
alias gcam="git commit -am"

alias be='bundle exec'

# envs

paths=(
	  ${HOME}/go/bin
	  ${HOME}/.rbenv/bin
	  ${HOME}/bin
)
for path in ${paths[*]}; do
	export PATH="${PATH}:${path}"
done

export PS1="\[\033[01;36m\][\u: \w]\[\033[01;33m\]\$(beautiful_git_branch)\[\033[00m\]\[\033[01;36m\] \$ \[\033[00m\]"
export PS2='🏃‍ '
export EDITOR='vim'

# Other settings

# rbenv
eval "$(rbenv init -)"

# nvm
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# gnupg agent for ssh
gpgconf --launch gpg-agent
export SSH_AUTH_SOCK=$HOME/.gnupg/S.gpg-agent.ssh

# nnn plugins
export NNN_PLUG='f:finder;o:fzopen;p:mocplay;d:diffs;t:nmount;v:imgview;b:preview-tui'
export NNN_FIFO=/tmp/nnn.fifo
