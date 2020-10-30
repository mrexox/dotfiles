# .bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
#PS1='[\u@\h \W]\$ '


alias s='git status --short'
alias dc='docker-compose $*'
alias ll='ls -alF'
alias ..='cd ..'

paths=( ${HOME}/go/bin $HOME/.rbenv/bin )
for path in ${paths[*]}; do
	export PATH="${PATH}:${path}"
done
eval "$(rbenv init -)"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

current_git_branch() {
  res=$(git rev-parse --abbrev-ref HEAD 2>/dev/null)
  [ "$res" != "" ] && echo " $res"
}

export PS1="\[\033[01;36m\][\u: \w]\[\033[01;33m\]\$(current_git_branch)\[\033[00m\]\[\033[01;36m\] \$ \[\033[00m\]"
export PS2='... '
