export ZSH="/Users/v.kiselev/.oh-my-zsh"
export NVM_DIR="${HOME}/.nvm"

ZSH_THEME="oxide"

plugins=(
  git
  bundler
  dotenv
  docker
  osx
  rake
  rbenv
  ruby
  zsh-autosuggestions
  colored-man-pages
  zsh-syntax-highlighting
  colorize
  command-not-found
)

# Aliases

alias s='git status --short'
alias dc='docker-compose $*'
alias g='git $*'
alias tags='find . -name "*.rb" | xargs ctags'

# Sources

source ${ZSH}/oh-my-zsh.sh
source ${HOME}/.rails-aliases.sh
source ${HOME}/tmux.helpers.sh

# This loads autojump aliases
[ -f /usr/local/etc/profile.d/autojump.sh ] && source /usr/local/etc/profile.d/autojump.sh

# This loads nvm
[ -s "$NVM_DIR/nvm.sh" ] && source "$NVM_DIR/nvm.sh"

# This loads nvm bash_completion
[ -s "$NVM_DIR/bash_completion" ] && source "$NVM_DIR/bash_completion"

# Exports

export PATH="/usr/local/opt/postgresql@9.5/bin:$PATH"
export PATH=${PATH}:${HOME}/bin

export PATH="$HOME/.rbenv/bin:$PATH"
export LDFLAGS="-L${HOME}/.rbenv/versions/2.6.5/openssl/lib $LDFLAGS"
