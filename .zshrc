export ZSH="/Users/v.kiselev/.oh-my-zsh"
export NVM_DIR="${HOME}/.nvm"

ZSH_THEME="oxide"

# Bindkeys
bindkey '^H' backward-kill-word

export ZSH_DOTENV_PROMPT=false

plugins=(
  fasd
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
#  zsh-syntax-highlighting
  colorize
  command-not-found
)

# Aliases

alias gs='git status --short'
alias dc='docker-compose $*'
alias tags='find . -name "*.rb" | xargs ctags'
alias branch='git rev-parse --abbrev-ref HEAD'
alias ec='emacsclient'
alias e='emacsclient -t'
alias less='bat'

# Sources

source ${ZSH}/oh-my-zsh.sh
source ${HOME}/.rails-aliases.sh
source ${HOME}/tmux.helpers.sh
source $HOME/.cargo/env

# This loads autojump aliases
# [ -f /usr/local/etc/profile.d/autojump.sh ] && source /usr/local/etc/profile.d/autojump.sh

# This loads nvm
[ -s "$NVM_DIR/nvm.sh" ] && source "$NVM_DIR/nvm.sh"

# This loads nvm bash_completion
[ -s "$NVM_DIR/bash_completion" ] && source "$NVM_DIR/bash_completion"

# Exports
export EDITOR=vim
export RUBY_CONFIGURE_OPTS=--with-readline-dir=`brew --prefix readline`

export PATH="/usr/local/opt/postgresql@9.5/bin:$PATH"
export PATH=${PATH}:${HOME}/bin

export PATH="$HOME/.rbenv/bin:$PATH"
export LDFLAGS="-L${HOME}/.rbenv/versions/2.6.5/openssl/lib $LDFLAGS"
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:/opt/local/lib"
export PKG_CONFIG_PATH="/usr/local/opt/openssl/lib/pkgconfig"
export GPG_TTY=$(tty)
export FZF_DEFAULT_OPTS='--height 40% --layout=reverse --border'

PATH="/Users/v.kiselev/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="/Users/v.kiselev/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/Users/v.kiselev/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/Users/v.kiselev/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/Users/v.kiselev/perl5"; export PERL_MM_OPT;
eval "$(perl -I$HOME/perl5/lib/perl5 -Mlocal::lib=$HOME/perl5)"

# Setopts
setopt no_hist_verify
