# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="RAsunrise"

### Aliases
alias zshconfig="sublime ~/.zshrc"
alias ohmyzsh="sublime ~/.oh-my-zsh"
alias g="hub"
alias localmongod='mongod --config /usr/local/etc/mongod.conf'
alias cpdir="cp -R"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Disable bi-weekly auto-update checks
# `upgrade_oh_my_zsh` to run manually
DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=()

### Custom functions

# pretty-print a zsh array
parray () { 
	echo ${(pj:\n:)*}
}
# git prompt helpers
function current_branch() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || \
  ref=$(git rev-parse --short HEAD 2> /dev/null) || return
  echo ${ref#refs/heads/}
}
function current_repository() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || \
  ref=$(git rev-parse --short HEAD 2> /dev/null) || return
  echo $(git remote -v | cut -d':' -f 2)
}

## Load OhMyZsh
source $ZSH/oh-my-zsh.sh

## Enable zsh syntax highlighting
source $HOME/.zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
