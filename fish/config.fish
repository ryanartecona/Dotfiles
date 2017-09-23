alias g=hub
alias httpv="http --print=HBhb"


if status --is-interactive
  set -xg ALTERNATE_EDITOR (which vim)
  if test "$INSIDE_EMACS"
    set -xg EDITOR "emacsclient"
  else
    set -xg EDITOR "emacsclient --create-frame"
  end

  set -g fish_prompt_git_prefix           ' ± '

  set -g fish_color_autosuggestion        black
  set -g fish_color_command               magenta --bold
  set -g fish_color_comment               red
  set -g fish_color_cwd                   cyan
  set -g fish_color_cwd_root              red
  set -g fish_color_error                 red --bold
  set -g fish_color_escape                cyan
  set -g fish_color_git_ahead             yellow --bold
  set -g fish_color_git_clean             green
  set -g fish_color_git_dirty             red
  set -g fish_color_history_current       cyan
  set -g fish_color_host                  black --bold
  set -g fish_color_match                 cyan
  set -g fish_color_normal                normal
  set -g fish_color_operator              cyan
  set -g fish_color_param                 magenta
  set -g fish_color_prompt_delimiter      black
  set -g fish_color_prompt_symbol         blue --bold
  set -g fish_color_quote                 cyan --bold
  set -g fish_color_redirection           normal
  set -g fish_color_search_match          --background=purple
  set -g fish_color_status                red
  set -g fish_color_user                  black --bold
  set -g fish_color_valid_path            cyan
  set -g fish_color_vi_normal             green --bold
  set -g fish_color_vi_insert             yellow
  set -g fish_color_vi_visual             magenta --bold
end


# Setup nix build daemon & multi-user stuff early, since other tools later may
# be installed by nix
if type -q bass; and test -f /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
  bass source /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh 2> /dev/null
else
  echo "WARN: Could not source nix-daemon.sh profile (with bass)"
end
# Set Nix path to default nixpkgs location
set -xg NIX_PATH nixpkgs="$HOME"/.nix-defexpr/channels/nixpkgs


function allowed_paths --description "User-allowed \$path dirs"
  echo $HOME/bin
  echo $HOME/.rvm/bin
  echo $HOME/.cabal/bin
  echo $HOME/Library/Haskell/bin
  echo /usr/local/texlive/2015/bin/x86_64-darwin
  echo $HOME/.nix-profile/bin
  echo $HOME/.nix-profile/sbin
  if type -Pq opam
    echo (opam config var bin)
    echo (opam config var sbin)
  end
  echo $HOME/.npm/bin
  echo $HOME/.go/bin
  echo /usr/local/bin
  echo /usr/local/sbin
  echo /usr/bin
  echo /bin
  echo /usr/sbin
  echo /sbin
  echo /var/setuid-wrappers
  echo /run/current-system/sw/bin
  echo /run/current-system/sw/sbin
end

function valid_allowed_paths --description "User-allowed \$path dirs (that currently exist)"
  for p in (allowed_paths)
    if test -d "$p"
      echo "$p"
    end
  end
end


# Fisherman wants fish at >2.3.0 for 'snippet' support, but this suffices
# until fish v2.3 is released (check here: https://github.com/fish-shell/fish-shell/releases)
for file in ~/.config/fish/conf.d/*.fish
  source $file
end

# $fish_user_paths is a magic variable that prepends to $PATH
set fish_user_paths (valid_allowed_paths)

# Turn on vi key bindings, unless we're in an Emacs shell
# if not test $INSIDE_EMACS
#   fish_vi_mode
# end

# OPAM configuration
if type -Pq opam
  # Let opam set the env vars it wants, but don't let it break my MANPATH
  env SHELL=fish opam config env | grep -vi 'MANPATH' | source
end

# RVM for Ruby needs to be run once to add stuff to $PATH
if type -q rvm
  rvm >/dev/null ^/dev/null
end

# Golang
if not test -d ~/.go
  mkdir -p ~/.go
end
set -gx GOPATH "$HOME/.go"

# Elixir via kiex
if test -f ~/.kiex/scripts/kiex.fish
  source ~/.kiex/scripts/kiex.fish
end

# iterm2 prompt integration helpers
if test -n "$ITERM_SESSION_ID" -a -f ~/.iterm2_shell_integration.fish
  source ~/.iterm2_shell_integration.fish
end

# support detection of shell nesting level for prompt
if test -z "$SHELL_NESTING_LEVEL"
  set -gx SHELL_NESTING_LEVEL '-1'
end
if not set -q THIS_SHELL_NESTING_LEVEL
  set -g THIS_SHELL_NESTING_LEVEL (expr "$SHELL_NESTING_LEVEL" + 1)
end
set -gx SHELL_NESTING_LEVEL "$THIS_SHELL_NESTING_LEVEL"


# Custom <TAB>-expandions
if type -q expand-word
  # gb<TAB> to choose among local git branches
  expand-word -p '^gb$' -e 'git branch | cut -c 3-'
  # gba<TAB> to choose among local & remote git branches
  expand-word -p '^gba$' -e 'git branch -a | cut -c 3- | sed "s|remotes/[[:alpha:]]\+/||"'
end

# setup initial autoenvstack, if starting from a dir with an .env.fish
if status is-interactive; and type -q _autoenvstack
  _autoenvstack
end
