if which hub  >/dev/null
  alias g=hub
end
if which http  >/dev/null
  alias httpv="http --print=HBhb"
end

if status --is-interactive
  set -xg ALTERNATE_EDITOR                          (which vim)
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


function allowed_paths --description "User-allowed \$path dirs"
  echo $HOME/bin
  echo /usr/local/share/npm/bin
  echo $HOME/.rvm/bin
  echo $HOME/.cabal/bin
  echo $HOME/Library/Haskell/bin
  echo /export/apps/xtools/bin
  echo /Applications/ghc-7.8.3.app/Contents/bin
  echo $HOME/.opam/4.02.1/bin
  echo $HOME/.opam/system/bin
  echo /usr/local/texlive/2015/bin/x86_64-darwin
  echo /usr/local/bin
  echo /usr/local/sbin
  echo /usr/bin
  echo /bin
  echo /usr/sbin
  echo /sbin
end

function valid_allowed_paths --description "User-allowed \$path dirs (that currently exist)"
  for p in (allowed_paths)
    if test -d "$p"
      echo "$p"
    end
  end
end

# $fish_user_paths is a magic variable that prepends to $PATH
set fish_user_paths (valid_allowed_paths)

# Turn on vi key bindings, unless we're in an Emacs shell
if not test $INSIDE_EMACS
  fish_vi_mode
end

# z-fish needs to be sourced
if test -f ~/.z-fish/z.fish  >/dev/null
  source ~/.z-fish/z.fish
end

# RVM needs to be run once to add stuff to $PATH
rvm >/dev/null ^/dev/null

# Tell Docker where it should connect
set -x DOCKER_HOST "tcp://localhost:2375"

# OPAM configuration
source /Users/ryanartecona/.opam/opam-init/init.fish > /dev/null 2> /dev/null or true
