if which hub  >/dev/null
  alias g=hub
end

if status --is-interactive
  set -xg EDITOR                          (which vim)

  set -g fish_prompt_git_prefix           ' | '

  set -g fish_color_autosuggestion        black
  set -g fish_color_command               magenta --bold
  set -g fish_color_comment               red
  set -g fish_color_cwd                   green
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
end


function allowed_paths --description "User-allowed \$path dirs"
  echo $HOME/bin
  echo /usr/local/share/npm/bin
  echo $HOME/.rvm/bin
  echo $HOME/.cabal/bin
  echo $HOME/Library/Haskell/bin
  echo /export/apps/xtools/bin
  echo /Applications/ghc-7.8.2.app/Contents/bin
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

# RVM needs to be run once to add stuff to $PATH
rvm >/dev/null ^/dev/null

# Tell Docker where it should connect
set -x DOCKER_HOST "tcp://localhost:2375"
