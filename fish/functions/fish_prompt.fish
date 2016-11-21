function fish_prompt --description 'Write out the prompt'
  set -l last_status $status

  type -q iterm2_status; and iterm2_status $last_status
  if test $last_status -ne 0
    set_color $fish_color_error
    echo "✖ $last_status"
    set_color normal
  end

  echo

  type -q iterm2_prompt_start; and iterm2_prompt_start

  # # User
  # set_color $fish_color_user
  # echo -n (whoami)
  # set_color normal
  # set_color $fish_color_prompt_delimiter

  # echo -n ' @ '

  # # Host
  # set_color $fish_color_host
  # echo -n (hostname -s)
  # set_color normal
  # set_color $fish_color_prompt_delimiter

  # echo -n ' : '

  # PWD
  set_color $fish_color_cwd
  echo -n (prompt_pwd)
  set_color normal
  set_color $fish_color_prompt_delimiter

  set_color normal
  fish_git_prompt
  set_color normal

  if test -n "$VIRTUAL_ENV"
    set_color $fish_color_prompt_delimiter
    echo -n ' (venv:'
    set_color $fish_color_host
    echo -n (basename (dirname $VIRTUAL_ENV))
    set_color normal
    set_color $fish_color_prompt_delimiter
    echo -n ')'
    set_color normal
  end

  if test -n "$IN_NIX_SHELL"
    set_color $fish_color_prompt_delimiter
    echo -n ' ('
    set_color $fish_color_host
    echo -n 'nix-shell'
    set_color normal
    set_color $fish_color_prompt_delimiter
    echo -n ')'
    set_color normal
  end

  echo

  fish_vi_mode_prompt

  set_color $fish_color_prompt_symbol

  echo -n '> '
  set_color normal

  type -q iterm2_prompt_end; and iterm2_prompt_end
end
