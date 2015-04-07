function fish_prompt --description 'Write out the prompt'
  set -l last_status $status

  if test $last_status -ne 0
    set_color $fish_color_error
    echo "✖ $last_status"
    set_color normal
  end

  echo

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

  echo

  set_color $fish_color_prompt_symbol

  echo -n '> '
  set_color normal
end
