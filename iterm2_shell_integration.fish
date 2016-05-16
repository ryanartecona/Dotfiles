# Modified from https://iterm2.com/misc/fish_startup.in

if begin; status --is-interactive; and not functions -q -- iterm2_status; and [ "$TERM" != screen ]; end
  function iterm2_status
    printf "\033]133;D;%s\007" $argv
  end

  # Mark start of prompt
  function iterm2_prompt_start
    printf "\033]133;A\007"
  end

  # Mark end of prompt
  function iterm2_prompt_end
    printf "\033]133;B\007"
  end

  # Tell terminal to create a mark at this location
  function iterm2_preexec
    # For other shells we would output status here but we can't do that in fish.
    printf "\033]133;C;\007"
  end

  # Usage: iterm2_set_user_var key value
  # These variables show up in badges (and later in other places). For example
  # iterm2_set_user_var currentDirectory "$PWD"
  # Gives a variable accessible in a badge by \(user.currentDirectory)
  # Calls to this go in iterm2_print_user_vars.
  function iterm2_set_user_var
    printf "\033]1337;SetUserVar=%s=%s\007" "$argv[1]" (printf "%s" "$argv[2]" | base64)
  end

  # iTerm2 inform terminal that command starts here
  function iterm2_precmd
    printf "\033]1337;RemoteHost=%s@%s\007\033]1337;CurrentDir=$PWD\007" $USER $iterm2_hostname

    # Users can define a function called iterm2_print_user_vars.
    # It should call iterm2_set_user_var and produce no other output.
    if functions -q -- iterm2_print_user_vars
      iterm2_print_user_vars
    end

  end

  function iterm2_pre_fish_prompt --description 'Write out the mode prompt; do not replace this. Instead, change fish_mode_prompt before sourcing .iterm2_shell_integration.fish, or modify iterm2_fish_mode_prompt instead.'
      set -l last_status $status
      iterm2_status $last_status
      iterm2_prompt_start
      sh -c "exit $last_status"
  end

  function -v _ underscore_change
    if [ x$_ = xfish ]
      iterm2_precmd
    else
      iterm2_preexec
    end
  end

  # If hostname -f is slow for you, set iterm2_hostname before sourcing this script
  if not set -q iterm2_hostname
    set iterm2_hostname (hostname -f)
  end

  iterm2_precmd
  printf "\033]1337;ShellIntegrationVersion=1\007"
end