function fish_vi_mode_prompt
  # Display vi mode (if in vi mode)
  if set -q __fish_vi_mode
    switch $fish_bind_mode
      case default
        set_color $fish_color_vi_normal
        echo -n 'N '
      case insert
        set_color $fish_color_vi_insert
        echo -n 'I '
      case visual
        set_color $fish_color_vi_visual
        echo -n 'V '
    end
    set_color normal
  end
end
