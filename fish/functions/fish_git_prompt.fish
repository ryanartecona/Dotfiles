function fish_git_prompt --description 'Write out the git prompt'

  if test -z $fish_prompt_git_status_order[1]
    # hack to get some variables populated
    __terlar_git_prompt >/dev/null
  end

  set -l branch (git rev-parse --abbrev-ref HEAD 2>/dev/null)
  if test -z $branch
    return
  end

  set_color $fish_color_prompt_delimiter
  echo -n $fish_prompt_git_prefix
  set_color normal

  set -l index (git status --porcelain 2>/dev/null|cut -c 1-2|sort -u)
  if test -z "$index"
    set_color $fish_color_git_clean
    echo -n $branch'✓'
    set_color normal
  end

  set -l ahead (git log origin/{$branch}..HEAD 2>/dev/null | grep '^commit' >/dev/null; echo -n $status)

  if test -z "$index"
    # return
  end

  set -l gs
  set -l staged

  for i in $index
    if echo $i | grep '^[AMRCD]' >/dev/null
      set staged 1
    end

    switch $i
      case 'A '               ; set gs $gs added
      case 'M ' ' M'          ; set gs $gs modified
      case 'R '               ; set gs $gs renamed
      case 'C '               ; set gs $gs copied
      case 'D ' ' D'          ; set gs $gs deleted
      case '\?\?'             ; set gs $gs untracked
      case 'U*' '*U' 'DD' 'AA'; set gs $gs unmerged
    end
  end

  if test -n "$index"
    if set -q staged[1]
      set_color $fish_color_git_staged
    else
      set_color $fish_color_git_dirty
    end

    echo -n $branch'⚡'
    set_color normal
  end

  for i in $fish_prompt_git_status_order
    if contains $i in $gs
      set -l color_name fish_color_git_$i
      set -l status_name fish_prompt_git_status_$i

      set_color $$color_name
      echo -n $$status_name
    end
  end

  if test "$ahead" -eq 0
    set_color $fish_color_git_ahead
    echo -n '⇡'
    set_color normal
  end

  set_color normal
end
