#-------------------------------------------------------------------------------
# Sunrise theme for oh-my-zsh by Adam Lindberg (eproxus@gmail.com)
# Intended to be used with Solarized: http://ethanschoonover.com/solarized
# (Needs Git plugin for current_branch method)
#-------------------------------------------------------------------------------

# Color shortcuts
Y=$fg[yellow]
R=$fg[red]
G=$fg[green]
B=$fg[blue] 
M=$fg[magenta]
L=$fg[black]
W=$fg[white]
RB=$fg_bold[red]
GB=$fg_bold[green]
YB=$fg_bold[yellow]
BB=$fg_bold[blue]
LB=$fg_bold[black]
RESET=$reset_color

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

me="$(whoami)"
if [ $me = "root" ]; then
    PROMPTCOLOR="%{$RB%}" PREFIX="-!-";
else
    PROMPTCOLOR="" PREFIX="---";
fi

# virtualenv support
export VIRTUAL_ENV_DISABLE_PROMPT=1

custom_venv_prompt() {
  local VENV_STATUS=""
  if [[ -n $VIRTUAL_ENV ]]; then
    VENV_STATUS="%{$G%}(venv)%{$RESET%}"
  fi
  echo $VENV_STATUS
}  

local return_code="%(?..%{$RB%}%? ↵%{$RESET%})"

# Get the status of the working tree (copied and modified from git.zsh)
custom_git_prompt_status() {
  INDEX=$(git status --porcelain 2> /dev/null)
  STATUS=""
  # Non-staged
  if $(echo "$INDEX" | grep '^?? ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_UNTRACKED$STATUS"
  fi
  if $(echo "$INDEX" | grep '^UU ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_UNMERGED$STATUS"
  fi
  if $(echo "$INDEX" | grep '^ D ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_DELETED$STATUS"
  fi
  if $(echo "$INDEX" | grep '^.M ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_MODIFIED$STATUS"
  elif $(echo "$INDEX" | grep '^AM ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_MODIFIED$STATUS"
  elif $(echo "$INDEX" | grep '^ T ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_MODIFIED$STATUS"
  fi
  # Staged
  if $(echo "$INDEX" | grep '^D  ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_STAGED_DELETED$STATUS"
  fi
  if $(echo "$INDEX" | grep '^R' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_STAGED_RENAMED$STATUS"
  fi
  if $(echo "$INDEX" | grep '^M' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_STAGED_MODIFIED$STATUS"
  fi
  if $(echo "$INDEX" | grep '^A' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_STAGED_ADDED$STATUS"
  fi

  if $(echo -n "$STATUS" | grep '.*' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_STATUS_PREFIX$STATUS"
  fi

  echo $STATUS
}

# get the name of the branch we are on (copied and modified from git.zsh)
function custom_git_prompt() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo "$ZSH_THEME_GIT_PROMPT_PREFIX${ref#refs/heads/}$(parse_git_dirty)$(git_prompt_ahead)$(custom_git_prompt_status)$ZSH_THEME_GIT_PROMPT_SUFFIX"
}

# %B sets bold text
PROMPT='
%{$L%}$PREFIX %2~ $(custom_git_prompt)$(custom_venv_prompt)
%{$BB%}>%{$RESET%} '
RPS1="${return_code}"

ZSH_THEME_GIT_PROMPT_PREFIX="%{$L%}%B‹"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$L%}%B›%b%{$RESET%} "

ZSH_THEME_GIT_PROMPT_DIRTY="%b%{$R%}*"
ZSH_THEME_GIT_PROMPT_CLEAN=""

ZSH_THEME_GIT_PROMPT_AHEAD="%{$Y%}%B»%b"

ZSH_THEME_GIT_STATUS_PREFIX=" "

# Staged
ZSH_THEME_GIT_PROMPT_STAGED_ADDED="%{$G%}A"
ZSH_THEME_GIT_PROMPT_STAGED_MODIFIED="%{$G%}M"
ZSH_THEME_GIT_PROMPT_STAGED_RENAMED="%{$G%}R"
ZSH_THEME_GIT_PROMPT_STAGED_DELETED="%{$G%}D"

# Not-staged
ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$Y%}⁇"
ZSH_THEME_GIT_PROMPT_MODIFIED="%{$Y%}M"
ZSH_THEME_GIT_PROMPT_DELETED="%{$Y%}D"
ZSH_THEME_GIT_PROMPT_UNMERGED="%{$Y%}UU"


# Additional customizaiton for zsh-syntax-highlighting
# ====================================================

typeset -gA ZSH_HIGHLIGHT_STYLES

ZSH_HIGHLIGHT_STYLES[default]=fg=magenta
ZSH_HIGHLIGHT_STYLES[alias]=fg=magenta,bold
ZSH_HIGHLIGHT_STYLES[builtin]=fg=magenta,bold
ZSH_HIGHLIGHT_STYLES[function]=fg=magenta,bold
ZSH_HIGHLIGHT_STYLES[command]=fg=magenta,bold
ZSH_HIGHLIGHT_STYLES[unknown-token]=fg=magenta
ZSH_HIGHLIGHT_STYLES[precommand]=fg=blue,bold
ZSH_HIGHLIGHT_STYLES[commandseparator]=fg=bold
ZSH_HIGHLIGHT_STYLES[assign]=fg=yellow
ZSH_HIGHLIGHT_STYLES[single-hyphen-option]=fg=blue
ZSH_HIGHLIGHT_STYLES[double-hyphen-option]=fg=blue
ZSH_HIGHLIGHT_STYLES[path]=fg=cyan,underline,bold
ZSH_HIGHLIGHT_STYLES[path_approx]=fg=cyan
ZSH_HIGHLIGHT_STYLES[path_prefix]=fg=cyan,underline
ZSH_HIGHLIGHT_STYLES[globbing]=fg=cyan,bold
