{
	# -----------------------------
	# --------- oh-my-zsh ---------
	# -----------------------------

	# Path to your oh-my-zsh configuration.
	ZSH=$HOME/.oh-my-zsh

	# from $ZSH/themes or $ZSH/custom
	ZSH_THEME="RAsunrise"

	DISABLE_AUTO_UPDATE="true"
	COMPLETION_WAITING_DOTS="true"

	# enabled oh-my-zsh plugins
	# format: plugins=(rails git textmate ruby lighthouse)
	plugins=()

	# Load OhMyZsh
	source $ZSH/oh-my-zsh.sh

	# Enable zsh syntax highlighting
	source $HOME/.zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
}

if [[ $(uname) == Linux ]] {
	# -----------------------------
	# ----------- Linux -----------
	# -----------------------------

	alias ls="ls --color=auto -Ap" # list dotfiles, trail dirs with /
	alias g="git" 
}

if [[ $(uname) == Darwin && -n $(whence brew) ]] {
	# ------------------------------
	# ----- Mac OSX, with brew -----
	# ------------------------------
	
	# --- Finder preferences ---
	# show hidden files by default
	defaults write com.apple.finder AppleShowAllFiles -bool true
	# Display full POSIX path as window title
	defaults write com.apple.finder _FXShowPosixPathInTitle -bool true


	# use GNU ls from coreutils (installed via brew: prefixed with g)
	eval `gdircolors $HOME/.dircolors-ansi-universal`
	alias ls="gls --color=auto -Ap" # list dotfiles, trail dirs with /

	alias g="hub" # github-flavored git, via brew
}

{
	# -----------------------------
	# ------- All platforms -------
	# -----------------------------

	# pretty-print a zsh array, e.g. $PATH
	parray () { 
		echo ${(pj:\n:)*}
	}

	# Override stupid OhMyZsh ls aliases
	alias ll="ls -l -hgG" # human sizes, no owner/group
	alias la="ls"
	alias l="ls -1"

	alias symlink="ln -s"
	alias cpdir="cp -R"

	export EDITOR="$(whence sublime) --new-window --wait"
}

# sensitive machine-specific config
if [[ -f ~/.local.zshrc ]] {

	source ~/.local.zshrc

}
