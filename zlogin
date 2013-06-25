### PATH
pathdirs=(
	$(brew --prefix)/share/npm/bin
	$HOME/Library/Haskell/bin
	/usr/local/bin # Homebrew
	/usr/local/sbin
	/usr/bin
	/bin
	/usr/sbin
	/sbin)

# auto-prune nonexistent directories
# as per http://stackoverflow.com/questions/9347478/how-to-edit-path-variable-in-zsh
path=($^pathdirs(N)) 

## helpful directories
alias m101p="~/Code/M101P/"
alias haskelldocs="open ~/Library/Haskell/doc/index.html"

# Load RVM into a shell session *as a function*
# and add rvm bin locations to PATH
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" 

