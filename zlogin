# $PATH
pathdirs=(
	$HOME/bin
	/usr/local/share/npm/bin
	/usr/local/opt/php54/bin
	$HOME/.rvm/bin
	$HOME/.cabal/bin
	$HOME/Library/Haskell/bin
	/export/apps/xtools/bin
	/usr/local/linkedin/bin
	/usr/local/share/python
	/usr/local/bin
	/usr/local/sbin
	/usr/bin
	/bin
	/usr/sbin
	/sbin)

# auto-prune nonexistent directories
# as per http://stackoverflow.com/questions/9347478/how-to-edit-path-variable-in-zsh
path=($^pathdirs(N)) 

# Load RVM into a shell session *as a function*
# and add rvm bin locations to PATH
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" 

