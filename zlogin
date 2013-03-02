### PATH
pathdirs=(
	# /Library/Frameworks/EPD64.framework/Versions/Current/bin
	# /Library/Frameworks/Python.framework/Versions/2.7/bin
	# use the brew python version (2.7.3) instead
	$HOME/Library/Haskell/bin
	/usr/local/bin
	/usr/local/sbin
	/usr/bin
	/bin
	/usr/sbin
	/sbin)

path=($^pathdirs(N)) # as per http://stackoverflow.com/questions/9347478/how-to-edit-path-variable-in-zsh

## helpful directories
alias m101p="~/Code/M101P/"
alias haskelldocs="open ~/Library/Haskell/doc/index.html"

# Load RVM into a shell session *as a function*
# and add rvm bin locations to PATH
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" 

