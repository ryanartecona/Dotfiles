### PATH
pathdirs=(
	/Library/Frameworks/EPD64.framework/Versions/Current/bin
	# path+=/Library/Frameworks/Python.framework/Versions/2.7/bin
	/usr/local/bin
	/usr/local/sbin
	/usr/bin
	/bin
	/usr/sbin
	/sbin)

path=($^pathdirs(N)) # as per http://stackoverflow.com/questions/9347478/how-to-edit-path-variable-in-zsh

# # debug PATH
# for dir in $path 
# do
# 	echo $dir
# done

# Load RVM into a shell session *as a function*
# and add rvm bin locations to PATH
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" 

