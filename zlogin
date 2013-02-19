

### PATH
path =/Library/Frameworks/EPD64.framework/Versions/Current/bin
# path+=/Library/Frameworks/Python.framework/Versions/2.7/bin
path+=/usr/local/bin
path+=/usr/local/sbin
path+=/usr/bin
path+=/bin
path+=/usr/sbin
path+=/sbin

path=($^path(N)) # as per http://stackoverflow.com/questions/9347478/how-to-edit-path-variable-in-zsh


# # debug PATH
# for dir in $path 
# do
# 	echo $dir
# done

# Load RVM into a shell session *as a function*
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" 

