export PATH="/usr/local/bin:/usr/local/sbin:/usr/local/mysql/bin:$PATH"

# This loads RVM
[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm" 

# Setting PATH for Python 2.7
# The orginal version is saved in .profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/2.7/bin:${PATH}"
export PATH

# Setting PATH for EPD-7.1-2
# The orginal version is saved in .profile.pysave
PATH="/Library/Frameworks/EPD64.framework/Versions/Current/bin:${PATH}"
export PATH

MKL_NUM_THREADS=1
export MKL_NUM_THREADS

EDITOR="/usr/bin/nano"
export EDITOR

#==========================================
#  Helpers
#==========================================

function confirm( ) {
	#confirm with the user
	read -r -p "$* [y/n]: " response
	case "$response" in
	    [yY][eE][sS]|[yY]) 
              #if yes, then execute the passed parameters
               "$@"
               ;;
	    *)
              #Otherwise exit...
              echo "cancelled."
              ;;
	esac
}

#==========================================
#  Aliases
#==========================================

alias rm='rm -i'
alias kill='confirm kill'

alias localmongod='mongod --config /usr/local/etc/mongod.conf'