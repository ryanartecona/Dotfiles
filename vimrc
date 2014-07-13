if &shell =~# 'fish$'
  set shell=/usr/local/bin/zsh
endif

set history=700

filetype plugin on
filetype indent on
syntax enable

set mouse=a
set selectmode-=mouse

set autoread

let mapleader = ","
let g:mapleader = ","

nmap <leader>w :w<cr>

"Wrap visual selections with chars
:vnoremap ( "zdi(<C-R>z)<ESC>
:vnoremap { "zdi{<C-R>z}<ESC>
:vnoremap [ "zdi[<C-R>z]<ESC>
:vnoremap ' "zdi'<C-R>z'<ESC>
:vnoremap " "zdi"<C-R>z"<ESC>

command W w !sudo tee % > /dev/null

" Autocompletion using the TAB key
" This function determines, whether we are on the start of the line text (then tab indents) or
" if we want to try autocompletion
function! InsertTabWrapper()
  let col = col('.') - 1
  if !col || getline('.')[col - 1] !~ '\k'
    return "\<tab>"
  else
    return "\<c-p>"
  endif
endfunction
" Remap the tab key to select action with InsertTabWrapper
inoremap <tab> <c-r>=InsertTabWrapper()<cr>


set so=7

set wildmenu

set wildignore=*.o,*~,*.pyc,.git\*,.hg\*,.svn\*

set ruler
set foldcolumn=1
set number

set cmdheight=1
set scrolloff=4

set hid

set backspace=eol,start,indent
set whichwrap+=<,>,h,l

set incsearch
set ignorecase
set smartcase
set hlsearch

set showmatch
set mat=2

set smarttab
set expandtab
set shiftwidth=2
set tabstop=4
set autoindent
set smartindent

set listchars=tab:⇢\ ,trail:·,eol:◦

set nowrap

set foldmethod=indent
set nofoldenable


" Return to last edit position when opening files
autocmd BufReadPost *
     \ if line("'\"") > 0 && line("'\"") <= line("$") |
     \   exe "normal! g`\"" |
     \ endif
" Remember info about open buffers on close
set viminfo^=%

autocmd FileType c,cpp,java,php,js,css,xml,xsl,s,go,py,hs,h,m autocmd BufWritePre * :%s/[ \t\r]\+$//e

set nobackup

"Printing (:hardcopy) options
set printoptions=paper:letter,syntax:y,number:y,duplex:off,left:5pc



set laststatus=2

set background=dark
set t_Co=256

" Set extra options when running in GUI mode
if has("gui_running")
    set guioptions-=T
    set guioptions-=e
    set guitablabel=%M\ %t
endif


"===============
"=== Airline ===
"===============

let g:airline_theme='bubblegum'
let g:airline_left_sep=' '
let g:airline_right_sep=' '


"==============
"=== Vundle ===
"==============

set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()

Plugin 'gmarik/Vundle.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'bling/vim-airline'
Plugin 'scrooloose/syntastic'
Plugin 'scrooloose/nerdtree'
Plugin 'dag/vim-fish'
Plugin 'terryma/vim-multiple-cursors'

call vundle#end()
filetype plugin indent on
