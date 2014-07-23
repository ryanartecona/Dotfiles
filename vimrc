if &shell =~# 'fish$'
  set shell=/bin/bash
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

"Wrap visual selections with chars
:vnoremap ( "zdi(<C-R>z)<ESC>
:vnoremap { "zdi{<C-R>z}<ESC>
:vnoremap [ "zdi[<C-R>z]<ESC>
:vnoremap ' "zdi'<C-R>z'<ESC>
:vnoremap " "zdi"<C-R>z"<ESC>

nmap <leader>w :w<cr>
command! W w !sudo tee % > /dev/null

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
set tabstop=4
set softtabstop=2
set shiftwidth=2
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

autocmd BufWritePost .vimrc source $MYVIMRC

set nobackup

"Printing (:hardcopy) options
set printoptions=paper:letter,syntax:y,number:y,duplex:off,left:5pc

set laststatus=2

"=============
"=== Keys! ===
"=============

" Split line to the left of the cursor
nnoremap K i<CR><Esc>

" Toggle invis chars
nnoremap <leader>l :set list!<CR>

" Toggle recent search highlight
nnoremap <leader>/ :nohlsearch<CR>

" Quick-open ~/.vimrc
nnoremap <leader>v :tabnew $MYVIMRC<CR>


"===============
"=== Airline ===
"===============

if has("gui_running")
  let g:airline_theme='solarized'
else
  let g:airline_theme='bubblegum'
endif
let g:airline_left_sep=' '
let g:airline_right_sep=' '


"=================
"=== GitGutter ===
"=================

highlight! link GitGutterAdd          DiffAdd
highlight! link GitGutterChange       DiffChange
highlight! link GitGutterDelete       DiffDelete
highlight! link GitGutterChangeDelete DiffDelete


"==============
"=== Vundle ===
"==============

set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()

" Plugins
Plugin 'gmarik/Vundle.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'airblade/vim-gitgutter'
Plugin 'bling/vim-airline'
Plugin 'scrooloose/syntastic'
Plugin 'scrooloose/nerdtree'
Plugin 'dag/vim-fish'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'Lokaltog/vim-easymotion'
Plugin 'kien/ctrlp.vim'
Plugin 'tommcdo/vim-exchange'

" Colors
Plugin 'altercation/vim-colors-solarized'

" Syntaxes
Plugin 'kchmck/vim-coffee-script'

call vundle#end()
filetype plugin indent on


"==========
"=== UI ===
"==========

set background=dark
set t_Co=256
colorscheme slate


"===================
"=== GUI Options ===
"===================
if has("gui_running")

  set guioptions=gtrLme
  set guitablabel=%M\ %t

  set background=dark
  colorscheme solarized
  highlight! link SignColumn Normal

  set cursorline

endif
