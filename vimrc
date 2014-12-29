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

" I don't want to deal with hidden buffers
set nohidden

set backspace=eol,start,indent
set whichwrap+=<,>,h,l

set incsearch
set ignorecase
set smartcase
set nohlsearch

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
augroup RememberLastPosition
  autocmd!
  autocmd BufReadPost *
       \ if line("'\"") > 0 && line("'\"") <= line("$") |
       \   exe "normal! g`\"" |
       \ endif
augroup END

" Remember info about open buffers on close
set viminfo^=%

augroup TrimTrailingWhitespace
  autocmd!
  autocmd FileType c,cpp,java,php,js,css,xml,xsl,s,go,py,haskell,h,m,ruby,coffee,yml autocmd BufWritePre * :%s/[ \t\r]\+$//e
augroup END

set nobackup
set noswapfile
set backupdir=~/.vim/backup//
set directory=~/.vim/swap//
set undodir=~/.vim/undo//

"Printing (:hardcopy) options
set printoptions=paper:letter,syntax:y,number:y,duplex:off,left:5pc

set laststatus=2

"=================
"=== FileTypes ===
"=================

augroup Markdown
  autocmd!
  autocmd BufRead,BufNewFile *.md setlocal filetype=markdown
  autocmd FileType markdown setlocal wrap linebreak
augroup END

augroup Haskell
  autocmd!
  autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
  let g:haddock_browser="open"
augroup END

augroup Rails
  autocmd!
  autocmd BufRead,BufNewFile *.hamlc setlocal filetype=haml
augroup END


"=============
"=== Keys! ===
"=============

" Split line to the left of the cursor,
" intended as the opposite of J
nnoremap K i<CR><Esc>k$

" Select previous selection
nnoremap gV `[v`]

" Toggle invis chars
nnoremap <leader>l :set list!<CR>

" Toggle recent search highlight
nnoremap <leader>/ :set hlsearch!<CR>

" Quick-open ~/.vimrc
nnoremap <leader>v :tabnew $MYVIMRC<CR>
" Quick-source ~/.vimrc
nnoremap <leader>V :source $MYVIMRC<CR>

" Toggle file browser sidebar
nnoremap <leader>o :NERDTreeToggle<CR>

" Trim trailing whitespace on every line of file
nnoremap <leader>tw :%s/[ \t\r]\+$//e<CR>


"================
"=== Commands ===
"================

" Get wordcount of current file
nnoremap <leader>wc :!wc -w %<CR>


"=======================
"=== EasyMotion keys ===
"=======================

" Using `g` as a leader for a selection of
" easymotion motions, binding all in
" normal (nmap) and operator-pending (omap) modes

" gf{c} -> find a char
nmap gf <Plug>(easymotion-s)
omap gf <Plug>(easymotion-s)
" gs    -> jump to any word or camelCase boundary
nmap gs <Plug>(easymotion-jumptoanywhere)
omap gs <Plug>(easymotion-jumptoanywhere)

" gj, gk -> jump to any beginning of line
nmap gj <Plug>(easymotion-bd-jk)
omap gj <Plug>(easymotion-bd-jk)
nmap gk <Plug>(easymotion-bd-jk)
omap gk <Plug>(easymotion-bd-jk)
" gh, gl -> jump to any word boundary in line
nmap gl <Plug>(easymotion-lineforward)
omap gl <Plug>(easymotion-lineforward)
nmap gh <Plug>(easymotion-linebackward)
omap gh <Plug>(easymotion-linebackward)
" gw     -> jump to any beginning of word
nmap gw <Plug>(easymotion-bd-w)
omap gw <Plug>(easymotion-bd-w)
" ge     -> jump to any end of word
nmap ge <Plug>(easymotion-bd-e)
omap ge <Plug>(easymotion-bd-e)

" Use easymotion search instead of default,
" for incremental highlighting and auto-unhighlighting
nmap / <Plug>(easymotion-sn)
omap / <Plug>(easymotion-sn)
nmap n <Plug>(easymotion-next)
"omap n <Plug>(easymotion-next)
nmap N <Plug>(easymotion-prev)
"omap N <Plug>(easymotion-prev)
" Rebind '*' to search for word under cursor
nmap * <Plug>(easymotion-sn)<C-r><C-w><CR>
"omap * <Plug>(easymotion-sn)<C-r><C-w><CR>

" Remap the default search bindings behind 'g' leader
nnoremap g/ /
nnoremap gn n
nnoremap gN N
nnoremap g* *

highlight! link EasyMotionMoveHL Search
highlight! link EasyMotionIncSearch EasyMotionTarget2FirstDefault


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


"================
"=== NERDTree ===
"================

" Show hidden files by default (togglable with I)
let NERDTreeShowHidden=1


"=================
"=== ShowMarks ===
"=================

let g:showmarks_include="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
" Prevent showmarks from keeping previous options
" if exists('loaded_showmarks')
"   unlet loaded_showmarks
" endif

highlight! link ShowMarksHLl LineNr
highlight! link ShowMarksHLu LineNr
highlight! link ShowMarksHLo LineNr
highlight! link ShowMarksHLm LineNr

let g:showmarks_textlower="\t "
let g:showmarks_textupper="\t "
let g:showmarks_textother="\t "


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
Plugin 'Shougo/vimproc.vim'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-unimpaired'
Plugin 'tpope/vim-surround'
Plugin 'mileszs/ack.vim'
Plugin 'vim-scripts/ShowMarks'

" Colors
Plugin 'altercation/vim-colors-solarized'

" Syntaxes
Plugin 'kchmck/vim-coffee-script'
Plugin 'vim-ruby/vim-ruby'
Plugin 'raichoo/purescript-vim'

" Haskell
"Plugin 'eagletmt/ghcmod-vim'
"Plugin 'lukerandall/haskellmode-vim'
Plugin 'eagletmt/neco-ghc'

call vundle#end()
filetype plugin indent on


"==========
"=== UI ===
"==========

set background=light
set t_Co=256
colorscheme slate


"===================
"=== GUI Options ===
"===================
if has("gui_running")

  set guioptions=gtrLmec
  set guitablabel=%M\ %t

  set background=dark
  colorscheme solarized
  highlight! link SignColumn LineNr

  set cursorline

endif
