"---------------------------------------------------------------------"
" File:   .vimrc                                                      "
" Author: Abdó Roig-Maranges <abdo.roig@gmail.com                     "
" Desc:   vim configuration                                           "
"---------------------------------------------------------------------"

" File backup
" --------------------

" sets alternative backup and swap file locations
set nobackup
set backupdir=~/var/vim/bak
set directory=~/var/vim/bak

" and prevents backup and swap for sensitive files
autocmd BufRead,BufNewFile pass.* set nobackup
autocmd BufRead,BufNewFile pass.* set noswapfile

autocmd BufRead,BufNewFile ~/priv/* set nobackup
autocmd BufRead,BufNewFile ~/priv/* set noswapfile


" UI stuff
" --------------------

" Enable mouse to scroll, select, etc
set mouse=a
set ttymouse=xterm2
