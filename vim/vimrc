"---------------------------------------------------------------------"
" File:   .vimrc                                                      "
" Author: Abdó Roig-Maranges <abdo.roig@gmail.com                     "
" Desc:   vim configuration                                           "
"---------------------------------------------------------------------"


" General options
" --------------------
set nocompatible                  " vim defaults. forget about vi compatibility!
set hidden                        " Don't force to save files when switching
set confirm                       " confirm instead of fail when saving is needed
set history=700                   " lines of history to remember

filetype plugin on                " enable filetype pluginso
set autoread                      " reload a file when changed externally
set ffs=unix,dos,mac              " file type preference


" sets alternative backup and swap file locations
set nobackup
set backupdir=~/.vim/bak
set directory=~/.vim/bak

" and prevents backup and swap for sensitive files
autocmd BufRead,BufNewFile pass.* set nobackup
autocmd BufRead,BufNewFile pass.* set noswapfile

autocmd BufRead,BufNewFile ~/priv/* set nobackup
autocmd BufRead,BufNewFile ~/priv/* set noswapfile


" Text options
" --------------------
filetype indent plugin on         " Smart indenting
set autoindent                    " automatic indents
set shiftwidth=2
set softtabstop=2
set tabstop=2
set expandtab



" Source vimrc.d stuff
" --------------------
source ~/.vim/vimrc.d/ui              " UI stuff
source ~/.vim/vimrc.d/mappings        " personal mappings



" UI stuff
" --------------------

" Use system clipboard. needs x-clipboard compiled in.
set clipboard=unnamedplus


" Enable mouse to scroll, select, etc
set mouse=a
set ttymouse=xterm2



" statusline
set statusline=\ %{toupper(mode())}\ \|                    "mode
set statusline+=\ %t\ \|                                   "tail of the filename
set statusline+=\ %{&fo}                                   "formatoptions
set statusline+=\ %y                                       "filetype
set statusline+=%=                                         "left/right separator
set statusline+=%l:%-2c\ %P\ \|                            "line:column percent
set statusline+=\ %m%r                                     "modified, read only flags
set statusline+=\ %{strlen(&fenc)?&fenc:'none'}\ %{&ff}    "file encoding, file format
set statusline+=\                 

