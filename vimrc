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
set backupdir=$HOME/.tmp/vim
set directory=$HOME/.tmp/vim



" Text options
" --------------------
filetype indent plugin on         " Smart indenting
set autoindent                    " automatic indents
set shiftwidth=2
set softtabstop=2
set tabstop=2
set expandtab



" UI stuff
" --------------------

" statusline
set statusline=%t                                         "tail of the filename
set statusline+=\ [%{strlen(&fenc)?&fenc:'none'},%{&ff}]  "file encoding, file format
set statusline+=\ %m%r                                    "modified, read only flags
set statusline+=\ %{&fo}                                  "formatoptions
set statusline+=\ %y                                      "filetype
set statusline+=%=                                        "left/right separator
set statusline+=%c,%l/%L                                  "column,line/total lines
set statusline+=\ \ %P                                    "percent through file



" Source vimrc.d stuff
" --------------------
source ~/.vimrc.d/ui              " UI stuff
source ~/.vimrc.d/mappings        " personal mappings


