
" General options
" --------------------
set nocompatible                  " vim defaults. forget about vi compatibility!

set hidden                        " Don't force to save files when switching
set confirm                       " confirm instead of fail when saving is needed
set history=700                   " lines of history to remember

filetype plugin on                " enable filetype pluginso
set autoread                      " reload a file when changed externally
set ffs=unix,dos,mac              " file type preference

syntax on                         " Enable syntax highlighting
set hlsearch                      " Highlight searches
set wildmenu                      " Better autocompletion
set showmatch                     " match parenthesis
set nowrapscan                    " do not loop to the top on searches

set ignorecase                    " case insensitive searches
set smartcase

set backspace=indent,eol,start    " enhance backspace
set nostartofline                 " prevent some movements to go to start of line

set nofoldenable                  " disable folding

" Suffixes that get lower priority when doing tab completion for filenames.
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc,.png,.jpg

" exclude fugitive from editorconfig
let g:EditorConfig_exclude_patterns = ['fugitive://.*']
