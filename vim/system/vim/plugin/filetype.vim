
" Text options
" --------------------
filetype indent plugin on         " Smart indenting
set autoindent                    " automatic indents
set shiftwidth=4
set softtabstop=4
set tabstop=4
set expandtab


" Latex Options
" --------------------
let g:tex_flavor = "latex"


" Local config
" --------------------

" use tabs for git config files
autocmd FileType gitconfig setlocal noexpandtab

" PR and Issue message for git-spindle. we do not want to wrap.
autocmd BufNewFile PULL_REQUEST_EDIT_MSG setlocal tw=99999
autocmd BufNewFile ISSUE_EDIT_MSG setlocal tw=99999
