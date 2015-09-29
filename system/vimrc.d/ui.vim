
" User Interface
" --------------------
colors abdo                       " default color scheme
set background=dark               " dark background
set t_Co=16                       " restrict to 16 colors

set showcmd                       " show partial command
set laststatus=2                  " always display status line

"set ruler                         " show cursor position


" Display
" --------------------
augroup ActiveWindowCursorLine
  autocmd!
  autocmd VimEnter,WinEnter,BufWinEnter * setlocal cursorline
  autocmd WinLeave * setlocal nocursorline
augroup END


" Status Line
" --------------------
function! StatuslineMode(m)
    let curm = toupper(mode())
    if a:m == curm
        return '[' . curm . ']'
    else
        return ''
    endif
endfunction

function! StatuslineFiletype()
    if &filetype != ''
        return &filetype . ' '
    else
        return ''
    endif
endfunction

function! StatuslineFormatoptions()
    if &formatoptions != ''
        return &formatoptions . ' '
    else
        return ''
    endif
endfunction

set statusline=

set statusline+=%#StatusLineInsert#%{StatuslineMode('I')}
set statusline+=%#StatusLineNormal#%{StatuslineMode('N')}
set statusline+=%#StatusLineReplace#%{StatuslineMode('R')}
set statusline+=%#StatusLineVisual#%{StatuslineMode('V')}
set statusline+=%*

"tail of the filename with flags 
set statusline+=\ %{StatuslineFiletype()}%{StatuslineFormatoptions()}\|
set statusline+=\ %t\ 
set statusline+=%1*%M%R%*

"left/right separator
set statusline+=%=

"git status
set statusline+=%2*%{fugitive#head()}%*\ 

set statusline+=\|\ %l:%-2c\ %P\                             "line:column percent
set statusline+=\|\ %{strlen(&fenc)?&fenc:'none'}\ %{&ff}\   "file encoding, file format
