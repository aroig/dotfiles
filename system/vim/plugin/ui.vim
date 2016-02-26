
" User Interface
" --------------------
colors abdo                       " default color scheme
set background=dark               " dark background

" restrict to 16 colors on gui terminals
if ! &term == "linux"
    set t_Co=16
endif

set showcmd                       " show partial command
set laststatus=2                  " always display status line

set ruler                         " show cursor position

" enable cursor line on active window
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
    let ret = ''
    if &filetype != ''
        let ret = &filetype

        if &formatoptions != ''
           let ret = ret . ': '
        else
           let ret = ret . ' '
        endif
    endif

    if &formatoptions != ''
        let ret = ret . &formatoptions . ' '
    endif

    return ret
endfunction

set statusline=

set statusline+=%#StatusLineInsert#%{StatuslineMode('I')}
set statusline+=%#StatusLineNormal#%{StatuslineMode('N')}
set statusline+=%#StatusLineReplace#%{StatuslineMode('R')}
set statusline+=%#StatusLineVisual#%{StatuslineMode('V')}
set statusline+=%*

"tail of the filename with flags
if ! &diff
    set statusline+=\ %{StatuslineFiletype()}\|
endif

if exists("g:vimpager")
    set statusline+=\ %n:\ vimpager
else
    set statusline+=\ %n:\ %f
    set statusline+=\ %1*%M%R%*
endif

"left/right separator
set statusline+=%=

"git status
if ! &diff
    set statusline+=%2*%{exists('g:loaded_fugitive')?fugitive#head():''}%*\ 
endif

"line:column percent
set statusline+=\|\ %l:%-2c\ %P\                             

"file encoding, file format
if ! &diff
    set statusline+=\|\ %{strlen(&fenc)?&fenc:'none'}\ %{&ff}\ 
endif
