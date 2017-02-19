
" always split Gdiff vertically
set diffopt+=vertical

" syntax highlight vim conflict markers
let g:conflict_marker_enable_highlight=1

" disable syntax in diff mode
if &diff
    syntax off
endif
