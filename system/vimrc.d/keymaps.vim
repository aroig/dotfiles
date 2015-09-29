
" Bindings
" --------------------

" movement by 30 lines
map J 30j
map K 30k


" Local Bindings
" --------------------

let mapleader=","

nnoremap <Leader>c :noh<cr>


" Commands
" --------------------

" vim internals
command SyntaxGroup echo synIDattr(synID(line("."),col("."),1),"name")

