
" key timeouts
" set timeoutlen=500 ttimeoutlen=10
set notimeout ttimeout ttimeoutlen=10

let mapleader=","


" Global Bindings
" --------------------

" movement by 30 lines
noremap J 30j
noremap K 30k

" rebind esc
" inoremap jj <Esc>
inoremap <C-s> <Esc>


" Local Bindings
" --------------------

nnoremap <Leader>c :noh<cr>


" Commands
" --------------------

" vim internals
command SyntaxGroup echo synIDattr(synID(line("."),col("."),1),"name")

