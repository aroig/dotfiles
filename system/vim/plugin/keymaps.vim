
" key timeouts
" set timeoutlen=500 ttimeoutlen=10
set notimeout ttimeout ttimeoutlen=10

let mapleader=","


" Global Bindings
" --------------------

" I sometimes mistype :Q
noremap Q q

" movement by 30 lines
noremap J 30j
noremap K 30k

" rebind esc
" inoremap jj <Esc>
inoremap <C-s> <Esc>


" Local Bindings
" --------------------

" clear hlsearch highlighting
nnoremap <Leader>c :noh<cr>

" diff mode
if &diff
    syntax off
    nnoremap <leader>N [c
    nnoremap <leader>n ]c
endif

" Gdiff mode
nnoremap <leader>dh :diffget //2 \| :diffupdate<cr>
nnoremap <leader>dl :diffget //3 \| :diffupdate<cr>


" Commands
" --------------------

" get syntax group under the cursor
command SyntaxGroup echo synIDattr(synID(line("."),col("."),1),"name")

