
" Bindings
" --------------------

let mapleader=","

" movement
map J 30j                         " movement by blocks of 30 lines
map K 30k

" diff mode
if &diff
    nmap N [c
    nmap n ]c
    nmap d< :diffget LOCAL<CR>
    nmap d> :diffget REMOTE<CR>
    nmap d^ :diffget BASE<CR>
endif

" clear search highlights with esc
nnoremap <Leader>c :noh<CR>


" Commands
" --------------------

" vim internals
command SyntaxGroup echo synIDattr(synID(line("."),col("."),1),"name")

