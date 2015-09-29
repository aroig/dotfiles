
" Bindings
" --------------------

" movement by 30 lines
map J 30j
map K 30k

" diff mode
if &diff
    nmap N [c
    nmap n ]c
    nmap d< :diffget LOCAL<cr>
    nmap d> :diffget REMOTE<cr>
    nmap d^ :diffget BASE<cr>
endif


" Local Bindings
" --------------------

let mapleader=","

nnoremap <Leader>c :noh<cr>


" Commands
" --------------------

" vim internals
command SyntaxGroup echo synIDattr(synID(line("."),col("."),1),"name")

