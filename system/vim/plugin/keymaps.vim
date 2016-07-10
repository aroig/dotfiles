
" key timeouts
" set timeoutlen=500 ttimeoutlen=10
set notimeout ttimeout ttimeoutlen=10

" Leader key
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

" do not define mappings, I'll do that myself, because some of them conflict with unimpaired.
let g:conflict_marker_enable_mappings=0

" conflict marker bindings
nmap <buffer>]h <Plug>(conflict-marker-next-hunk)
nmap <buffer>[h <Plug>(conflict-marker-prev-hunk)
nmap <buffer>c> <Plug>(conflict-marker-themselves)
nmap <buffer>c< <Plug>(conflict-marker-ourselves)
nmap <buffer>cn <Plug>(conflict-marker-none)
nmap <buffer>cb <Plug>(conflict-marker-both)


" Commands
" --------------------

" get syntax group under the cursor
command SyntaxGroup echo synIDattr(synID(line("."),col("."),1),"name")

