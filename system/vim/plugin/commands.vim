

" get syntax group under the cursor. nvim already provides this.
if !has('nvim')
    command SyntaxGroup echo synIDattr(synID(line("."),col("."),1),"name")
endif
