au BufRead,BufNewFile *                call s:FThglog()

fun! s:FThglog()
  if getline(1) =~ 'changeset:'
    setf hg
    return
  endif
endfun
