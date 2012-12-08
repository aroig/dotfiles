" Vim syntax file
" Language:	sage docstring
" Maintainer:	Abdó Roig-Maranges <abdo.roig@gmail.com
" Last Change:	2012 Dec 08

" ReST syntax as a base
:runtime! syntax/rst.vim
:unlet b:current_syntax


syn region  rstLiteralBlock         matchgroup=rstDelimiter
      \ start='::\_s*\n\ze\z(\s\+\)' skip='^$' end='^\z1\@!'
      \ contains=@NoSpell,sageCommand

      
syn region sageCommand contained matchgroup=sagePrompt
      \ containedin=rstLiteralBlock
      \ start='sage:\s'
      \ end='$' keepend
      \ contains=ALL     

syn region sageCommand contained matchgroup=sagePrompt
      \ containedin=rstLiteralBlock
      \ start='\.\.\.\s'
      \ end='$' keepend
      \ contains=ALL     

      
" Highlighting
" ----------------------------------------------
hi def link sageDocstring           Comment
hi def link sagePrompt              Statement

let b:current_syntax = "sagedocstr"
