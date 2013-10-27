" Vim syntax file
" Language:	Sage
" Maintainer:	Abdó Roig-Maranges <abdo.roig@gmail.com
" Last Change:	2012 Dec 08

" Python syntax as a base
:runtime! syntax/python.vim
:unlet b:current_syntax

" Include syntax rules for sage docstring
syn include @DOCSTRING syntax/sagedocstr.vim


" reST docstrings
" ----------------------------------------------

syn region sageDocstring
      \ start=+^\s*[uU]\=[rR]\z('''\|"""\)+
      \ end="\z1" keepend
      \ contains=@DOCSTRING
      

" Highlighting
" ----------------------------------------------
hi def link sageDocHeadKey          Statement


let b:current_syntax = "sage"
