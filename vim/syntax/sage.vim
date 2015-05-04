" Vim syntax file
" Language:	Sage
" Maintainer:	Abdó Roig-Maranges <abdo.roig@gmail.com
" Last Change:	2015 May 03

" sage code syntax as a base
:runtime! syntax/sagecode.vim
:unlet b:current_syntax

" Include syntax rules for sage
syn include @SAGEDOC       syntax/sagedoc.vim



" Docstrings
" ----------------------------------------------

syn region sageDocstring
    \ matchgroup=sagedocContent
    \ start=+^\s*[uU]\=[rR]\=\z('''\|"""\)+
    \ end="\z1" keepend
    \ contains=@SAGEDOC



" Highlighting
" ----------------------------------------------


let b:current_syntax = "sage"
