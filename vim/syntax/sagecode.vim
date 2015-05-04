" Vim syntax file
" Language:	Sage
" Maintainer:	Abdó Roig-Maranges <abdo.roig@gmail.com
" Last Change:	2015 May 03

" Python syntax as a base
:runtime! syntax/python.vim
:unlet b:current_syntax

" clear the pythonDoctest and pythonDoctestValue syntax groups
syntax clear pythonDoctest
syntax clear pythonDoctestValue



" Sage
" ----------------------------------------------

" TODO: cython keywords, etc.



" Highlighting
" ----------------------------------------------


let b:current_syntax = "sagecode"
