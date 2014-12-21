" Vim syntax file
" Language:	sage help
" Maintainer:	Abdó Roig-Maranges <abdo.roig@gmail.com>
" Last Change:	2014 Dec 21

" Include syntax rules for sage
syn include @SAGE          syntax/sage.vim

" always sync from the start
syn sync fromstart


" Header from sage introspection output
" ----------------------------------------------

syn region sageHelpType
    \ matchgroup=sageHelpKey
    \ start=/^Type:/
    \ end=/$/    
    \ oneline


syn region sageHelpString
    \ matchgroup=sageHelpKey
    \ start=/^String form:/
    \ end=/$/
    \ oneline

syn region sageHelpInit
    \ matchgroup=sageHelpKey
    \ start=/^Init definition:/
    \ end=/$/
    \ oneline

    
syn region sageHelpFile
    \ matchgroup=sageHelpKey
    \ start=/^File:/
    \ end=/$/
    \ oneline



" Content
" ----------------------------------------------

syn region sageHelpDefinition
    \ matchgroup=sageHelpKey
    \ start=/^Definition:/
    \ end=/$/
    \ oneline
    \ contains=@SAGE

" TODO: Force sageDocstring into this thing
syn region sageHelpDocstring
    \ matchgroup=sageHelpKey
    \ start=/^Docstring:/
    \ end=/\%$/

syn region sageHelpSource
    \ matchgroup=sageHelpKey
    \ start=/^Source:/
    \ end=/\%$/
    \ contains=@SAGE


      
" Highlighting
" ----------------------------------------------
hi def link sageHelpKey          Statement
hi def link sageHelpType         Identifier
hi def link sageHelpString       Identifier
hi def link sageHelpInit         Identifier
hi def link sageHelpFile         Identifier


let b:current_syntax = "sagehelp"
