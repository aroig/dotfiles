" Vim syntax file
" Language:	sage help
" Maintainer:	Abdó Roig-Maranges <abdo.roig@gmail.com>
" Last Change:	2015 May 03

" Include syntax rules for sage
syn include @SAGE          syntax/sage.vim
syn include @SAGECODE      syntax/sagecode.vim
syn include @SAGEDOC       syntax/sagedoc.vim

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
 
syn region sageHelpFile
    \ matchgroup=sageHelpKey
    \ start=/^File:/
    \ end=/$/
    \ oneline



" Content
" ----------------------------------------------

syn region sageHelpSignature
    \ matchgroup=sageHelpKey
    \ start=/^Signature:/
    \ end=/$/
    \ oneline
    \ contains=@SAGECODE

syn region sageHelpInitSignature
    \ matchgroup=sageHelpKey
    \ start=/^Init signature:/
    \ end=/$/
    \ oneline
    \ contains=@SAGECODE

syn region sageHelpSource
    \ matchgroup=sageHelpKey
    \ start=/^Source:/
    \ end=/^File:\|^[^ ]* docstring:/me=s-1
    \ contains=@SAGE

syn region sageHelpDocstring
    \ matchgroup=sageHelpKey
    \ start=/^Docstring:/
    \ end=/^File:\|^[^ ]* docstring:/me=s-1
    \ contains=@SAGEDOC

syn region sageHelpInitDocstring
    \ matchgroup=sageHelpKey
    \ start=/^[^ ]* docstring:/
    \ end=/^File:\|^[^ ]* docstring:/me=s-1
    \ contains=@SAGEDOC


      
" Highlighting
" ----------------------------------------------
hi def link sageHelpKey          Statement
hi def link sageHelpType         String
hi def link sageHelpString       String
hi def link sageHelpFile         String


let b:current_syntax = "sagehelp"
