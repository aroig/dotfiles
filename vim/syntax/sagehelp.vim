" Vim syntax file
" Language:	sage help
" Maintainer:	Abdó Roig-Maranges <abdo.roig@gmail.com
" Last Change:	2012 Dec 08

" Include syntax rules for sage and sagedocstring
syn include @SAGE          syntax/sage.vim
syn include @DOCSTRING     syntax/sagedocstr.vim

" always sync from the start
syn sync fromstart

" Header from sage introspection output
" ----------------------------------------------

syn region sageHelpType      oneline matchgroup=sageHelpKey start=/^Type:/        end=/$/    
syn region sageHelpString    oneline matchgroup=sageHelpKey start=/^Base Class:/  end=/$/
syn region sageHelpString    oneline matchgroup=sageHelpKey start=/^String Form:/ end=/$/
syn region sageHelpNamespace oneline matchgroup=sageHelpKey start=/^Namespace:/   end=/$/
syn region sageHelpFile      oneline matchgroup=sageHelpKey start=/^Loaded File:/ end=/$/
syn region sageHelpFile      oneline matchgroup=sageHelpKey start=/^Source File:/ end=/$/

syn region sageHelpDefinition oneline matchgroup=sageHelpKey
    \ start=/^Definition:/
    \ end=/$/
    \ contains=@SAGE

syn region sageHelpSource matchgroup=sageHelpKey
    \ start=/^Source:/
    \ end=/\%$/
    \ contains=@SAGE
    
syn region sageHelpDocstring matchgroup=sageHelpKey
    \ start=/^Docstring:/
    \ end=/\%$/
    \ contains=@DOCSTRING

      
" Highlighting
" ----------------------------------------------
hi def link sageHelpNamespace    Identifier
hi def link sageHelpType         Identifier
hi def link sageHelpKey          Statement
hi def link sageHelpString       String
hi def link sageHelpFile         Identifier


let b:current_syntax = "sagehelp"
