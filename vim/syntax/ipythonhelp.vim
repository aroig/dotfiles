" Vim syntax file
" Language:	python help
" Maintainer:	Abdó Roig-Maranges <abdo.roig@gmail.com>
" Last Change:	2012 Dec 30

" Include syntax rules for sage and sagedocstring
syn include @PYTHON        syntax/python.vim

" always sync from the start
syn sync fromstart

" Header from ipython introspection output
" ----------------------------------------------

syn region ipyHelpType      oneline matchgroup=ipyHelpKey start=/^Type:/        end=/$/    
syn region ipyHelpString    oneline matchgroup=ipyHelpKey start=/^Base Class:/  end=/$/
syn region ipyHelpString    oneline matchgroup=ipyHelpKey start=/^String Form:/ end=/$/
syn region ipyHelpNamespace oneline matchgroup=ipyHelpKey start=/^Namespace:/   end=/$/
syn region ipyHelpFile      oneline matchgroup=ipyHelpKey start=/^File:/        end=/$/

syn region ipyHelpDefinition oneline matchgroup=ipyHelpKey
    \ start=/^Definition:/
    \ end=/$/
    \ contains=@PYTHON

syn region ipyHelpSource matchgroup=ipyHelpKey
    \ start=/^Source:/
    \ end=/\%$/
    \ contains=@PYTHON
   
      
" Highlighting
" ----------------------------------------------
hi def link ipyHelpNamespace    Identifier
hi def link ipyHelpType         Identifier
hi def link ipyHelpKey          Statement
hi def link ipyHelpString       String
hi def link ipyHelpFile         Identifier


let b:current_syntax = "ipythonhelp"
