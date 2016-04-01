" Vim syntax file
" Language:	python help
" Maintainer:	Abdó Roig-Maranges <abdo.roig@gmail.com>
" Last Change:	2014 Dec 21

" Include syntax rules for python
syn include @PYTHON        syntax/python.vim

" always sync from the start
syn sync fromstart

" Header from ipython introspection output
" ----------------------------------------------

syn region ipyHelpType     oneline matchgroup=ipyHelpKey start=/^Type:/             end=/$/    
syn region ipyHelpString     oneline matchgroup=ipyHelpKey start=/^String form:/    end=/$/
syn region ipyHelpInit     oneline matchgroup=ipyHelpKey start=/^Init definition:/  end=/$/
syn region ipyHelpFile      oneline matchgroup=ipyHelpKey start=/^File:/            end=/$/

syn region ipyHelpDefinition oneline matchgroup=ipyHelpKey
    \ start=/^Definition:/    end=/$/
    \ contains=@PYTHON

syn region ipyHelpDocstring matchgroup=ipyHelpKey
    \ start=/^Docstring:/     end=/\%$/

syn region ipyHelpSource matchgroup=ipyHelpKey
    \ start=/^Source:/        end=/\%$/
    \ contains=@DOCSTRING
   


" Highlighting
" ----------------------------------------------
hi def link ipyHelpKey          Statement
hi def link ipyHelpType         Identifier
hi def link ipyHelpString       Identifier
hi def link ipyHelpInit         Identifier
hi def link ipyHelpFile         Identifier


let b:current_syntax = "ipythonhelp"
