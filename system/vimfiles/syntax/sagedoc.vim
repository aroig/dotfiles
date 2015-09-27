" Vim syntax file
" Language:	Sage
" Maintainer:	Abdó Roig-Maranges <abdo.roig@gmail.com
" Last Change:	2015 May 03

" Include syntax rules for sage
syn include @SAGECODE      syntax/sagecode.vim


" Docstrings
" ----------------------------------------------

" NOTE: Assumes that a codeblock ends by a blank line or a totally unindented line.

syn region sagedocContent
    \ start=/^/
    \ end=/^\s*$\|^[^ ]/me=s-1
    \ contains=sagedocCodeblock,sagedocTitle,sagedocLiteral,sagedocInput

syntax region sagedocTitle
    \ start=/^\s\+[A-Z]\+[A-Z ]*:\s*/
    \ end=/$/
    \ oneline
    \ contained

syntax region sagedocLiteral
    \ start=/[`"']\+/
    \ end=/[`"']\+/
    \ oneline
    \ contained

syntax region sagedocCodeblock
    \ start=/^\s*sage:/
    \ end=/^\s*$\|^[^ ]/me=s-1
    \ contains=sagedocInput
    \ contained

syntax region sagedocInput
    \ matchgroup=sagedocPrompt
    \ start=/^\s*sage:/
    \ end=/$/
    \ keepend
    \ oneline
    \ contains=@SAGECODE
    \ contained




" Highlighting
" ----------------------------------------------

hi def link sagedocPrompt   Identifier
hi def link sagedocContent  Comment

hi link sagedocTitle        Statement
hi link sagedocLiteral      String

hi link sagedocCodeblock    String

let b:current_syntax = "sagedoc"
