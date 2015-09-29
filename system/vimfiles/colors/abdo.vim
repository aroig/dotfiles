" A simple 16 color theme for vim
" Maintainer:   Abdó Roig-Maranges <abdo.roig@gmail.com>
" License:      GNU GPL <http://www.gnu.org/licenses/gpl.html>

set background=dark
hi! clear

if exists("syntax_on")
  syntax reset
endif

let g:colors_name = "abdo"


" General vim
" ----------------------------------------------------------
hi ColorColumn                            ctermbg=Black        cterm=none
hi Cursor            ctermfg=White                             cterm=bold
hi CursorLine                             ctermbg=DarkGray     cterm=none
hi CursorColumn                           ctermbg=DarkGray     cterm=none
hi CursorLineNr      ctermfg=Yellow       ctermbg=Black        cterm=none
hi Debug             ctermfg=Gray                              cterm=none
hi Directory         ctermfg=Blue                              cterm=none
hi Error             ctermfg=Red          ctermbg=Black        cterm=bold
hi ErrorMsg          ctermfg=Red          ctermbg=Black        cterm=bold
hi Exception         ctermfg=Yellow                            cterm=bold
hi FoldColumn        ctermfg=Green                             cterm=bold
hi Folded            ctermfg=Green                             cterm=bold
hi Ignore            ctermfg=Black                             cterm=none
hi IncSearch         ctermfg=Yellow                            cterm=none
hi LineNr            ctermfg=DarkGray     ctermbg=none         cterm=none
hi MatchParen        ctermfg=White        ctermbg=none         cterm=underline
hi MoreMsg           ctermfg=Yellow                            cterm=none
hi NonText           ctermfg=Gray                              cterm=none
hi Normal            ctermfg=none                              cterm=none
hi Pmenu             ctermfg=White        ctermbg=Black        cterm=none
hi PmenuThumb        ctermfg=Gray         ctermbg=Black        cterm=none
hi PmenuSBar         ctermfg=Gray         ctermbg=Black        cterm=none
hi PmenuSel          ctermfg=Black        ctermbg=Blue         cterm=none
hi Search            ctermfg=Black        ctermbg=Yellow       cterm=none
hi TabLine           ctermfg=Gray         ctermbg=Black        cterm=none
hi TabLineSel        ctermfg=Gray         ctermbg=Black        cterm=none
hi Title             ctermfg=Cyan                              cterm=bold
hi Underlined                                                  cterm=underline
hi VertSplit                              ctermbg=Black        cterm=none
hi Visual            ctermfg=Black        ctermbg=Blue         cterm=none


hi! link SignColumn    LineNr
hi! link WildMenu      Visual
hi! link FoldColumn    SignColumn
hi! link WarningMsg    ErrorMsg
hi! link Question      MoreMsg
hi! link ModeMsg       MoreMsg
hi! link SpecialKey    NonText

hi SpellBad          ctermfg=Red                               cterm=bold
hi SpellCap          ctermfg=Red                               cterm=bold
hi SpellLocal        ctermfg=Red                               cterm=bold
hi SpellRare         ctermfg=Red                               cterm=bold


" Standard syntax
" ----------------------------------------------------------
hi Boolean           ctermfg=Yellow                            cterm=bold
hi Character         ctermfg=Red                               cterm=none
hi Comment           ctermfg=Green                             cterm=none
hi Conditional       ctermfg=Yellow                            cterm=bold
hi Constant          ctermfg=Gray                              cterm=none
hi Define            ctermfg=Yellow                            cterm=bold
hi Delimiter         ctermfg=Gray                              cterm=none
hi Float             ctermfg=Gray                              cterm=none
hi Function          ctermfg=Cyan                              cterm=none
hi Identifier        ctermfg=Cyan                              cterm=none
hi Include           ctermfg=Cyan                              cterm=none
hi Keyword           ctermfg=Cyan                              cterm=none
hi Label             ctermfg=Green                             cterm=none
hi Number            ctermfg=Gray                              cterm=none
hi Operator          ctermfg=Gray                              cterm=none
hi PreProc           ctermfg=Blue                              cterm=none
hi Repeat            ctermfg=Yellow                            cterm=bold
hi Special           ctermfg=Red                               cterm=none
hi SpecialChar       ctermfg=Red                               cterm=none
hi Statement         ctermfg=Yellow                            cterm=bold
hi StorageClass      ctermfg=Yellow                            cterm=bold
hi String            ctermfg=Red                               cterm=none
hi Structure         ctermfg=Yellow                            cterm=bold
hi Tag               ctermfg=Gray                              cterm=none
hi Todo              ctermfg=Yellow                            cterm=bold
hi Type              ctermfg=Cyan                              cterm=none
hi Typedef           ctermfg=Yellow                            cterm=bold


" Web stuff
" ----------------------------------------------------------

" html
hi htmlTagName              ctermfg=DarkGreen                  cterm=none
hi htmlTag                  ctermfg=DarkGreen                  cterm=none
hi htmlArg                  ctermfg=Green                      cterm=none
hi htmlBold                                                    cterm=bold
hi htmlItalic                                                  cterm=underline
hi htmlUnderline                                               cterm=underline
hi htmlBoldItalic                                              cterm=bold,underline
hi htmlBoldUnderline                                           cterm=bold,underline
hi htmlUnderlineItalic                                         cterm=underline
hi htmlBoldUnderlineItalic                                     cterm=bold,underline
hi htmlLink                 ctermfg=Yellow                     cterm=underline
hi! link htmlH1       Title
hi! link htmlEndTag   htmlTag

" xml
hi xmlTagName               ctermfg=DarkBlue
hi xmlTag                   ctermfg=Blue
hi! link xmlString  xmlTagName
hi! link xmlAttrib  xmlTag
hi! link xmlEndTag  xmlTag
hi! link xmlEqual   xmlTag

" javascript
hi! link javaScript        Normal
hi! link javaScriptBraces  Delimiter

"help
hi! link helpExample         String
hi! link helpHeadline        Title
hi! link helpSectionDelim    Comment
hi! link helpHyperTextEntry  Statement
hi! link helpHyperTextJump   Underlined
hi! link helpURL             Underlined


" Programming Languages
" ----------------------------------------------------------

" ruby
hi! link rubyDefine                 Statement
hi! link rubyLocalVariableOrMethod  Identifier
hi! link rubyConstant               Constant
hi! link rubyInstanceVariable       Number
hi! link rubyStringDelimiter        rubyString

"vim
hi! link vimSetSep    Delimiter
hi! link vimContinue  Delimiter
hi! link vimHiAttrib  Constant


" Text Documents
" ----------------------------------------------------------

" markdown
hi! link markdownHeadingRule        NonText
hi! link markdownHeadingDelimiter   markdownHeadingRule
hi! link markdownLinkDelimiter      Delimiter
hi! link markdownURLDelimiter       Delimiter
hi! link markdownCodeDelimiter      NonText
hi! link markdownLinkTextDelimiter  markdownLinkDelimiter
hi! link markdownUrl                markdownLinkText
hi! link markdownAutomaticLink      markdownLinkText
hi! link markdownCodeBlock          String
hi! link markdownCode               String
hi markdownBold                     cterm=bold
hi markdownItalic                   cterm=underline

" tex
hi texBoldStyle      ctermfg=Green                             cterm=bold
hi texSection        ctermfg=Yellow                            cterm=bold
hi texBeginEndName   ctermfg=Cyan                              cterm=none
hi texMath           ctermfg=DarkYellow                        cterm=none
hi texMathDelim      ctermfg=DarkYellow                        cterm=none
hi texAccent         ctermfg=Gray                              cterm=none
hi texCmdName        ctermfg=Yellow                            cterm=bold
hi texCmdArgs        ctermfg=Cyan                              cterm=none
hi texDef            ctermfg=Yellow                            cterm=bold
hi texDefParm        ctermfg=Gray                              cterm=none


" Tools
" ----------------------------------------------------------

" sagemath
hi sagePrompt        ctermfg=DarkYellow                        cterm=none

" git
hi gitCommitBranch        ctermfg=Blue
hi gitCommitOverflow      ctermfg=Red                               cterm=none
hi gitCommitSelectedType  ctermfg=Green
hi gitCommitSelectedFile  ctermfg=DarkGreen
hi gitCommitSummary       ctermfg=Cyan                              cterm=none
hi gitCommitUnmergedType  ctermfg=Red
hi gitCommitUnmergedFile  ctermfg=DarkRed
hi! link gitCommitFile           Directory
hi! link gitCommitUntrackedFile  gitCommitUnmergedFile
hi! link gitCommitDiscardedType  gitCommitUnmergedType
hi! link gitCommitDiscardedFile  gitCommitUnmergedFile

hi GitGutterAdd      ctermfg=DarkGreen                         cterm=none
hi GitGutterChange   ctermfg=DarkYellow                        cterm=none
hi GitGutterDelete   ctermfg=DarkRed                           cterm=none
hi GitGutterChangeDelete ctermfg=DarkRed                       cterm=none

" diff
hi DiffAdd           ctermfg=DarkGreen    ctermbg=Black        cterm=none
hi DiffChange        ctermfg=DarkYellow   ctermbg=Black        cterm=none
hi DiffDelete        ctermfg=DarkRed      ctermbg=Black        cterm=none
hi DiffText          ctermfg=Black        ctermbg=Yellow       cterm=none
hi DiffAdded         ctermfg=DarkGreen    ctermbg=Black        cterm=none
hi DiffChanged       ctermfg=DarkYellow   ctermbg=Black        cterm=none
hi DiffRemoved       ctermfg=DarkRed      ctermbg=Black        cterm=none 
hi DiffFile                                                    cterm=none
hi DiffLine                                                    cterm=none

" NERDTree
hi! link NERDTreeHelp      Comment
hi! link NERDTreeExecFile  String


" Statusline
" ---------------------------------------------------------

" vim
hi StatusLine        ctermfg=Black        ctermbg=Gray         cterm=none
hi StatusLineNC      ctermfg=White        ctermbg=DarkGray     cterm=none
hi TabLineFill       ctermfg=White        ctermbg=DarkGray     cterm=none

" custom
hi StatusLineNormal  ctermfg=Black        ctermbg=Green        cterm=none
hi StatusLineInsert  ctermfg=Black        ctermbg=Yellow       cterm=none
hi StatusLineReplace ctermfg=Black        ctermbg=Red          cterm=none
hi StatusLineVisual  ctermfg=Black        ctermbg=Blue         cterm=none

" statusline user
hi User1             ctermfg=Black        ctermbg=Gray         cterm=bold   " modified flag
hi User2             ctermfg=Black        ctermbg=Gray         cterm=bold   " git branch


" Other Stuff
" ----------------------------------------------------------

"hi SpecialComment  guifg=#82a282 gui=bold
"hi VisualNOS       guifg=#333333 guibg=#f18c96 gui=bold,underline







