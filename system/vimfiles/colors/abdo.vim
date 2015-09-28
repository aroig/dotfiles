" A simple 16 color theme for vim
" Maintainer:   Abdó Roig-Maranges <abdo.roig@gmail.com>
" License:      GNU GPL <http://www.gnu.org/licenses/gpl.html>

set background=dark
highlight clear
if exists("syntax_on")
  syntax reset
endif
let g:colors_name = "abdo"


" General vim
" ----------------------------------------------------------
hi Bold                                                        cterm=bold
hi Cursor            ctermfg=White                             cterm=bold
hi Debug             ctermfg=Gray                              cterm=none
hi Directory         ctermfg=Blue                              cterm=none
hi Error             ctermfg=Red          ctermbg=Black        cterm=bold
hi ErrorMsg          ctermfg=Red          ctermbg=Black        cterm=bold
hi Exception         ctermfg=Yellow                            cterm=bold
hi FoldColumn        ctermfg=Green                             cterm=bold
hi Folded            ctermfg=Green                             cterm=bold
hi Ignore            ctermfg=Black                             cterm=none
hi IncSearch         ctermfg=Yellow                            cterm=none
hi ModeMsg           ctermfg=Yellow                            cterm=none
hi MoreMsg           ctermfg=Yellow                            cterm=none
hi Normal            ctermfg=none                              cterm=none
hi Question          ctermfg=Yellow                            cterm=none
hi Search            ctermfg=Black        ctermbg=Yellow       cterm=none
hi Title             ctermfg=Cyan                              cterm=bold
hi Underlined                                                  cterm=underline
hi VertSplit         ctermbg=Black                             cterm=none
hi Visual            ctermbg=Gray                              cterm=none


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
hi Todo              ctermfg=DarkRed      ctermbg=Yellow       cterm=none
hi Type              ctermfg=Cyan                              cterm=none
hi Typedef           ctermfg=Yellow                            cterm=bold



" Sagemath
" ----------------------------------------------------------
hi sagePrompt        ctermfg=DarkYellow                        cterm=none


" text
" ----------------------------------------------------------

hi SpellBad          ctermfg=Red                               cterm=bold
hi SpellCap          ctermfg=Red                               cterm=bold
hi SpellLocal        ctermfg=Red                               cterm=bold
hi SpellRare         ctermfg=Red                               cterm=bold


" tex
" ----------------------------------------------------------
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


" diff
" ----------------------------------------------------------
hi DiffAdd           ctermfg=DarkGreen    ctermbg=Black        cterm=none
hi DiffChange        ctermfg=DarkYellow   ctermbg=Black        cterm=none
hi DiffDelete        ctermfg=DarkRed      ctermbg=Black        cterm=none
hi DiffText          ctermfg=DarkYellow   ctermbg=Black        cterm=none
hi DiffAdded         ctermfg=DarkGreen    ctermbg=Black        cterm=none
hi DiffChanged       ctermfg=DarkYellow   ctermbg=Black        cterm=none
hi DiffRemoved       ctermfg=DarkRed      ctermbg=Black        cterm=none 
hi DiffFile                                                    cterm=none
hi DiffLine                                                    cterm=none


" git
" ---------------------------------------------------------
hi gitCommitOverflow ctermfg=Red                               cterm=none
hi gitCommitSummary  ctermfg=Cyan                              cterm=none

hi GitGutterAdd      ctermfg=DarkGreen                         cterm=none
hi GitGutterChange   ctermfg=DarkYellow                        cterm=none
hi GitGutterDelete   ctermfg=DarkRed                           cterm=none
hi GitGutterChangeDelete ctermfg=DarkRed                       cterm=none


" Statusline
" ---------------------------------------------------------

" vim
hi StatusLine        ctermfg=Black        ctermbg=Gray         cterm=none
hi StatusLineNC      ctermfg=Black        ctermbg=DarkGray     cterm=none

" custom
hi StatusLineNormal  ctermfg=Black        ctermbg=Green        cterm=none
hi StatusLineInsert  ctermfg=Black        ctermbg=Yellow       cterm=none
hi StatusLineReplace ctermfg=Black        ctermbg=Red          cterm=none
hi StatusLineVisual  ctermfg=Black        ctermbg=Blue         cterm=none

hi StatusLineGit     ctermfg=DarkCyan                          cterm=none
hi StatusLineBuffer  ctermfg=White        ctermbg=Black        cterm=none

hi AirlineInactive   ctermfg=Gray         ctermbg=Black        cterm=none
hi AirlineWarning    ctermfg=DarkYellow   ctermbg=Black        cterm=none
hi AirlineModified   ctermfg=Red          ctermbg=Black        cterm=none
hi AirlineReadonly   ctermfg=DarkRed      ctermbg=Black        cterm=none


" Other Stuff
" ----------------------------------------------------------

"hi SpecialChar     guifg=#dca3a3 gui=bold
"hi SpecialComment  guifg=#82a282 gui=bold
"hi SpecialKey      guifg=#9ece9e
"hi VertSplit       guifg=#2e3330 guibg=#688060
"hi VisualNOS       guifg=#333333 guibg=#f18c96 gui=bold,underline
"hi WarningMsg      cterm=yellow
"hi WildMenu        guibg=#2c302d guifg=#cbecd0 gui=underline





