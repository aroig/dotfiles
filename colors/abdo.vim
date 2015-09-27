" A simple 16 color theme for vim
" Maintainer:   Abdó Roig-Maranges <abdo.roig@gmail.com>
" License:      GNU GPL <http://www.gnu.org/licenses/gpl.html>

set background=dark
highlight clear
if exists("syntax_on")
  syntax reset
endif
let g:colors_name = "abdo"


" Vim syntax
" ----------------------------------------------------------
hi Bold                                 cterm=bold
hi Debug           ctermfg=Gray         cterm=none
hi Directory       ctermfg=Blue         cterm=none
hi ErrorMsg        ctermfg=Red          cterm=bold
hi Exception       ctermfg=Yellow       cterm=bold
hi Error           ctermfg=Red          cterm=bold
hi Normal          ctermfg=none         cterm=none
hi Underlined                           cterm=underline
hi Ignore          ctermfg=Black 
hi Title           ctermfg=Cyan         cterm=bold
hi Cursor          ctermfg=White        cterm=bold
hi StatusLine      ctermfg=Gray         cterm=reverse


" Standard syntax
" ----------------------------------------------------------
hi Boolean         ctermfg=Yellow       cterm=bold
hi Character       ctermfg=Red          cterm=none
hi Comment         ctermfg=Green        cterm=none
hi Conditional     ctermfg=Yellow       cterm=bold
hi Constant        ctermfg=Gray         cterm=none
hi Define          ctermfg=Yellow       cterm=bold
hi Delimiter       ctermfg=Gray         cterm=none
hi Float           ctermfg=Gray         cterm=none
hi Function        ctermfg=Cyan         cterm=none
hi Identifier      ctermfg=Cyan         cterm=none
hi Include         ctermfg=Cyan         cterm=none
hi Keyword         ctermfg=Cyan         cterm=none
hi Label           ctermfg=Green        cterm=none
hi Number          ctermfg=Gray         cterm=none
hi Operator        ctermfg=Gray         cterm=none
hi PreProc         ctermfg=Blue         cterm=none
hi Repeat          ctermfg=Yellow       cterm=bold
hi Special         ctermfg=Red          cterm=none
hi SpecialChar     ctermfg=Red          cterm=none
hi Statement       ctermfg=Yellow       cterm=bold
hi StorageClass    ctermfg=Yellow       cterm=bold
hi String          ctermfg=Red          cterm=none
hi Structure       ctermfg=Yellow       cterm=bold
hi Tag             ctermfg=Gray         cterm=none
hi Todo            ctermfg=Red          cterm=bold          ctermbg=Black
hi Type            ctermfg=Cyan         cterm=none
hi Typedef         ctermfg=Yellow       cterm=bold



" sage
" ----------------------------------------------------------
hi sagePrompt      ctermfg=DarkYellow   cterm=none


" text
" ----------------------------------------------------------

hi SpellBad        ctermfg=Red          cterm=bold
hi SpellCap        ctermfg=Red          cterm=bold
hi SpellRare       ctermfg=Red          cterm=bold
hi SpellLocal      ctermfg=Red          cterm=bold


" tex
" ----------------------------------------------------------
hi texBoldStyle    ctermfg=Green        cterm=bold

hi texSection      ctermfg=Yellow       cterm=bold
hi texBeginEndName ctermfg=Cyan         cterm=none

hi texMath         ctermfg=DarkYellow   cterm=none
hi texMathDelim    ctermfg=DarkYellow   cterm=none

hi texAccent       ctermfg=Gray         cterm=none

hi texCmdName      ctermfg=Yellow       cterm=bold
hi texCmdArgs      ctermfg=Cyan         cterm=none

hi texDef          ctermfg=Yellow       cterm=bold
hi texDefParm      ctermfg=Gray         cterm=none


" diff
" ----------------------------------------------------------
hi DiffAdd         ctermfg=DarkGreen    cterm=none
hi DiffChange      ctermfg=DarkYellow   cterm=none
hi DiffDelete      ctermfg=DarkRed      cterm=none
hi DiffText        ctermfg=Gray         cterm=none

hi DiffAdded       ctermfg=DarkGreen    cterm=none
hi DiffChanged     ctermfg=DarkYellow   cterm=none
hi DiffRemoved     ctermfg=DarkRed      cterm=none 

hi DiffFile        ctermfg=Red          cterm=none
hi DiffLine        ctermfg=DarkCyan     cterm=none


" git
" ---------------------------------------------------------
hi gitCommitOverflow ctermfg=Red        cterm=none
hi gitCommitSummary  ctermfg=Cyan       cterm=none

hi GitGutterAdd          ctermfg=DarkGreen    cterm=none
hi GitGutterChange       ctermfg=DarkYellow   cterm=none
hi GitGutterDelete       ctermfg=DarkRed      cterm=none
hi GitGutterChangeDelete ctermfg=DarkRed      cterm=none


" modeline
" ---------------------------------------------------------
hi AirlineNormal   ctermfg=Green    ctermbg=Black   cterm=bold
hi AirlineInsert   ctermfg=Yellow   ctermbg=Black   cterm=none
hi AirlineReplace  ctermfg=Red      ctermbg=Black   cterm=none
hi AirlineVisual   ctermfg=Blue     ctermbg=Black   cterm=none

hi AirlineBranch   ctermfg=White    ctermbg=Black   cterm=none
hi AirlineBuffer   ctermfg=White    ctermbg=Black   cterm=none

hi AirlineInactive ctermfg=Gray        ctermbg=Black   cterm=none
hi AirlineWarning  ctermfg=DarkYellow  ctermbg=Black   cterm=none
hi AirlineModified ctermfg=Red         ctermbg=Black   cterm=none
hi AirlineReadonly ctermfg=DarkRed     ctermbg=Black   cterm=none



" Other Stuff
" ----------------------------------------------------------

hi Directory       ctermfg=cyan     cterm=bold
hi ErrorMsg        ctermfg=red      cterm=bold     ctermbg=none


hi FoldColumn      ctermfg=green    cterm=bold
hi Folded          ctermfg=green    cterm=bold
hi IncSearch       ctermfg=green    cterm=bold

hi ModeMsg         ctermfg=yellow   cterm=none
hi MoreMsg         ctermfg=white    cterm=none


"hi Question        guifg=#ffffff gui=bold
"hi Repeat          guifg=#ffd7a7 gui=bold
"hi Search          guifg=#ffffe0 guibg=#284f28
"hi SpecialChar     guifg=#dca3a3 gui=bold
"hi SpecialComment  guifg=#82a282 gui=bold

"hi SpecialKey      guifg=#9ece9e


hi StorageClass    ctermfg=cyan     cterm=none


"hi VertSplit       guifg=#2e3330 guibg=#688060
"hi VisualNOS       guifg=#333333 guibg=#f18c96 gui=bold,underline
"hi WarningMsg      cterm=yellow
"hi WildMenu        guibg=#2c302d guifg=#cbecd0 gui=underline





