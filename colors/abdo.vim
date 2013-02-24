" A simple 16 color theme for vim
" Maintainer:   Abdó Roig-Maranges <abdo.roig@gmail.com>
" License:      GNU GPL <http://www.gnu.org/licenses/gpl.html>

set background=dark
highlight clear
if exists("syntax_on")
  syntax reset
endif
let g:colors_name = "abdo"


" general
" ----------------------------------------------------------
hi Error           ctermfg=Red          cterm=bold
hi Normal          ctermfg=none         cterm=none
hi Todo            ctermfg=Red          cterm=bold          ctermbg=Black
hi Underlined      ctermfg=none         cterm=underline
hi Ignore          ctermfg=Black 


" programming
" ----------------------------------------------------------
hi Constant        ctermfg=Gray         cterm=none
hi Boolean         ctermfg=Yellow       cterm=bold
hi Character       ctermfg=Red          cterm=none
hi Float           ctermfg=Gray         cterm=none
hi Number          ctermfg=Gray         cterm=none
hi String          ctermfg=Red          cterm=none

hi Comment         ctermfg=Green        cterm=none
hi Identifier      ctermfg=Cyan         cterm=none
hi Function        ctermfg=Cyan         cterm=none

hi Statement       ctermfg=Yellow       cterm=bold
hi Conditional     ctermfg=Yellow       cterm=bold
hi Repeat          ctermfg=Yellow       cterm=bold
hi Label           ctermfg=Green        cterm=none
hi Operator        ctermfg=Gray         cterm=none
hi Keyword         ctermfg=Cyan         cterm=none
hi Exception       ctermfg=Yellow       cterm=bold

hi PreProc         ctermfg=Blue         cterm=none

hi Type            ctermfg=Cyan         cterm=none
hi StorageClass    ctermfg=Yellow       cterm=bold
hi Structure       ctermfg=Yellow       cterm=bold
hi Typedef         ctermfg=Yellow       cterm=bold

hi Special         ctermfg=Red          cterm=none
hi Delimiter       ctermfg=Gray         cterm=none
hi Tag             ctermfg=Gray         cterm=none
hi Debug           ctermfg=Gray         cterm=none


" sage
" ----------------------------------------------------------
hi sagePrompt      ctermfg=DarkYellow   cterm=none


" text
" ----------------------------------------------------------
hi Title           ctermfg=Cyan         cterm=bold

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
hi diffAdded       ctermfg=DarkGreen    cterm=none
hi diffChanged     ctermfg=DarkYellow   cterm=none
hi diffRemoved     ctermfg=DarkRed      cterm=none 
hi diffFile        ctermfg=Red          cterm=none
hi diffLine        ctermfg=DarkCyan     cterm=none


" Vim
" ----------------------------------------------------------

hi Cursor          ctermfg=White        cterm=bold
hi StatusLine      ctermfg=Gray         cterm=reverse

"hi StatusLineNC    guifg=#2e3330 guibg=#88b090



" Other Stuff
" ----------------------------------------------------------


hi Directory       ctermfg=cyan     cterm=bold
hi ErrorMsg        ctermfg=red      cterm=bold


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





