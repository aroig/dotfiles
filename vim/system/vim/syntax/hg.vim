" Vim syntax file
" Language:	generic hg output
" Maintainer:	OHASHI Hideya
" Last Change:	2009 Oct 31

" see /usr/share/vim/vimcurrent/syntax/git.vim

if exists("b:current_syntax")
    finish
endif

syn case match
syn sync minlines=50

syn include @hgDiff syntax/diff.vim

syn region hgHead start=/\%^/ end=/^$/
syn region hgHead start=/\%(^changeset:  *\d\d*:\x\{12\}$\)\@=/ end=/^$/

" For hg reflog and hg show ...^{tree}, avoid sync issues
syn match hgHead /^\d\{6\} \%(\w\{4} \)\=\x\{40\}\%( [0-3]\)\=\t.*/
syn match hgHead /^\x\{40\} \x\{40}\t.*/

syn region hgDiff start=/^\%(diff -r \)\@=/ end=/^\%(diff -r \|$\)\@=/ contains=@hgDiff fold
syn region hgDiff start=/^\%(@@ -\)\@=/ end=/^\%(diff -r \|$\)\@=/ contains=@hgDiff

syn match  hgKeyword /^\%(object\|type\|tag\|changeset\|tree\|parent\|encoding\)\>/ contained containedin=hgHead nextgroup=hgHash,hgType skipwhite
syn match  hgKeyword /^\%(tag\>\|ref:\)/ contained containedin=hgHead nextgroup=hgReference skipwhite
syn match  hgKeyword /^Merge:/  contained containedin=hgHead nextgroup=hgHashAbbrev skipwhite
syn match  hgMode    /^\d\{6\}/ contained containedin=hgHead nextgroup=hgType,hgHash skipwhite
syn match  hgIdentityKeyword /^\%(user\|committer\|tagger\)\>/ contained containedin=hgHead nextgroup=hgIdentity skipwhite
syn match  hgIdentityHeader /^\%(user\|Commit\|Tagger\):/ contained containedin=hgHead nextgroup=hgIdentity skipwhite
syn match  hgDateHeader /^\%(AuthorDate\|CommitDate\|date\):/ contained containedin=hgHead nextgroup=hgDate skipwhite
syn match  hgSummaryHeader /^\%(summary\):/ contained containedin=hgHead nextgroup=hgIdentity skipwhite
syn match  hgFilesHeader /^\%(files\):/ contained containedin=hgHead nextgroup=hgIdentity skipwhite
syn match  hgDescriptionHeader /^\%(description\):/ contained containedin=hgHead nextgroup=hgIdentity skipwhite
syn match  hgIdentity /\S.\{-\} <[^>]*>/ contained nextgroup=hgDate skipwhite
syn region hgEmail matchgroup=hgEmailDelimiter start=/</ end=/>/ keepend oneline contained containedin=hgIdentity

syn match  hgReflogHeader /^Reflog:/ contained containedin=hgHead nextgroup=hgReflogMiddle skipwhite
syn match  hgReflogHeader /^Reflog message:/ contained containedin=hgHead skipwhite
syn match  hgReflogMiddle /\S\+@{\d\+} (/he=e-2 nextgroup=hgIdentity

syn match  hgDate      /\<\u\l\l \u\l\l \d\=\d \d\d:\d\d:\d\d \d\d\d\d [+-]\d\d\d\d/ contained
syn match  hgDate      /-\=\d\+ [+-]\d\d\d\d\>/               contained
syn match  hgDate      /\<\d\+ \l\+ ago\>/                    contained
syn match  hgType      /\<\%(tag\|commit\|tree\|blob\)\>/     contained nextgroup=hgHash skipwhite
syn match  hgStage     /\<\d\t\@=/                            contained
syn match  hgReference /\S\+\S\@!/                            contained
syn match  hgHash      /\<\x\{40\}\>/                         contained nextgroup=hgIdentity,hgStage skipwhite
syn match  hgHash      /^\<\x\{40\}\>/ containedin=hgHead contained nextgroup=hgHash skipwhite
syn match  hgHashAbbrev /\<\x\{4,39\}\.\.\./he=e-3 contained nextgroup=hgHashAbbrev skipwhite
syn match  hgHashAbbrev /\<\x\{40\}\>/             contained nextgroup=hgHashAbbrev skipwhite

hi def link hgDateHeader        hgIdentityHeader
hi def link hgSummaryHeader     hgIdentityHeader
hi def link hgFilesHeader       hgIdentityHeader
hi def link hgDescriptionHeader hgIdentityHeader
hi def link hgIdentityHeader    hgIdentityKeyword
hi def link hgIdentityKeyword   Label
hi def link hgReflogHeader      hgKeyword
hi def link hgKeyword           Keyword
hi def link hgIdentity          String
hi def link hgEmailDelimiter    Delimiter
hi def link hgEmail             Special
hi def link hgDate              Number
hi def link hgMode              Number
hi def link hgHashAbbrev        hgHash
hi def link hgHash              Identifier
hi def link hgReflogMiddle      hgReference
hi def link hgReference         Function
hi def link hgStage             hgType
hi def link hgType              Type

let b:current_syntax = "hg"
