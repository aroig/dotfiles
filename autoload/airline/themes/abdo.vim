" Each theme is contained in its own file and declares variables scoped to the
" file.  These variables represent the possible "modes" that airline can
" detect.  The mode is the return value of mode(), which gets converted to a
" readable string.  The following is a list currently supported modes: normal,
" insert, replace, visual, and inactive.
"
" Each mode can also have overrides.  These are small changes to the mode that
" don't require a completely different look.  "modified" and "paste" are two
" such supported overrides.  These are simply suffixed to the major mode,
" separated by an underscore.  For example, "normal_modified" would be normal
" mode where the current buffer is modified.
"
" The theming algorithm is a 2-pass system where the mode will draw over all
" parts of the statusline, and then the override is applied after.  This means
" it is possible to specify a subset of the theme in overrides, as it will
" simply overwrite the previous colors.  If you want simultaneous overrides,
" then they will need to change different parts of the statusline so they do
" not conflict with each other.
"
" First, let's define an empty dictionary and assign it to the "palette"
" variable. The # is a separator that maps with the directory structure. If
" you get this wrong, Vim will complain loudly.
let g:airline#themes#abdo#palette = {}

" First let's define some arrays. The s: is just a VimL thing for scoping the
" variables to the current script. Without this, these variables would be
" declared globally. Now let's declare some colors for normal mode and add it
" to the dictionary.  The array is in the format:
" [ guifg, guibg, ctermfg, ctermbg, opts ]. See "help attr-list" for valid
" values for the "opt" value.

let s:BR = airline#themes#get_highlight2(['AirlineBranch', 'bg'], ['AirlineBranch', 'fg'])
let s:BF = airline#themes#get_highlight2(['AirlineBuffer', 'bg'], ['AirlineBuffer', 'fg'])

let s:N1 = airline#themes#get_highlight2(['AirlineNormal', 'bg'], ['AirlineNormal', 'fg'])
let g:airline#themes#abdo#palette.normal = airline#themes#generate_color_map(s:N1, s:BR, s:BF)

let s:I1 = airline#themes#get_highlight2(['AirlineInsert', 'bg'], ['AirlineInsert', 'fg'])
let g:airline#themes#abdo#palette.insert = airline#themes#generate_color_map(s:I1, s:BR, s:BF)

let s:V1 = airline#themes#get_highlight2(['AirlineVisual', 'bg'], ['AirlineVisual', 'fg'])
let g:airline#themes#abdo#palette.visual = airline#themes#generate_color_map(s:V1, s:BR, s:BF)

let s:R1 = airline#themes#get_highlight2(['AirlineReplace', 'bg'], ['AirlineReplace', 'fg'])
let g:airline#themes#abdo#palette.replace = airline#themes#generate_color_map(s:R1, s:BR, s:BF)

let s:IA1 = airline#themes#get_highlight2(['AirlineInactive', 'bg'], ['AirlineInactive', 'fg'])
let g:airline#themes#abdo#palette.inactive = airline#themes#generate_color_map(s:IA1, s:BR, s:BF)

" Here we define overrides for when the buffer is modified.  This will be
" applied after g:airline#themes#abdo#palette.normal, hence why only certain keys are
" declared.
let s:MO = airline#themes#get_highlight2(['AirlineModified', 'bg'], ['AirlineBuffer', 'fg'])
let s:WA = airline#themes#get_highlight2(['AirlineWarning', 'bg'], ['AirlineWarning', 'fg'])

let g:airline#themes#abdo#palette.normal.airline_warning = s:WA
let g:airline#themes#abdo#palette.insert.airline_warning = s:WA
let g:airline#themes#abdo#palette.visual.airline_warning = s:WA
let g:airline#themes#abdo#palette.inactive.airline_warning = s:WA

let g:airline#themes#abdo#palette.normal_modified = { 'airline_c': s:MO, 'airline_warning': s:WA }
let g:airline#themes#abdo#palette.insert_modified = { 'airline_c': s:MO, 'airline_warning': s:WA }
let g:airline#themes#abdo#palette.visual_modified = { 'airline_c': s:MO, 'airline_warning': s:WA }
let g:airline#themes#abdo#palette.inactive_modified = { 'airline_c': s:MO, 'airline_warning': s:WA }

let s:PA = airline#themes#get_highlight2(['AirlineModified', 'bg'], ['AirlineBuffer', 'fg'])
let g:airline#themes#abdo#palette.insert_paste = { 'airline_a': s:PA }

" Accents are used to give parts within a section a slightly different look or
" color. Here we are defining a "red" accent, which is used by the 'readonly'
" part by default. Only the foreground colors are specified, so the background
" colors are automatically extracted from the underlying section colors. What
" this means is that regardless of which section the part is defined in, it
" will be red instead of the section's foreground color. You can also have
" multiple parts with accents within a section.

let s:RO = airline#themes#get_highlight2(['AirlineReadonly', 'bg'], ['AirlineReadonly', 'fg'])
let g:airline#themes#abdo#palette.accents = { 'red': s:RO }
" let g:airline#themes#abdo#palette.accents = { 'red': s:AC1 }

