#------------------------------
# Colors
#------------------------------

# LS_COLORS. color configuration for ls
# eval $(dircolors)                   # default LS_COLORS
# Docs: http://www.linux-sxs.org/housekeeping/lscolors.html
_LS_COLORS=(
    # Permissions
    'rs=0'              'di=1;34'           'ln=1;36'           'mh=0'
    'pi=40;33'          'so=1;35'           'do=01;35'          'bd=33;01'
    'cd=1;33'           'or=1;31'           'su=37;41'          'sg=37;41'
    'ca=37;41'          'ex=1;32'
    'tw=1;34;40'        'ow=1;34;40'        'st=1;34;40'

    # packages
     '*.tar=0;31'       '*.tgz=0;31'       '*.arj=0;31'       '*.taz=0;31'
     '*.lzh=0;31'      '*.lzma=0;31'       '*.tlz=0;31'       '*.txz=0;31'
     '*.zip=0;31'         '*.z=0;31'         '*.Z=0;31'        '*.dz=0;31'
      '*.gz=0;31'        '*.lz=0;31'        '*.xz=0;31'       '*.bz2=0;31'
      '*.bz=0;31'       '*.tbz=0;31'      '*.tbz2=0;31'        '*.tz=0;31'
     '*.deb=0;31'       '*.rpm=0;31'       '*.jar=0;31'       '*.war=0;31'
     '*.ear=0;31'       '*.sar=0;31'       '*.rar=0;31'       '*.ace=0;31'
     '*.zoo=0;31'      '*.cpio=0;31'        '*.7z=0;31'        '*.rz=0;31'

    # pictures
     '*.jpg=0;35'      '*.jpeg=0;35'       '*.gif=0;35'       '*.bmp=0;35'
     '*.pbm=0;35'       '*.pgm=0;35'       '*.ppm=0;35'       '*.tga=0;35'
     '*.xbm=0;35'       '*.xpm=0;35'       '*.tif=0;35'      '*.tiff=0;35'
     '*.png=0;35'       '*.svg=0;35'      '*.svgz=0;35'       '*.mng=0;35'
     '*.pcx=0;35'

    # video
     '*.mov=0;35'       '*.mpg=0;35'      '*.mpeg=0;35'       '*.m2v=0;35'
     '*.mkv=0;35'      '*.webm=0;35'       '*.ogm=0;35'       '*.mp4=0;35'
     '*.m4v=0;35'      '*.mp4v=0;35'       '*.vob=0;35'        '*.qt=0;35'
     '*.nuv=0;35'       '*.wmv=0;35'       '*.asf=0;35'        '*.rm=0;35'
    '*.rmvb=0;35'       '*.flc=0;35'       '*.avi=0;35'       '*.fli=0;35'
     '*.flv=0;35'        '*.gl=0;35'

    # audio
     '*.aac=0;36'        '*.au=0;36'      '*.flac=0;36'       '*.mid=0;36'
    '*.midi=0;36'       '*.mka=0;36'       '*.mp3=0;36'       '*.mpc=0;36'
     '*.ogg=0;36'        '*.ra=0;36'       '*.wav=0;36'

    # unknown
      '*.dl=0;35'       '*.xcf=0;35'       '*.xwd=0;35'       '*.yuv=0;35'
     '*.cgm=0;35'       '*.emf=0;35'       '*.axv=0;35'       '*.anx=0;35'
     '*.ogv=0;35'       '*.ogx=0;35'
     '*.axa=0;36'       '*.oga=0;36'       '*.spx=0;36'      '*.xspf=0;36'

    # markup
    '*.html=0;35'     '*.xhtml=0;35'       '*.xml=0;35'

    # latex
     '*.tex=0;32'       '*.ltb=0;32'       '*.bib=0;32'       '*.sty=0;32'

    # documents
     '*.pdf=0;35'      '*.djvu=0;35'       '*.dvi=0;35'        '*.ps=0;35'
    '*.epub=0;35'      '*.mobi=0;35'       '*.chm=0;35'       '*.azw=0;35'

    # text
     '*.org=0;33'       '*.rst=0;33'       '*.rst=0;33'       '*.txt=0;33'
   '*README=1;33'  '*README.md=1;33' '*README.rst=1;33'
     '*TODO=1;33'    '*TODO.md=1;33'   '*TODO.rst=1;33'

    # source code
 '*Makefile=0;31' '*CMakeLists.txt=0;31' '*.cmake=0;31'
      '*.hs=0;32'
      '*.sh=0;32'       '*.zsh=0;32'        '*.el=0;32'
      '*.py=0;32'      '*.sage=0;34'       '*.lua=0;32'
       '*.c=0;32'        '*.cc=0;32'       '*.cpp=0;32'
       '*.h=0;32'        '*.hh=0;32'       '*.hpp=0;32'

    # config
    '*.conf=0;34'         '*rc=0;34'       '*.yml=0;34'

    # data
  '*.pickle=0;34'      '*.json=0;34'

    # systemd units
  '*.target=0;31'   '*.service=0;32'     '*.timer=0;34'      '*.path=0;36'
   '*.mount=0;33' '*.automount=0;33'      '*.swap=0;33'
   '*.scope=0;36'     '*.slice=0;36'
  '*.socket=0;35'   '*.network=0;35'    '*.netdev=0;35'
  '*.device=0;36'      '*.link=0;36'
)
export LS_COLORS=$(printf "%s:" "${_LS_COLORS[@]}")
unset _LS_COLORS

_EXA_COLORS=(
  'ur=0' 'uw=0' 'ux=0' 'ue=0'
  'gr=0' 'gw=0' 'gx=0'
  'tr=0' 'tw=0' 'tx=0'
  'uu=0' 'un=0'
  'gu=0' 'gn=0'
  'sb=0' 'sn=0'
  'da=0'
  'lp=1;34'
)

export EXA_COLORS=$(printf "%s:" "${_EXA_COLORS[@]}")
unset _EXA_COLORS

# SYSTEMD_COLORS. color configuration for systemd units in zsh autocompletion
_SYSTEMD_COLORS=(
          'rs=0'     'run-*.service=00;37'    'run-*.scope=00;37'
   '*.target=00;33'    '*.service=00;32'      '*.timer=00;34'      '*.path=00;36'
    '*.mount=00;31'  '*.automount=00;31'       '*.swap=00;31'
    '*.scope=00;33'      '*.slice=00;33'
   '*.socket=00;35'    '*.network=00;35'     '*.netdev=00;35'
   '*.device=00;35'       '*.link=00;35'
)
export SYSTEMD_COLORS=$(printf "%s:" "${_SYSTEMD_COLORS[@]}")
unset _SYSTEMD_COLORS

# gcc colors
_GCC_COLORS=(
    'error=01;31'   'warning=01;33'   'note=01;36'
    'caret=01;32'   'locus=00;36'     'quote=00;36'
)
export GCC_COLORS=$(printf "%s:" "${_GCC_COLORS[@]}")
unset _GCC_COLORS
