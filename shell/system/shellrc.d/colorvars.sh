#------------------------------
# Colors
#------------------------------

# LS_COLORS. color configuration for ls
# eval $(dircolors)                   # default LS_COLORS
_LS_COLORS=(
        'rs=0'              'di=01;34'          'ln=01;36'          'mh=00'
        'pi=40;33'          'so=01;35'          'do=01;35'          'bd=40;33;01' 
        'cd=40;33;01'       'or=40;31;01'       'su=37;41'          'sg=30;43'   
        'ca=30;41'          'tw=30;42'          'ow=34;42'          'st=37;44'    
        'ex=01;32'   

# packages
     '*.tar=00;31'       '*.tgz=00;31'       '*.arj=00;31'       '*.taz=00;31' 
     '*.lzh=00;31'      '*.lzma=00;31'       '*.tlz=00;31'       '*.txz=00;31' 
     '*.zip=00;31'         '*.z=00;31'         '*.Z=00;31'        '*.dz=00;31'     
      '*.gz=00;31'        '*.lz=00;31'        '*.xz=00;31'       '*.bz2=00;31'    
      '*.bz=00;31'       '*.tbz=00;31'      '*.tbz2=00;31'        '*.tz=00;31'    
     '*.deb=00;31'       '*.rpm=00;31'       '*.jar=00;31'       '*.war=00;31'    
     '*.ear=00;31'       '*.sar=00;31'       '*.rar=00;31'       '*.ace=00;31'   
     '*.zoo=00;31'      '*.cpio=00;31'        '*.7z=00;31'        '*.rz=00;31'    

# pictures
     '*.jpg=00;35'      '*.jpeg=00;35'       '*.gif=00;35'       '*.bmp=00;35'
     '*.pbm=00;35'       '*.pgm=00;35'       '*.ppm=00;35'       '*.tga=00;35' 
     '*.xbm=00;35'       '*.xpm=00;35'       '*.tif=00;35'      '*.tiff=00;35'  
     '*.png=00;35'       '*.svg=00;35'      '*.svgz=00;35'       '*.mng=00;35'  
     '*.pcx=00;35'     

# video
     '*.mov=00;35'       '*.mpg=00;35'      '*.mpeg=00;35'       '*.m2v=00;35'
     '*.mkv=00;35'      '*.webm=00;35'       '*.ogm=00;35'       '*.mp4=00;35'    
     '*.m4v=00;35'      '*.mp4v=00;35'       '*.vob=00;35'        '*.qt=00;35'     
     '*.nuv=00;35'       '*.wmv=00;35'       '*.asf=00;35'        '*.rm=00;35'     
    '*.rmvb=00;35'       '*.flc=00;35'       '*.avi=00;35'       '*.fli=00;35' 
     '*.flv=00;35'        '*.gl=00;35'      

# audio
     '*.aac=00;36'        '*.au=00;36'      '*.flac=00;36'       '*.mid=00;36'   
    '*.midi=00;36'       '*.mka=00;36'       '*.mp3=00;36'       '*.mpc=00;36'   
     '*.ogg=00;36'        '*.ra=00;36'       '*.wav=00;36'     

# unknown
      '*.dl=00;35'       '*.xcf=00;35'       '*.xwd=00;35'       '*.yuv=00;35'    
     '*.cgm=00;35'       '*.emf=00;35'       '*.axv=00;35'       '*.anx=00;35'    
     '*.ogv=00;35'       '*.ogx=00;35'    
     '*.axa=00;36'       '*.oga=00;36'       '*.spx=00;36'      '*.xspf=00;36'

# markup
    '*.html=00;35'     '*.xhtml=00;35'       '*.xml=00;35'

# latex
     '*.tex=00;32'       '*.ltb=00;32'       '*.bib=00;32'       '*.sty=00;32'     

# documents
     '*.pdf=00;35'      '*.djvu=00;35'       '*.dvi=00;35'        '*.ps=00;35'
    '*.epub=00;35'      '*.mobi=00;35'       '*.chm=00;35'       '*.azw=00;35'

# text
     '*.org=00;33'       '*.rst=00;33'       '*.rst=00;33'       '*.txt=00;33'
   '*README=01;33'  '*README.md=01;33' '*README.rst=01;33'
     '*TODO=01;33'    '*TODO.md=01;33'   '*TODO.rst=01;33'     

# source code
 '*Makefile=00;31' '*CMakeLists.txt=00;31' '*.cmake=00;31'
      '*.hs=00;32'    
      '*.sh=00;32'       '*.zsh=00;32'        '*.el=00;32'
      '*.py=00;32'      '*.sage=00;34'       '*.lua=00;32'
       '*.c=00;32'       '*.cpp=00;32'         '*.h=00;32'       '*.hpp=00;32'

# config
    '*.conf=00;34'         '*rc=00;34'       '*.yml=00;34'

# data
  '*.pickle=00;34'      '*.json=00;34'

# systemd units
  '*.target=00;31'   '*.service=00;32'     '*.timer=00;34'      '*.path=00;36'  
   '*.mount=00;33' '*.automount=00;33'      '*.swap=00;33'
   '*.scope=00;36'     '*.slice=00;36'
  '*.socket=00;35'   '*.network=00;35'    '*.netdev=00;35'
  '*.device=00;36'      '*.link=00;36'
)
export LS_COLORS=$(printf "%s:" "${_LS_COLORS[@]}")


# SYSTEMD_COLORS. color configuration for systemd units in zsh autocompletion
_SYSTEMD_COLORS=(
          'rs=0'    '=run-*.service=0;37'   '=run-*.scope=0;37'
   '=*.target=00;33'    '=*.service=00;32'      '=*.timer=00;34'      '=*.path=00;36'   
    '=*.mount=00;31'  '=*.automount=00;31'       '=*.swap=00;31'
    '=*.scope=00;33'      '=*.slice=00;33'
   '=*.socket=00;35'    '=*.network=00;35'     '=*.netdev=00;35'
   '=*.device=00;35'       '=*.link=00;35' 

)
export SYSTEMD_COLORS=$(printf "%s:" "${_SYSTEMD_COLORS[@]}")

# gcc colors
_GCC_COLORS=(
    'error=01;31'   'warning=01;33'   'note=01;36' 
    'caret=01;32'   'locus=00;36'     'quote=00;36'
)

export GCC_COLORS=$(printf "%s:" "${_GCC_COLORS[@]}")

