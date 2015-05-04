require("apps")

local qref_dir = "/home/abdo/work/qref"

return {
   -- local html docs
   doxygen               = "file:///usr/share/doc/packages/doxygen/html/index.html",
   cmake                 = "file:///usr/share/doc/cmake/index.html",

   python2               = "file:///usr/share/doc/python2/html/index.html",
   python3               = "file:///usr/share/doc/python/html/index.html",
   sage                  = "file:///usr/share/doc/sage/output/html/en/index.html",
   scipy                 = "file:///usr/share/doc/python-scipy/html",
   lua                   = "file:///usr/share/doc/lua/contents.html",

   gcc                   = "file:///usr/share/doc/gcc/index.html",
   ["libstdc++"]         = "file:///usr/share/doc/libstdc++/index.html",

   qtcore                = "file:///usr/share/doc/qt/qtcore/qtcore-index.html",
   qtopengl              = "file:///usr/share/doc/qt/qtopengl/qtopengl-index.html",
   qtlinguist            = "file:///usr/share/doc/qt/qtlinguist/qtlinguist-index.html",
   qtquick               = "file:///usr/share/doc/qt/qtquick/qtquick-index.html",
   qtsensors             = "file:///usr/share/doc/qt/qtsensors/qtsensors-index.html",
   qtmultimedia          = "file:///usr/share/doc/qt/qtmultimedia/qtmultimedia-index.html",

   gtk3                  = "file:///usr/share/gtk-doc/html/gtk3/index.html",
   gdk3                  = "file:///usr/share/gtk-doc/html/gdk3/index.html",
   gobject               = "file:///usr/share/gtk-doc/html/gobject/index.html",
   glib                  = "file:///usr/share/gtk-doc/html/glib/index.html",
   gio                   = "file:///usr/share/gtk-doc/html/gio/index.html",
   cairo                 = "file:///usr/share/gtk-doc/html/cairo/index.html",
   rsvg                  = "file:///usr/share/gtk-doc/html/rsvg-2.0/index.html",

   haskell               = "file:///usr/share/doc/ghc/html/libraries/index.html",
   zsh                   = "file:///usr/share/doc/zsh/zsh.html",
   bash                  = "file:///usr/share/doc/bash/bashref.html",
   awesome               = "file:///usr/share/doc/awesome/doc/index.html",

   org                   = "file:///usr/share/doc/org-mode/org.html",
   elisp                 = "file:///usr/share/doc/elisp/index.html",
   emacs                 = "file:///usr/share/doc/emacs/index.html",
   mu4e                  = "file:///usr/share/doc/mu4e/index.html",

   xapian                = "file:///usr/share/doc/xapian-core/index.html",

   boost                 = "file:///usr/share/doc/boost/index.html",
   ogre                  = "file:///usr/share/doc/OGRE/api/html/index.html",
   openscenegraph        = "file:///usr/share/doc/openscenegraph/openscenegraph/index.html",
   openthreads           = "file:///usr/share/doc/openscenegraph/openthreads/index.html",
   bullet                = "file:///usr/share/doc/bullet/html/index.html",

   ["android-ndk"]       = "file:///opt/android-ndk/docs/Programmers_Guide/html/index.html",
   ["android-sdk"]       = "file:///opt/android-sdk/docs/index.html",

   -- online docs
   rst                   = "http://docutils.sourceforge.net/docs/user/rst/quickref.html",
   markdown              = "http://daringfireball.net/projects/markdown/syntax",

   ["arch-wiki"]         = "https://wiki.archlinux.org/index.php/Main_Page",
   ["zsh-wiki"]          = "http://zshwiki.org/home/start",
   ["bash-wiki"]         = "http://wiki.bash-hackers.org/doku.php",

   -- TODO: replace PDF docs with html ones
   -- ["latex-qref"]        = apps.pdfviewer  .. string.format(" %s/latex.pdf", qref_dir),
   -- ["vim-qref"]          = apps.pdfviewer  .. string.format(" %s/vim.pdf", qref_dir),
   -- ["emacs-qref"]        = apps.pdfviewer  .. string.format(" %s/emacs.pdf", qref_dir),

}
