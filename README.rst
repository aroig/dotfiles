===============
Abdó's dotfiles
===============

Those are my config files. I keep them in separate repos, and then aggregate
into this single repository for github using ``git merge -s subtree``. I use
`arch linux`_ with `awesome wm`_ as my window manager.

``arch/``
  Some mr config files to automatize building arch packages from source or from
  AUR, and export them into a local pacman repo.

``async/``
  Configuration for a python script I wrote to synchronize my machines. Can
  start and stop amazon ec2 instances, as well as sync individual directories
  with rsync_, unison_, git_ or git-annex_.

``awesome/``
  `awesome wm`_ setup. Includes some customized widgets, dropdown clients,
  notification boxes and a prompt similar to emacs ido-mode, and some helper
  functions to launch applications as transient systemd services.

``dwb/``
  Configuration for dwb_, a lightweight webkit-based web browser with vim-style
  key bindings

``emacs/``
  My emacs_ configuration files, including org-mode_, jabber and irc client,
  mu4e_, etc.

``ranger/``
  Configuration of ranger_ file manager. I use it in a dropdown window for
  easy access.

``shell/``
  Shell related config files. This includes zsh_ configuration, with some
  personal aliases, a customized prompt, and automatic tmux_ session for ssh
  connections...

``systemd/`` 
  These are the configuration files (in ``~/.config/systemd``) for a `systemd
  user session`_. Systemd is an awesome init system, that can be used to manage
  user owned daemons.

``texa/``
  Configuration for texa, a script I wrote to help me quickly setup latex
  projects for writing papers.

``vim/``
  My vim configuration. I do use vim from time to time...

``zathura/``
  Configuration for `zathura`_, a pdf/djvu viewer with vim-style bindings.

.. _`arch linux`: https://www.archlinux.org
.. _`rsync`: http://rsync.samba.org
.. _`unison`: http://www.cis.upenn.edu/~bcpierce/unison
.. _`git`: http://git-scm.com
.. _`git-annex`: https://git-annex.branchable.com
.. _`awesome wm`: http://awesome.naquadah.org
.. _`dwb`: http://portix.bitbucket.org/dwb
.. _`emacs`: http://www.gnu.org/software/emacs
.. _`org-mode`: http://orgmode.org
.. _`mu4e`: http://www.djcbsoftware.nl/code/mu/mu4e.html
.. _ranger: http://ranger.nongnu.org
.. _zsh: http://www.zsh.org
.. _tmux: http://tmux.sourceforge.net
.. _`systemd user session`: https://wiki.archlinux.org/index.php/Systemd/User
.. _systemd: http://www.freedesktop.org/wiki/Software/systemd
.. _vim: http://www.vim.org
.. _zathura: http://pwmt.org/projects/zathura
