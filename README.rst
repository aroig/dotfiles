===============
Abdó's dotfiles
===============

Those are my config files. I keep them in separate repos, and aggregate them
into this single repository with ``git merge -s subtree``. I use `arch linux`_
with `awesome wm`_ as my window manager.

``awesome/``
  `awesome wm`_ setup. Includes some customized widgets, dropdown clients,
  notification boxes and a prompt similar to emacs ido-mode.

``ranger/``
  Configuration of ranger_ file manager. I use it in a dropdown window for
  easy access.

``shell/``
  Shell setup. This includes zsh_ configuration, with some personal aliases,
  a customized prompt, and automatic tmux session for ssh connections.

``systemd/`` 
  These are the configuration files (in ``~/.config/systemd``) for a `systemd
  user session`_. Systemd is an awesome init system, that can be used to manage
  user owned daemons.

  The main systemd process launches a user session with its own systemd
  instance, which then is the one responsible for handling X, window manager's,
  audio, fetching mail, etc.

``zathura/``
  Configuration for `zathura`_, a pdf/djvu viewer with vim-style bindings.

.. _`arch linux`: https://www.archlinux.org
.. _`awesome wm`: http://awesome.naquadah.org
.. _ranger: http://ranger.nongnu.org
.. _zsh: http://www.zsh.org
.. _`systemd user session`: https://wiki.archlinux.org/index.php/Systemd/User
.. _systemd: http://www.freedesktop.org/wiki/Software/systemd
.. _zathura: http://pwmt.org/projects/zathura
