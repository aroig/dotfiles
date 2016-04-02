Abdó's dotfiles
===============

Those are some my config files. I keep them in separate repos, and then
aggregate them into this single repository for github, using `git merge -s
subtree`. I use [arch linux] with [awesome wm] as my window manager, and
[systemd user] services.

`awesome/`
: [awesome] setup. Includes some customized widgets, dropdown clients,
  notification boxes and a prompt similar to emacs ido-mode, and some helper
  functions to launch applications as transient systemd services.

`compton/`
: [compton] configuration.

`dbus-1/`
: [dbus] configuration.

`ranger/`
: Configuration of [ranger] file manager. I use it in a dropdown window for
  easy access.

`shell/`
: Shell related config files. This includes [zsh] configuration, with some
  personal aliases, a customized prompt, etc.

`spacemacs/`
: My [spacemacs] configuration files, including [org-mode], [mu4e], etc.

`systemd/`
: These are the configuration files in `~/.config/systemd`) for [systemd user]
  services. Systemd is an awesome init system, that can be used to manage user
  owned daemons.

`vim/`
: My [vim] configuration. I do use vim from time to time...

`zathura/` 
: Configuration for [zathura], a pdf/djvu viewer with vim-style bindings.

[arch linux]: https://www.archlinux.org
[compton]: https://github.com/chjj/compton
[dbus]: https://www.freedesktop.org/wiki/Software/dbus
[rsync]: http://rsync.samba.org
[unison]: http://www.cis.upenn.edu/~bcpierce/unison
[git]: http://git-scm.com
[git-annex]: https://git-annex.branchable.com
[awesome]: http://awesome.naquadah.org
[dwb]: http://portix.bitbucket.org/dwb
[emacs]: http://www.gnu.org/software/emacs
[org-mode]: http://orgmode.org
[mu4e]: http://www.djcbsoftware.nl/code/mu/mu4e.html
[ranger]: http://ranger.nongnu.org
[zsh]: http://www.zsh.org
[tmux]: http://tmux.sourceforge.net
[systemd user]: https://wiki.archlinux.org/index.php/Systemd/User
[systemd]: http://www.freedesktop.org/wiki/Software/systemd
[vim]: http://www.vim.org
[zathura]: http://pwmt.org/projects/zathura
