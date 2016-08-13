TODO
====

## submit upstream

* commands for `frame-layout` and `find-file-in-project`

## devel

* Exclude `glsl-mode` from flycheck

* Automatically close compilation window using workspaces.

* Use spacemacs automatic success detection?


## latex

* make sure the compile buffer works fine.

* make sure we integrate the outline-minor-mode.


## compilation

* make per-mode `error-regexp-alist` variable. Need to use a `:filter-args`
  advice that changes the compilation mode to something set in a buffer-local
  variable. Then do whatever you want in a compilation-mode hook.

## org-mode

* make sure all mobile and calendar stuff still works


## mu4e


## completion

* completion may not work with snippets having special characters (e.g `eqn*`).

* company-mode does not complete full words.
  https://github.com/company-mode/company-mode/issues/451


## chat

* open all windows int the @chat persp

* close all chat connections when closing the chat perspective (mimic mu4e)

* configure logging

* configure notifications


## frames and perspectives

* Add `helm-find-files-in-project`. Can't add open in project as a helm action.
  Helm does not allow action customization.

## General

* setup package updates as a batch job (pending on https://github.com/syl20bnr/spacemacs/pull/5477)

* add layer for coq + proofgeneral

* configure personal dictionaries

* handle inter-layer dependencies: latex depends on some stuff in devel.

* use theming mode and get rid of the zenburn hack

* setup outline-minor-mode-prefix globally and evilify that minor mode

* tweak zenburn colors with the commits I've got in the zenburn-theme repo.

* get rid of all my custom scripts to run emacs, etc.


