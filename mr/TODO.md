TODO
====

## mr scripts

* homedir: run pre-sync hooks locally and on remote

* homedir: handle ~/opt. right now it is set as dir on galois. This be broken if an mr
  --force init is executed! Need a way to ignore syncs on some replicas, for instance when
  no uuid is setup.
  



## home directory structure

* move arch, devel etc to a mr-based sync scheme

* fix `mr checkout` and `mr init` sequence. Right now `mr checkout` fails, and `mr init`
  skips because can't determine the directory type.
