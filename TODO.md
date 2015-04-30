TODO
====

## mr homedir

* write hooks to sync collections of git repos at devel, etc, work, etc.

* trigger homedir-presync.target before syncs. What about triggering this on the remote?

* skip when repo has no remote.
  - NOT GOOD. I can make sure all dirs have all remotes, and set remote.blah.annex-sync=false.
  - GOOD. I can modify mr so that it passes the full command line args to skip.
  - EVEN BETTER. I can implement a skip command to skip in the middle of an action!

* handle non-git directories uniformly, so they do not fail
  - Add an include that defines noop versions of the git commands?
  - Make all directories belong to a vcs type. If a vcs type does not have a command, mr ignores it

* Make arch directory toplevel

* move arch, devel etc to a mr-based sync scheme

* add build directory with custom gc action. Could get rid of cleanup timers for build, then!
