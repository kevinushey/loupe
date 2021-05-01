
## loupe

> loupe [noun]. a small magnifying glass used by jewelers and watchmakers.

**NOTE: This package is currently experimental. It will probably break, and will probably crash your R processes when you use it. Hopefully that will be less true in the future. Proceed at your own risk!**

Inspect running R processes from R, or from the command line via the bundled `loupe` script.

`loupe` works by using `lldb` / `gdb` to attach to your running R process,
and then invokes an R script to capture the stack trace and R call frames.
