
#' Inspect a Running R Process
#'
#' @param pid The \R process ID. When passed a string, `loupe` will attempt to
#'   find the most recently launched \R process with the command matching the
#'   requested string. You can also pass the name of a process embedding \R
#'   to attach to that process; e.g. use `"rsession"` to attach to the
#'   most recently launched RStudio `rsession` process.
#'
#' @param debugger The path to a debugger. When unset, loupe will use the
#'   debugger found on the `PATH` (`lldb` on macOS; `gdb` otherwise).
#'
#' @export
inspect <- function(pid = NULL, debugger = NULL) {

   args <- c(
      if (!is.null(debugger))
         c("-d", shQuote(debugger)),
      pid
   )

   loupe <- system.file("bin/loupe", package = "loupe")
   system2(loupe, args)
}
