
#' Inspect a Running R Process
#'
#' @param pid The \R process PID. When passed a string, `loupe` will attempt to
#'   find the most recently launched \R process with the command matching the
#'   requested string. You can also pass the name of a process embedding \R
#'   to attach to that process; e.g. use `"rsession"` to attach to the
#'   most recently launched RStudio `rsession` process.
#'
#' @export
inspect <- function(pid = NULL) {

   # find active debugger
   debugger <- Sys.getenv("R_LOUPE_DEBUGGER", unset = loupe_inspect_debugger())
   if (!nzchar(Sys.which(debugger)))
      stop("requested debugger '", debugger, "' is not available")

   # resolve pid
   pid <- loupe_inspect_pid(pid)

   # invoke it
   switch(
      debugger,
      gdb  = loupe_inspect_gdb(pid),
      lldb = loupe_inspect_lldb(pid)
   )

}

loupe_inspect_pid <- function(pid) {

   if (is.null(pid))
      pid <- "R"

   if (is.character(pid))
      return(system(paste("pgrep -nx", pid), intern = TRUE))

   pid

}

loupe_inspect_debugger <- function() {
   if (Sys.info()[["sysname"]] == "Darwin") "lldb" else "gdb"
}

loupe_inspect_gdb <- function(pid) {

   # generate script to be run
   data <- loupe_inspect_init(pid)

   # tell gdb to run and source the generated R script.
   # TODO: why do I need to explicitly de-reference things here?
   # TODO: why doesn't GDB support double-quoted strings?
   # TODO: segfaults on second attempt
   template <- "attach {PID}
call *Rf_protect(*Rf_eval(*Rf_protect(*Rf_lang2(*Rf_install('sys.source'), *Rf_protect(*Rf_mkString('{SCRIPT}')))), *R_BaseEnv))
call *Rf_unprotect(2)
quit"

   template <- gsub("{PID}", pid, template, fixed = TRUE)
   template <- gsub("{SCRIPT}", data$rscript, template, fixed = TRUE)
   gdbcode <- template

   gdbscript <- tempfile("loupe-gdb-", fileext = ".gdb")
   writeLines(gdbcode, con = gdbscript)
   args <- c("-batch", "-x", shQuote(gdbscript))
   writeLines(gdbcode)

   # run lldb
   cat("Attaching to process '", pid, "' ... ", sep = "")
   output <- system2("gdb", args, stdout = TRUE, stderr = TRUE)
   cat("Done!", sep = "\n")

   # check for generated output
   if (!file.exists(data$output)) {
      warning("debugger failed to return any output")
      print(output)
      return(NULL)
   }

   # copy generated output to console
   output <- readLines(data$output, warn = FALSE)
   writeLines(output)

   # read generated RDS for user
   readRDS(data$frames)

}

loupe_inspect_lldb <- function(pid) {

   # generate script to be run
   data <- loupe_inspect_init(pid)

   # tell lldb to run and source the generated R script
   fmt <- "expr Rf_eval(Rf_lang2(Rf_install(\"sys.source\"), Rf_mkString(\"%s\")), R_BaseEnv)"
   expr <- sprintf(fmt, data$rscript)
   args <- c("-p", pid, "--batch", "-o", shQuote(expr))

   # run lldb
   cat("Attaching to process '", pid, "' ... ", sep = "")
   output <- system2("lldb", args, stdout = TRUE, stderr = TRUE)
   cat("Done!", sep = "\n")

   # check for generated output
   if (!file.exists(data$output)) {
      warning("debugger failed to return any output")
      return(NULL)
   }

   # copy generated output to console
   output <- readLines(data$output, warn = FALSE)
   writeLines(output)

   # read generated RDS for user
   readRDS(data$frames)

}

loupe_inspect_init <- function(pid) {

   # initialize root directory
   pattern <- sprintf("loupe-%s-", pid)
   root <- tempfile(pattern)
   dir.create(root, recursive = TRUE, showWarnings = FALSE)

   # initialize various paths
   data <- list(
      root    = root,
      output  = file.path(root, "output.log"),
      frames  = file.path(root, "frames.rds"),
      rscript = file.path(root, "script.R")
   )

   # generate script to be run
   code <- substitute({

      local({

         # if rlang is available, use it to write calls
         if (requireNamespace("rlang", quietly = TRUE)) {
            calls <- rlang::trace_back()
            writeLines(format(calls), con = output)
         }

         # read frames
         status <- sys.status()

         # trim our calls off
         offset <- -10L
         status$sys.calls   <- utils::head(status$sys.calls,   n = offset)
         status$sys.parents <- utils::head(status$sys.parents, n = offset)
         status$sys.frames  <- utils::head(status$sys.frames,  n = offset)

         # save to file
         saveRDS(status, file = frames)

      })

   }, data)

   # write script to file
   writeLines(deparse(code), con = data$rscript)

   # return data
   data

}
