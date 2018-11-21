# Ideas from https://stackoverflow.com/questions/6216968/r-force-local-scope
# (answer from Tommy)

check_globals <- function(f, label, silent = FALSE) { # nocov start
  vars <- codetools::findGlobals(f)
  found <- !vapply(vars, exists, logical(1), envir=parent.frame())
  if (!silent && any(found)) {
    warning(label,": Global variables used: ", paste(names(found)[found], collapse=', '), "\n")
    return(invisible(FALSE))
  }
  !any(found)
}

check_function <- function(label) {
  f <- get(label, mode = "function", envir = parent.frame())
  codetools::checkUsage(f, report = stop)
  res <- check_globals(f, label)
  res
} # nocov end
