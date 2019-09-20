assert_installed <- function(pkg, cran = TRUE) {
  if (!requireNamespace(pkg))
    stop("package '", pkg, "' must be installed to run this function. ",
         if (cran) "Install it with install.packages('", pkg, "').")
}
