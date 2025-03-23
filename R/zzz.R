.onAttach <- function(libname, pkgname) {
  startup()
  invisible()
}

startup <- function() {
  packageStartupMessage(
    "Package 'tna' version ", utils::packageVersion("tna"), "\n",
    "Please type 'citation(\"tna\")' for citing this R package in publications"
  )
}
