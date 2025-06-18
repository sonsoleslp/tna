.onAttach <- function(libname, pkgname) {
  startup()
  invisible()
}

startup <- function() {
  packageStartupMessage(
    "'tna' package version ", utils::packageVersion("tna"), "\n",
    "------------------------------------------------------\n",
    "  Tikka, S., L\u00f3pez-Pernas, S., and Saqr, M. (2025). \n",
    "  tna: An R Package for Transition Network Analysis.\n",
    "  Applied Psychological Measurement.\n",
    "  https://doi.org/10.1177/01466216251348840\n",
    "------------------------------------------------------\n",
    "Please type 'citation(\"tna\")' for more citation information.\n",
    "See the package website at https://sonsoles.me/tna/\n"
  )
}
