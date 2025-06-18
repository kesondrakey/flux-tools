#' @title Citation for Fluxtools
#' @description  Custom citation method so that `citation("fluxtools")` always returns exactly what you want.
#' @param pkg,name,lib.loc,quiet, ...  Standard arguments (ignored).
#' @export
citation.fluxtools <- function(package = "fluxtools", lib.loc = NULL, quiet = FALSE, ...) {
  # Print header if not quiet
  if (!quiet) {
    cat("To cite package 'fluxtools' in publications use:\n\n")
  }
  # Build the single bibentry
  cit <- bibentry(
    bibtype = "Manual",
    title   = "Fluxtools: Interactive QA/QC for AmeriFlux Data",
    author  = person("Kesondra", "Key", email = "keyke@iu.edu", role = c("aut", "cre")),
    year    = "2025",
    note    = "R package version 0.2.0; doi:10.5281/zenodo.15597159",
    url     = "https://github.com/kesondrakey/fluxtools"
  )
  # Return with the proper class
  class(cit) <- "citation"
  cit
}
