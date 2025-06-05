# R/zzz.R

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "fluxtools: Interactive QA/QC for AmeriFlux Data\n",
    "Citation:\n",
    "  Key, K. (2025). fluxtools (version 0.1.0) [Computer software]. Zenodo. ",
    "https://doi.org/10.5281/zenodo.15597160\n"
  )
}


