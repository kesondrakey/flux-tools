# R/zzz.R

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "fluxtools: Interactive QA/QC for AmeriFlux Data\n\n",
    "To run the app, replace with your site’s UTC offset (i.e., –5 for EST):\n",
    "fluxtools::run_flux_qaqc(-5)\n\n",
    "Citation:\n",
    "  Key, K. (2025). fluxtools (version 0.2.0) [Computer software]. Zenodo. ",
    "https://doi.org/10.5281/zenodo.15597159\n"
  )
}


