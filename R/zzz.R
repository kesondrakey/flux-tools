# R/zzz.R

# .onAttach <- function(libname, pkgname) {
#   packageStartupMessage(
#     "fluxtools: Interactive QA/QC for AmeriFlux Data\n\n",
#     "Citation:\n",
#     "  Key, K. (2025). fluxtools (version 0.2.0) [Computer software]. Zenodo. ",
#     "https://doi.org/10.5281/zenodo.15597159\n"
#   )
# }


.onAttach <- function(libname, pkgname) {
<<<<<<< Updated upstream
<<<<<<< Updated upstream
  packageStartupMessage(
    "fluxtools: Interactive QA/QC for AmeriFlux Data\n\n",
    "To run the app, replace with your site’s UTC offset (i.e., –5 for EST):\n",
    "fluxtools::run_flux_qaqc(-5)\n\n",
    "To see the how-to vignette, run:\n\n",
    "vignette('introduction', package = 'fluxtools')\n\n",
    "Citation:\n",
    "  Key, K. (2025). fluxtools (version 0.2.0) [Computer software]. Zenodo. ",
    "https://doi.org/10.5281/zenodo.15597159\n"
  )
}

=======
  version <- utils::packageVersion(pkgname)
  packageStartupMessage("Welcome to fluxtools!")
  packageStartupMessage("Version: ", version)
  packageStartupMessage("To cite, run citation('fluxtools')")
}
>>>>>>> Stashed changes
=======
  version <- utils::packageVersion(pkgname)
  packageStartupMessage("Welcome to fluxtools!")
  packageStartupMessage("Version: ", version)
  packageStartupMessage("To cite, run citation('fluxtools')")
}
>>>>>>> Stashed changes
