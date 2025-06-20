# R/zzz.R

.onAttach <- function(libname, pkgname) {
  version <- utils::packageVersion(pkgname)
  packageStartupMessage("Welcome to fluxtools!\n")
  packageStartupMessage("Version: ", version)
  packageStartupMessage("To cite, run citation('fluxtools')")
  #packageStartupMessage("To see the vignette, run vignette('introduction', package = 'fluxtools')")
}
