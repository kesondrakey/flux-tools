#' Launch the interactive QA/QC app for flux data
#'
#' @param offset Integer. The site's UTC offset in hours (e.g. –5 for “UTC–5”, +2 for “UTC+2”).
#'               This argument is required.
#' @export
run_flux_qaqc <- function(offset) {
  # 1) Validate offset
  if (missing(offset) ||
      !is.numeric(offset) ||
      length(offset) != 1 ||
      offset < -12 ||
      offset > 14 ||
      offset != as.integer(offset)) {
    stop("`offset` must be a single integer between –12 and +14.", call. = FALSE)
  }
  
  # 2) Store it as an R option so the Shiny server can read it
  old_opts <- options(shiny.initialOffset = as.integer(offset))
  on.exit(options(old_opts), add = TRUE)
  
  # 3) Run the app. In a **released** package, we'll use system.file("app", …).
  #    But for local testing, you can point directly at "inst/app".
  app_dir <- system.file("app", package = "fluxtools")
  if (app_dir == "") {
    # We're likely in “development mode” (not yet installed). Use the local path:
    app_dir <- "inst/app"
  }
  
  shiny::runApp(
    appDir        = app_dir,
    launch.browser = TRUE
  )
}










