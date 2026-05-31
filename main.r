source("setup.R")

# Actual economic analysis modules, each in its own file for better organization.
source("descriptives.r")
source("acf.r")
source("adf.r")
source("capm.r")
source("corr.r")
source("garch.r")
source("reccs.r")

# Single entry point: prepare the data once, then run everything 
run_all <- function(...) {
  dir.create("tables", showWarnings = FALSE)
  dir.create("plots",  showWarnings = FALSE)

  dat <- prepare_data(...)  

  run_descriptives(dat)
  run_acf(dat)
  run_adf(dat)
  run_corr(dat)
  run_garch(dat)
  run_capm(dat)
  run_reccs(dat)

}

# Auto-run only when executed non-interactively (e.g. `Rscript main.r`).
# In RStudio: source this file, then call run_all() yourself.
if (!interactive()) {
  run_all()
}