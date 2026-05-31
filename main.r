source("setup.R")

# Actual economic analysis modules, each in its own file for better organization.
source("descriptives.r")
source("acf.r")
source("adf.r")
source("capm.r")
source("corr.r")
source("garch.r")
source("reccs.r")
source("varranger.r")   # VAR / Granger causality + 2025 out-of-sample evaluation

# Single entry point: prepare the data once, then run everything.
# w_rec is the recommended allocation; set it here once so run_reccs() and
# run_eval_2025() always use exactly the same weights.
run_all <- function(..., w_rec = c(KO = 0.05, NVDA = 0.65, XOM = 0.30)) {
  dir.create("tables", showWarnings = FALSE)
  dir.create("plots",  showWarnings = FALSE)
  
  dat <- prepare_data(...)
  
  run_descriptives(dat)
  run_acf(dat)
  run_adf(dat)
  run_corr(dat)
  run_garch(dat)
  run_capm(dat)
  run_reccs(dat, w_rec = w_rec)
  
  run_var_granger(dat)               # multivariate: VAR(p) + Granger causality
  run_eval_2025(dat, w_rec = w_rec)  # pulls actual 2025 data (needs internet)
}

# Auto-run only when executed non-interactively (e.g. `Rscript main.r`).
# In RStudio: source this file, then call run_all() yourself.
if (!interactive()) {
  run_all()
}