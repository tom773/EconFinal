# garch.r ─ GARCH(1,1) models and ARCH-LM tests
# This produces:
#     tables/arch_lm.png
#     tables/garch_params.png
#     plots/cond_volatility.png

.cond_vol_panel <- function(d, title, colour, show_x = FALSE) {
  ggplot(d, aes(Date, csd)) +
    geom_line(colour = colour, linewidth = 0.4) +
    labs(title = title, x = if (show_x) "Date" else NULL, y = "Cond. SD (% daily)") +
    theme_dark_roboto() + layout_fix
}
run_garch <- function(dat) {
  ret_vecs <- dat$ret_vecs
  ret_xts  <- dat$ret_xts
  arch_lag <- 10
  arch_tbl_df <- imap_dfr(ret_vecs, function(x, nm) {
    at <- ArchTest(x - mean(x), lags = arch_lag)
    tibble(
      Ticker         = nm,
      `LM statistic` = unname(at$statistic),
      df             = unname(at$parameter),
      `p-value`      = at$p.value
    )
  })
  arch_tbl <- arch_tbl_df |>
    gt(rowname_col = "Ticker") |>
    tab_header(
      title    = "ARCH-LM Tests for Conditional Heteroskedasticity",
      subtitle = "Daily log returns- KO, NVDA & XOM, 2020–2024"
    ) |>
    fmt_number(columns = `LM statistic`, decimals = 2) |>
    fmt_integer(columns = df) |>
    fmt_number(columns = `p-value`, decimals = 4) |>
    cols_align("center", columns = c(`LM statistic`, df, `p-value`)) |>
    tab_source_note(paste(
      "Engle's LM test, lag = 10. H\u2080: no ARCH effects.", SRC_NOTE)) |>
    gt_theme_dr()
  save_table(arch_tbl, "tables/arch_lm.png")
  garch_fits <- map(ret_xts,
                    ~ ugarchfit(spec = .garch_spec("std"), data = .x, solver = "hybrid"))
  garch_tbl_df <- imap_dfr(garch_fits, function(fit, nm) {
    cf <- coef(fit)
    pv <- fit@fit$robust.matcoef[, 4]
    z  <- as.numeric(residuals(fit, standardize = TRUE))
    tibble(
      Ticker      = nm,
      mu          = sprintf("%.2e%s", cf["mu"],     stars(pv["mu"])),
      omega       = sprintf("%.2e%s", cf["omega"],  stars(pv["omega"])),
      alpha       = sprintf("%.4f%s", cf["alpha1"], stars(pv["alpha1"])),
      beta        = sprintf("%.4f%s", cf["beta1"],  stars(pv["beta1"])),
      persistence = as.numeric(persistence(fit)),
      lr_vol      = sqrt(uncvariance(fit)) * sqrt(252) * 100,
      lb2_p       = Box.test(z^2, lag = 10, type = "Ljung-Box", fitdf = 2)$p.value
    )
  })
  garch_tbl <- garch_tbl_df |>
    gt(rowname_col = "Ticker") |>
    tab_header(
      title    = "GARCH(1,1) Volatility Models",
      subtitle = "ARIMA(0,0,0)-GARCH(1,1), daily log returns, 2020–2024"
    ) |>
    fmt_number(columns = persistence, decimals = 4) |>
    fmt_number(columns = lr_vol,      decimals = 2) |>
    fmt_number(columns = lb2_p,       decimals = 4) |>
    cols_label(
      mu          = "\u03bc",
      omega       = "\u03c9",
      alpha       = "\u03b1\u2081",
      beta        = "\u03b2\u2081",
      persistence = "\u03b1\u2081+\u03b2\u2081",
      lr_vol      = "Long-run vol (ann. %)",
      lb2_p       = "LB\u00b2(10) p"
    ) |>
    cols_align("center",
               columns = c(mu, omega, alpha, beta, persistence, lr_vol, lb2_p)) |>
    tab_source_note(paste(
      "Robust significance: * p<0.05, ** p<0.01, *** p<0.001.",
      "LB\u00b2(10) p = Ljung-Box p-value on squared standardised residuals",
      "(H\u2080: no remaining ARCH).", SRC_NOTE)) |>
    gt_theme_dr()
  save_table(garch_tbl, "tables/garch_params.png")
  cvol_df <- imap_dfr(garch_fits, function(fit, nm) {
    s <- sigma(fit)
    tibble(Date = index(s), Ticker = nm, csd = as.numeric(s) * 100)
  })
  tk_last <- tail(dat$tickers, 1)
  vol_panels <- map(set_names(dat$tickers), function(nm) {
    .cond_vol_panel(filter(cvol_df, Ticker == nm), nm, dat$cols[[nm]],
                    show_x = (nm == tk_last))
  })
  f_vol <- purrr::reduce(vol_panels, `/`) +
    plot_annotation(
      title    = "Conditional Volatility- KO, NVDA & XOM",
      subtitle = "Daily conditional SD from ARIMA(0,0,0)-GARCH(1,1), 2020–2024",
      theme    = theme_dark_roboto()
    )
  save_plot(f_vol, "plots/cond_volatility.png", width = 10, height = 12)
  invisible(list(arch = arch_tbl_df, garch = garch_tbl_df, fits = garch_fits))
}