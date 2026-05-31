# CAPM and Fama-French 3-Factor Regressions
# This script produces:
#     tables/ff3_compare.png

.fit_tidy <- function(reg_df, formula) {
  reg_df |>
    nest(data = -Ticker) |>
    mutate(
      model  = map(data, \(d) lm(formula, data = d)),
      tidied = map(model, \(m) broom::tidy(m, conf.int = TRUE))
    ) |>
    select(Ticker, tidied) |>
    unnest(tidied)
}


.coef_cells <- function(model, ticker) {
  co <- broom::tidy(model) |>
    transmute(term, cell = paste0(sprintf("%.4f", estimate), stars(p.value)))
  gl <- broom::glance(model)
  tibble(Ticker = ticker, term = co$term, cell = co$cell,
         R2 = gl$r.squared, AdjR2 = gl$adj.r.squared, N = gl$nobs)
}

run_capm <- function(dat) {
  if (is.null(dat$reg_df))
    stop("run_capm() needs Fama-French factors; check ffm_path in prepare_data().")
  reg_df <- dat$reg_df

  capm_tidy <- .fit_tidy(reg_df, excess_ret ~ Mkt_RF)
  ff3_tidy  <- .fit_tidy(reg_df, excess_ret ~ Mkt_RF + SMB + HML)

  coef_tab <- reg_df |>
    group_split(Ticker) |>
    map_dfr(\(d) .coef_cells(lm(excess_ret ~ Mkt_RF + SMB + HML, data = d),
                             d$Ticker[1]))

  wide <- coef_tab |>
    pivot_wider(names_from = term, values_from = cell) |>
    select(Ticker, Alpha = `(Intercept)`, `Mkt-RF` = Mkt_RF, SMB, HML,
           R2, AdjR2, N)

  reg_tbl <- wide |>
    gt(rowname_col = "Ticker") |>
    fmt_markdown(columns = c(Alpha, `Mkt-RF`, SMB, HML)) |>
    fmt_number(columns = c(R2, AdjR2), decimals = 3) |>
    fmt_integer(columns = N) |>
    cols_label(R2 = "R²", AdjR2 = "Adj. R²") |>
    cols_align("center", columns = c(Alpha, `Mkt-RF`, SMB, HML, R2, AdjR2, N)) |>
    tab_header(
      title    = "Fama-French 3-Factor Regressions",
      subtitle = "Daily excess log returns - KO, NVDA & XOM, 2020–2024"
    ) |>
    tab_source_note(paste(
      "Estimates with significance stars. * p<0.05, ** p<0.01, *** p<0.001.",
      "Source: Yahoo Finance via quantmod; Fama-French factors.")) |>
    gt_theme_dr()
  save_table(reg_tbl, "tables/ff3_compare.png")

  invisible(list(capm = capm_tidy, ff3 = ff3_tidy, ff3_table = reg_tbl))
}