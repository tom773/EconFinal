run_adf <- function(dat) {
  adf_tbl_df <- imap_dfr(dat$ret_vecs, function(x, nm) {
    a <- suppressWarnings(adf.test(x, alternative = "stationary"))
    tibble(
      Ticker          = nm,
      `ADF statistic` = unname(a$statistic),
      `Lag order`     = unname(a$parameter),
      `p-value`       = a$p.value
    )
  })

  adf_tbl <- adf_tbl_df |>
    gt(rowname_col = "Ticker") |>
    tab_header(
      title    = "Augmented Dickey-Fuller Unit Root Tests",
      subtitle = "Daily log returns- KO, NVDA & XOM, 2020–2024"
    ) |>
    fmt_number(columns = `ADF statistic`, decimals = 3) |>
    fmt_integer(columns = `Lag order`) |>
    fmt_number(columns = `p-value`, decimals = 3) |>
    cols_align("center", columns = c(`ADF statistic`, `Lag order`, `p-value`)) |>
    tab_source_note(paste(
      "H\u2080: unit root (non-stationary).",
      "tseries reports a floor of 0.01 on the p-value.", SRC_NOTE)) |>
    gt_theme_dr()
  save_table(adf_tbl, "tables/adf_tests.png")

  invisible(adf_tbl_df)
}