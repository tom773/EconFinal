# corr.r ─ Correlation and covariance matrices, rolling correlations 
# This produces:
#     tables/corr_matrix.png
#     tables/cov_matrix.png
#     plots/rolling_corr.png

.mat_to_tbl <- function(m) as_tibble(m, rownames = "Ticker")

run_corr <- function(dat) {
  R         <- dat$R
  dates     <- dat$dates
  Sigma_ann <- dat$Sigma_ann
  corr_mat <- cor(R)
  cat("Annualised vol (sqrt diag, %):",
      round(sqrt(diag(Sigma_ann)) * 100, 2), "\n")
  corr_tbl <- .mat_to_tbl(corr_mat) |>
    gt(rowname_col = "Ticker") |>
    tab_header(
      title    = "Return Correlation Matrix",
      subtitle = "Pearson correlation of daily log returns- KO, NVDA & XOM, 2020–2024"
    ) |>
    fmt_number(columns = c(KO, NVDA, XOM), decimals = 3) |>
    cols_align("center", columns = c(KO, NVDA, XOM)) |>
    tab_source_note(SRC_NOTE) |>
    gt_theme_dr()
  save_table(corr_tbl, "tables/corr_matrix.png")
  cov_tbl <- .mat_to_tbl(Sigma_ann) |>
    gt(rowname_col = "Ticker") |>
    tab_header(
      title    = "Annualised Covariance Matrix",
      subtitle = "Daily log-return covariance \u00d7 252- KO, NVDA & XOM, 2020–2024"
    ) |>
    fmt_number(columns = c(KO, NVDA, XOM), decimals = 5) |>
    cols_align("center", columns = c(KO, NVDA, XOM)) |>
    tab_source_note(paste(
      "Diagonal = annualised variance; \u221adiagonal = annualised volatility.",
      SRC_NOTE)) |>
    gt_theme_dr()
  save_table(cov_tbl, "tables/cov_matrix.png")
  win <- 63
  roll_corr <- tibble(
    Date       = dates,
    `KO–NVDA`  = runCor(R[, "KO"],   R[, "NVDA"], n = win),
    `KO–XOM`   = runCor(R[, "KO"],   R[, "XOM"],  n = win),
    `NVDA–XOM` = runCor(R[, "NVDA"], R[, "XOM"],  n = win)
  ) |>
    pivot_longer(-Date, names_to = "Pair", values_to = "Correlation") |>
    drop_na(Correlation)
  pair_cols <- c(`KO–NVDA` = "#E8C44F", `KO–XOM` = "#A57BCC", `NVDA–XOM` = "#4FC6C6")
  f_roll <- ggplot(roll_corr, aes(Date, Correlation, colour = Pair)) +
    geom_hline(yintercept = 0, colour = "#4A4F61", linewidth = 0.4) +
    geom_line(linewidth = 0.5) +
    scale_colour_manual(values = pair_cols) +
    coord_cartesian(ylim = c(-1, 1)) +
    labs(
      title    = "Rolling 63-Day Return Correlations- KO, NVDA & XOM",
      subtitle = "Trailing one-quarter Pearson correlation of daily log returns, 2020–2024",
      x        = "Date",
      y        = "Correlation",
      colour   = NULL
    ) +
    theme_dark_roboto() +
    theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
  save_plot(f_roll, "plots/rolling_corr.png", width = 10, height = 6)
  invisible(list(corr = corr_mat, cov_ann = Sigma_ann, rolling = roll_corr))
}