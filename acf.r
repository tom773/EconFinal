# ACF / PACF correlograms + Ljung-Box tests 
# This script produces:
#     plots/acf.png
#     plots/pacf.png
#     tables/ljung_box.png

.acf_tidy <- function(x, lag_max = 20, type = c("correlation", "partial")) {
  type <- match.arg(type)
  a <- if (type == "correlation")
    acf(x, lag.max = lag_max, plot = FALSE)
  else
    pacf(x, lag.max = lag_max, plot = FALSE)
  tibble(lag = as.numeric(a$lag), value = as.numeric(a$acf)) |>
    filter(lag >= 1)
}

.correlogram <- function(x, nm, colour, lag_max = 20,
                         type = "correlation", ylim = NULL, show_x = FALSE) {
  n     <- length(x)
  ci    <- qnorm(0.975) / sqrt(n)
  d     <- .acf_tidy(x, lag_max, type)
  y_lab <- if (type == "correlation") "ACF" else "PACF"
  coord <- if (!is.null(ylim)) coord_cartesian(ylim = ylim) else NULL

  ggplot(d, aes(x = lag, y = value)) +
    geom_hline(yintercept = 0, colour = "#4A4F61", linewidth = 0.4) +
    geom_hline(yintercept = c(-ci, ci), linetype = "dashed",
               colour = "#9AA0AD", linewidth = 0.35) +
    geom_segment(aes(xend = lag, yend = 0), colour = colour, linewidth = 0.7) +
    geom_point(colour = colour, size = 1.2) +
    scale_x_continuous(breaks = seq(0, lag_max, by = 5)) +
    coord +
    labs(title = nm, x = if (show_x) "Lag" else NULL, y = y_lab) +
    theme_dark_roboto() + layout_fix
}


.correlogram_stack <- function(dat, type, ylim) {
  tk_last <- tail(dat$tickers, 1)
  plots <- imap(dat$ret_vecs, function(x, nm) {
    .correlogram(x, TICKER_LABELS[[nm]], dat$cols[[nm]],
                 type = type, ylim = ylim, show_x = (nm == tk_last))
  })
  purrr::reduce(plots, `/`)
}

run_acf <- function(dat) {
  ret_vecs <- dat$ret_vecs

  acf_rng  <- map_dbl(ret_vecs, ~ max(abs(.acf_tidy(.x, 20)$value)))
  ylim_acf <- c(-1, 1) * max(acf_rng, 0.06) * 1.15


  f_acf <- .correlogram_stack(dat, "correlation", ylim_acf) +
    plot_annotation(
      title    = "Return Autocorrelation (Correlogram)- KO, NVDA & XOM",
      subtitle = "ACF of daily log returns with 95% white-noise bands, 2020–2024",
      theme    = theme_dark_roboto()
    )
  save_plot(f_acf, "plots/acf.png", width = 10, height = 12)


  f_pacf <- .correlogram_stack(dat, "partial", ylim_acf) +
    plot_annotation(
      title    = "Partial Autocorrelation (PACF)- KO, NVDA & XOM",
      subtitle = "PACF of daily log returns, 2020–2024",
      theme    = theme_dark_roboto()
    )
  save_plot(f_pacf, "plots/pacf.png", width = 10, height = 12)


  lb_lag <- 10
  lb_tbl_df <- imap_dfr(ret_vecs, function(x, nm) {
    lb_r <- Box.test(x,   lag = lb_lag, type = "Ljung-Box")
    lb_s <- Box.test(x^2, lag = lb_lag, type = "Ljung-Box")
    tibble(
      Ticker = nm,
      Q_ret  = unname(lb_r$statistic), p_ret = lb_r$p.value,
      Q_sq   = unname(lb_s$statistic), p_sq  = lb_s$p.value
    )
  })

  lb_tbl <- lb_tbl_df |>
    gt(rowname_col = "Ticker") |>
    tab_header(
      title    = "Ljung-Box Autocorrelation Tests",
      subtitle = "Daily log returns vs squared returns, lag = 10, 2020–2024"
    ) |>
    fmt_number(columns = c(Q_ret, Q_sq), decimals = 2) |>
    fmt_number(columns = c(p_ret, p_sq), decimals = 4) |>
    cols_label(Q_ret = "Q(10)", p_ret = "p-value",
               Q_sq  = "Q(10)", p_sq  = "p-value") |>
    tab_spanner(label = "Returns",         columns = c(Q_ret, p_ret)) |>
    tab_spanner(label = "Squared returns", columns = c(Q_sq, p_sq)) |>
    cols_align("center", columns = c(Q_ret, p_ret, Q_sq, p_sq)) |>
    tab_source_note(paste("H\u2080: no autocorrelation up to lag 10.", SRC_NOTE)) |>
    gt_theme_dr()
  save_table(lb_tbl, "tables/ljung_box.png")

  invisible(list(ljung_box = lb_tbl_df, acf_plot = f_acf, pacf_plot = f_pacf))
} 