source("main.r")
source("theme_dark_roboto_table.R")
ret_vecs <- map(rets, ~ as.numeric(.x)[-1])
ko_x   <- ret_vecs$KO
nvda_x <- ret_vecs$NVDA
xom_x  <- ret_vecs$XOM

acf_tidy <- function(x, lag_max = 20, type = c("correlation", "partial")) {
    type <- match.arg(type)
    a <- if (type == "correlation")
        acf(x, lag.max = lag_max, plot = FALSE)
    else
        pacf(x, lag.max = lag_max, plot = FALSE)
    tibble(lag = as.numeric(a$lag), value = as.numeric(a$acf)) |>
        filter(lag >= 1)                       # drop lag 0 (always = 1 for ACF)
}
 
make_correlogram <- function(x, nm, colour, lag_max = 20,
                             type = "correlation", ylim = NULL, show_x = FALSE) {
    n     <- length(x)
    ci    <- qnorm(0.975) / sqrt(n)            # ±95% white-noise band
    d     <- acf_tidy(x, lag_max, type)
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
 
acf_rng  <- map_dbl(ret_vecs, ~ max(abs(acf_tidy(.x, 20)$value)))
ylim_acf <- c(-1, 1) * max(acf_rng, 0.06) * 1.15
 
acf_ko   <- make_correlogram(ko_x,   "Coca-Cola (KO)",   "#F4845F", ylim = ylim_acf)
acf_nvda <- make_correlogram(nvda_x, "NVIDIA (NVDA)",    "#56C596", ylim = ylim_acf)
acf_xom  <- make_correlogram(xom_x,  "ExxonMobil (XOM)", "#5B9BD5", ylim = ylim_acf,
                             show_x = TRUE)
 
f_acf <- acf_ko / acf_nvda / acf_xom +
    plot_annotation(
        title    = "Return Autocorrelation (Correlogram)- KO, NVDA & XOM",
        subtitle = "ACF of daily log returns with 95% white-noise bands, 2020–2024",
        theme    = theme_dark_roboto()
    )
 
ggsave("plots/acf.png", f_acf, width = 10, height = 12, dpi = 300)
 

pacf_ko   <- make_correlogram(ko_x,   "Coca-Cola (KO)",   "#F4845F", type = "partial", ylim = ylim_acf)
pacf_nvda <- make_correlogram(nvda_x, "NVIDIA (NVDA)",    "#56C596", type = "partial", ylim = ylim_acf)
pacf_xom  <- make_correlogram(xom_x,  "ExxonMobil (XOM)", "#5B9BD5", type = "partial", ylim = ylim_acf,
                              show_x = TRUE)
f_pacf <- pacf_ko / pacf_nvda / pacf_xom +
    plot_annotation(
        title    = "Partial Autocorrelation (PACF)- KO, NVDA & XOM",
        subtitle = "PACF of daily log returns, 2020–2024",
        theme    = theme_dark_roboto()
    )
ggsave("plots/pacf.png", f_pacf, width = 10, height = 12, dpi = 300)
 
 
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
 
print(lb_tbl_df)
 
lb_tbl <- lb_tbl_df |>
    gt(rowname_col = "Ticker") |>
    tab_header(
        title    = "Ljung-Box Autocorrelation Tests",
        subtitle = "Daily log returns vs squared returns, lag = 10, 2020–2024"
    ) |>
    fmt_number(columns = c(Q_ret, Q_sq), decimals = 2) |>
    fmt_number(columns = c(p_ret, p_sq), decimals = 4) |>
    cols_label(
        Q_ret = "Q(10)", p_ret = "p-value",
        Q_sq  = "Q(10)", p_sq  = "p-value"
    ) |>
    tab_spanner(label = "Returns",         columns = c(Q_ret, p_ret)) |>
    tab_spanner(label = "Squared returns", columns = c(Q_sq, p_sq)) |>
    cols_align("center", columns = c(Q_ret, p_ret, Q_sq, p_sq)) |>
    tab_source_note(
        "H\u2080: no autocorrelation up to lag 10. Source: Yahoo Finance via quantmods."
    ) |>
    gt_theme_dr()
 
gtsave(lb_tbl, "tables/ljung_box.png", zoom = 3, expand = 10)