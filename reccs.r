# reccs.r ─ Portfolio construction and risk comparison
# This produces:
#     tables/portfolio_compare.png
#     plots/efficient_frontier.png
#     tables/var_1day.png

# Key Variable, basically the crux of the whole assignment is w_rec. 
# Play around with this.

run_reccs <- function(dat,
                      A0_total = 1500000,
                      p_var    = 0.05,
                      w_rec    = c(KO = 0.15, NVDA = 0.6, XOM = 0.25)) {
  if (is.null(dat$reg_df))
    stop("run_reccs() needs the risk-free rate from Fama-French factors; check ffm_path.")

  R         <- dat$R
  dates     <- dat$dates
  tickers   <- dat$tickers
  mu_ann    <- dat$mu_ann
  Sigma_ann <- dat$Sigma_ann
  sig_ann   <- dat$sig_ann
  rf_ann    <- mean(dat$reg_df$RF) * 252

  port_stats <- function(w) {
    w   <- w[tickers]
    ret <- sum(w * mu_ann)
    vol <- sqrt(as.numeric(t(w) %*% Sigma_ann %*% w))
    c(ret = ret, vol = vol, sharpe = (ret - rf_ann) / vol)
  }

  w_ew <- setNames(rep(1 / 3, 3), tickers)
  w_iv <- setNames((1 / sig_ann) / sum(1 / sig_ann), tickers)

  s <- seq(0, 1, by = 0.01)
  g <- expand.grid(KO = s, NVDA = s)
  g$XOM <- 1 - g$KO - g$NVDA
  g <- g[g$XOM >= -1e-9 & g$XOM <= 1 + 1e-9, ]
  g$XOM <- pmax(g$XOM, 0)
  W     <- as.matrix(g[, tickers])
  
  ret_v <- as.numeric(W %*% mu_ann)
  vol_v <- sqrt(rowSums((W %*% Sigma_ann) * W))
  shp_v <- (ret_v - rf_ann) / vol_v
  
  w_mv <- setNames(W[which.min(vol_v), ], tickers)
  w_ms <- setNames(W[which.max(shp_v), ], tickers)
  w_rec <- w_rec[tickers]
  
  stopifnot(abs(sum(w_rec) - 1) < 1e-8, all(w_rec >= 0))
  
  cand <- list(
    `Recommended`      = w_rec,
    `Equal-weight`     = w_ew,
    `Minimum-variance` = w_mv,
    `Inverse-vol`      = w_iv,
    `Maximum-Sharpe`   = w_ms
  )
  compare_df <- imap_dfr(cand, function(w, nm) {
    st <- port_stats(w)
    tibble(
      Portfolio = nm,
      KO = w["KO"], NVDA = w["NVDA"], XOM = w["XOM"],
      `Exp. return` = st["ret"], Volatility = st["vol"], Sharpe = st["sharpe"]
    )
  })
  compare_tbl <- compare_df |>
    gt(rowname_col = "Portfolio") |>
    tab_header(
      title    = "Portfolio Comparison- Weights, Risk & Return",
      subtitle = "Annualised expected return and volatility; daily log returns, 2020–2024"
    ) |>
    tab_spanner(label = "Weights", columns = c(KO, NVDA, XOM)) |>
    fmt_percent(columns = c(KO, NVDA, XOM, `Exp. return`, Volatility), decimals = 1) |>
    fmt_number(columns = Sharpe, decimals = 3) |>
    cols_align("center", columns = c(KO, NVDA, XOM, `Exp. return`, Volatility, Sharpe)) |>
    tab_source_note(sprintf(
      "Risk-free rate %.1f%% p.a. Returns are log-based annualisations (\u00d7252). Source: Yahoo Finance via quantmod; author's calculations.",
      rf_ann * 100)) |>
    gt_theme_dr()
  save_table(compare_tbl, "tables/portfolio_compare.png")
  frontier_df <- tibble(vol = vol_v * 100, ret = ret_v * 100, sharpe = shp_v)
  cand_df <- imap_dfr(cand, function(w, nm) {
    st <- port_stats(w)
    tibble(Portfolio = nm, vol = st["vol"] * 100, ret = st["ret"] * 100)
  })
  f_frontier <- ggplot(frontier_df, aes(vol, ret)) +
    geom_point(aes(colour = sharpe), size = 0.6, alpha = 0.55) +
    scale_colour_gradient(low = "#5B9BD5", high = "#F4845F", name = "Sharpe") +
    geom_point(data = cand_df, aes(fill = Portfolio),
               shape = 21, colour = "white", size = 3.4, stroke = 0.6) +
    scale_fill_manual(values = c(
      `Recommended`      = "#E8C44F",
      `Equal-weight`     = "#A57BCC",
      `Minimum-variance` = "#4FC6C6",
      `Inverse-vol`      = "#E96F8B",
      `Maximum-Sharpe`   = "#56C596"
    ), name = NULL) +
    geom_text_repel(
      data = cand_df, aes(label = Portfolio),
      colour = "#FFFFFF", bg.color = "#1A1D23", bg.r = 0.18,
      family = "roboto", size = 5.4, box.padding = 0.8, point.padding = 0.5,
      min.segment.length = 0, segment.color = "#9AA0AD", segment.size = 0.3,
      max.overlaps = Inf, seed = 1
    ) +
    labs(
      title    = "Efficient Frontier- KO, NVDA & XOM",
      subtitle = "Long-only portfolios; annualised risk vs return, 2020–2024",
      x        = "Annualised volatility (%)",
      y        = "Annualised expected return (%)"
    ) +
    theme_dark_roboto() +
    theme(plot.title = element_text(hjust = 0.5))
  save_plot(f_frontier, "plots/efficient_frontier.png", width = 10, height = 7)
  var1d <- function(fit, A0, p = p_var) {
    fc    <- ugarchforecast(fit, n.ahead = 1)
    mu_f  <- as.numeric(fitted(fc))
    sig_f <- as.numeric(sigma(fc))
    dist  <- fit@model$modeldesc$distribution
    q05   <- if (dist == "std")
      rugarch::qdist("std", p, mu = mu_f, sigma = sig_f, shape = coef(fit)["shape"])
    else
      rugarch::qdist("norm", p, mu = mu_f, sigma = sig_f)
    c(sigma = sig_f, var_pct = -q05, var_dollar = -q05 * A0)
  }
  var_rows <- function(series_list, A0_vec) {
    imap_dfr(series_list, function(x, nm) {
      fn <- ugarchfit(.garch_spec("norm"), data = x, solver = "hybrid")
      ft <- ugarchfit(.garch_spec("std"),  data = x, solver = "hybrid")
      vn <- var1d(fn, A0_vec[[nm]])
      vt <- var1d(ft, A0_vec[[nm]])
      tibble(
        Item         = nm,
        Allocation   = A0_vec[[nm]],
        `VaR Normal` = vn["var_dollar"],
        `VaR t`      = vt["var_dollar"],
        `VaR t (%)`  = vt["var_pct"]
      )
    })
  }
  asset_xts <- dat$ret_xts
  port_xts  <- list(
    `Recommended portfolio`  = xts(as.numeric(R %*% w_rec), order.by = dates),
    `Equal-weight portfolio` = xts(as.numeric(R %*% w_ew),  order.by = dates)
  )
  A0_asset <- A0_total * w_rec
  A0_port  <- c(`Recommended portfolio` = A0_total, `Equal-weight portfolio` = A0_total)
  var_assets <- var_rows(asset_xts, A0_asset)
  var_ports  <- var_rows(port_xts, as.list(A0_port))
  var_df     <- bind_rows(var_assets, var_ports)
  div_benefit_norm <- sum(var_assets$`VaR Normal`) - var_ports$`VaR Normal`[1]
  div_benefit_t    <- sum(var_assets$`VaR t`)      - var_ports$`VaR t`[1]
  var_tbl <- var_df |>
    gt(rowname_col = "Item") |>
    tab_header(
      title    = "1-Day 95% Conditional Value-at-Risk",
      subtitle = "ARIMA(0,0,0)-GARCH(1,1) next-day forecast on a $1.5m mandate"
    ) |>
    fmt_currency(columns = c(Allocation, `VaR Normal`, `VaR t`),
                 currency = "USD", decimals = 0) |>
    fmt_percent(columns = `VaR t (%)`, decimals = 2) |>
    cols_label(
      `VaR Normal` = "VaR 95% (Normal)",
      `VaR t`      = "VaR 95% (Student-t)",
      `VaR t (%)`  = "VaR 95% (% of position, t)"
    ) |>
    cols_align("center", columns = c(Allocation, `VaR Normal`, `VaR t`, `VaR t (%)`)) |>
    tab_source_note(sprintf(
      "Per-company VaR is on its slice of the recommended allocation. Diversification benefit (recommended): $%s (Normal), $%s (Student-t). Student-t fitted to capture heavy tails. Source: Yahoo Finance via quantmod; author's calculations.",
      format(round(div_benefit_norm), big.mark = ","),
      format(round(div_benefit_t),    big.mark = ","))) |>
    gt_theme_dr()
  save_table(var_tbl, "tables/var_1day.png")
  invisible(list(compare = compare_df, var = var_df,
                 weights = cand, frontier = frontier_df))
}