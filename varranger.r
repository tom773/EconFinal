# VarGranger.r ─ Out-of-sample & multivariate extension
# Adds the two analyses the in-sample pipeline doesn't yet cover:
#   (A) VAR(p) estimation + Granger causality between KO, NVDA & XOM
#   (B) 2025 out-of-sample performance evaluation of the recommended book
#
# Produces:
#   VAR / Granger
#     tables/var_lag_select.png
#     tables/var_coefs.png
#     tables/granger_pairwise.png
#     tables/granger_block.png
#     plots/var_irf.png
#     plots/var_fevd.png
#   2025 evaluation
#     tables/eval_asset_2025.png
#     tables/eval_summary.png
#     tables/eval_exante_vs_real.png
#     tables/eval_var_backtest.png
#     plots/eval_cumulative_2025.png
#     plots/eval_drawdown_2025.png
#     plots/eval_asset_returns_2025.png
#     plots/eval_var_backtest.png

.var_coef_tidy <- function(v) {
  imap_dfr(coef(v), function(m, eqn) {
    as_tibble(m, rownames = "term") |>
      transmute(
        Equation = eqn,
        term     = term,
        cell     = paste0(sprintf("%.4f", Estimate), stars(`Pr(>|t|)`))
      )
  })
}

.tidy_irf <- function(ir) {
  imap_dfr(ir$irf, function(mat, imp) {
    H <- nrow(mat)
    tibble(
      impulse  = imp,
      horizon  = rep(seq_len(H) - 1L, times = ncol(mat)),
      response = rep(colnames(mat), each = H),
      irf      = as.numeric(mat),
      lower    = as.numeric(ir$Lower[[imp]]),
      upper    = as.numeric(ir$Upper[[imp]])
    )
  })
}

.tidy_fevd <- function(fe) {
  imap_dfr(fe, function(mat, resp) {
    H <- nrow(mat)
    tibble(
      response = resp,
      horizon  = rep(seq_len(H), times = ncol(mat)),
      shock    = rep(colnames(mat), each = H),
      share    = as.numeric(mat)
    )
  })
}

run_var_granger <- function(dat, p = NULL, lag_max = 10) {
  dir.create("tables", showWarnings = FALSE)
  dir.create("plots",  showWarnings = FALSE)

  tickers <- dat$tickers
  R       <- dat$R[, tickers]                 # 2020–2024 log-return matrix

  # Lag-order selection (AIC / HQ / SC / FPE)
  sel  <- vars::VARselect(R, lag.max = lag_max, type = "const")
  crit <- t(sel$criteria)                     # rows = lags, cols = criteria

  sel_df <- as_tibble(crit, rownames = "Lag") |>
    mutate(Lag = as.integer(Lag)) |>
    rename(AIC = `AIC(n)`, HQ = `HQ(n)`, SC = `SC(n)`, FPE = `FPE(n)`)

  star_min <- function(x) ifelse(x == min(x), "*", "")
  sel_show <- sel_df |>
    mutate(across(c(AIC, HQ, SC), ~ paste0(sprintf("%.4f", .x), star_min(.x))),
           FPE = paste0(sprintf("%.3e", FPE), star_min(FPE)))

  if (is.null(p)) p <- as.integer(sel$selection["SC(n)"])   # parsimonious default

  lag_tbl <- sel_show |>
    gt(rowname_col = "Lag") |>
    tab_header(
      title    = "VAR Lag-Order Selection",
      subtitle = "Information criteria by lag- KO, NVDA & XOM daily log returns, 2020–2024"
    ) |>
    cols_align("center", columns = c(AIC, HQ, SC, FPE)) |>
    tab_source_note(sprintf(
      "* = minimum for that criterion. Selected order p = %d (SC / most parsimonious). %s",
      p, SRC_NOTE)) |>
    gt_theme_dr()
  save_table(lag_tbl, "tables/var_lag_select.png")

  # VAR(p) estimation
  v <- vars::VAR(R, p = p, type = "const")

  coef_wide <- .var_coef_tidy(v) |>
    pivot_wider(names_from = Equation, values_from = cell)

  coef_tbl <- coef_wide |>
    gt(rowname_col = "term") |>
    tab_header(
      title    = sprintf("VAR(%d) Coefficient Estimates", p),
      subtitle = "Each column is one equation (dependent variable); rows are the regressors"
    ) |>
    cols_align("center", columns = all_of(tickers)) |>
    tab_source_note(paste(
      "Significance: * p<0.05, ** p<0.01, *** p<0.001.",
      "Off-diagonal lag terms (e.g. NVDA.l1 in the KO equation) are the Granger-causal channels.",
      SRC_NOTE)) |>
    gt_theme_dr()
  save_table(coef_tbl, "tables/var_coefs.png")

  # Granger causality- pairwise, both directions 
  pairs <- combn(tickers, 2, simplify = FALSE)
  gc_pairwise <- map_dfr(pairs, function(pr) {
    vp <- vars::VAR(R[, pr], p = p, type = "const")
    map_dfr(pr, function(cz) {
      g <- vars::causality(vp, cause = cz)$Granger
      tibble(
        Cause     = cz,
        Effect    = setdiff(pr, cz),
        `F-stat`  = as.numeric(g$statistic),
        df1       = as.numeric(g$parameter[1]),
        df2       = as.numeric(g$parameter[2]),
        `p-value` = as.numeric(g$p.value)
      )
    })
  }) |>
    mutate(`Granger-causal? (5%)` =
             ifelse(`p-value` < 0.05, paste0("Yes", stars(`p-value`)), "No"))

  gc_tbl <- gc_pairwise |>
    gt() |>
    tab_header(
      title    = "Pairwise Granger Causality Tests",
      subtitle = sprintf("Bivariate VAR(%d); F-test that the cause's lags are jointly zero", p)
    ) |>
    fmt_number(columns = `F-stat`, decimals = 3) |>
    fmt_integer(columns = c(df1, df2)) |>
    fmt_number(columns = `p-value`, decimals = 4) |>
    cols_align("center",
               columns = c(`F-stat`, df1, df2, `p-value`, `Granger-causal? (5%)`)) |>
    tab_source_note(paste(
      "H\u2080: the cause does not Granger-cause the effect. Reject if p < 0.05.",
      "* p<0.05, ** p<0.01, *** p<0.001.", SRC_NOTE)) |>
    gt_theme_dr()
  save_table(gc_tbl, "tables/granger_pairwise.png")

  # Granger causality- multivariate block test (each name vs the rest)
  gc_block <- map_dfr(tickers, function(cz) {
    cc <- vars::causality(v, cause = cz)
    tibble(
      Cause                        = cz,
      `Block F`                    = as.numeric(cc$Granger$statistic),
      `Block p`                    = as.numeric(cc$Granger$p.value),
      `Instantaneous χ²`           = as.numeric(cc$Instant$statistic),
      `Instantaneous p`            = as.numeric(cc$Instant$p.value)
    )
  })

  gc_block_tbl <- gc_block |>
    gt(rowname_col = "Cause") |>
    tab_header(
      title    = "Multivariate Granger & Instantaneous Causality",
      subtitle = sprintf("VAR(%d) system- each company tested against the other two jointly", p)
    ) |>
    fmt_number(columns = c(`Block F`, `Instantaneous χ²`), decimals = 3) |>
    fmt_number(columns = c(`Block p`, `Instantaneous p`), decimals = 4) |>
    cols_align("center", columns = everything()) |>
    tab_source_note(paste(
      "Block H\u2080: the named company Granger-causes neither of the others.",
      "Instantaneous H\u2080: no contemporaneous correlation.", SRC_NOTE)) |>
    gt_theme_dr()
  save_table(gc_block_tbl, "tables/granger_block.png")

  # Impulse responses
  ir   <- vars::irf(v, n.ahead = 10, boot = TRUE, ci = 0.95, runs = 200, seed = 1)
  ir_d <- .tidy_irf(ir) |>
    mutate(impulse  = factor(impulse,  levels = tickers),
           response = factor(response, levels = tickers))

  f_irf <- ggplot(ir_d, aes(horizon, irf)) +
    geom_hline(yintercept = 0, colour = "#4A4F61", linewidth = 0.4) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#5B9BD5", alpha = 0.18) +
    geom_line(colour = "#5B9BD5", linewidth = 0.7) +
    facet_grid(response ~ impulse, switch = "y") +
    labs(
      title    = "Impulse Response Functions- KO, NVDA & XOM",
      subtitle = sprintf(
        "Response (rows) to a one-SD shock in each series (columns); VAR(%d), 95%% bootstrap bands", p),
      x = "Days ahead", y = "Response of daily log return"
    ) +
    theme_dark_roboto() +
    theme(plot.title = element_text(hjust = 0.5))
  save_plot(f_irf, "plots/var_irf.png", width = 11, height = 9)

  #Forecast error variance decomposition
  fe   <- vars::fevd(v, n.ahead = 10)
  fe_d <- .tidy_fevd(fe) |>
    mutate(response = factor(response, levels = tickers),
           shock    = factor(shock,    levels = tickers))

  f_fevd <- ggplot(fe_d, aes(horizon, share, fill = shock)) +
    geom_area(colour = "#1A1D23", linewidth = 0.2) +
    scale_fill_manual(values = TICKER_COLS, name = "Shock") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    facet_wrap(~ response, nrow = 1) +
    labs(
      title    = "Forecast Error Variance Decomposition- KO, NVDA & XOM",
      subtitle = sprintf(
        "Share of each series' forecast-error variance attributable to each shock; VAR(%d)", p),
      x = "Days ahead", y = "Share of variance"
    ) +
    theme_dark_roboto() +
    theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
  save_plot(f_fevd, "plots/var_fevd.png", width = 11, height = 5)

  invisible(list(
    p = p, varselect = sel, var = v,
    granger_pairwise = gc_pairwise, granger_block = gc_block,
    irf = ir, fevd = fe
  ))
}

Pull actual 2025 daily log returns (own fetch; mirrors prepare_data()).
.fetch_returns_2025 <- function(tickers, from = "2024-12-15", to = "2025-12-31") {
  rets <- map(set_names(tickers), function(tk) {
    px <- getSymbols(tk, auto.assign = FALSE, from = from, to = to)
    dailyReturn(Ad(px), type = "log")
  })
  rmat <- na.omit(do.call(merge, rets))
  colnames(rmat) <- tickers
  rmat[index(rmat) >= as.Date("2025-01-01"), ]    # keep calendar-2025 only
}

.max_drawdown <- function(value) min(value / cummax(value) - 1)

run_eval_2025 <- function(dat,
                          A0_total = 1500000,
                          p_var    = 0.05,
                          w_rec    = c(KO = 0.05, NVDA = 0.65, XOM = 0.30),
                          to_2025  = "2025-12-31") {

  dir.create("tables", showWarnings = FALSE)
  dir.create("plots",  showWarnings = FALSE)

  tickers <- dat$tickers
  w_rec   <- w_rec[tickers]
  w_ew    <- setNames(rep(1 / length(tickers), length(tickers)), tickers)
  rf_ann  <- if (!is.null(dat$reg_df)) mean(dat$reg_df$RF) * 252 else 0

  # Realised 2025 data 
  R25_xts <- .fetch_returns_2025(tickers, to = to_2025)
  dates25 <- index(R25_xts)
  R25_log <- coredata(R25_xts)[, tickers]
  R25_sim <- exp(R25_log) - 1                      # daily simple returns

  # Fixed-weight (daily-rebalanced) portfolio simple returns
  rec_sim <- as.numeric(R25_sim %*% w_rec)
  ew_sim  <- as.numeric(R25_sim %*% w_ew)

  val_rec <- A0_total * cumprod(1 + rec_sim)
  val_ew  <- A0_total * cumprod(1 + ew_sim)

  ann <- function(sim, val) {
    n <- length(sim)
    annret <- (val[n] / A0_total)^(252 / n) - 1
    annvol <- sd(sim) * sqrt(252)
    c(total  = val[n] / A0_total - 1,
      annret = annret,
      annvol = annvol,
      sharpe = (annret - rf_ann) / annvol,
      maxdd  = .max_drawdown(val),
      term   = val[n])
  }
  s_rec <- ann(rec_sim, val_rec)
  s_ew  <- ann(ew_sim,  val_ew)

  # Per-asset 2025 summary 
  asset_df <- map_dfr(tickers, function(tk) {
    sim <- R25_sim[, tk]; n <- length(sim); cumv <- cumprod(1 + sim)
    tibble(
      Ticker         = tk,
      `2025 return`  = cumv[n] - 1,
      `Ann. vol`     = sd(sim) * sqrt(252),
      `Max drawdown` = .max_drawdown(cumv),
      `Best day`     = max(sim),
      `Worst day`    = min(sim)
    )
  })
  asset_tbl <- asset_df |>
    gt(rowname_col = "Ticker") |>
    tab_header(
      title    = "Individual Stock Performance- Calendar 2025",
      subtitle = "Realised daily simple returns, actual out-of-sample data"
    ) |>
    fmt_percent(columns = c(`2025 return`, `Ann. vol`, `Max drawdown`,
                            `Best day`, `Worst day`), decimals = 2) |>
    cols_align("center", columns = everything()) |>
    tab_source_note(SRC_NOTE) |>
    gt_theme_dr()
  save_table(asset_tbl, "tables/eval_asset_2025.png")

  # Portfolio realised summary- Recommended vs Equal-weight 
summ_df <- tibble(
    Portfolio        = c("Recommended", "Equal-weight"),
    KO               = c(w_rec["KO"],   w_ew["KO"]),
    NVDA             = c(w_rec["NVDA"], w_ew["NVDA"]),
    XOM              = c(w_rec["XOM"],  w_ew["XOM"]),
    `2025 return`    = c(s_rec["total"],  s_ew["total"]),
    `Ann. return`    = c(s_rec["annret"], s_ew["annret"]),
    `Ann. vol`       = c(s_rec["annvol"], s_ew["annvol"]),
    Sharpe           = c(s_rec["sharpe"], s_ew["sharpe"]),
    `Max drawdown`   = c(s_rec["maxdd"],  s_ew["maxdd"]),
    `Terminal value` = c(s_rec["term"],   s_ew["term"])
  )
  summ_tbl <- summ_df |>
    gt(rowname_col = "Portfolio") |>
    tab_header(
      title    = "Recommended vs Equal-Weight- Realised 2025 Performance",
      subtitle = sprintf("$%s allocated on 31 Dec 2024; fixed-weight, daily-rebalanced",
                         format(A0_total, big.mark = ","))
    ) |>
    tab_spanner(label = "Weights", columns = c(KO, NVDA, XOM)) |>
    fmt_percent(columns = c(KO, NVDA, XOM, `2025 return`, `Ann. return`,
                            `Ann. vol`, `Max drawdown`), decimals = 1) |>
    fmt_number(columns = Sharpe, decimals = 3) |>
    fmt_currency(columns = `Terminal value`, currency = "USD", decimals = 0) |>
    cols_align("center", columns = everything()) |>
    tab_source_note(sprintf(
      "Risk-free %.1f%% p.a. (2020–2024 average). Sharpe on annualised figures. %s",
      rf_ann * 100, SRC_NOTE)) |>
    gt_theme_dr()
  save_table(summ_tbl, "tables/eval_summary.png")

  # Ex-ante (2020–2024) 
  exante <- function(w) {
    w   <- w[tickers]
    ret <- sum(w * dat$mu_ann)
    vol <- sqrt(as.numeric(t(w) %*% dat$Sigma_ann %*% w))
    c(ret = ret, vol = vol, sharpe = (ret - rf_ann) / vol)
  }
  ea_rec <- exante(w_rec); ea_ew <- exante(w_ew)

  cmp_df <- tibble(
    Portfolio                = c("Recommended", "Equal-weight"),
    `Exp. return (ex-ante)`  = c(ea_rec["ret"],    ea_ew["ret"]),
    `Realised return 2025`   = c(s_rec["annret"],  s_ew["annret"]),
    `Exp. vol (ex-ante)`     = c(ea_rec["vol"],    ea_ew["vol"]),
    `Realised vol 2025`      = c(s_rec["annvol"],  s_ew["annvol"]),
    `Sharpe (ex-ante)`       = c(ea_rec["sharpe"], ea_ew["sharpe"]),
    `Sharpe (realised)`      = c(s_rec["sharpe"],  s_ew["sharpe"])
  )
  cmp_tbl <- cmp_df |>
    gt(rowname_col = "Portfolio") |>
    tab_header(
      title    = "Did the Recommendation Hold Up? Ex-Ante vs Realised",
      subtitle = "Annualised expectations from 2020–2024 vs actual calendar-2025 outcomes"
    ) |>
    tab_spanner(label = "Return",
                columns = c(`Exp. return (ex-ante)`, `Realised return 2025`)) |>
    tab_spanner(label = "Volatility",
                columns = c(`Exp. vol (ex-ante)`, `Realised vol 2025`)) |>
    tab_spanner(label = "Sharpe",
                columns = c(`Sharpe (ex-ante)`, `Sharpe (realised)`)) |>
    fmt_percent(columns = c(`Exp. return (ex-ante)`, `Realised return 2025`,
                            `Exp. vol (ex-ante)`, `Realised vol 2025`), decimals = 1) |>
    fmt_number(columns = c(`Sharpe (ex-ante)`, `Sharpe (realised)`), decimals = 3) |>
    cols_align("center", columns = everything()) |>
    tab_source_note(paste(
      "Ex-ante figures use 2020–2024 annualised mean and covariance",
      "(the information set available at the recommendation date).", SRC_NOTE)) |>
    gt_theme_dr()
  save_table(cmp_tbl, "tables/eval_exante_vs_real.png")

  # Cumulative value chart 
  pal_port <- c(Recommended = "#E8C44F", `Equal-weight` = "#A57BCC")
  growth_df <- bind_rows(
    tibble(Date = dates25, Portfolio = "Recommended",  Value = val_rec),
    tibble(Date = dates25, Portfolio = "Equal-weight", Value = val_ew)
  )
  f_cum <- ggplot(growth_df, aes(Date, Value, colour = Portfolio)) +
    geom_hline(yintercept = A0_total, colour = "#4A4F61",
               linewidth = 0.4, linetype = "dashed") +
    geom_line(linewidth = 0.9) +
    scale_colour_manual(values = pal_port, name = NULL) +
    scale_y_continuous(labels = scales::dollar_format(prefix = "$", big.mark = ",")) +
    labs(
      title    = "Portfolio Value Through 2025- Recommended vs Equal-Weight",
      subtitle = sprintf("Growth of a $%s allocation made on 31 Dec 2024",
                         format(A0_total, big.mark = ",")),
      x = "Date", y = "Portfolio value"
    ) +
    theme_dark_roboto() +
    theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
  save_plot(f_cum, "plots/eval_cumulative_2025.png", width = 10, height = 6)

  # Drawdown chart 
  dd_df <- bind_rows(
    tibble(Date = dates25, Portfolio = "Recommended",  Drawdown = val_rec / cummax(val_rec) - 1),
    tibble(Date = dates25, Portfolio = "Equal-weight", Drawdown = val_ew  / cummax(val_ew)  - 1)
  )
  f_dd <- ggplot(dd_df, aes(Date, Drawdown, colour = Portfolio)) +
    geom_hline(yintercept = 0, colour = "#4A4F61", linewidth = 0.4) +
    geom_line(linewidth = 0.8) +
    scale_colour_manual(values = pal_port, name = NULL) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title    = "Drawdown Through 2025- Recommended vs Equal-Weight",
      subtitle = "Decline from each portfolio's running peak",
      x = "Date", y = "Drawdown"
    ) +
    theme_dark_roboto() +
    theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
  save_plot(f_dd, "plots/eval_drawdown_2025.png", width = 10, height = 6)

  f_bar <- ggplot(asset_df,
                  aes(x = reorder(Ticker, `2025 return`), y = `2025 return`, fill = Ticker)) +
    geom_col(width = 0.6) +
    geom_hline(yintercept = 0, colour = "#4A4F61", linewidth = 0.4) +
    scale_fill_manual(values = TICKER_COLS, guide = "none") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    coord_flip() +
    labs(
      title    = "Realised 2025 Return by Stock",
      subtitle = "Total calendar-2025 return, actual out-of-sample data",
      x = NULL, y = "2025 total return"
    ) +
    theme_dark_roboto() +
    theme(plot.title = element_text(hjust = 0.5))
  save_plot(f_bar, "plots/eval_asset_returns_2025.png", width = 9, height = 5)

  in_port   <- xts(as.numeric(dat$R[, tickers] %*% w_rec), order.by = dat$dates)
  out_port  <- xts(as.numeric(R25_log %*% w_rec),          order.by = dates25)
  full_port <- rbind(in_port, out_port)
  n_in      <- nrow(in_port)

  bt <- tryCatch({
    roll <- ugarchroll(
      .garch_spec("std"), data = full_port,
      n.start = n_in, refit.every = 10, refit.window = "recursive",
      solver = "hybrid", calculate.VaR = TRUE, VaR.alpha = p_var,
      keep.coef = FALSE
    )
    vr       <- roll@forecast$VaR
    var_col  <- vr[[grep("alpha", names(vr), value = TRUE)[1]]]
    realized <- vr[["realized"]]
    bdates   <- as.Date(rownames(vr))
    vt       <- rugarch::VaRTest(alpha = p_var, actual = realized, VaR = var_col)
    list(dates = bdates, VaR = var_col, realized = realized, test = vt)
  }, error = function(e) {
    warning("ugarchroll failed (", conditionMessage(e),
            "); skipping conditional VaR backtest.")
    NULL
  })

  if (!is.null(bt)) {
    exceed <- bt$realized < bt$VaR
    bt_df <- tibble(
      Metric = c("Observations (2025)", "Expected exceptions",
                 "Actual exceptions", "Exception rate",
                 "Kupiec (uc) p-value", "Christoffersen (cc) p-value"),
      Value  = c(
        as.character(length(bt$realized)),
        sprintf("%.1f", p_var * length(bt$realized)),
        as.character(sum(exceed)),
        sprintf("%.2f%%", 100 * mean(exceed)),
        sprintf("%.3f", bt$test$uc.LRp),
        sprintf("%.3f", bt$test$cc.LRp)
      )
    )
    bt_tbl <- bt_df |>
      gt() |>
      tab_header(
        title    = sprintf("Conditional VaR Backtest- Recommended Book, %.0f%% 1-Day VaR",
                          (1 - p_var) * 100),
        subtitle = "Rolling ARIMA(0,0,0)-GARCH(1,1) Student-t, expanding window through 2025"
      ) |>
      cols_align("center", columns = Value) |>
      tab_source_note(paste(
        "An exception is a day whose realised loss exceeded the model's VaR.",
        "Kupiec/Christoffersen H\u2080: the exception rate matches the target.", SRC_NOTE)) |>
      gt_theme_dr()
    save_table(bt_tbl, "tables/eval_var_backtest.png")

    bt_plot_df <- tibble(Date = bt$dates, Return = bt$realized * 100,
                         VaR = bt$VaR * 100, Exception = exceed)
    exc_df <- dplyr::filter(bt_plot_df, Exception)
    f_bt <- ggplot(bt_plot_df, aes(Date)) +
      geom_hline(yintercept = 0, colour = "#4A4F61", linewidth = 0.4) +
      geom_line(aes(y = Return), colour = "#9AA0AD", linewidth = 0.35) +
      geom_step(aes(y = VaR), colour = "#5B9BD5", linewidth = 0.7) +
      geom_point(data = exc_df, aes(y = Return), colour = "#E96F8B", size = 1.8) +
      labs(
        title    = sprintf("Conditional %.0f%% VaR vs Realised Returns- Recommended Book, 2025",
                          (1 - p_var) * 100),
        subtitle = "Blue = 1-day conditional VaR; rose dots = exceptions (loss beyond VaR)",
        x = "Date", y = "Daily return (%)"
      ) +
      theme_dark_roboto() +
      theme(plot.title = element_text(hjust = 0.5))
    save_plot(f_bt, "plots/eval_var_backtest.png", width = 10, height = 6)
  }

  invisible(list(
    asset = asset_df, summary = summ_df, exante_vs_real = cmp_df,
    value_rec = val_rec, value_ew = val_ew, dates = dates25, backtest = bt
  ))
}