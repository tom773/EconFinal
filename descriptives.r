# This produces desc_stats.png, daily_returns.png and growth_of_1.png

.ret_panel <- function(r, title, colour, show_x = FALSE) {
  ggplot(r, aes(x = index(r), y = daily.returns)) +
    geom_line(color = colour, linewidth = 0.4) +
    labs(title = title, x = if (show_x) "Date" else NULL, y = "Daily Log Return") +
    theme_dark_roboto() + layout_fix
}

run_descriptives <- function(dat) {
  rets    <- dat$rets
  cols    <- dat$cols
  tk_last <- tail(dat$tickers, 1)


  desc <- imap_dfr(rets, function(r, nm) {
    x <- as.numeric(r)
    x <- x[x != 0]
    tibble(
      Ticker               = nm,
      `Mean (%)`           = mean(x) * 100,
      `Std Dev (%)`        = sd(x) * 100,
      `Annualised Vol (%)` = sd(x) * sqrt(252) * 100,
      `Min (%)`            = min(x) * 100,
      `Max (%)`            = max(x) * 100,
      Skewness             = e1071::skewness(x),
      `Excess Kurtosis`    = e1071::kurtosis(x)
    )
  })

  desc_tbl <- desc |>
    gt() |>
    tab_header(
      title    = "Descriptive Statistics- Daily Log Returns",
      subtitle = "Coca-Cola (KO), NVIDIA (NVDA) & ExxonMobil (XOM), 2020–2024"
    ) |>
    fmt_number(columns = c(`Mean (%)`, `Std Dev (%)`, Skewness, `Excess Kurtosis`),
               decimals = 4) |>
    fmt_number(columns = c(`Annualised Vol (%)`, `Min (%)`, `Max (%)`), decimals = 2) |>
    tab_source_note(SRC_NOTE) |>
    gt_theme_dr()

  save_table(desc_tbl, "tables/desc_stats.png")


  panels <- imap(rets, function(r, nm) {
    .ret_panel(r, TICKER_LABELS[[nm]], cols[[nm]], show_x = (nm == tk_last))
  })

  f_returns <- purrr::reduce(panels, `/`) +
    plot_annotation(
      title    = "Daily Log Returns- KO, NVDA & XOM",
      subtitle = "2020–2024",
      theme    = theme_dark_roboto()
    )
  save_plot(f_returns, "plots/daily_returns.png", width = 10, height = 12)


  growth_1 <- imap_dfr(rets, function(r, nm) {
    tibble(
      Date           = index(r),
      Ticker         = nm,
      `Growth of $1` = exp(cumsum(as.numeric(r)))
    )
  })

  f_growth <- ggplot(growth_1, aes(x = Date, y = `Growth of $1`, color = Ticker)) +
    geom_line(linewidth = 0.8) +
    scale_color_manual(values = cols) +
    scale_y_continuous(labels = scales::dollar_format(prefix = "$", accuracy = 0.01)) +
    labs(
      title    = "Growth of $1- KO, NVDA & XOM",
      subtitle = "Based on adjusted close daily log returns, 2020–2024",
      x        = "Date",
      y        = "Portfolio Value",
      color    = NULL
    ) +
    theme_dark_roboto() +
    theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
  save_plot(f_growth, "plots/growth_of_1.png", width = 10, height = 6)

  invisible(list(desc = desc, desc_tbl = desc_tbl,
                 returns_plot = f_returns, growth_plot = f_growth))
}