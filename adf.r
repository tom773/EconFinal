source("main.r")
source("theme_dark_roboto_table.R")
library(tseries)
library(gt)
library(tidyverse)

adf.test(ko_ret$daily.returns, alternative = "stationary")
adf.test(nvda_ret$daily.returns, alternative = "stationary")
adf.test(xom_ret$daily.returns, alternative = "stationary")



run_adf <- function(x, ticker) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]

  test <- adf.test(x, alternative = "stationary")

  tibble(
    Ticker     = ticker,
    ADF        = unname(test$statistic),
    Lags       = unname(test$parameter),
    p_value    = unname(test$p.value),
    p_display  = if_else(test$p.value <= 0.01, "≤ 0.01", sprintf("%.3f", test$p.value)),
    decision   = if_else(test$p.value < 0.05, "Reject H0", "Fail to reject H0"),
    conclusion = if_else(test$p.value < 0.05, "Stationary daily returns", "Unit root not ruled out")
  )
}

adf_results <- bind_rows(
  run_adf(ko_ret$daily.returns,   "KO"),
  run_adf(nvda_ret$daily.returns, "NVDA"),
  run_adf(xom_ret$daily.returns,  "XOM")
)
adf_tbl_df <- imap_dfr(ret_vecs, function(x, nm) {
    a <- suppressWarnings(adf.test(x, alternative = "stationary"))
    tibble(
        Ticker          = nm,
        `ADF statistic` = unname(a$statistic),
        `Lag order`     = unname(a$parameter),
        `p-value`       = a$p.value
    )
})
 
print(adf_tbl_df)
 
adf_tbl <- adf_tbl_df |>
    gt(rowname_col = "Ticker") |>
    tab_header(
        title    = "Augmented Dickey-Fuller Unit Root Tests",
        subtitle = "Daily log returns — KO, NVDA & XOM, 2020–2024"
    ) |>
    fmt_number(columns = `ADF statistic`, decimals = 3) |>
    fmt_integer(columns = `Lag order`) |>
    fmt_number(columns = `p-value`, decimals = 3) |>
    cols_align("center", columns = c(`ADF statistic`, `Lag order`, `p-value`)) |>
    tab_source_note(
        "H\u2080: unit root (non-stationary). tseries reports a floor of 0.01 on the p-value. Source: Yahoo Finance via quantmod; author's calculations."
    ) |>
    gt_theme_dr()
 
gtsave(adf_tbl, "tables/adf_tests.png", zoom = 3, expand = 10)