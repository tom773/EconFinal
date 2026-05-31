suppressPackageStartupMessages({
  library(tidyverse)
  library(quantmod)
  library(gt)
  library(patchwork)
  library(tseries)
  library(TTR)
  library(FinTS)
  library(rugarch)
  library(broom)
  library(ggrepel)
})
source("theme_dark_roboto.R")
source("theme_dark_roboto_table.R")
TICKER_COLS   <- c(KO = "#F4845F", NVDA = "#56C596", XOM = "#5B9BD5")
TICKER_LABELS <- c(KO = "Coca-Cola (KO)", NVDA = "NVIDIA (NVDA)",
                   XOM = "ExxonMobil (XOM)")
SRC_NOTE      <- "Source: Yahoo Finance via quantmod."
layout_fix <- theme(
  plot.title.position = "panel",
  plot.title          = element_text(hjust = 0.5),
  axis.title          = element_text(size = rel(0.62))
)
stars <- function(p) dplyr::case_when(
  p < .001 ~ "***",
  p < .01  ~ "**",
  p < .05  ~ "*",
  TRUE     ~ ""
)
.garch_spec <- function(dist = "std") {
  ugarchspec(
    mean.model         = list(armaOrder = c(0, 0), include.mean = TRUE),
    variance.model     = list(model = "sGARCH", garchOrder = c(1, 1)),
    distribution.model = dist
  )
}
save_table <- function(gt_tbl, path, zoom = 3, expand = 10) {
  gtsave(gt_tbl, path, zoom = zoom, expand = expand)
}
save_plot <- function(plot, path, width, height, dpi = 300) {
  ggsave(path, plot, width = width, height = height, dpi = dpi)
}
prepare_data <- function(tickers  = c("KO", "NVDA", "XOM"),
                         from     = "2020-01-01",
                         to       = "2024-12-31",
                         ffm_path = "data/ffm.csv",
                         cols     = TICKER_COLS) {
  setup_roboto()
  rets <- map(set_names(tickers), function(tk) {
    px <- getSymbols(tk, auto.assign = FALSE, from = from, to = to)
    dailyReturn(Ad(px), type = "log")
  })
  ret_vecs <- map(rets, ~ as.numeric(.x)[-1])
  ret_xts  <- map(rets, ~ .x[-1, ])
  ret_mat <- na.omit(do.call(merge, rets))
  colnames(ret_mat) <- tickers
  ret_mat <- ret_mat[-1, ]
  R       <- coredata(ret_mat)
  dates   <- index(ret_mat)
  mu_ann    <- colMeans(R) * 252
  Sigma_ann <- cov(R) * 252
  sig_ann   <- sqrt(diag(Sigma_ann))
  ffm <- reg_df <- NULL
  if (file.exists(ffm_path)) {
    ffm <- read.csv(ffm_path, check.names = FALSE) |> as_tibble()
    names(ffm)[1] <- "Date"
    ffm <- ffm |>
      filter(grepl("^[0-9]{8}$", trimws(Date))) |>
      mutate(
        Date   = ymd(Date),
        Mkt_RF = `Mkt-RF` / 100,
        SMB    = SMB / 100,
        HML    = HML / 100,
        RF     = RF / 100
      ) |>
      select(Date, Mkt_RF, SMB, HML, RF)
    ret_df <- imap_dfr(rets, function(r, nm) {
      tibble(Date = as.Date(index(r)), Ticker = nm, ret = as.numeric(r)) |>
        slice(-1)
    })
    reg_df <- ret_df |>
      inner_join(ffm, by = "Date") |>
      mutate(excess_ret = ret - RF)
  } else {
    warning("FF factor file not found at '", ffm_path,
            "'. run_capm() and run_reccs() will be unavailable.")
  }
  list(
    tickers   = tickers,
    cols      = cols,
    rets      = rets,        
    ret_vecs  = ret_vecs,    
    ret_xts   = ret_xts,  
    R         = R,           
    dates     = dates,
    mu_ann    = mu_ann,
    Sigma_ann = Sigma_ann,
    sig_ann   = sig_ann,
    ffm       = ffm,
    reg_df    = reg_df
  )
}