## CAPM REGRESSIONS
source("main.r")  
library(broom)    

ffm <- read.csv("data/ffm.csv", check.names = FALSE) |>
    as_tibble()

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

print(tail(ffm))

ret_df <- imap_dfr(rets, function(r, nm) {
    tibble(
        Date   = as.Date(index(r)),
        Ticker = nm,
        ret    = as.numeric(r)
    ) |>
        slice(-1)  # first row is the seed 0 return
})

reg_df <- ret_df |>
    inner_join(ffm, by = "Date") |>
    mutate(excess_ret = ret - RF)

capm <- reg_df |>
    nest(data = -Ticker) |>
    mutate(
        model  = map(data, \(d) lm(excess_ret ~ Mkt_RF, data = d)),
        tidied = map(model, \(m) tidy(m, conf.int = TRUE))
    ) |>
    select(Ticker, tidied) |>
    unnest(tidied)

print(capm)

ff3 <- reg_df |>
    nest(data = -Ticker) |>
    mutate(
        model  = map(data, \(d) lm(excess_ret ~ Mkt_RF + SMB + HML, data = d)),
        tidied = map(model, \(m) tidy(m, conf.int = TRUE))
    ) |>
    select(Ticker, tidied) |>
    unnest(tidied)
library(gt)
library(broom)

stars <- function(p) dplyr::case_when(
    p < .001 ~ "***",
    p < .01  ~ "**",
    p < .05  ~ "*",
    TRUE     ~ ""
)

grab <- function(model, ticker) {
    co <- tidy(model) |>
        transmute(
            term,
            cell = paste0(sprintf("%.4f", estimate), stars(p.value))
        )
    gl <- glance(model)
    tibble(Ticker = ticker, term = co$term, cell = co$cell,
           R2 = gl$r.squared, AdjR2 = gl$adj.r.squared, N = gl$nobs)
}

coef_tab <- reg_df |>
    group_split(Ticker) |>
    map_dfr(\(d) grab(lm(excess_ret ~ Mkt_RF + SMB + HML, data = d), d$Ticker[1]))

wide <- coef_tab |>
    pivot_wider(names_from = term, values_from = cell) |>
    select(Ticker,
           Alpha = `(Intercept)`, `Mkt-RF` = Mkt_RF, SMB, HML,
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
    tab_source_note(
        "Estimates with significance stars; standard errors in parentheses. * p<0.05, ** p<0.01, *** p<0.001. Source: Yahoo Finance via quantmod; Fama-French factors."
    ) |>
    gt_theme_dr()

gtsave(reg_tbl, "tables/ff3_compare.png", zoom = 3, expand = 10)