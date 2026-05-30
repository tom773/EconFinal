source("main.r") # rets, themes, layout_fix
source("theme_dark_roboto_table.R")
setup_roboto()
library(rugarch)
library(ggrepel)

A0_total <- 1500000 # client investment ($)
rf_ann <- mean(reg_df$RF) * 252 # annual risk-free rate (edit to your source)
p_var <- 0.05 # VaR tail probability (95% VaR)

ret_mat <- na.omit(do.call(merge, rets))
colnames(ret_mat) <- names(rets) # KO, NVDA, XOM
ret_mat <- ret_mat[-1, ] # drop seed-0 row
R <- coredata(ret_mat)
dates <- index(ret_mat)
tickers <- colnames(R)

mu_daily <- colMeans(R)
mu_ann <- mu_daily * 252
Sigma_ann <- cov(R) * 252 # annualised covariance
sig_ann <- sqrt(diag(Sigma_ann)) # annualised vol per asset

## annualised stats for any weight vector
port_stats <- function(w) {
    w <- w[tickers]
    ret <- sum(w * mu_ann)
    vol <- sqrt(as.numeric(t(w) %*% Sigma_ann %*% w))
    c(ret = ret, vol = vol, sharpe = (ret - rf_ann) / vol)
}

w_ew <- setNames(rep(1 / 3, 3), tickers) # equal weight
w_iv <- setNames((1 / sig_ann) / sum(1 / sig_ann), tickers) # inverse-volatility

step <- 0.01
s <- seq(0, 1, by = step)
g <- expand.grid(KO = s, NVDA = s)
g$XOM <- 1 - g$KO - g$NVDA
g <- g[g$XOM >= -1e-9 & g$XOM <= 1 + 1e-9, ]
g$XOM <- pmax(g$XOM, 0)

W <- as.matrix(g[, tickers])
ret_v <- as.numeric(W %*% mu_ann)
vol_v <- sqrt(rowSums((W %*% Sigma_ann) * W))
shp_v <- (ret_v - rf_ann) / vol_v

w_mv <- setNames(W[which.min(vol_v), ], tickers) # minimum variance
w_ms <- setNames(W[which.max(shp_v), ], tickers) # maximum Sharpe

# Recommendation, we should play around with this. !!!
w_rec <- c(KO = 0.30, NVDA = 0.40, XOM = 0.30)[tickers]
stopifnot(abs(sum(w_rec) - 1) < 1e-8, all(w_rec >= 0))

cand <- list(
    `Recommended` = w_rec,
    `Equal-weight` = w_ew,
    `Minimum-variance` = w_mv,
    `Inverse-vol` = w_iv,
    `Maximum-Sharpe` = w_ms
)

compare_df <- imap_dfr(cand, function(w, nm) {
    st <- port_stats(w)
    tibble(
        Portfolio = nm,
        KO = w["KO"], NVDA = w["NVDA"], XOM = w["XOM"],
        `Exp. return` = st["ret"],
        Volatility = st["vol"],
        Sharpe = st["sharpe"]
    )
})

print(compare_df)

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
    tab_source_note(
        sprintf("Risk-free rate %.1f%% p.a. Returns are log-based annualisations (\u00d7252). Source: Yahoo Finance via quantmod.", rf_ann * 100)
    ) |>
    gt_theme_dr()

gtsave(compare_tbl, "tables/portfolio_compare.png", zoom = 3, expand = 10)

frontier_df <- tibble(vol = vol_v * 100, ret = ret_v * 100, sharpe = shp_v)

cand_df <- imap_dfr(cand, function(w, nm) {
    st <- port_stats(w)
    tibble(Portfolio = nm, vol = st["vol"] * 100, ret = st["ret"] * 100)
})

f_frontier <- ggplot(frontier_df, aes(vol, ret)) +
    geom_point(aes(colour = sharpe), size = 0.6, alpha = 0.55) +
    scale_colour_gradient(low = "#5B9BD5", high = "#F4845F", name = "Sharpe") +
    geom_point(
        data = cand_df, aes(fill = Portfolio),
        shape = 21, colour = "white", size = 3.4, stroke = 0.6
    ) +
    scale_fill_manual(values = c(
        `Recommended`      = "#E8C44F",
        `Equal-weight`     = "#A57BCC",
        `Minimum-variance` = "#4FC6C6",
        `Inverse-vol`      = "#E96F8B",
        `Maximum-Sharpe`   = "#56C596"
    ), name = NULL) +
    geom_text_repel(
        data = cand_df, aes(label = Portfolio),
        colour = "#FFFFFF",
        bg.color = "#1A1D23", # dark background for text
        bg.r = 0.18,
        family = "roboto", size = 5.4,
        box.padding = 0.8, point.padding = 0.5,
        min.segment.length = 0,
        segment.color = "#9AA0AD", segment.size = 0.3,
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

ggsave("plots/efficient_frontier.png", f_frontier, width = 10, height = 7, dpi = 300)

spec_for <- function(dist) {
    ugarchspec(
        mean.model         = list(armaOrder = c(0, 0), include.mean = TRUE),
        variance.model     = list(model = "sGARCH", garchOrder = c(1, 1)),
        distribution.model = dist
    )
}

var1d <- function(fit, A0, p = p_var) {
    fc <- ugarchforecast(fit, n.ahead = 1)
    mu_f <- as.numeric(fitted(fc))
    sig_f <- as.numeric(sigma(fc))
    dist <- fit@model$modeldesc$distribution
    q05 <- if (dist == "std") {
        rugarch::qdist("std", p, mu = mu_f, sigma = sig_f, shape = coef(fit)["shape"])
    } else {
        rugarch::qdist("norm", p, mu = mu_f, sigma = sig_f)
    }
    c(sigma = sig_f, var_pct = -q05, var_dollar = -q05 * A0) # loss reported positive
}

asset_xts <- map(rets, ~ .x[-1, ])
port_xts <- list(
    `Recommended portfolio`  = xts(as.numeric(R %*% w_rec), order.by = dates),
    `Equal-weight portfolio` = xts(as.numeric(R %*% w_ew), order.by = dates)
)

A0_asset <- A0_total * w_rec
A0_port <- c(`Recommended portfolio` = A0_total, `Equal-weight portfolio` = A0_total)

var_rows <- function(series_list, A0_vec) {
    imap_dfr(series_list, function(x, nm) {
        fn <- ugarchfit(spec_for("norm"), data = x, solver = "hybrid")
        ft <- ugarchfit(spec_for("std"), data = x, solver = "hybrid")
        vn <- var1d(fn, A0_vec[[nm]])
        vt <- var1d(ft, A0_vec[[nm]])
        tibble(
            Item          = nm,
            Allocation    = A0_vec[[nm]],
            `VaR Normal`  = vn["var_dollar"],
            `VaR t`       = vt["var_dollar"],
            `VaR t (%)`   = vt["var_pct"]
        )
    })
}

var_assets <- var_rows(asset_xts, A0_asset)
var_ports <- var_rows(port_xts, as.list(A0_port))
var_df <- bind_rows(var_assets, var_ports)

print(var_df)

div_benefit_norm <- sum(var_assets$`VaR Normal`) - var_ports$`VaR Normal`[1]
div_benefit_t <- sum(var_assets$`VaR t`) - var_ports$`VaR t`[1]

var_tbl <- var_df |>
    gt(rowname_col = "Item") |>
    tab_header(
        title    = "1-Day 95% Conditional Value-at-Risk",
        subtitle = "ARIMA(0,0,0)-GARCH(1,1) next-day forecast on a $1.5m mandate"
    ) |>
    fmt_currency(
        columns = c(Allocation, `VaR Normal`, `VaR t`),
        currency = "USD", decimals = 0
    ) |>
    fmt_percent(columns = `VaR t (%)`, decimals = 2) |>
    cols_label(
        `VaR Normal` = "VaR 95% (Normal)",
        `VaR t` = "VaR 95% (Student-t)",
        `VaR t (%)` = "VaR 95% (% of position, t)"
    ) |>
    cols_align("center", columns = c(Allocation, `VaR Normal`, `VaR t`, `VaR t (%)`)) |>
    tab_source_note(
        sprintf(
            "Source: Yahoo Finance via quantmod."
        )
    ) |>
    gt_theme_dr()

gtsave(var_tbl, "tables/var_1day.png", zoom = 3, expand = 10)
