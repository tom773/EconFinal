source("main.r")                    
source("theme_dark_roboto_table.R")
setup_roboto()                      # load Roboto fonts for showtext (idempotent)

library(TTR)                        

ret_mat <- na.omit(do.call(merge, rets))
colnames(ret_mat) <- names(rets)              # KO, NVDA, XOM
ret_mat <- ret_mat[-1, ]                      # drop seed-0 row
R       <- coredata(ret_mat)                  # T x 3 numeric matrix
dates   <- index(ret_mat)

corr_mat   <- cor(R)                          # Pearson correlation
cov_daily  <- cov(R)                          # daily covariance
Sigma_ann  <- cov_daily * 252                 # annualised covariance (portfolio input)

print(round(corr_mat, 4))
print(round(Sigma_ann, 5))
cat("Annualised vol (sqrt diag, %):",
    round(sqrt(diag(Sigma_ann)) * 100, 2), "\n")   # sanity vs descriptive stats

mat_to_tbl <- function(m) {
    as_tibble(m, rownames = "Ticker")
}

corr_tbl <- mat_to_tbl(corr_mat) |>
    gt(rowname_col = "Ticker") |>
    tab_header(
        title    = "Return Correlation Matrix",
        subtitle = "Pearson correlation of daily log returns- KO, NVDA & XOM, 2020–2024"
    ) |>
    fmt_number(columns = c(KO, NVDA, XOM), decimals = 3) |>
    cols_align("center", columns = c(KO, NVDA, XOM)) |>
    tab_source_note(
        "Source: Yahoo Finance via quantmods."
    ) |>
    gt_theme_dr()

gtsave(corr_tbl, "tables/corr_matrix.png", zoom = 3, expand = 10)

cov_tbl <- mat_to_tbl(Sigma_ann) |>
    gt(rowname_col = "Ticker") |>
    tab_header(
        title    = "Annualised Covariance Matrix",
        subtitle = "Daily log-return covariance \u00d7 252- KO, NVDA & XOM, 2020–2024"
    ) |>
    fmt_number(columns = c(KO, NVDA, XOM), decimals = 5) |>
    cols_align("center", columns = c(KO, NVDA, XOM)) |>
    tab_source_note(
        "Diagonal = annualised variance; \u221adiagonal = annualised volatility. Source: Yahoo Finance via quantmods."
    ) |>
    gt_theme_dr()

gtsave(cov_tbl, "tables/cov_matrix.png", zoom = 3, expand = 10)

# Pairwsie Correlations: 63 trading days = one quarter
win <- 63

roll_corr <- tibble(
    Date       = dates,
    `KO–NVDA`  = runCor(R[, "KO"],   R[, "NVDA"], n = win),
    `KO–XOM`   = runCor(R[, "KO"],   R[, "XOM"],  n = win),
    `NVDA–XOM` = runCor(R[, "NVDA"], R[, "XOM"],  n = win)
) |>
    pivot_longer(-Date, names_to = "Pair", values_to = "Correlation") |>
    drop_na(Correlation)

f_roll <- ggplot(roll_corr, aes(Date, Correlation, colour = Pair)) +
    geom_hline(yintercept = 0, colour = "#4A4F61", linewidth = 0.4) +
    geom_line(linewidth = 0.5) +
    scale_colour_manual(values = c(
        `KO–NVDA`  = "#E8C44F",   # amber
        `KO–XOM`   = "#A57BCC",   # lavender
        `NVDA–XOM` = "#4FC6C6"    # teal
    )) +
    coord_cartesian(ylim = c(-1, 1)) +
    labs(
        title    = "Rolling 63-Day Return Correlations- KO, NVDA & XOM",
        subtitle = "Trailing one-quarter Pearson correlation of daily log returns, 2020–2024",
        x        = "Date",
        y        = "Correlation",
        colour   = NULL
    ) +
    theme_dark_roboto() +
    theme(
        legend.position = "bottom",
        plot.title      = element_text(hjust = 0.5)
    )

ggsave("plots/rolling_corr.png", f_roll, width = 10, height = 6, dpi = 300)