source("theme_dark_roboto.R") # Custom theming for ggplot2 charts; also loads gt for tables
source("theme_dark_roboto_table.R")
 
library(tidyverse)
library(quantmod)
library(gt)
data_ko <- getSymbols("KO", auto.assign = FALSE, from = "2020-01-01", to = "2024-12-31") # Source: Yahoo Finance via quantmod
data_nvda <- getSymbols("NVDA", auto.assign = FALSE, from = "2020-01-01", to = "2024-12-31")
data_xom <- getSymbols("XOM", auto.assign = FALSE, from = "2020-01-01", to = "2024-12-31")
 
ko_ret <- dailyReturn(data_ko$KO.Adjusted, type = "log") # Convert to daily log returns
nvda_ret <- dailyReturn(data_nvda$NVDA.Adjusted, type = "log")
xom_ret <- dailyReturn(data_xom$XOM.Adjusted, type = "log")
 
rets <- list(KO = ko_ret, NVDA = nvda_ret, XOM = xom_ret)
 
# Descriptive statistics for the three stocks
desc <- imap_dfr(rets, function(r, nm) {
    x <- as.numeric(r)
    x <- x[x != 0] # drop the seed 0 return from dailyReturn
    tibble(
        Ticker               = nm,
        `Mean (%)`           = mean(x) * 100,
        `Std Dev (%)`        = sd(x) * 100,
        `Annualised Vol (%)` = sd(x) * sqrt(252) * 100,
        `Min (%)`            = min(x) * 100,
        `Max (%)`            = max(x) * 100,
        Skewness             = e1071::skewness(x),
        `Excess Kurtosis`    = e1071::kurtosis(x) # e1071 returns excess kurtosis
    )
})
 
f1 <- desc |>
    gt() |>
    tab_header(
        title    = "Descriptive Statistics- Daily Log Returns",
        subtitle = "Coca-Cola (KO), NVIDIA (NVDA) & ExxonMobil (XOM), 2020–2024"
    ) |>
    fmt_number(columns = c(`Mean (%)`, `Std Dev (%)`, Skewness, `Excess Kurtosis`), decimals = 4) |>
    fmt_number(columns = c(`Annualised Vol (%)`, `Min (%)`, `Max (%)`), decimals = 2) |>
    tab_source_note("Source: Yahoo Finance via quantmod.") |>
    gt_theme_dr()
 
gtsave(f1, "tables/desc_stats.png", zoom = 3, expand = 10)
 
library(patchwork)
 
layout_fix <- theme(
    plot.title.position = "panel",                        
    plot.title          = element_text(hjust = 0.5),      
    axis.title          = element_text(size = rel(0.62))  
)
 
ko_plot <- ggplot(ko_ret, aes(x = index(ko_ret), y = daily.returns)) +
    geom_line(color = "#F4845F", linewidth = 0.4) +
    labs(title = "Coca-Cola (KO)", x = NULL, y = "Daily Log Return") +
    theme_dark_roboto() + layout_fix
nvda_plot <- ggplot(nvda_ret, aes(x = index(nvda_ret), y = daily.returns)) +
    geom_line(color = "#56C596", linewidth = 0.4) +
    labs(title = "NVIDIA (NVDA)", x = NULL, y = "Daily Log Return") +
    theme_dark_roboto() + layout_fix
xom_plot <- ggplot(xom_ret, aes(x = index(xom_ret), y = daily.returns)) +
    geom_line(color = "#5B9BD5", linewidth = 0.4) +
    labs(title = "ExxonMobil (XOM)", x = "Date", y = "Daily Log Return") +
    theme_dark_roboto() + layout_fix
 
f2 <- ko_plot / nvda_plot / xom_plot +
    plot_annotation(
        title    = "Daily Log Returns- KO, NVDA & XOM",
        subtitle = "2020–2024",
        theme    = theme_dark_roboto() # themes the title block + overall background
    )
 
ggsave("plots/daily_returns.png", f2, width = 10, height = 12, dpi = 300)

# Growth of $1 chart using cumulative log returns
# Since the returns above are log returns, exp(cumsum(r)) converts them back into a cumulative wealth index.
growth_1 <- imap_dfr(rets, function(r, nm) {
    tibble(
        Date        = index(r),
        Ticker      = nm,
        `Growth of $1` = exp(cumsum(as.numeric(r)))
    )
})

f3 <- ggplot(growth_1, aes(x = Date, y = `Growth of $1`, color = Ticker)) +
    geom_line(linewidth = 0.8) +
    scale_color_manual(values = c(
        KO   = "#F4845F",
        NVDA = "#56C596",
        XOM  = "#5B9BD5"
    )) +
    scale_y_continuous(labels = scales::dollar_format(prefix = "$", accuracy = 0.01)) +
    labs(
        title    = "Growth of $1- KO, NVDA & XOM",
        subtitle = "Based on adjusted close daily log returns, 2020–2024",
        x        = "Date",
        y        = "Portfolio Value",
        color    = NULL
    ) +
    theme_dark_roboto() +
    theme(
        legend.position = "bottom",
        plot.title      = element_text(hjust = 0.5)
    )

ggsave("plots/growth_of_1.png", f3, width = 10, height = 6, dpi = 300)