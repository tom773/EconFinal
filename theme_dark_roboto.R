# ── theme_dark_roboto ────────────────────────────────────────────────────────
#
#  A clean, dark ggplot2 theme built around Roboto / Roboto Condensed.
#
#  Dependencies:
#    install.packages(c("ggplot2", "showtext", "sysfonts"))
#
#  Quick-start:
#    source("theme_dark_roboto.R")
#    setup_roboto()          # load fonts (call once per session)
#    ggplot(...) + theme_dark_roboto()
# ─────────────────────────────────────────────────────────────────────────────

library(ggplot2)
library(showtext)
library(sysfonts)


# ── 1.  Font setup ────────────────────────────────────────────────────────────

setup_roboto <- function() {
  font_add_google("Roboto",           family = "roboto")
  font_add_google("Roboto Condensed", family = "roboto_condensed")
  showtext_auto()
  invisible(NULL)
}


# ── 2.  Palette ───────────────────────────────────────────────────────────────

.dr_pal <- list(
  # backgrounds
  bg          = "#1A1D23",   # near-black blue-tinted canvas
  panel       = "#21252E",   # slightly lighter panel
  strip       = "#2C3040",   # facet strip
  
  # text / grid
  text_title  = "#F3F3F3",   # near-white title
  text_body   = "#F2F2F2",   # softer axis text
  text_light  = "#636363",   # captions / minor labels
  grid_major  = "#2E3344",   # subtle major grid
  grid_minor  = "#262A37",   # barely-there minor grid
  border      = "#333849",   # panel border
  
  # accent sequence (discrete)
  accent = c(
    "#5B9BD5",  # steel blue
    "#F4845F",  # coral
    "#56C596",  # mint green
    "#E8C44F",  # amber
    "#A57BCC",  # lavender
    "#E96F8B",  # rose
    "#4FC6C6",  # teal
    "#F0A050"   # warm orange
  )
)


# ── 3.  Core theme ────────────────────────────────────────────────────────────

#' Dark Roboto ggplot2 theme
#'
#' @param base_size   Base font size in pts (default 13).
#' @param grid        Which grid lines to draw: "both", "x", "y", or "none".
#' @param border      Draw a panel border? (default TRUE)
#'
#' @return A ggplot2 theme object.
#' @export
theme_dark_roboto <- function(base_size = 18,
                              grid      = "both",
                              border    = TRUE) {
  
  p <- .dr_pal
  
  t <- theme_minimal(base_size = base_size) %+replace%
    
    theme(
      # ── canvas ──────────────────────────────────────────────────────────────
      plot.background  = element_rect(fill = p$bg,    colour = NA),
      panel.background = element_rect(fill = p$panel, colour = NA),
      
      # ── panel border ────────────────────────────────────────────────────────
      panel.border = if (border)
        element_rect(fill = NA, colour = p$border, linewidth = 0.6)
      else
        element_blank(),
      
      # ── grid lines ──────────────────────────────────────────────────────────
      panel.grid.major.x = if (grid %in% c("both", "x"))
        element_line(colour = p$grid_major, linewidth = 0.35)
      else element_blank(),
      
      panel.grid.major.y = if (grid %in% c("both", "y"))
        element_line(colour = p$grid_major, linewidth = 0.35)
      else element_blank(),
      
      panel.grid.minor   = element_line(colour = p$grid_minor, linewidth = 0.2),
      
      # ── axes ────────────────────────────────────────────────────────────────
      axis.line        = element_blank(),
      axis.ticks       = element_line(colour = p$border, linewidth = 0.4),
      axis.ticks.length = unit(4, "pt"),
      
      axis.text   = element_text(
        family = "roboto", colour = p$text_body, size = rel(0.62)
      ),
      axis.title  = element_text(
        family = "roboto", colour = p$text_body, size = rel(0.72),
        margin = margin(4, 4, 4, 4)
      ),
      
      # ── plot labels ─────────────────────────────────────────────────────────
      plot.title = element_text(
        family = "roboto_condensed", colour = p$text_title,
        size   = rel(1.35),
        margin = margin(b = 6)
      ),
      plot.subtitle = element_text(
        family = "roboto", colour = p$text_body,
        size   = rel(0.95),
        margin = margin(b = 12)
      ),
      plot.caption = element_text(
        family = "roboto", colour = p$text_light,
        size   = rel(0.75), hjust = 1,
        margin = margin(t = 10)
      ),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.margin = margin(16, 16, 12, 16),
      
      # ── legend ──────────────────────────────────────────────────────────────
      legend.background = element_rect(fill = p$bg,    colour = NA),
      legend.key        = element_rect(fill = p$panel, colour = NA),
      legend.text  = element_text(
        family = "roboto", colour = p$text_body, size = rel(0.83)
      ),
      legend.title = element_text(
        family = "roboto", colour = p$text_body, size = rel(0.88),
        face   = "bold"
      ),
      legend.margin      = margin(6, 6, 6, 6),
      legend.key.size    = unit(14, "pt"),
      legend.position    = "right",
      
      # ── facet strips ────────────────────────────────────────────────────────
      strip.background = element_rect(fill = p$strip, colour = NA),
      strip.text = element_text(
        family = "roboto_condensed", colour = p$text_title,
        size   = rel(0.88), face = "bold",
        margin = margin(5, 8, 5, 8)
      )
    )
  
  t
}


# ── 4.  Matching colour / fill scales ─────────────────────────────────────────

#' Discrete colour scale that pairs with theme_dark_roboto
#' @param ... Passed to \code{scale_colour_manual}.
#' @export
scale_colour_dr <- function(...) {
  scale_colour_manual(values = .dr_pal$accent, ...)
}

#' Discrete fill scale that pairs with theme_dark_roboto
#' @param ... Passed to \code{scale_fill_manual}.
#' @export
scale_fill_dr <- function(...) {
  scale_fill_manual(values = .dr_pal$accent, ...)
}


# ── 5.  Convenience: set as session default ───────────────────────────────────

#' Set theme_dark_roboto as the ggplot2 session default.
#' @param ... Passed to \code{theme_dark_roboto()}.
#' @export
set_theme_dr <- function(...) {
  theme_set(theme_dark_roboto(...))
  invisible(NULL)
}
