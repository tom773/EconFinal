# ── theme_dark_roboto_table ──────────────────────────────────────────────────
#
#  Companion gt theme to theme_dark_roboto.R.
#  Reproduces the clean, ruled table look from Assignment 1 using the same
#  Roboto / Roboto Condensed fonts and palette as the chart theme, so tables
#  and figures in the report look like a matched set.
#
#  Dependencies:
#    install.packages("gt")
#    install.packages("webshot2")   # only needed to gtsave() a PNG for the report
#
#  Quick-start:
#    source("theme_dark_roboto_table.R")
#    my_tbl |> gt() |> gt_theme_dr()              # light look (matches A1 tables)
#    my_tbl |> gt() |> gt_theme_dr(dark = TRUE)   # dark look (matches the charts)
# ─────────────────────────────────────────────────────────────────────────────

library(gt)


# ── Palette (mirrors .dr_pal in theme_dark_roboto.R) ──────────────────────────

.dr_tbl_pal <- list(
  accent = "#5B9BD5",   # steel blue, same accent[1] as the chart theme

  # Light variant — matches the white-background gt tables in Assessment 1
  light = list(
    bg          = "#FFFFFF",
    text        = "#1A1D23",
    subtitle    = "#636363",
    rule_strong = "#8C8C8C",
    rule_soft   = "#D9D9D9",
    source      = "#7A7A7A"
  ),

  # Dark variant — matches theme_dark_roboto() for a unified dark report
  dark = list(
    bg          = "#21252E",
    text        = "#F2F2F2",
    subtitle    = "#B8BCC6",
    rule_strong = "#4A4F61",
    rule_soft   = "#333849",
    source      = "#8A8F9C"
  )
)


# ── Theme function ────────────────────────────────────────────────────────────

#' Dark-Roboto companion theme for gt tables
#'
#' @param gt_tbl  A gt table object.
#' @param dark    Use the dark palette (matches the charts)? Default FALSE.
#' @param accent  Accent colour for optional highlighting. Default steel blue.
#'
#' @return A styled gt table object.
#' @export
gt_theme_dr <- function(gt_tbl, dark = FALSE, accent = .dr_tbl_pal$accent) {

  p <- if (dark) .dr_tbl_pal$dark else .dr_tbl_pal$light

  gt_tbl |>
    opt_table_font(font = list(google_font("Roboto"), default_fonts())) |>
    tab_options(
      # ── canvas ──────────────────────────────────────────────────────────────
      table.background.color           = p$bg,
      table.font.color                 = p$text,
      table.font.size                  = px(15),

      # ── outer rules ─────────────────────────────────────────────────────────
      table.border.top.style           = "none",
      table.border.bottom.style        = "solid",
      table.border.bottom.color        = p$rule_strong,
      table.border.bottom.width        = px(2),

      # ── heading (title + subtitle) ──────────────────────────────────────────
      heading.align                    = "center",
      heading.title.font.size          = px(20),
      heading.subtitle.font.size       = px(13),
      heading.border.bottom.style      = "none",
      heading.padding                  = px(2),

      # ── column labels ───────────────────────────────────────────────────────
      column_labels.background.color   = p$bg,
      column_labels.font.weight        = "bold",
      column_labels.border.top.style   = "solid",
      column_labels.border.top.color   = p$rule_strong,
      column_labels.border.top.width   = px(2),
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.color = p$rule_soft,
      column_labels.border.bottom.width = px(1),

      # ── body ────────────────────────────────────────────────────────────────
      table_body.hlines.style          = "none",
      table_body.border.top.style      = "none",
      table_body.border.bottom.style   = "solid",
      table_body.border.bottom.color   = p$rule_soft,
      row.striping.include_table_body  = FALSE,
      data_row.padding                 = px(6),

      # ── notes ───────────────────────────────────────────────────────────────
      source_notes.font.size           = px(11),
      footnotes.font.size              = px(11)
    ) |>
    # Title in Roboto Condensed (matches plot.title in the chart theme)
    tab_style(
      style     = cell_text(font = google_font("Roboto Condensed"),
                            weight = "bold", color = p$text),
      locations = cells_title(groups = "title")
    ) |>
    # Subtitle in muted grey Roboto
    tab_style(
      style     = cell_text(font = google_font("Roboto"), color = p$subtitle),
      locations = cells_title(groups = "subtitle")
    ) |>
    # Source note in light grey
    tab_style(
      style     = cell_text(color = p$source),
      locations = cells_source_notes()
    )
}


# ── Optional helper: bold + accent-colour a set of cells ──────────────────────

#' Highlight cells in the accent colour (e.g. significant coefficients)
#' @param gt_tbl  A gt table object.
#' @param columns Columns to target (tidyselect).
#' @param rows    Rows to target (predicate or indices).
#' @param accent  Accent colour. Default steel blue.
#' @export
gt_highlight_dr <- function(gt_tbl, columns, rows = everything(),
                            accent = .dr_tbl_pal$accent) {
  gt_tbl |>
    tab_style(
      style     = cell_text(color = accent, weight = "bold"),
      locations = cells_body(columns = {{ columns }}, rows = {{ rows }})
    )
}