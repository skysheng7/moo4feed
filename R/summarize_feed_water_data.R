#' Summarize daily intake, duration, and visit counts
#'
#' @description
#' Internal helper that summarizes feeding or drinking visits **per cow per day**.
#'
#' @param df A data frame containing visit-level data for feed or water.
#' @param type A string, either `"feeding"` or `"drinking"` (lowercase).
#'
#' @return A data frame with daily intake, duration, and visit counts per cow.
#' @keywords internal
#' @noRd
#'
#' @importFrom dplyr group_by summarise n ungroup
#' @importFrom rlang .data sym
summarize_feed_water_data <- function(df, type = c("feeding", "drinking")) {
  type <- match.arg(type)

  id_col      <- rlang::sym(id_col2())
  intake_col  <- rlang::sym(intake_col2())
  duration_col <- rlang::sym(duration_col2())

  out <- df |>
    dplyr::group_by(date, !!id_col) |>
    dplyr::summarise(
      !!paste0(type, "_intake")   := sum(!!intake_col, na.rm = TRUE),
      !!paste0(type, "_duration") := sum(!!duration_col, na.rm = TRUE),
      !!paste0(type, "_visits")   := dplyr::n(),
      .groups = "drop"
    )

  return(out)
}