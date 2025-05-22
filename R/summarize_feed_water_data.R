#' Summarize daily intake, duration, and visit counts
#'
#' @description
#' Summarize feeding or drinking visits **per cow per day**.
#'
#' @param df A data frame containing visit-level data for feed or water.
#' @param type A string, either `"feeding"` or `"drinking"` (lowercase).
#'
#' @return A data frame with daily intake, duration, and visit counts per cow.
#'
#' @examples
#' df <- tibble::tibble(
#'   date = as.Date(c("2024-01-01", "2024-01-01", "2024-01-02")),
#'   cow = c("A", "A", "B"),
#'   intake = c(10, 20, 30),
#'   duration = c(100, 200, 150)
#' )
#' set_id_col2("cow")
#' set_intake_col2("intake")
#' set_duration_col2("duration")
#' summarize_feed_water_data(df, type = "feeding")
#'
#' @export
summarize_feed_water_data <- function(df, type = c("feeding", "drinking")) {
  type <- match.arg(type)

  id_col       <- rlang::sym(id_col2())
  intake_col   <- rlang::sym(intake_col2())
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