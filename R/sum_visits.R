#' Summarize daily intake, duration, and visit counts 
#'
#' @description
#' Summarize feed or water visits **per cow per day** to get daily intake, 
#' duration, and visit counts.
#'
#' @inheritParams qc
#' @param df A data frame containing visit-level data for feed or water.
#' @param type A string, either `"feed"` or `"water"` (lowercase).
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

#' sum_visits(df, 
#'           type = "feed", 
#'           id_col = "cow", 
#'           intake_col = "intake", 
#'           dur_col = "duration")
#'
#' @noRd
sum_visits <- function(df, 
                      type = c("feed", "water"), 
                      id_col = id_col2(), 
                      intake_col = intake_col2(), 
                      dur_col = duration_col2()) {
  type <- match.arg(type)

  id_col       <- rlang::sym(id_col)
  intake_col   <- rlang::sym(intake_col)
  dur_col <- rlang::sym(dur_col)

  out <- df |>
    dplyr::group_by(date, !!id_col) |>
    dplyr::summarise(
      !!paste0(type, "_intake")   := sum(!!intake_col, na.rm = TRUE),
      !!paste0(type, "_duration") := sum(!!dur_col, na.rm = TRUE),
      !!paste0(type, "_visits")   := dplyr::n(),
      .groups = "drop"
    )

  return(out)
}