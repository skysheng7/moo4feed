# -----------------------------------------------------------------------------#
# -------------------- External user-facing functions -------------------------#
# -----------------------------------------------------------------------------#


#' Capitalize the First Letter of a String
#'
#' Takes a character string and capitalizes the first letter, while making
#' all other characters lowercase.
#'
#' @param s A character string to capitalize.
#'
#' @return A character string with only the first letter capitalized.
#'
#' @examples
#' cap_first("hello world")
#' cap_first("r programming")
#'
#' @export
cap_first <- function(s) {
  # 1) Validate input must be a single item
  if (length(s) != 1) {
    stop("`s` must be a single character string.")
  }
  # 2) check for NA
  if (is.na(s)){
    return(NA_character_)
  }
  # 3) check for if it's a character
  if(!is.character(s)) {
    stop("`s` must be a single character string.")
  }

  # 2) Capitalize
  out <- paste0(
    toupper(substr(s, 1, 1)),
    tolower(substr(s, 2, nchar(s)))
  )

  return(out)
}


#' De-capitalize the First Letter of a String to Lower Case
#'
#' Takes a character string and de-capitalizes the first letter, keeping the
#' rest of the string unchanged.
#'
#' @inheritParams cap_first
#'
#' @return A character string with only the first letter de-capitalized.
#'
#' @examples
#' lower_first("Hello World")
#' lower_first("R Programming")
#'
#' @export
lower_first <- function(s) {
  # 1) Validate input as a single item
  if (length(s) != 1) {
    stop("`s` must be a single character string.")
  }
  # 2) check for NA
  if (is.na(s)){
    return(NA_character_)
  }
  # 3) check for if it's a character
  if(!is.character(s)) {
    stop("`s` must be a single character string.")
  }

  # 2) De-capitalize
  out <- paste0(
    tolower(substr(s, 1, 1)),
    substr(s, 2, nchar(s))
  )

  return(out)
}
