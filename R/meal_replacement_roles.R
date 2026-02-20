#' Analyze Within-Meal Actor/Reactor Roles
#'
#' @description
#' Analyzes the percentage of visits within each meal where an animal entered
#' as an actor (replaced another animal) or left as a reactor (was replaced by
#' another animal). Returns meal-level detail showing role percentages.
#'
#' @param visit_data A named list of daily data frames or a single data frame
#'   containing visit records with meal labels.
#' @param replacement_data A named list of daily data frames or a single data
#'   frame containing replacement events from [record_replacement_days()].
#' @param time_tolerance Numeric. Time tolerance in seconds for matching visits
#'   to replacement events. Default is 1 second.
#' @inheritParams set_global_cols
#'
#' @return A data frame (or named list of data frames) with columns:
#'   \itemize{
#'     \item `date` - Date
#'     \item `[id_col]` - Animal identifier
#'     \item `meal_id` - Meal identifier
#'     \item `total_visits_in_meal` - Total visits in the meal
#'     \item `actor_visits` - Number of visits where animal entered as actor
#'     \item `reactor_visits` - Number of visits where animal left as reactor
#'     \item `actor_reactor_visits` - Number of visits with both roles
#'     \item `pct_actor` - Percentage of visits as actor
#'     \item `pct_reactor` - Percentage of visits as reactor
#'     \item `pct_actor_reactor` - Percentage of visits with both roles
#'   }
#'
#' @examples
#' # Create sample visit data with meal labels
#' visits <- data.frame(
#'   date = "2024-01-01",
#'   cow = c("A", "A", "B", "B"),
#'   bin = c(1, 2, 1, 2),
#'   start = as.POSIXct(c(
#'     "2024-01-01 08:00:00",
#'     "2024-01-01 08:15:00",
#'     "2024-01-01 08:10:00",
#'     "2024-01-01 08:20:00"
#'   ), tz = "UTC"),
#'   end = as.POSIXct(c(
#'     "2024-01-01 08:10:00",
#'     "2024-01-01 08:20:00",
#'     "2024-01-01 08:15:00",
#'     "2024-01-01 08:25:00"
#'   ), tz = "UTC"),
#'   meal_id = c(1, 1, 1, 1),
#'   meal_start = as.POSIXct("2024-01-01 08:00:00", tz = "UTC")
#' )
#'
#' # Create replacement data
#' replacements <- data.frame(
#'   actor_cow = "B",
#'   reactor_cow = "A",
#'   bin = 1,
#'   time = as.POSIXct("2024-01-01 08:10:00", tz = "UTC")
#' )
#'
#' # Analyze actor/reactor roles within meals
#' meal_roles <- meal_replacement_roles(
#'   visit_data = visits,
#'   replacement_data = replacements
#' )
#'
#' @export
meal_replacement_roles <- function(visit_data,
                                   replacement_data,
                                   time_tolerance = 1,
                                   id_col = id_col2(),
                                   bin_col = bin_col2(),
                                   start_col = start_col2(),
                                   end_col = end_col2()) {

  # Validate inputs
  .validate_role_data(visit_data, replacement_data)

  # Determine if input is a list or single dataframe
  is_list <- is.list(visit_data) && !is.data.frame(visit_data)

  if (!is_list) {
    day_name <- if ("date" %in% names(visit_data) && nrow(visit_data) > 0 && !is.na(visit_data$date[1])) {
      as.character(visit_data$date[1])
    } else {
      "day1"
    }
    visit_data <- list(visit_data)
    replacement_data <- list(replacement_data)
    names(visit_data) <- day_name
    names(replacement_data) <- day_name
  }

  # Ensure both have same names
  if (!all(names(visit_data) == names(replacement_data))) {
    stop("visit_data and replacement_data must have matching day names", call. = FALSE)
  }

  # Process each day
  result <- lapply(names(visit_data), function(day_name) {
    visits <- visit_data[[day_name]]
    replacements <- replacement_data[[day_name]]

    # Extract date from data if available
    date_val <- if ("date" %in% names(visits)) visits$date[1] else day_name

    # Check required columns in visits
    required_visit_cols <- c(id_col, bin_col, start_col, end_col, "meal_id")
    missing_cols <- setdiff(required_visit_cols, names(visits))
    if (length(missing_cols) > 0) {
      stop("Missing required columns in visit_data: ",
           paste(missing_cols, collapse = ", "), call. = FALSE)
    }

    # Check required columns in replacements
    required_repl_cols <- c("actor_cow", "reactor_cow", "bin", "time")
    missing_repl_cols <- setdiff(required_repl_cols, names(replacements))
    if (length(missing_repl_cols) > 0) {
      stop("Missing required columns in replacement_data: ",
           paste(missing_repl_cols, collapse = ", "), call. = FALSE)
    }

    # Filter to valid meals only
    visits <- visits |>
      dplyr::filter(.data$meal_id > 0)

    # If no valid meals, return empty structure
    if (nrow(visits) == 0) {
      return(.empty_meal_role_summary(date_val, id_col))
    }

    # Classify each visit's role using vectorized join approach
    visits <- .classify_roles_vectorized(
      visits = visits,
      replacements = replacements,
      id_col = id_col,
      bin_col = bin_col,
      start_col = start_col,
      end_col = end_col,
      time_tolerance = time_tolerance
    )

    # Calculate role percentages per meal
    meal_roles <- visits |>
      dplyr::group_by(.data[[id_col]], .data$meal_id) |>
      dplyr::summarise(
        total_visits_in_meal = dplyr::n(),
        actor_visits = sum(.data$is_actor, na.rm = TRUE),
        reactor_visits = sum(.data$is_reactor, na.rm = TRUE),
        actor_reactor_visits = sum(.data$is_actor & .data$is_reactor, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        pct_actor = (.data$actor_visits / .data$total_visits_in_meal) * 100,
        pct_reactor = (.data$reactor_visits / .data$total_visits_in_meal) * 100,
        pct_actor_reactor = (.data$actor_reactor_visits / .data$total_visits_in_meal) * 100,
        date = date_val
      ) |>
      dplyr::select("date", dplyr::all_of(id_col), "meal_id",
                   dplyr::everything())

    return(as.data.frame(meal_roles))
  })

  # Name the result list
  names(result) <- names(visit_data)

  # Return list or single dataframe based on input
  if (!is_list) {
    return(result[[1]])
  } else {
    return(result)
  }
}


#' Classify Actor/Reactor Roles Using Vectorized Join
#'
#' @description
#' Internal function to classify visits as actor/reactor using a vectorized join
#' approach instead of rowwise operations for better performance.
#'
#' @keywords internal
#' @noRd
.classify_roles_vectorized <- function(visits, replacements, id_col, bin_col,
                                        start_col, end_col, time_tolerance) {
  # Add row identifier
  visits <- visits |>
    dplyr::mutate(.visit_row_id = dplyr::row_number())

  # If no replacements, all visits are neutral
  if (nrow(replacements) == 0) {
    visits <- visits |>
      dplyr::mutate(
        is_actor = FALSE,
        is_reactor = FALSE
      ) |>
      dplyr::select(-".visit_row_id")
    return(visits)
  }

  replacements <- replacements |>
    dplyr::mutate(.repl_row_id = dplyr::row_number())

  # Reactor: match replacement timestamp to reactor visit end at the same bin.
  reactor_matches <- visits |>
    dplyr::inner_join(
      replacements |>
        dplyr::select(".repl_row_id", "reactor_cow", "bin", "time") |>
        dplyr::rename(.repl_time = "time"),
      by = c(stats::setNames("reactor_cow", id_col), stats::setNames("bin", bin_col)),
      relationship = "many-to-many"
    ) |>
    dplyr::filter(abs(as.numeric(.data[[end_col]]) - as.numeric(.data$.repl_time)) <= time_tolerance) |>
    dplyr::distinct(.data$.visit_row_id) |>
    dplyr::pull(.data$.visit_row_id)

  # Actor: replacement time marks reactor leaving; actor is the first subsequent
  # visit at the same bin, and should match actor_cow from replacement_data.
  actor_matches <- visits |>
    dplyr::inner_join(
      replacements |>
        dplyr::select(".repl_row_id", "actor_cow", "bin", "time") |>
        dplyr::rename(.repl_time = "time"),
      by = c(stats::setNames("bin", bin_col)),
      relationship = "many-to-many"
    ) |>
    dplyr::filter(.data[[start_col]] >= .data$.repl_time) |>
    dplyr::group_by(.data$.repl_row_id) |>
    dplyr::slice_min(order_by = .data[[start_col]], n = 1, with_ties = TRUE) |>
    dplyr::ungroup() |>
    dplyr::filter(.data[[id_col]] == .data$actor_cow) |>
    dplyr::distinct(.data$.visit_row_id) |>
    dplyr::pull(.data$.visit_row_id)

  # Add role flags
  visits <- visits |>
    dplyr::mutate(
      is_actor = .data$.visit_row_id %in% actor_matches,
      is_reactor = .data$.visit_row_id %in% reactor_matches
    ) |>
    dplyr::select(-".visit_row_id")

  return(visits)
}


#' Create Empty Meal Role Summary
#'
#' @description
#' Internal function to create an empty result dataframe with proper structure.
#'
#' @keywords internal
#' @noRd
.empty_meal_role_summary <- function(date_val, id_col) {
  result <- data.frame(
    date = character(0),
    meal_id = integer(0),
    total_visits_in_meal = integer(0),
    actor_visits = integer(0),
    reactor_visits = integer(0),
    actor_reactor_visits = integer(0),
    pct_actor = numeric(0),
    pct_reactor = numeric(0),
    pct_actor_reactor = numeric(0),
    stringsAsFactors = FALSE
  )
  # Add id_col as second column
  result[[id_col]] <- character(0)
  result <- result |>
    dplyr::select("date", dplyr::all_of(id_col), dplyr::everything())
  return(result)
}


#' Validate Role Data
#'
#' @description
#' Internal function to validate inputs for role analysis.
#'
#' @keywords internal
#' @noRd
.validate_role_data <- function(visit_data, replacement_data) {
  if (is.null(visit_data)) {
    stop("visit_data cannot be NULL", call. = FALSE)
  }

  if (is.null(replacement_data)) {
    stop("replacement_data cannot be NULL", call. = FALSE)
  }

  # Check if visit_data has meal labels
  check_visits <- if (is.data.frame(visit_data)) {
    visit_data
  } else if (is.list(visit_data) && length(visit_data) > 0) {
    visit_data[[1]]
  } else {
    stop("visit_data must be a data frame or named list of data frames", call. = FALSE)
  }

  if (!"meal_id" %in% names(check_visits)) {
    stop("visit_data must have meal labels. Run meal_label_visits() first.",
         call. = FALSE)
  }
}


#' Summarize Actor/Reactor Roles Per Animal Per Day
#'
#' @description
#' Takes the output from [meal_replacement_roles()] and calculates daily summary
#' statistics (mean, median, SD) of the role percentages across meals for each animal.
#'
#' @param meal_roles_data A data frame or named list of data frames from
#'   [meal_replacement_roles()].
#' @inheritParams set_global_cols
#'
#' @return A data frame (or named list of data frames) with columns:
#'   \itemize{
#'     \item `date` - Date
#'     \item `[id_col]` - Animal identifier
#'     \item `mean_pct_actor` - Average percentage of actor visits across meals
#'     \item `median_pct_actor` - Median percentage of actor visits across meals
#'     \item `sd_pct_actor` - Standard deviation of actor percentage
#'     \item `mean_pct_reactor` - Average percentage of reactor visits across meals
#'     \item `median_pct_reactor` - Median percentage of reactor visits across meals
#'     \item `sd_pct_reactor` - Standard deviation of reactor percentage
#'     \item `mean_pct_actor_reactor` - Average percentage of dual-role visits
#'     \item `median_pct_actor_reactor` - Median percentage of dual-role visits
#'     \item `sd_pct_actor_reactor` - Standard deviation of dual-role percentage
#'     \item `total_meals` - Total number of meals analyzed
#'   }
#'
#' @examples
#' # Create sample meal roles data
#' meal_roles <- data.frame(
#'   date = "2024-01-01",
#'   cow = c("A", "A", "B", "B"),
#'   meal_id = c(1, 2, 1, 2),
#'   total_visits_in_meal = c(5, 4, 6, 3),
#'   actor_visits = c(2, 1, 3, 1),
#'   reactor_visits = c(1, 2, 2, 1),
#'   actor_reactor_visits = c(0, 1, 1, 0),
#'   pct_actor = c(40, 25, 50, 33),
#'   pct_reactor = c(20, 50, 33, 33),
#'   pct_actor_reactor = c(0, 25, 17, 0)
#' )
#'
#' # Summarize per animal per day
#' daily_summary <- meal_replacement_roles_summary(meal_roles)
#'
#' @export
meal_replacement_roles_summary <- function(meal_roles_data,
                                            id_col = id_col2()) {

  # Validate inputs
  if (is.null(meal_roles_data)) {
    stop("meal_roles_data cannot be NULL", call. = FALSE)
  }

  # Determine if input is a list or single dataframe
  is_list <- is.list(meal_roles_data) && !is.data.frame(meal_roles_data)

  if (!is_list) {
    day_name <- if ("date" %in% names(meal_roles_data) && nrow(meal_roles_data) > 0 && !is.na(meal_roles_data$date[1])) {
      as.character(meal_roles_data$date[1])
    } else {
      "day1"
    }
    meal_roles_data <- list(meal_roles_data)
    names(meal_roles_data) <- day_name
  }

  # Process each day
  result <- lapply(names(meal_roles_data), function(day_name) {
    df <- meal_roles_data[[day_name]]

    # Extract date from data if available
    date_val <- if ("date" %in% names(df) && nrow(df) > 0) df$date[1] else day_name

    # Check required columns
    required_cols <- c(id_col, "meal_id", "pct_actor", "pct_reactor", "pct_actor_reactor")
    missing_cols <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
      stop("Missing required columns: ", paste(missing_cols, collapse = ", "),
           ". Data must be from meal_replacement_roles().", call. = FALSE)
    }

    # If empty, return empty structure
    if (nrow(df) == 0) {
      return(.empty_role_summary(date_val, id_col))
    }

    # Summarize per animal per day
    daily_summary <- df |>
      dplyr::group_by(.data[[id_col]]) |>
      dplyr::summarise(
        mean_pct_actor = mean(.data$pct_actor, na.rm = TRUE),
        median_pct_actor = stats::median(.data$pct_actor, na.rm = TRUE),
        sd_pct_actor = stats::sd(.data$pct_actor, na.rm = TRUE),
        mean_pct_reactor = mean(.data$pct_reactor, na.rm = TRUE),
        median_pct_reactor = stats::median(.data$pct_reactor, na.rm = TRUE),
        sd_pct_reactor = stats::sd(.data$pct_reactor, na.rm = TRUE),
        mean_pct_actor_reactor = mean(.data$pct_actor_reactor, na.rm = TRUE),
        median_pct_actor_reactor = stats::median(.data$pct_actor_reactor, na.rm = TRUE),
        sd_pct_actor_reactor = stats::sd(.data$pct_actor_reactor, na.rm = TRUE),
        total_meals = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::mutate(date = date_val) |>
      dplyr::select("date", dplyr::everything())

    return(as.data.frame(daily_summary))
  })

  # Name the result list
  names(result) <- names(meal_roles_data)

  # Return list or single dataframe based on input
  if (!is_list) {
    return(result[[1]])
  } else {
    return(result)
  }
}


#' Create Empty Role Summary
#'
#' @description
#' Internal function to create an empty result dataframe with proper structure
#' for role summary.
#'
#' @keywords internal
#' @noRd
.empty_role_summary <- function(date_val, id_col) {
  result <- data.frame(
    date = character(0),
    mean_pct_actor = numeric(0),
    median_pct_actor = numeric(0),
    sd_pct_actor = numeric(0),
    mean_pct_reactor = numeric(0),
    median_pct_reactor = numeric(0),
    sd_pct_reactor = numeric(0),
    mean_pct_actor_reactor = numeric(0),
    median_pct_actor_reactor = numeric(0),
    sd_pct_actor_reactor = numeric(0),
    total_meals = integer(0),
    stringsAsFactors = FALSE
  )
  # Add id_col as second column
  result[[id_col]] <- character(0)
  result <- result |>
    dplyr::select("date", dplyr::all_of(id_col), dplyr::everything())
  return(result)
}
