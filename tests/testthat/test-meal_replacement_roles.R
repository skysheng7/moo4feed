# ----------------------------------------------------------------------------- #
# Synthetic data helpers                                                        #
# ----------------------------------------------------------------------------- #

make_role_visit_data <- function(cow_ids, bin_ids, meal_ids, start_times, end_times) {
  n <- length(cow_ids)
  df <- data.frame(
    date = if (n == 0) character(0) else rep("2025-01-15", n),
    cow = cow_ids,
    bin = bin_ids,
    meal_id = meal_ids,
    start = as.POSIXct(start_times, tz = "America/Vancouver"),
    end = as.POSIXct(end_times, tz = "America/Vancouver"),
    stringsAsFactors = FALSE
  )
  colnames(df) <- c("date", id_col2(), bin_col2(), "meal_id", start_col2(), end_col2())
  df
}

make_role_replacement_data <- function(actor_ids, reactor_ids, bin_ids, times) {
  data.frame(
    actor_cow = actor_ids,
    reactor_cow = reactor_ids,
    bin = bin_ids,
    time = as.POSIXct(times, tz = "America/Vancouver"),
    stringsAsFactors = FALSE
  )
}

# ----------------------------------------------------------------------------- #
# Happy path                                                                    #
# ----------------------------------------------------------------------------- #

test_that("meal_replacement_roles identifies actor visits", {
  # Animal 1 enters as actor (replaces animal 2)
  visits <- make_role_visit_data(
    cow_ids = c(1, 1),
    bin_ids = c("B1", "B2"),
    meal_ids = c(1, 1),
    start_times = c("2025-01-15 08:30:00", "2025-01-15 09:00:00"),
    end_times = c("2025-01-15 08:50:00", "2025-01-15 09:20:00")
  )

  replacements <- make_role_replacement_data(
    actor_ids = c(1),
    reactor_ids = c(2),
    bin_ids = c("B1"),
    times = c("2025-01-15 08:30:00")  # Matches start of first visit
  )

  result <- meal_replacement_roles(visits, replacements)

  expect_equal(nrow(result), 1)
  expect_equal(result$actor_visits[1], 1)
  expect_equal(result$pct_actor[1], 50)  # 1 out of 2 visits
})

test_that("meal_replacement_roles identifies reactor visits", {
  # Animal 1 leaves as reactor (replaced by animal 2)
  visits <- make_role_visit_data(
    cow_ids = c(1, 1),
    bin_ids = c("B1", "B2"),
    meal_ids = c(1, 1),
    start_times = c("2025-01-15 08:00:00", "2025-01-15 09:00:00"),
    end_times = c("2025-01-15 08:30:00", "2025-01-15 09:20:00")
  )

  replacements <- make_role_replacement_data(
    actor_ids = c(2),
    reactor_ids = c(1),
    bin_ids = c("B1"),
    times = c("2025-01-15 08:30:00")  # Matches end of first visit
  )

  result <- meal_replacement_roles(visits, replacements)

  expect_equal(result$reactor_visits[1], 1)
  expect_equal(result$pct_reactor[1], 50)
})

test_that("meal_replacement_roles identifies actor_reactor visits", {
  # Animal 1: enters as actor, leaves as reactor (same visit)
  visits <- make_role_visit_data(
    cow_ids = c(1),
    bin_ids = c("B1"),
    meal_ids = c(1),
    start_times = c("2025-01-15 08:30:00"),
    end_times = c("2025-01-15 09:00:00")
  )

  replacements <- make_role_replacement_data(
    actor_ids = c(1, 2),
    reactor_ids = c(2, 1),
    bin_ids = c("B1", "B1"),
    times = c("2025-01-15 08:30:00", "2025-01-15 09:00:00")
  )

  result <- meal_replacement_roles(visits, replacements)

  expect_equal(result$actor_reactor_visits[1], 1)
  expect_equal(result$pct_actor_reactor[1], 100)
})

test_that("time matching allows ±1 second tolerance by default for reactor match", {
  visits <- make_role_visit_data(
    cow_ids = c(2, 1),
    bin_ids = c("B1", "B1"),
    meal_ids = c(1, 1),
    start_times = c("2025-01-15 08:00:00", "2025-01-15 08:30:02"),
    end_times = c("2025-01-15 08:30:00", "2025-01-15 08:50:00")
  )

  replacements <- make_role_replacement_data(
    actor_ids = c(1),
    reactor_ids = c(2),
    bin_ids = c("B1"),
    times = c("2025-01-15 08:30:01")  # 1 second after reactor leave
  )

  result <- meal_replacement_roles(visits, replacements)

  expect_equal(result$reactor_visits[result[[id_col2()]] == 2], 1)
})

test_that("time_tolerance parameter is configurable for reactor matching", {
  visits <- make_role_visit_data(
    cow_ids = c(2, 1),
    bin_ids = c("B1", "B1"),
    meal_ids = c(1, 1),
    start_times = c("2025-01-15 08:00:00", "2025-01-15 08:30:04"),
    end_times = c("2025-01-15 08:30:00", "2025-01-15 08:50:00")
  )

  replacements <- make_role_replacement_data(
    actor_ids = c(1),
    reactor_ids = c(2),
    bin_ids = c("B1"),
    times = c("2025-01-15 08:30:03")  # 3 seconds after reactor leave
  )

  # With default tolerance of 1 second, should not match
  result_strict <- meal_replacement_roles(visits, replacements, time_tolerance = 1)
  expect_equal(result_strict$reactor_visits[result_strict[[id_col2()]] == 2], 0)

  # With tolerance of 5 seconds, should match
  result_lenient <- meal_replacement_roles(visits, replacements, time_tolerance = 5)
  expect_equal(result_lenient$reactor_visits[result_lenient[[id_col2()]] == 2], 1)
})

test_that("meal_replacement_roles calculates percentages correctly", {
  # Animal 1: Meal 1 has 3 visits (1 actor, 1 reactor, 1 neutral)
  visits <- make_role_visit_data(
    cow_ids = c(1, 1, 1),
    bin_ids = c("B1", "B2", "B3"),
    meal_ids = c(1, 1, 1),
    start_times = c("2025-01-15 08:00:00", "2025-01-15 08:30:00", "2025-01-15 09:00:00"),
    end_times = c("2025-01-15 08:20:00", "2025-01-15 08:50:00", "2025-01-15 09:20:00")
  )

  replacements <- make_role_replacement_data(
    actor_ids = c(1, 2),
    reactor_ids = c(2, 1),
    bin_ids = c("B1", "B2"),
    times = c("2025-01-15 08:00:00", "2025-01-15 08:50:00")
  )

  result <- meal_replacement_roles(visits, replacements)

  expect_equal(result$total_visits_in_meal[1], 3)
  expect_equal(result$actor_visits[1], 1)
  expect_equal(result$reactor_visits[1], 1)
  expect_equal(result$pct_actor[1], 100 / 3)  # ~33.33%
  expect_equal(result$pct_reactor[1], 100 / 3)
})

test_that("meal_replacement_roles returns meal-level data not daily summary", {
  # Animal 1: 2 meals
  visits <- make_role_visit_data(
    cow_ids = c(1, 1, 1, 1),
    bin_ids = c("B1", "B2", "B1", "B2"),
    meal_ids = c(1, 1, 2, 2),
    start_times = c("2025-01-15 08:00:00", "2025-01-15 08:30:00",
                   "2025-01-15 11:00:00", "2025-01-15 11:30:00"),
    end_times = c("2025-01-15 08:20:00", "2025-01-15 08:50:00",
                 "2025-01-15 11:20:00", "2025-01-15 11:50:00")
  )

  replacements <- make_role_replacement_data(
    actor_ids = c(1, 1),
    reactor_ids = c(2, 2),
    bin_ids = c("B1", "B1"),
    times = c("2025-01-15 08:00:00", "2025-01-15 11:00:00")
  )

  result <- meal_replacement_roles(visits, replacements)

  expect_equal(nrow(result), 2)  # One row per meal
  expect_equal(result$meal_id, c(1, 2))
})

test_that("meal_replacement_roles works with list input", {
  visits <- make_role_visit_data(
    cow_ids = c(1),
    bin_ids = c("B1"),
    meal_ids = c(1),
    start_times = c("2025-01-15 08:30:00"),
    end_times = c("2025-01-15 09:00:00")
  )

  replacements <- make_role_replacement_data(
    actor_ids = c(1),
    reactor_ids = c(2),
    bin_ids = c("B1"),
    times = c("2025-01-15 08:30:00")
  )

  visit_list <- list("2025-01-15" = visits)
  repl_list <- list("2025-01-15" = replacements)

  result <- meal_replacement_roles(visit_list, repl_list)

  expect_true(is.list(result))
  expect_equal(length(result), 1)
  expect_equal(result[[1]]$actor_visits[1], 1)
})

# ----------------------------------------------------------------------------- #
# Edge cases                                                                    #
# ----------------------------------------------------------------------------- #

test_that("outlier visits (meal_id == 0) are excluded", {
  visits <- make_role_visit_data(
    cow_ids = c(1, 1),
    bin_ids = c("B1", "B2"),
    meal_ids = c(0, 1),  # First is outlier
    start_times = c("2025-01-15 08:00:00", "2025-01-15 08:30:00"),
    end_times = c("2025-01-15 08:20:00", "2025-01-15 08:50:00")
  )

  replacements <- make_role_replacement_data(
    actor_ids = c(1, 1),
    reactor_ids = c(2, 2),
    bin_ids = c("B1", "B2"),
    times = c("2025-01-15 08:00:00", "2025-01-15 08:30:00")
  )

  result <- meal_replacement_roles(visits, replacements)

  expect_equal(nrow(result), 1)  # Only meal_id 1
  expect_equal(result$meal_id[1], 1)
})

test_that("no replacements returns zero counts", {
  visits <- make_role_visit_data(
    cow_ids = c(1, 1),
    bin_ids = c("B1", "B2"),
    meal_ids = c(1, 1),
    start_times = c("2025-01-15 08:00:00", "2025-01-15 08:30:00"),
    end_times = c("2025-01-15 08:20:00", "2025-01-15 08:50:00")
  )

  replacements <- make_role_replacement_data(
    actor_ids = integer(0),
    reactor_ids = integer(0),
    bin_ids = character(0),
    times = character(0)
  )

  result <- meal_replacement_roles(visits, replacements)

  expect_equal(result$actor_visits[1], 0)
  expect_equal(result$reactor_visits[1], 0)
  expect_equal(result$pct_actor[1], 0)
  expect_equal(result$pct_reactor[1], 0)
})

test_that("empty visits returns empty result", {
  visits <- make_role_visit_data(
    cow_ids = integer(0),
    bin_ids = character(0),
    meal_ids = integer(0),
    start_times = character(0),
    end_times = character(0)
  )

  replacements <- make_role_replacement_data(
    actor_ids = integer(0),
    reactor_ids = integer(0),
    bin_ids = character(0),
    times = character(0)
  )

  result <- meal_replacement_roles(visits, replacements)

  expect_equal(nrow(result), 0)
  expect_true(all(c("date", id_col2(), "meal_id", "total_visits_in_meal",
                    "pct_actor") %in% names(result)))
})

test_that("all outlier meals returns empty result", {
  visits <- make_role_visit_data(
    cow_ids = c(1, 1),
    bin_ids = c("B1", "B2"),
    meal_ids = c(0, 0),  # All outliers
    start_times = c("2025-01-15 08:00:00", "2025-01-15 08:30:00"),
    end_times = c("2025-01-15 08:20:00", "2025-01-15 08:50:00")
  )

  replacements <- make_role_replacement_data(
    actor_ids = c(1),
    reactor_ids = c(2),
    bin_ids = c("B1"),
    times = c("2025-01-15 08:00:00")
  )

  result <- meal_replacement_roles(visits, replacements)

  expect_equal(nrow(result), 0)
})

test_that("neutral visits have zero role counts", {
  # Visits with no replacement events
  visits <- make_role_visit_data(
    cow_ids = c(1, 1),
    bin_ids = c("B1", "B2"),
    meal_ids = c(1, 1),
    start_times = c("2025-01-15 08:00:00", "2025-01-15 08:30:00"),
    end_times = c("2025-01-15 08:20:00", "2025-01-15 08:50:00")
  )

  replacements <- make_role_replacement_data(
    actor_ids = c(3),  # Different animal
    reactor_ids = c(2),
    bin_ids = c("B3"),
    times = c("2025-01-15 10:00:00")
  )

  result <- meal_replacement_roles(visits, replacements)

  expect_equal(result$total_visits_in_meal[1], 2)
  expect_equal(result$actor_visits[1], 0)
  expect_equal(result$reactor_visits[1], 0)
  expect_equal(result$pct_actor[1], 0)
})

test_that("multiple animals in same meal tracked separately", {
  visits <- make_role_visit_data(
    cow_ids = c(1, 1, 2, 2),
    bin_ids = c("B1", "B2", "B1", "B2"),
    meal_ids = c(1, 1, 1, 1),
    start_times = c("2025-01-15 08:00:00", "2025-01-15 08:30:00",
                   "2025-01-15 07:50:00", "2025-01-15 08:40:00"),
    end_times = c("2025-01-15 08:20:00", "2025-01-15 08:50:00",
                 "2025-01-15 08:00:00", "2025-01-15 09:00:00")
  )

  replacements <- make_role_replacement_data(
    actor_ids = c(1),
    reactor_ids = c(2),
    bin_ids = c("B1"),
    times = c("2025-01-15 08:00:00")
  )

  result <- meal_replacement_roles(visits, replacements)

  expect_equal(nrow(result), 2)  # Two animals
  expect_equal(result$actor_visits[result[[id_col2()]] == 1], 1)
  expect_equal(result$reactor_visits[result[[id_col2()]] == 2], 1)
})

# ----------------------------------------------------------------------------- #
# Input validation                                                              #
# ----------------------------------------------------------------------------- #

test_that("NULL visit_data throws error", {
  replacements <- make_role_replacement_data(
    actor_ids = integer(0),
    reactor_ids = integer(0),
    bin_ids = character(0),
    times = character(0)
  )

  expect_error(
    meal_replacement_roles(NULL, replacements),
    "visit_data cannot be NULL"
  )
})

test_that("NULL replacement_data throws error", {
  visits <- make_role_visit_data(
    cow_ids = 1,
    bin_ids = "B1",
    meal_ids = 1,
    start_times = "2025-01-15 08:00:00",
    end_times = "2025-01-15 08:20:00"
  )

  expect_error(
    meal_replacement_roles(visits, NULL),
    "replacement_data cannot be NULL"
  )
})

test_that("missing meal_id in visit_data throws error", {
  incomplete_visits <- data.frame(
    cow = 1,
    bin = "B1",
    start = as.POSIXct("2025-01-15 08:00:00", tz = "America/Vancouver"),
    end = as.POSIXct("2025-01-15 08:20:00", tz = "America/Vancouver")
  )
  colnames(incomplete_visits)[1] <- id_col2()
  colnames(incomplete_visits)[2] <- bin_col2()
  colnames(incomplete_visits)[3] <- start_col2()
  colnames(incomplete_visits)[4] <- end_col2()

  replacements <- make_role_replacement_data(
    actor_ids = integer(0),
    reactor_ids = integer(0),
    bin_ids = character(0),
    times = character(0)
  )

  expect_error(
    meal_replacement_roles(incomplete_visits, replacements),
    "meal_label_visits"
  )
})

test_that("missing required columns in replacement_data throws error", {
  visits <- make_role_visit_data(
    cow_ids = 1,
    bin_ids = "B1",
    meal_ids = 1,
    start_times = "2025-01-15 08:00:00",
    end_times = "2025-01-15 08:20:00"
  )

  incomplete_replacements <- data.frame(
    actor_cow = 2,
    reactor_cow = 1
  )

  expect_error(
    meal_replacement_roles(visits, incomplete_replacements),
    "Missing required columns in replacement_data"
  )
})

test_that("mismatched day names throws error", {
  visits_list <- list("2025-01-15" = make_role_visit_data(1, "B1", 1,
                                                          "2025-01-15 08:00:00",
                                                          "2025-01-15 08:20:00"))
  repl_list <- list("2025-01-16" = make_role_replacement_data(1, 2, "B1",
                                                               "2025-01-16 08:00:00"))

  expect_error(
    meal_replacement_roles(visits_list, repl_list),
    "matching day names"
  )
})

# ----------------------------------------------------------------------------- #
# meal_replacement_roles_summary tests                                          #
# ----------------------------------------------------------------------------- #

test_that("meal_replacement_roles_summary calculates correct statistics", {
  # Create meal-level data manually (simulating output from meal_replacement_roles)
  meal_roles <- data.frame(
    date = c("2025-01-15", "2025-01-15"),
    meal_id = c(1, 2),
    total_visits_in_meal = c(3, 4),
    actor_visits = c(1, 2),
    reactor_visits = c(1, 1),
    actor_reactor_visits = c(0, 1),
    pct_actor = c(100/3, 50),           # 33.33%, 50%
    pct_reactor = c(100/3, 25),          # 33.33%, 25%
    pct_actor_reactor = c(0, 25),
    stringsAsFactors = FALSE
  )
  meal_roles[[id_col2()]] <- c(1, 1)

  result <- meal_replacement_roles_summary(meal_roles)

  expect_equal(nrow(result), 1)
  expect_equal(result$total_meals[1], 2)
  expect_equal(result$mean_pct_actor[1], (100/3 + 50)/2, tolerance = 0.01)
  expect_equal(result$mean_pct_reactor[1], (100/3 + 25)/2, tolerance = 0.01)
})

test_that("meal_replacement_roles_summary works with list input", {
  meal_roles <- data.frame(
    date = "2025-01-15",
    meal_id = 1,
    total_visits_in_meal = 3,
    actor_visits = 1,
    reactor_visits = 1,
    actor_reactor_visits = 0,
    pct_actor = 33.33,
    pct_reactor = 33.33,
    pct_actor_reactor = 0,
    stringsAsFactors = FALSE
  )
  meal_roles[[id_col2()]] <- 1

  roles_list <- list("2025-01-15" = meal_roles)

  result <- meal_replacement_roles_summary(roles_list)

  expect_true(is.list(result))
  expect_equal(length(result), 1)
})

test_that("meal_replacement_roles_summary returns empty for empty input", {
  empty_roles <- data.frame(
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
  empty_roles[[id_col2()]] <- character(0)

  result <- meal_replacement_roles_summary(empty_roles)

  expect_equal(nrow(result), 0)
  expect_true(all(c("date", id_col2(), "mean_pct_actor", "total_meals") %in% names(result)))
})

test_that("meal_replacement_roles_summary handles multiple animals", {
  meal_roles <- data.frame(
    date = rep("2025-01-15", 4),
    meal_id = c(1, 2, 1, 2),
    total_visits_in_meal = c(3, 3, 4, 4),
    actor_visits = c(1, 1, 2, 2),
    reactor_visits = c(1, 1, 1, 1),
    actor_reactor_visits = c(0, 0, 0, 0),
    pct_actor = c(33.33, 33.33, 50, 50),
    pct_reactor = c(33.33, 33.33, 25, 25),
    pct_actor_reactor = c(0, 0, 0, 0),
    stringsAsFactors = FALSE
  )
  meal_roles[[id_col2()]] <- c(1, 1, 2, 2)

  result <- meal_replacement_roles_summary(meal_roles)

  expect_equal(nrow(result), 2)
  expect_equal(result$total_meals, c(2, 2))
})

test_that("meal_replacement_roles_summary NULL input throws error", {
  expect_error(
    meal_replacement_roles_summary(NULL),
    "meal_roles_data cannot be NULL"
  )
})

test_that("meal_replacement_roles_summary missing columns throws error", {
  incomplete_data <- data.frame(
    date = "2025-01-15",
    meal_id = 1
  )
  incomplete_data[[id_col2()]] <- 1

  expect_error(
    meal_replacement_roles_summary(incomplete_data),
    "Missing required columns"
  )
})
