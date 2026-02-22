test_that("tz2() / set_tz2() work", {
  orig <- tz2()
  new  <- if (orig == "UTC") "America/Vancouver" else "UTC"

  out  <- set_tz2(new)
  expect_identical(out, orig)
  expect_identical(tz2(), new)

  set_tz2(orig)                # restore
  expect_identical(tz2(), orig)
})

test_that("id_col2() / set_id_col2() work", {
  orig <- id_col2()
  new  <- "animal_id"

  out  <- set_id_col2(new)
  # id setter return old value
  expect_identical(out, orig)
  expect_identical(id_col2(), new)

  set_id_col2(orig)
  expect_identical(id_col2(), orig)
})

test_that("trans_col2() / set_trans_col2() work", {
  orig <- trans_col2()
  new  <- "tag_id"

  out  <- set_trans_col2(new)
  expect_identical(out, orig)
  expect_identical(trans_col2(), new)

  set_trans_col2(orig)
})

test_that("start_col2() / set_start_col2() work", {
  orig <- start_col2()
  new  <- "visit_begin"

  out  <- set_start_col2(new)
  expect_identical(out, orig)
  expect_identical(start_col2(), new)

  set_start_col2(orig)
})

test_that("end_col2() / set_end_col2() work", {
  orig <- end_col2()
  new  <- "visit_end"

  out  <- set_end_col2(new)
  expect_identical(out, orig)
  expect_identical(end_col2(), new)

  set_end_col2(orig)
})

test_that("bin_col2() / set_bin_col2() work", {
  orig <- bin_col2()
  new  <- "feeder_bin"

  out  <- set_bin_col2(new)
  expect_identical(out, orig)
  expect_identical(bin_col2(), new)

  set_bin_col2(orig)
})

test_that("bin_offset2() / set_bin_offset2() work", {
  orig <- bin_offset2()
  new  <- orig + 10

  out  <- set_bin_offset2(new)
  expect_identical(out, orig)
  expect_identical(bin_offset2(), new)

  set_bin_offset2(orig)
})

test_that("bins_feed2() / set_bins_feed2() work", {
  orig <- bins_feed2()
  new  <- 1:5

  out  <- set_bins_feed2(new)
  expect_identical(out, orig)
  expect_identical(bins_feed2(), new)

  set_bins_feed2(orig)
})

test_that("bins_wat2() / set_bins_wat2() work", {
  orig <- bins_wat2()
  new  <- c(1L, 3L)

  out  <- set_bins_wat2(new)
  expect_identical(out, orig)
  expect_identical(bins_wat2(), new)

  set_bins_wat2(orig)
})

test_that("bin_layout2() / set_bin_layout2() work", {
  orig <- bin_layout2()
  new  <- "1-101-2-102-3"

  out  <- set_bin_layout2(new)
  expect_identical(out, orig)
  expect_identical(bin_layout2(), new)

  set_bin_layout2(orig)
  expect_identical(bin_layout2(), orig)
})

# Comprehensive tests for bin_layout2 validation
test_that("set_bin_layout2() validates input correctly", {
  orig <- bin_layout2()
  
  # Test error for non-character input
  expect_error(set_bin_layout2(c(1, 2, 3)), 
               "`new_layout` must be a character string")
  
  # Test error for empty input
  expect_error(set_bin_layout2(""), 
               "`new_layout` cannot be empty")
  
  # Test error for invalid bin numbers (non-numeric)
  expect_error(set_bin_layout2("a-b-c"), 
               "Invalid bin numbers in layout")
  
  # Test error for duplicate bin IDs
  expect_error(set_bin_layout2("1-2-1-3"), 
               "Duplicate bin IDs found in layout: 1")
  
  # Test error for multiple duplicates
  expect_error(set_bin_layout2("1-2-1-3-2"), 
               "Duplicate bin IDs found in layout:")
  
  # Restore original after tests
  set_bin_layout2(orig)
})

test_that("set_bin_layout2() handles overlapping raw bin IDs correctly based on offset", {
  orig_layout <- bin_layout2()
  orig_feed <- bins_feed2()
  orig_wat <- bins_wat2()
  orig_offset <- bin_offset2()
  
  # Scenario 1: Raw bins overlap (1:5 feed, 1:3 water), but offset = 100 resolves it
  # After offset: feed stays 1:5, water becomes 101:103 - no conflict!
  set_bins_feed2(1:5)
  set_bins_wat2(1:3)  # Raw overlap with feed bins - this is OK!
  set_bin_offset2(100)
  
  # Should work without warnings because offset resolves the conflict
  expect_no_warning(set_bin_layout2("1-2-101-3-4-102-5-103"))
  
  # Scenario 2: Raw bins overlap AND offset doesn't resolve it
  # Feed: 1:10, Water: 1:5, Offset: 5
  # After offset: feed stays 1:10, water becomes 6:10 - still overlaps with feed 6:10!
  set_bins_feed2(1:10)
  set_bins_wat2(1:5)
  set_bin_offset2(5)  # Too small to resolve conflict
  
  # Using a layout where water bins use their offset IDs (6-10)
  # This will trigger BOTH warnings:
  # 1. Warning about raw overlap not being resolved by offset
  # 2. Warning about ambiguous bin IDs in layout (6,7 could be feed OR water)
  expect_warning(
    expect_warning(
      set_bin_layout2("1-2-6-3-4-7"),
      "match both feed bins and water bins"
    ),
    "Even after applying bin_offset"
  )
  
  # Scenario 3: Raw bins overlap, but offset = 0 means they stay overlapped
  set_bins_feed2(1:5)
  set_bins_wat2(1:3)
  set_bin_offset2(0)  # No offset - conflict remains
  
  # With offset = 0, water_bins_with_offset = 1:3, which overlaps with feed 1:5
  # Layout "1-2-3" has ambiguous bins that could be either feed or water
  expect_warning(
    expect_warning(
      set_bin_layout2("1-2-3"),
      "match both feed bins and water bins"
    ),
    "Even after applying bin_offset"
  )
  
  # Restore original values
  set_bin_layout2(orig_layout)
  set_bins_feed2(orig_feed)
  set_bins_wat2(orig_wat)
  set_bin_offset2(orig_offset)
})

test_that("set_bin_layout2() handles bin_offset correctly with water bin transformation", {
  orig_layout <- bin_layout2()
  orig_feed <- bins_feed2()
  orig_wat <- bins_wat2()
  orig_offset <- bin_offset2()
  
  # User scenario: raw water bins are 1:5, will be transformed to 101:105
  set_bins_feed2(1:30)
  set_bins_wat2(1:5)
  set_bin_offset2(100)
  
  # Layout includes the TRANSFORMED water bin IDs (101-105)
  # This should work without warnings, with informative message about offset
  expect_message(
    set_bin_layout2("1-2-3-4-5-6-101-102-7-8-9-10-103-104-105"),
    "Bin offset is set to 100"
  )
  
  # Verify no warning about overlapping IDs (because offset will resolve it)
  expect_no_warning(set_bin_layout2("1-2-3-4-5-6-101-102-7-8-9-10-103-104-105"))
  
  # Restore original values
  set_bin_layout2(orig_layout)
  set_bins_feed2(orig_feed)
  set_bins_wat2(orig_wat)
  set_bin_offset2(orig_offset)
})

test_that("set_bin_layout2() provides helpful messages about missing/extra bins", {
  orig_layout <- bin_layout2()
  orig_feed <- bins_feed2()
  orig_wat <- bins_wat2()
  
  # Set up specific bin lists
  set_bins_feed2(1:3)
  set_bins_wat2(101:102)
  
  # Test with missing bins (should get message)
  expect_message(set_bin_layout2("1-2"), 
                 "not included in the layout")
  
  # Test with extra bins (should get message)
  expect_message(set_bin_layout2("1-2-3-101-102-999"), 
                 "not in your feed/water bin lists")
  
  # Restore original values
  set_bin_layout2(orig_layout)
  set_bins_feed2(orig_feed)
  set_bins_wat2(orig_wat)
})

test_that("set_bin_layout2() handles valid layouts correctly", {
  orig_layout <- bin_layout2()
  orig_feed <- bins_feed2()
  orig_wat <- bins_wat2()
  orig_offset <- bin_offset2()
  
  # Scenario: Raw bins overlap (feed 1:5, water 1:3) but offset = 100 makes water bins 101:103
  set_bins_feed2(1:5)
  set_bins_wat2(1:3)
  set_bin_offset2(100)
  
  # Valid single-row layout with offset water bins (101-103)
  expect_no_warning(set_bin_layout2("1-2-101-3-4-102-5-103"))
  expect_equal(bin_layout2(), "1-2-101-3-4-102-5-103")
  
  # Valid multi-row layout should also work
  expect_no_warning(set_bin_layout2("1-2-101\n3-4-102\n5-103"))
  expect_equal(bin_layout2(), "1-2-101\n3-4-102\n5-103")
  
  # Restore original values
  set_bin_layout2(orig_layout)
  set_bins_feed2(orig_feed)
  set_bins_wat2(orig_wat)
  set_bin_offset2(orig_offset)
})

test_that("set_bin_layout2() handles multi-row layouts with different row lengths", {
  orig_layout <- bin_layout2()
  orig_feed <- bins_feed2()
  orig_wat <- bins_wat2()
  orig_offset <- bin_offset2()
  
  # Set up bin lists with raw overlap (feed 1:10, water 1:3, offset makes water 101:103)
  set_bins_feed2(1:10)
  set_bins_wat2(1:3)
  set_bin_offset2(100)
  
  # Multi-row layout with different row lengths (common in real barns)
  # Row 1: 5 bins, Row 2: 6 bins, Row 3: 2 bins
  layout_str <- "1-2-3-4-5\n6-7-8-9-10-101\n102-103"
  expect_no_warning(set_bin_layout2(layout_str))
  expect_equal(bin_layout2(), layout_str)
  
  # Restore original values
  set_bin_layout2(orig_layout)
  set_bins_feed2(orig_feed)
  set_bins_wat2(orig_wat)
  set_bin_offset2(orig_offset)
})

test_that("set_bin_layout2() handles layouts with extra whitespace", {
  orig_layout <- bin_layout2()
  orig_feed <- bins_feed2()
  orig_wat <- bins_wat2()
  orig_offset <- bin_offset2()
  
  # Set up bin lists with raw overlap (feed 1:5, water 1:2, offset makes water 101:102)
  set_bins_feed2(1:5)
  set_bins_wat2(1:2)
  set_bin_offset2(100)
  
  # Layout with extra spaces (should be trimmed)
  layout_str <- " 1 - 2 - 3 \n 4 - 5 - 101 - 102 "
  expect_no_warning(set_bin_layout2(layout_str))
  
  # Restore original values
  set_bin_layout2(orig_layout)
  set_bins_feed2(orig_feed)
  set_bins_wat2(orig_wat)
  set_bin_offset2(orig_offset)
})

# duration_col2 / set_duration_col2 test
test_that("duration_col2() / set_duration_col2() work", {
  orig <- duration_col2()
  new  <- if (orig == "duration") "visit_duration" else "duration"

  out  <- set_duration_col2(new)
  expect_identical(out, orig)
  expect_identical(duration_col2(), new)

  set_duration_col2(orig) # restore
  expect_identical(duration_col2(), orig)
})

# intake_col2 / set_intake_col2 test
test_that("intake_col2() / set_intake_col2() work", {
  orig <- intake_col2()
  new  <- if (orig == "intake") "feed_intake" else "intake"

  out  <- set_intake_col2(new)
  expect_identical(out, orig)
  expect_identical(intake_col2(), new)

  set_intake_col2(orig) # restore
  expect_identical(intake_col2(), orig)
})

# start_weight_col2 / set_start_weight_col2 test
test_that("start_weight_col2() / set_start_weight_col2() work", {
  orig <- start_weight_col2()
  new  <- if (orig == "start_weight") "initial_weight" else "start_weight"

  out  <- set_start_weight_col2(new)
  expect_identical(out, orig)
  expect_identical(start_weight_col2(), new)

  set_start_weight_col2(orig) # restore
  expect_identical(start_weight_col2(), orig)
})

# end_weight_col2 / set_end_weight_col2 test
test_that("end_weight_col2() / set_end_weight_col2() work", {
  orig <- end_weight_col2()
  new  <- if (orig == "end_weight") "final_weight" else "end_weight"

  out  <- set_end_weight_col2(new)
  expect_identical(out, orig)
  expect_identical(end_weight_col2(), new)

  set_end_weight_col2(orig) # restore
  expect_identical(end_weight_col2(), orig)
})

# Tests for set_global_cols()
test_that("set_global_cols() correctly sets multiple global variables", {
  # Save original values
  orig_tz <- tz2()
  orig_id_col <- id_col2()
  orig_trans_col <- trans_col2()
  orig_start_col <- start_col2()
  orig_end_col <- end_col2()
  orig_bin_col <- bin_col2()
  orig_dur_col <- duration_col2()
  orig_intake_col <- intake_col2()
  orig_start_weight_col <- start_weight_col2()
  orig_end_weight_col <- end_weight_col2()
  orig_bin_offset <- bin_offset2()
  orig_bins_feed <- bins_feed2()
  orig_bins_wat <- bins_wat2()
  orig_bin_layout <- bin_layout2()

  # Set new values
  set_global_cols(
    tz = "UTC",
    id_col = "animal_id",
    trans_col = "tag_id",
    start_col = "start_time",
    end_col = "end_time",
    bin_col = "feeder_bin",
    dur_col = "visit_duration",
    intake_col = "feed_intake",
    start_weight_col = "initial_weight",
    end_weight_col = "final_weight",
    bin_offset = 50,
    bins_feed = 1:20,
    bins_wat = 1:3,
    bin_layout = "1-2-101-3-4-102"
  )

  # Check if new values are set correctly
  expect_equal(tz2(), "UTC")
  expect_equal(id_col2(), "animal_id")
  expect_equal(trans_col2(), "tag_id")
  expect_equal(start_col2(), "start_time")
  expect_equal(end_col2(), "end_time")
  expect_equal(bin_col2(), "feeder_bin")
  expect_equal(duration_col2(), "visit_duration")
  expect_equal(intake_col2(), "feed_intake")
  expect_equal(start_weight_col2(), "initial_weight")
  expect_equal(end_weight_col2(), "final_weight")
  expect_equal(bin_offset2(), 50)
  expect_equal(bins_feed2(), 1:20)
  expect_equal(bins_wat2(), 1:3)
  expect_equal(bin_layout2(), "1-2-101-3-4-102")

  # Restore original values
  set_global_cols(
    tz = orig_tz,
    id_col = orig_id_col,
    trans_col = orig_trans_col,
    start_col = orig_start_col,
    end_col = orig_end_col,
    bin_col = orig_bin_col,
    dur_col = orig_dur_col,
    intake_col = orig_intake_col,
    start_weight_col = orig_start_weight_col,
    end_weight_col = orig_end_weight_col,
    bin_offset = orig_bin_offset,
    bins_feed = orig_bins_feed,
    bins_wat = orig_bins_wat,
    bin_layout = orig_bin_layout
  )

  # Verify original values are restored
  expect_equal(tz2(), orig_tz)
  expect_equal(id_col2(), orig_id_col)
  expect_equal(trans_col2(), orig_trans_col)
  expect_equal(start_col2(), orig_start_col)
  expect_equal(end_col2(), orig_end_col)
  expect_equal(bin_col2(), orig_bin_col)
  expect_equal(duration_col2(), orig_dur_col)
  expect_equal(intake_col2(), orig_intake_col)
  expect_equal(start_weight_col2(), orig_start_weight_col)
  expect_equal(end_weight_col2(), orig_end_weight_col)
  expect_equal(bin_offset2(), orig_bin_offset)
  expect_equal(bins_feed2(), orig_bins_feed)
  expect_equal(bins_wat2(), orig_bins_wat)
  expect_equal(bin_layout2(), orig_bin_layout)
})
