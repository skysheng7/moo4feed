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
