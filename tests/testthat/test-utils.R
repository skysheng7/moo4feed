# ----------------------- test for read_data_safely() -------------------------#

test_that("reads a well-formed CSV file correctly", {
  df_orig <- data.frame(a = 1:3, b = 4:6)
  tmp <- tempfile(fileext = ".csv")
  write.csv(df_orig, tmp, row.names = FALSE)

  df <- read_data_safely(tmp, sep = ",", header = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(names(df), names(df_orig))
  expect_equal(df, df_orig)

  unlink(tmp)
})

test_that("returns empty data frame for non‑existent file", {
  missing <- tempfile()
  expect_message(
    out <- read_data_safely(missing),
    "File missing, or the file is empty"
  )
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
  expect_equal(ncol(out), 0L)
})

test_that("returns empty data frame for zero‑length file", {
  tmp <- tempfile()
  file.create(tmp)
  # ensure size == 0
  expect_equal(file.info(tmp)$size, 0)

  expect_message(
    out <- read_data_safely(tmp),
    "File missing, or the file is empty"
  )
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
  expect_equal(ncol(out), 0L)

  unlink(tmp)
})

test_that("returns empty data frame for header‑only file when header=TRUE", {
  tmp <- tempfile()
  writeLines("x,y,z", tmp)
  expect_message(
    out <- read_data_safely(tmp, sep = ",", header = TRUE),
    "File has no data rows; returning an empty data frame"
  )
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
  expect_equal(ncol(out), 0L)
  unlink(tmp)
})

test_that("catches read errors and returns empty data frame", {
  # point at a directory to trigger a read error
  dir_path <- tempdir()
  expect_message(
    out <- read_data_safely(dir_path),
    "this path is to a folder instead of a file"
  )
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
  expect_equal(ncol(out), 0L)
})

test_that("errors on invalid `file` argument", {
  expect_error(
    read_data_safely(123),
    "`file` must be a single character string"
  )
  expect_error(
    read_data_safely(letters[1:2]),
    "`file` must be a single character string"
  )
})


# ----------------------- test for moo4feed_example() -------------------------#
test_that("moo4feed_example() lists the six bundled .DAT files", {
  files <- moo4feed_example()

  # The listing should be character, length 6, and match the expected names
  expect_type(files, "character")
  expect_length(files, 4)
  expect_setequal(
    files,
    c(
      "VR201031.DAT", "VR201101.DAT",
      "VW201031.DAT", "VW201101.DAT"
    )
  )
})

test_that("moo4feed_example() returns a valid absolute path", {
  # Retrieve one of the files (same as in your example)
  fp <- moo4feed_example("VR201101.DAT")

  # It should exist on disk and end with the correct filename
  expect_true(file.exists(fp))
  expect_true(grepl("VR201101\\.DAT$", fp))
})

test_that("moo4feed_example() errors on a missing file name", {
  # Intentionally ask for something that is NOT in extdata
  expect_error(moo4feed_example("totally_not_there.DAT"))
})
