# ------------------------ tests for process_feeder() -------------------------#

# Helper to create a temp CSV with known contents
make_csv <- function(df) {
  tmp <- tempfile(fileext = ".csv")
  write.csv(df, tmp, row.names = FALSE)
  tmp
}

test_that("process_feeder reads, filters, and subsets correctly", {
  # Create a toy feeder data frame
  original <- data.frame(
    cow         = c("A","B","C","A"),
    transponder = c("X1","X2","X3","X2"),
    bin         = c(1,2,3,2),
    value       = c(10,20,30,40),
    stringsAsFactors = FALSE
  )
  tmp <- make_csv(original)

  # Drop cow "A" and transponder "X2", keep bins 2:3, select only cow, bin, value
  out <- process_feeder(
    file        = tmp,
    col_names   = names(original),
    drop_ids    = "A",
    drop_trans  = "X2",
    bins        = 2:3,
    select_cols = c("cow","bin","value"),
    header = TRUE
  )
  unlink(tmp)

  # Expect only rows where cow ∉ "A", transponder ∉ "X2", bin ∈ {2,3}
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 1L)
  expect_equal(out$cow, "C")
  expect_equal(out$bin, 3)
  expect_equal(out$value, 30)
})

test_that("when select_cols is NULL, return all columns", {
  # Create a toy feeder data frame
  original <- data.frame(
    cow         = c("A","B","C","A"),
    transponder = c("X1","X2","X3","X2"),
    bin         = c(1,2,3,2),
    value       = c(10,20,30,40),
    stringsAsFactors = FALSE
  )
  tmp <- make_csv(original)

  # Drop cow "A" and transponder "X2", keep bins 2:3
  out <- process_feeder(
    file        = tmp,
    col_names   = names(original),
    drop_ids    = "A",
    drop_trans  = "X2",
    bins        = 2:3,
    header = TRUE
  )
  unlink(tmp)

  # Expect only rows where cow ∉ "A", transponder ∉ "X2", bin ∈ {2,3}
  expect_s3_class(out, "data.frame")
  expect_equal(colnames(out), colnames(original))
  expect_equal(nrow(out), 1L)
  expect_equal(out$cow, "C")
  expect_equal(out$bin, 3)
  expect_equal(out$value, 30)
})


# Create a small CSV to exercise normal and edge cases
df_sample <- data.frame(
  cow         = c("A","B","C"),
  transponder = c("T1","T2","T3"),
  bin         = c(1,2,3),
  value       = c(10,20,30),
  stringsAsFactors = FALSE
)
tmp <- make_csv(df_sample)

test_that("errors if `file` is not a single string", {
  expect_error(
    process_feeder(
      file        = 123,
      col_names   = names(df_sample),
      bins         = 1:3,
      select_cols = c("cow","bin","value"),
      header = TRUE
    ),
    "`file` must be a single character string path"
  )
})

test_that("errors if `col_names` is not a character vector", {
  expect_error(
    process_feeder(
      file        = tmp,
      col_names   = 1:4,
      bins         = 1:3,
      select_cols = c("cow","bin","value"),
      header = TRUE
    ),
    "`col_names` must be a character vector"
  )
})

test_that("errors if `id_col` is not a single string", {
  expect_error(
    process_feeder(
      file        = tmp,
      col_names   = names(df_sample),
      id_col      = c("cow","x"),
      bins        = 1:3,
      select_cols = c("cow","bin","value"),
      header = TRUE
    ),
    "`id_col` must be a single character string"
  )
})

test_that("errors if `trans_col` is not a single string", {
  expect_error(
    process_feeder(
      file        = tmp,
      col_names   = names(df_sample),
      trans_col   = 1,
      bins         = 1:3,
      select_cols = c("cow","bin","value"),
      header = TRUE
    ),
    "`trans_col` must be a single character string"
  )
})

test_that("errors if `bin_col` is not a single string", {
  expect_error(
    process_feeder(
      file        = tmp,
      col_names   = names(df_sample),
      bin_col     = c("bin","x"),
      bins         = 1:3,
      select_cols = c("cow","bin","value"),
      header = TRUE
    ),
    "`bin_col` must be a single character string"
  )
})

test_that("errors if `bins` is not numeric", {
  expect_error(
    process_feeder(
      file        = tmp,
      col_names   = names(df_sample),
      bins         = c("a","b"),
      select_cols = c("cow","bin","value"),
      header = TRUE
    ),
    "`bins` must be a numeric vector"
  )
})

test_that("errors if `select_cols` is not a character vector", {
  expect_error(
    process_feeder(
      file        = tmp,
      col_names   = names(df_sample),
      bins         = 1:3,
      select_cols = 1:3,
      header = TRUE
    ),
    "`select_cols` must be a character vector"
  )
})


test_that("edge case: no drop_ids/trans, only bins filtering", {
  out <- process_feeder(
    file        = tmp,
    col_names   = names(df_sample),
    bins        = 2:3,
    select_cols = c("cow","bin"),
    header = TRUE
  )
  expect_equal(sort(out$bin), c(2,3))
  expect_equal(sort(out$cow), c("B","C"))
})

test_that("edge case: bins exclude all rows → zero‐row output", {
  out <- process_feeder(
    file        = tmp,
    col_names   = names(df_sample),
    bins        = 10:20,
    select_cols = names(df_sample),
    header = TRUE
  )
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
  expect_equal(ncol(out), length(names(df_sample)))
})

# clean up
unlink(tmp)


# ------------------------- tests for process_water() -------------------------#

test_that("process_water reads, shifts bins, and subsets correctly", {
  original <- data.frame(
    cow         = c("D","E","F","D"),
    transponder = c("Y1","Y2","Y3","Y2"),
    bin         = c(5,6,7,6),
    value       = c(50,60,70,80),
    stringsAsFactors = FALSE
  )
  tmp <- make_csv(original)

  # Drop nothing, keep bins 5:7, offset +100, select cow, bin, value
  out <- process_water(
    file        = tmp,
    col_names   = names(original),
    drop_ids    = NULL,
    drop_trans  = NULL,
    bins        = 5:7,
    select_cols = c("cow","bin","value"),
    bin_offset  = 100,
    header = TRUE
  )
  unlink(tmp)

  expect_s3_class(out, "data.frame")
  # After shift, original bins 5,6,7 → 105,106,107
  expect_equal(out$cow, c("D","E","F","D"))
  expect_equal(out$bin, c(105,106,107,106))
  expect_equal(out$value, c(50,60,70,80))
})

test_that("process_water returns empty df when no rows survive", {
  df <- data.frame(cow="X", transponder="T", bin=1, value=1L, stringsAsFactors=FALSE)
  tmp <- make_csv(df)
  # bins do not include 1 → result should be zero rows
  out <- process_water(
    file        = tmp,
    col_names   = names(df),
    bins        = 2:3,
    select_cols = c("cow","bin","value"),
    bin_offset  = 5,
    header = TRUE
  )
  unlink(tmp)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})


test_that("process_water drops only drop_ids", {
  original <- data.frame(
    cow         = c("D","E","F","D"),
    transponder = c("Y1","Y2","Y3","Y2"),
    bin         = c(5,6,7,6),
    value       = c(50,60,70,80),
    stringsAsFactors = FALSE
  )
  tmp <- make_csv(original)

  out <- process_water(
    file        = tmp,
    col_names   = names(original),
    drop_ids    = "E",
    drop_trans  = NULL,
    bins        = 5:7,
    select_cols = c("cow","bin","value"),
    bin_offset  = 100,
    header      = TRUE
  )
  unlink(tmp)

  expect_equal(out$cow,       c("D","F","D"))
  expect_equal(out$bin,       c(105,107,106))
  expect_equal(out$value,     c(50,70,80))
})

test_that("process_water drops only drop_trans", {
  original <- data.frame(
    cow         = c("D","E","F"),
    transponder = c("Y1","Y2","Y3"),
    bin         = c(5,6,7),
    value       = c(50,60,70),
    stringsAsFactors = FALSE
  )
  tmp <- make_csv(original)

  out <- process_water(
    file        = tmp,
    col_names   = names(original),
    drop_ids    = NULL,
    drop_trans  = "Y2",
    bins        = 5:7,
    select_cols = c("cow","bin","value"),
    bin_offset  = 100,
    header      = TRUE
  )
  unlink(tmp)

  expect_equal(out$cow,   c("D","F"))
  expect_equal(out$bin,   c(105,107))
  expect_equal(out$value, c(50,70))
})

test_that("process_water with bin_offset = 0 leaves bins unchanged", {
  original <- data.frame(
    cow         = c("G","H"),
    transponder = c("Z1","Z2"),
    bin         = c(8,9),
    value       = c(80,90),
    stringsAsFactors = FALSE
  )
  tmp <- make_csv(original)

  out <- process_water(
    file        = tmp,
    col_names   = names(original),
    drop_ids    = NULL,
    drop_trans  = NULL,
    bins        = 8:9,
    select_cols = c("cow","bin","value"),
    bin_offset  = 0,
    header      = TRUE
  )
  unlink(tmp)

  expect_equal(out$bin,   c(8,9))
  expect_equal(out$value, c(80,90))
})

test_that("process_water supports custom id_col, trans_col, and bin_col", {
  original2 <- data.frame(
    animal = c("X","Y","Z"),
    tag    = c("A1","B2","C3"),
    bin_id = c(2,3,4),
    val    = c(20,30,40),
    stringsAsFactors = FALSE
  )
  tmp2 <- make_csv(original2)

  out2 <- process_water(
    file        = tmp2,
    col_names   = names(original2),
    id_col      = "animal",
    drop_ids    = "Y",
    trans_col   = "tag",
    drop_trans  = "A1",
    bin_col     = "bin_id",
    bins        = 2:4,
    select_cols = c("animal","bin_id","val"),
    bin_offset  = 10,
    header      = TRUE
  )
  unlink(tmp2)

  expect_equal(out2$animal,  "Z")
  expect_equal(out2$bin_id,  14)  # 4 + 10
  expect_equal(out2$val,     40)
})

test_that("errors if select_cols contains missing column", {
  original <- data.frame(
    cow         = c("D","E","F"),
    transponder = c("Y1","Y2","Y3"),
    bin         = c(5,6,7),
    value       = c(50,60,70),
    stringsAsFactors = FALSE
  )
  tmp <- make_csv(original)

  expect_error(
    process_water(
      file        = tmp,
      col_names   = names(original),
      bins        = 5:7,
      select_cols = c("cow","missing"),
      bin_offset  = 100,
      header      = TRUE
    ),
    "vector contains columns that do not exist in this dataframe"
  )
  unlink(tmp)
})
