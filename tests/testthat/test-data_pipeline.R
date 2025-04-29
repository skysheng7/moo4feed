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


test_that("when header = FALSE, and there is no col_names defined, error is prompted", {
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
  expect_error(
    (out <- process_feeder(
      file        = tmp,
      drop_ids    = "A",
      drop_trans  = "X2",
      bins        = 2:3,
      header = FALSE
    )),
    "must be a character vector when header = FALSE."
  )

  unlink(tmp)

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
      header = FALSE
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

test_that("When the length of `col_names` does not equal to the number of columns in the file", {
  original <- data.frame(
    cow         = c("D","E","F","D"),
    transponder = c("Y1","Y2","Y3","Y2"),
    bin         = c(5,6,7,6),
    value       = c(50,60,70,80),
    stringsAsFactors = FALSE
  )
  tmp <- make_csv(original)

  # Drop nothing, keep bins 5:7, offset +100, select cow, bin, value
  expect_error(
    (out <- process_water(
      file        = tmp,
      col_names   = c("cow", "transponder"),
      drop_ids    = NULL,
      drop_trans  = NULL,
      bins        = 5:7,
      select_cols = c("cow","bin","value"),
      bin_offset  = 100,
      header = FALSE
    )),
    "Length of `col_names` must equal number of columns in the file"
  )
  unlink(tmp)

})

test_that("When there is no header, but provided the right col_names", {
  original <- data.frame(
    cow         = c("D","E","F","D"),
    transponder = c("Y1","Y2","Y3","Y2"),
    bin         = c(5,6,7,6),
    value       = c(50,60,70,80),
    stringsAsFactors = FALSE
  )
  tmp <- tempfile(fileext = ".csv")
  write.table(original, tmp, sep       = ",", row.names = FALSE, col.names = FALSE)

  # Drop nothing, keep bins 5:7, offset +100, select cow, bin, value
  out <- process_water(
    file        = tmp,
    col_names   = names(original),
    drop_ids    = NULL,
    drop_trans  = NULL,
    bins        = 5:7,
    select_cols = c("cow","bin","value"),
    bin_offset  = 100,
    header = FALSE
  )
  unlink(tmp)

  expect_s3_class(out, "data.frame")
  # After shift, original bins 5,6,7 → 105,106,107
  expect_equal(out$cow, c("D","E","F","D"))
  expect_equal(out$bin, c(105,106,107,106))
  expect_equal(out$value, c(50,60,70,80))
})


test_that("When there is no header, provided the right col_names, but select_cols is NULL, dataframe return all columns in col_names", {
  original <- data.frame(
    cow         = c("D","E","F","D"),
    transponder = c("Y1","Y2","Y3","Y2"),
    bin         = c(5,6,7,6),
    value       = c(50,60,70,80),
    stringsAsFactors = FALSE
  )
  tmp <- tempfile(fileext = ".csv")
  write.table(original, tmp, sep       = ",", row.names = FALSE, col.names = FALSE)

  # Drop nothing, keep bins 5:7, offset +100, select cow, bin, value
  out <- process_water(
    file        = tmp,
    col_names   = names(original),
    drop_ids    = NULL,
    drop_trans  = NULL,
    bins        = 5:7,
    bin_offset  = 100,
    header = FALSE
  )
  unlink(tmp)

  expect_s3_class(out, "data.frame")
  # After shift, original bins 5,6,7 → 105,106,107
  expect_equal(colnames(out), names(original))
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
    "Some `select_cols` are not present in the data.frame"
  )
  unlink(tmp)
})



# ----------------------- tests for process_all_feed() -------------------------#

test_that("process_all_feed() errors on bad inputs", {
  expect_error(
    process_all_feed(files = 1:2, col_names = c("cow","bin","start","end"), bins = 1),
    "`files` must be a nonempty character vector"
  )
  expect_error(
    process_all_feed(files = character(0), col_names = c("cow","bin","start","end"), bins = 1),
    "`files` must be a nonempty character vector"
  )
  expect_error(
    process_all_feed(files = "x.csv", col_names = list(), bins = 1),
    "`col_names` must be a character vector"
  )
})

test_that("process_all_feed() with empty CSVs yields zero‐row data.frames named by date", {
  tmp <- tempdir()
  # create two empty CSVs with headers only
  files <- file.path(tmp, c("20220101.csv", "20220102.csv"))
  for(f in files) {
    write.csv(
      data.frame(cow=character(), transponder=character(), bin=integer(),
                 start=character(), end=character()),
      file = f, row.names = FALSE
    )
  }

  expect_message(
    expect_message(
      expect_message(out <- process_all_feed(
        files       = files,
        col_names   = c("cow","transponder","bin","start","end"),
        bins        = 1:10,
        select_cols = c("cow","bin","start","end"),
        sep         = ",",
        header      = TRUE,
        tz          = "UTC"),
        "No DST transitions found for the given years and time zone"),
      "File has no data rows"
    ),
  "File has no data rows"
  )


  expect_length(out, 2)
  expect_named(out, c("2022-01-01","2022-01-02"))
  lapply(out, function(df) {
    expect_s3_class(df, "data.frame")
    expect_equal(nrow(df), 0L)
    expect_equal(colnames(df), c("cow","bin","start","end"))
  })
})

test_that("process_all_feed() correctly reads and time‐converts one row", {
  tmp <- tempdir()
  f <- file.path(tmp, "20220103.csv")
  write.csv(
    data.frame(
      cow         = "A",
      transponder = "T1",
      bin         = 2,
      start       = "05:30:00",
      end         = "05:31:00"
    ),
    file = f, row.names = FALSE
  )

  expect_message(
    (out <- process_all_feed(
      files       = f,
      col_names   = c("cow","transponder","bin","start","end"),
      bins        = 1:5,
      select_cols = c("cow","bin","start","end"),
      sep         = ",",
      header      = TRUE,
      tz          = "UTC"
    )),
    "No DST transitions found for the given years and time zone"
  )


  df <- out[["2022-01-03"]]
  expect_equal(nrow(df), 1L)
  expect_equal(df$cow, "A")
  expect_equal(df$bin, 2)
  # start/end should now be POSIXct
  expect_s3_class(df$start, c("POSIXct","POSIXt"))
  expect_equal(format(df$start, "%Y-%m-%d %H:%M:%S", tz="UTC"),
               "2022-01-03 05:30:00")
  expect_s3_class(df$end, c("POSIXct","POSIXt"))
  expect_equal(format(df$end, "%Y-%m-%d %H:%M:%S", tz="UTC"),
               "2022-01-03 05:31:00")
  expect_equal(format(df$date, "%Y-%m-%d", tz="UTC"),
               "2022-01-03")
})


# ---------------------- tests for process_all_water() ------------------------#

test_that("process_all_water() applies bin_offset and same time‐conversion logic", {
  tmp <- tempdir()
  f <- file.path(tmp, "20220301.csv")
  write.csv(
    data.frame(
      cow         = "B",
      transponder = "T2",
      bin         = 3,
      start       = "12:00:00",
      end         = "12:05:00"
    ),
    file = f, row.names = FALSE
  )

  expect_message(
    out <- process_all_water(
      files       = f,
      col_names   = c("cow","transponder","bin","start","end"),
      bins        = 1:5,
      select_cols = c("cow","bin","start","end"),
      bin_offset  = 10,
      sep         = ",",
      header      = TRUE,
      tz          = "UTC"
    ),
  "No DST transitions found for the given years and time zone"
  )

  df <- out[["2022-03-01"]]
  expect_equal(nrow(df), 1L)
  expect_equal(df$cow, "B")
  # bin should have been offset by +10
  expect_equal(df$bin, 3 + 10)
  # times still converted to POSIXct
  expect_s3_class(df$start, c("POSIXct","POSIXt"))
  expect_equal(format(df$start, "%Y-%m-%d %H:%M:%S", tz="UTC"),
               "2022-03-01 12:00:00")
  expect_s3_class(df$end, c("POSIXct","POSIXt"))
  expect_equal(format(df$end, "%Y-%m-%d %H:%M:%S", tz="UTC"),
               "2022-03-01 12:05:00")
  expect_equal(format(df$date, "%Y-%m-%d", tz="UTC"),
               "2022-03-01")
})

test_that("process_all_water() errors on bad inputs too", {
  expect_error(
    process_all_water(files = NULL, col_names = c("cow","bin"), bins = 1),
    "`files` must be a nonempty character vector"
  )
  expect_error(
    process_all_water(files = "x.csv", col_names = 1:5, bins = 1),
    "`col_names` must be a character vector"
  )
})
