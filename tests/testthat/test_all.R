# test make_filename
test_that("make_filename generates the file name", {
  expect_equal(make_filename(2010), "accident_2010.csv.bz2")
})

# test fars_read_years
test_that("fars_read_years return NULL if the file is missing", {
  expect_equal(fars_read_years(2010), list(NULL))
})
