# test make_filename
test_that("make_filename generates the file name", {
  expect_equal(make_filename(2010), "accident_2010.csv.bz2")
})

