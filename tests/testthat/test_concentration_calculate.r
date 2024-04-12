library(testthat)

# Define a test for concentration_category
test_that("concentration_category returns expected output", {

data(ctd_roi_merge)
  # Call concentration_category with binSize = 1 and imageVolume = 1000
  result <- concentration_category(ctd_roi_merge, "Calanus", 1, 1000)

  # Check that the result is a data frame with the expected columns
  expect_is(result, "data.frame")
  expect_true(unique(c("n_roi_bin", "conc_m3") %in% colnames(result)))


  # Check that the result has the expected values (weak to changes in test data)
  expect_equal(range(result$n_roi_bin), c(0, 2)) # tests specific to this dataset
  expect_true(max(result$conc) > 150000)
  expect_true(max(result$conc) != 0)

})
