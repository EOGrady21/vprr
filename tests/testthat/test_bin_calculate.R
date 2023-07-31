context('Binning and calculations')
library(testthat)
# library(vprr)

data("ctd_roi_merge")
data("roimeas_dat_combine")

year <- 2019
test_date  <- '2019-08-11'
binSize <- 5
imageVolume <- 83663
category_of_interest <- 'Calanus'


data <- ctd_roi_merge %>%
  dplyr::mutate(., avg_hr = time_ms / 3.6e+06)

test_that('VPR dates are properly calculated',{

  expect_silent(data <- vpr_ctd_ymd(data, year))
  expect_equal(unique(as.Date(data$ymdhms)), as.Date(test_date)) #check that date is accurate


})


test_that('VPR data is properly binned',{

  expect_silent(ctd_roi_oce <- vpr_oce_create(data))
  expect_true('ctd' %in% class(ctd_roi_oce)) # check that output is oce ctd object


  # bin and calculate concentration for all category (combined)
  # expect_silent(vpr_depth_bin <- bin_cast(ctd_roi_oce = ctd_roi_oce, binSize =  binSize, imageVolume = imageVolume))
  # expect_true(max(vpr_depth_bin$depth_diff) < binSize) # check that bin size is enforced
  # expect_true(diff(vpr_depth_bin$depth)[1] >0) # check that depth is increasing

  # test reversed bins
  # expect_silent(vpr_depth_bin_rev <- bin_cast(ctd_roi_oce = ctd_roi_oce, binSize =  binSize, imageVolume = imageVolume, rev = TRUE))
  # expect_true(max(vpr_depth_bin_rev$depth_diff) < binSize) # check that bin size is enforced
  # expect_true(diff(vpr_depth_bin_rev$depth)[1] >0) # check that depth is increasing
  #
  # expect_true(max(vpr_depth_bin_rev$max_depth) > max(vpr_depth_bin$max_depth)) # max bin depth should be greater if bins are reversed
  # expect_true(min(vpr_depth_bin$min_depth) < min(vpr_depth_bin_rev$min_depth)) # min bin depth should be greater with reversed bins
  # expect_equal(vpr_depth_bin$max_cast_depth, vpr_depth_bin_rev$max_cast_depth) # max cast depth should not change
  #
  #
  # category_list <- unique(roimeas_dat_combine$category)
  #
  # # bin and calculate concentrations for each category
  # expect_silent(category_conc_n <- vpr_roi_concentration(data, category_list, station_of_interest, binSize, imageVolume))
  # expect_true(is.data.frame(category_conc_n)) # check output is data frame
  # expect_equal(length(category_conc_n[[1]])/length(category_list), length(vpr_depth_bin[[1]])) # same number of bins, just multiplied by number of category
  # expect_identical(category_list, unique(category_conc_n$category)) # check that all category are included

  # TODO: try and test concentration calculation...


  # bin size data
  expect_message(size_df_f <- vpr_ctdroisize_merge(data, data_mea = roimeas_dat_combine, category_of_interest = category_of_interest))
  expect_true(is.data.frame(size_df_f))
  expect_true(length(size_df_f[[1]]) > 0)
  expect_identical(unique(size_df_f$category), category_of_interest)
  expect_identical(stringr::str_sub(size_df_f$roi, 1, 8), as.character(size_df_f$time_ms))
  expect_identical(stringr::str_sub(size_df_f$roi, 1, 8), size_df_f$roi_ID)
  expect_true(is.numeric(size_df_f$long_axis_length))



})
