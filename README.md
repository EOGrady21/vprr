
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vprr

<!-- badges: start -->

<!-- badges: end -->

The goal of vprr is to process Video Plankton Recorder (VPR) data in R.
This package helps to format data, calculate important ecological
metrics such as concentration of plankton, save data in easy to analyze
formats with self contained meta data, as well as analyze and display
data in plots.

Mpre detailed information about the package and its uses can be found in
the VPR\_processing vignette.

## Installation

You can install the released version of vprr from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("vprr")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Echisholm21/vprr")
```

## Example

VPRR has an interactive GUI which can be used to manually check Visual
Plankton image classifications.

``` r
drive <- 'C:/'
auto_id_folder <- paste0(drive, "cruise_", cruise, "/", autoid)

day <- '222' 
hr <- '03'

category_of_interest <-
  c(
    'krill',
    'Calanus')

# reclassify images
vpr_manual_classification(day = day, hour= hr, basepath = auto_id_folder,gr = FALSE, 
          taxa_of_interest = category_of_interest, scale = 'x300',
          opticalSetting = 'S2')
```

Data files from image classification can be read in and combined into
easy to work with data frames.

``` r
library(vprr)

# CTD data read in 
ctd_files[[1]] <- system.file('extdata/COR2019002/rois/vpr5/d222', 'h03ctd.dat', package = 'vprr', mustWork = TRUE)
ctd_files[[2]] <- system.file('extdata/COR2019002/rois/vpr5/d222', 'h04ctd.dat', package = 'vprr', mustWork = TRUE)

station_of_interest <- 'example'

ctd_data <- vpr_ctd_read(ctd_files, station_of_interest)

# VP autoID file read in 

autoid_files <- list.files(system.file('extdata/COR2019002/autoid/', package = 'vprr'), recursive = TRUE)
autoidfull_files <- file.path(system.file('extdata/COR2019002/autoid/', package = 'vprr'), autoid_files)

aidmea_files <- grep(autoidfull_files, pattern = 'aidmea', value = TRUE)
aid_files <- autoidfull_files[!autoidfull_files %in% aidmea_files]

roi_dat_combine <-
    vpr_autoid_read(
      file_list_aid = aid_files,
      file_list_aidmeas = aidmea_files,
      export = 'aid',
      station_of_interest = station_of_interest,
      opticalSetting = opticalSetting
    )
```

Plankton concentration can be calculated from VPR data. Concentration is
calculated in bins along the VPR path for each category identified in
image classifications.

``` r

data("ctd_roi_merge") # CTD and ROI data
data("roimeas_dat_combine") # Measurement data

binSize <- 5
imageVolume <- 83663


data <- ctd_roi_merge %>%
  dplyr::mutate(., avg_hr = time_ms / 3.6e+06)

taxas_list <- unique(roimeas_dat_combine$taxa)

taxa_conc_n <- vpr_roi_concentration(data, taxas_list, station_of_interest, binSize, imageVolume)
```
