##### Import packages ####
#' Packages
#' @name package-imports
#' @import dplyr ggplot2 oce
#' @importFrom graphics hist par plot.new
#' @importFrom stats aggregate median quantile
#' @importFrom utils menu read.csv read.table write.table
#' @importFrom usethis use_data
#' @importFrom data.table rbindlist
#' @importFrom fs file_copy dir_create
#' @importFrom tools file_ext
#' @importFrom rjson toJSON
#'
#' @rawNamespace import(gridExtra, except = combine)
#' @rawNamespace import(metR, except = coriolis)
#'
NULL

options(dplyr.summarise.inform = FALSE) # TODO: is this needed?

#### PROCESSING FUNCTIONS ####

### CNN model helpers ----

read_aid_cnn <- function(aid_file) {
  #' Read aid files produced by automated classification
  #'
  #' @param aid_file a file path to an aid file produced by automated classification (with ROI path and probability value)
  #'
  #' @return ROI path and probability values in a table
  #' @export
  #'
  aid_table <- read.table(aid_file, sep = " ")
  names(aid_table) <- c('roi', 'confidence')

  return(aid_table)
}

vpr_pred_read <- function(filename) {
  #' Read prediction output from a CNN model
  #'
  #' @param filename model prediction output file (.txt) from `vpr_transferlearn::save_output()`
  #'
  #' @return a dataframe
  #' @export
  #'
  #'
  # Check that the file exists
  if (!file.exists(filename)) {
    stop("File not found")
  }

  # Check that the file is a .txt file
  if (tolower(tools::file_ext(filename)) != "txt") {
    stop("File must be a .txt file")
  }

  # Check that the data index exists
  all_lines <- readLines(filename)
  dat_index <- grep(all_lines, pattern = 'DATA ----')
  if (length(dat_index) == 0) {
    stop("Data index not found")
  }

  all_lines <- readLines(filename)
  dat_index <- grep(all_lines, pattern = 'DATA ----')
  dat_tb <- read.table(filename, header = TRUE, sep = ',', skip = dat_index)

  dat <- list()
  dat$metadata <- as.list(all_lines[seq_len(dat_index - 1)])
  dat$data <- dat_tb

  md_names <- stringr::str_split_fixed(dat$metadata, pattern = ':', 2)[, 1]
  md_values <- stringr::str_split_fixed(dat$metadata, pattern = ':', 2)[, 2]

  dat$metadata <- md_values
  names(dat$metadata) <- md_names

  return(dat)
}

### Data Sharing ----

vpr_export <- function(data, metadata, columnNames, file) {
  #' Format and export VPR data for publication (IN DEVELOPMENT)
  #' Exports a csv file with standard column names based on British Oceanographic
  #' Data Centre, BODC::P01 and DarwinCore (DwC) naming conventions,
  #'  and a JSON metadata file for station level metadata
  #'
  #'
  #' @param data a VPR data frame
  #' @param metadata (optional) a named list of character values giving metadata
  #' to be included in JSON file
  #' @param columnNames (optional) a named list of character values giving
  #'  relationships between existing names of data columns and standard names
  #' @param file a file name for the data.csv
  #'
  #' @examples
  #'
  #'
  #' \dontrun{
  #' data(category_conc_n)
  #' metadata <- list(
  #'   "station_level" = list(
  #'     "title" = list("en" = "VPR data from the Scotian Shelf",
  #'                    "fr" = "Données VPR de l'étagère néo-écossaise"),
  #'     "dataset_ID" = 1,
  #'     "decimalLatitudeStart" = 44.5,
  #'     "decimalLongitudeStart" = -64.5,
  #'     "decimalLatitudeEnd" = 45.5,
  #'     "decimalLongitudeEnd" = -65.5,
  #'     "maximumDepthInMeters" = 1000,
  #'     "eventDate" = "2019-08-11",
  #'     "eventTime" = "00:00:00",
  #'     "basisOfRecord" = "MachineObservation",
  #'    "associatedMedia" = "https://ecotaxa.obs-vlfr.fr/ipt/archive.do?r=iml2018051",
  #'    "identificationReferences" = "Iv3 model v3.3",
  #'    "instrument" = list("opticalSetting" = "S2",
  #'                        "imageVolume" = 83663),
  #'    "resources" = list(
  #'       "data" = list("name" = "vpr123_station25.csv",
  #'                     "creationDate" = "2023-01-01"),
  #'       "metadata" = list("name" = "vpr123_station25-metadata.json",
  #'                         "creationDate" = "2023-01-01")
  #'     ),
  #'     "dataAttributes" = list(
  #'       "eventID" = list(
  #'         "dataType" = "chr",
  #'         "definition" = "An identifier for the set of information associated
  #'         with a dwc:Event (something that occurs at a place and time). May be
  #'         a global unique identifier or an identifier specific to the data set.",
  #'         "vocabulary" = "dwc"
  #'       ),
  #'       "minimumDepthInMeters" = list(
  #'         "dataType" = "float",
  #'         "definition" = "The lesser depth of a range of depth below the local",
  #'         "vocabulary" = "dwc"
  #'       ),
  #'       "maximumDepthInMeters" = list(
  #'         "dataType" = "float",
  #'         "definition" = "The greater depth of a range of depth below the local",
  #'         "vocabulary" = "dwc"
  #'       ),
  #'       "DEPHPRST" = list(
  #'         "dataType" = "float",
  #'         "definition" = "Depth (spatial coordinate) of sampling event start
  #'         relative to water surface in the water body by profiling pressure
  #'          sensor and conversion to depth using unspecified algorithm",
  #'         "vocabulary" = "BODC::P01"
  #'       ),
  #'       "individualCount" = list(
  #'         "dataType" = "float",
  #'         "definition" = "The number of individuals present at the time of the
  #'          dwc:Occurrence.",
  #'         "vocabulary" = "dwc"
  #'       ),
  #'       "verbatimIdentification" = list(
  #'         "dataType" = "chr",
  #'        "definition" = "A string representing the taxonomic identification as
  #'        it appeared in the original record.",
  #'         "vocabulary" = "dwc"
  #'       ),
  #'       "SDBIOL01" = list(
  #'         "dataType" = "float",
  #'         "definition" = "Abundance of biological entity specified elsewhere
  #'         per unit volume of the water body",
  #'         "vocabulary" = "BODC::P01"
  #'       ),
  #'       "TEMPST01" = list(
  #'         "dataType" = "float",
  #'         "definition" = "Temperature of the water body by CTD or STD",
  #'         "vocabulary" = "BODC::P01"
  #'       ),
  #'       "PSALST01" = list(
  #'         "dataType" = "float",
  #'         "definition" = "Practical salinity of the water body by CTD and
  #'         computation using UNESCO 1983 algorithm",
  #'         "vocabulary" = "BODC::P01"
  #'       ),
  #'       "POTDENS0" = list(
  #'         "dataType" = "float",
  #'         "definition" = "Density (potential) of the water body by computation
  #'          from salinity and potential temperature using UNESCO algorithm with
  #'           0 decibar reference pressure",
  #'         "vocabulary" = "BODC::P01"
  #'       ),
  #'       "FLUOZZZZ" = list(
  #'         "dataType" = "float",
  #'         "definition" = "Fluorescence of the water body",
  #'         "vocabulary" = "BODC::P01"
  #'       ),
  #'       "TURBXXXX" = list(
  #'         "dataType" = "float",
  #'         "definition" = "Turbidity of water in the water body",
  #'        "vocabulary" = "BODC::P01"
  #'      ),
  #'       "sampleSizeValue" = list(
  #'         "dataType" = "float",
  #'         "definition" = "A numeric value for a measurement of the size (time
  #'         duration, length, area, or volume) of a sample in a sampling
  #'         dwc:Event.",
  #'         "vocabulary" = "dwc"
  #'       ),
  #'       "sampleSizeUnit" = list(
  #'         "dataType" = "chr",
  #'         "definition" = "The unit of measurement of the size (time duration,
  #'         length, area, or volume) of a sample in a sampling dwc:Event.",
  #'        "vocabulary" = "dwc"
  #'       ),
  #'       "scientificName" = list(
  #'         "dataType" = "chr",
  #'         "definition" = "The full scientific name, with authorship and date
  #'         information if known. When forming part of a dwc:Identification, this
  #'          should be the name in lowest level taxonomic rank that can be
  #'          determined. This term should not contain identification
  #'          qualifications, which should instead be supplied in the
  #'          dwc:identificationQualifier term.",
  #'         "vocabulary" = "dwc"
  #'       ),
  #'       "identifiedBy" = list(
  #'         "dataType" = "chr",
  #'         "definition" = "A list (concatenated and separated) of names of
  #'         people, groups, or organisations who assigned the Taxon to the subject.",
  #'         "vocabulary" = "dwc"
  #'       ),
  #'       "identificationVerificationStatus" = list(
  #'         "dataType" = "chr",
  #'         "definition" = "A categorical indicator of the extent to which the
  #'         taxonomic identification has been verified to be correct.",
  #'         "vocabulary" = "dwc"
  #'       ),
  #'       "depthDifferenceMeters" = list(
  #'        "dataType" = "float",
  #'        "definition" = "Difference between maximumDepthInMeters and
  #'        minimumDepthInMeters of an individual data bin, in meters",
  #'         "vocabulary" = "BIO"
  #'       ),
  #'       "minimumTimeSeconds" = list(
  #'         "dataType" = "float",
  #'         "definition" = "minimum time value in a data bin, measured in seconds
  #'          from the start of the day of sampling",
  #'         "vocabulary" = "BIO"
  #'       ),
  #'       "maximumTimeSeconds" = list(
  #'         "dataType" = "float",
  #'         "definition" = "maximum time value in a data bin, measured in seconds
  #'          from the start of the day of sampling",
  #'         "vocabulary" = "BIO"
  #'       ),
  #'       "timeDifferenceSeconds" = list(
  #'         "dataType" = "float",
  #'         "definition" = "Difference between maximumTimeSeconds and
  #'         minimumTimeSeconds of an individual data bin, in seconds",
  #'         "vocabulary" = "BIO"
  #'       ),
  #'       "numberOfFrames" = list(
  #'         "dataType" = "float",
  #'         "definition" = "number of VPR frames captured within an individual data bin",
  #'         "vocabulary" = "BIO"
  #'       ),
  #'       "timeMilliseconds" = list(
  #'         "dataType" = "float",
  #'         "definition" = "Time measured in milliseconds since the start of the sampling day",
  #'         "vocabulary" = "BIO"
  #'       ),
  #'       "towyoID" = list(
  #'         "dataType" = "chr",
  #'         "definition" = "A string identifying the section of the cast to which
  #'          the data point belongs",
  #'         "vocabulary" = "BIO"
  #'       ),
  #'       "maximumCastDepthInMeters" = list(
  #'         "dataType" = "float",
  #'         "definition" = "Maximum depth in Meters of the cast dataset",
  #'         "vocabulary" = "BIO"
  #'       )
  #'     )
  #'   )
  #' )
  #'
  #' # new_name = old_name
  #' columnNames = list( "DEPHPRST" = "depth" ,
  #'                     "verbatimIdentification" = "category",
  #'                     "eventID" = "station",
  #'                    "minimumDepthInMeters" = "min_depth",
  #'                     "maximumDepthInMeters" = "max_depth",
  #'                     "individualCount" = "n_roi_bin",
  #'                     "SDBIOL01" = "conc_m3",
  #'                     "TEMPST01" = "temperature",
  #'                     "PSALST01" = "salinity",
  #'                     "POTDENS0" = "density",
  #'                     "FLUOZZZZ" = "fluorescence",
  #'                     "TURBXXXX" = "turbidity",
  #'                     "sampleSizeValue" = "vol_sampled_bin_m3",
  #'                     "depthDifferenceMeters" = "depth_diff",
  #'                     "minimumTimeSeconds" = "min_time_s",
  #'                     "maximumTimeSeconds" = "max_time_s",
  #'                     "timeDifferenceSeconds" = "time_diff_s",
  #'                     "numberOfFrames" = "n_frames",
  #'                     "timeMilliseconds" = "time_ms",
  #'                     "towyoID" = "towyo",
  #'                     "maximumCastDepthInMeters" = "max_cast_depth"
  #')
  #'
  #' # add any new data columns required
  #' # (eg. sampleSizeUnit, scientificName, identifiedBy, identificationVerificationStatus)
  #' sampleSizeUnit <- "cubic metre"
  #' identifiedBy <- "K. Sorochan"
  #' identificationVerificationStatus <- "ValidatedByHuman"
  #'
  #' data <- category_conc_n %>%
  #'   mutate(., identifiedBy = identifiedBy,
  #'          sampleSizeUnit = sampleSizeUnit,
  #'          identificationVerificationStatus = identificationVerificationStatus)
  #'
  #' # Define the mapping between category and scientific name
  #' # scientific names based ecotaxa taxonomic system
  #' scientificName <- list("blurry" = "bad_image_blurry",
  #'                       "artefact" = c("bad_image_malfunction", "bad_image_strobe"),
  #'                       "Calanus" = "Calanus")
  #'
  #' # Create a new column of data called scientificName based on matches to category
  #' data <- data %>%
  #'   dplyr::mutate(., scientificName = case_when(
  #'     category %in% scientificName[["blurry"]] ~ "blurry",
  #'     category %in% scientificName[["artefact"]] ~ "artefact",
  #'     category == scientificName[["Calanus"]] ~ "Calanus",
  #'     TRUE ~ NA
  #'   ))
  #'
  #' vpr_export(data, metadata, columnNames, file = "vpr123_station25")
  #' }
  #' @export
  #' @importFrom utils write.csv

## input validation
# check that data is a dataframe
if (!is.data.frame(data)) {
    stop("Data must be a data frame")
  }

# check that metadata is a named list
if (!is.null(metadata) && !is.list(metadata)) {
    stop("Metadata must be a named list")
  }
# check that columnNames is a named list
if (!is.null(columnNames) && !is.list(columnNames)) {
    stop("columnNames must be a named list")
  }

# check that columnNames matches data
if (!is.null(columnNames)) {
    if (!all(unlist(columnNames) %in% names(data))) {
      stop("columnNames must contain all column names in data")
    }
  }

## update column names in dataframe based on columnNames
new_data <- dplyr::rename(data, all_of(unlist(columnNames)))


## do some data checks
# check for null/NA values
# Check for null or NA values in data
cols_with_null_or_na <- sapply(new_data, function(x) any(is.null(x) | is.na(x)))

# Print column names with null or NA values
if (any(cols_with_null_or_na)) {
  warning("The following columns have null or NA values:\n ", names(new_data)[cols_with_null_or_na])
}

# remove file extension from file name if required
# so that file name is generic and applicable to data and metadata strings
file <- gsub(".csv", "", file)

## write data to csv
# BE SURE ROW NAMES ARE FALSE
write.csv(new_data, file = paste0(file, '.csv'), row.names = FALSE)


## write metadata to json
exstr <- rjson::toJSON(metadata, indent = 1) # indent makes pretty formatting
cat(exstr, file = paste0(file, '-metadata.json'))


}

vpr_save <- function(data, metadata) {
  #' Save VPR data as an \link[oce]{as.oce} object
  #'
  #' @details This function will pass a VPR data frame to an `oce` object.
  #'   Using an `oce` object as the default export format for VPR data allows for
  #'   metadata and data to be kept in the same, space efficient file, and avoid
  #'   redundancy in the data frame. The function checks for data parameters that
  #'   may actually be metadata parameters (rows which have the same value
  #'   repeated for every observation). These parameters will automatically be
  #'   copied into the metadata slot of the `oce` object. The function will also
  #'   prompt for a variety of required metadata fields. Depending on specific
  #'   research / archiving requirements, these metadata parameters could be
  #'   updated by providing the argument `metadata`.
  #'
  #'   Default metadata parameters include 'deploymentType', 'waterDepth',
  #'   'serialNumber', 'latitudeStart', 'longitudeStart', 'castDate', 'castStartTime',
  #'   'castEndTime', 'processedBy', 'opticalSetting', 'imageVolume', 'comment'.
  #'
  #'
  #' @param data a VPR data frame
  #' @param metadata (optional) a named list of character values giving metadata
  #'   values. If this argument is not provided user will be prompted for a few
  #'   generic metadata requirements.
  #'
  #'
  #' @return an oce CTD object with all VPR data as well as metadata
  #' @export
  #'
  #' @examples
  #' data("category_conc_n")
  #' metadata <- list('deploymentType' = 'towyo', 'waterDepth' =
  #' max(ctd_roi_merge$pressure), 'serialNumber' = NA, 'latitudeStart' = 47,
  #' 'longitudeStart' = -65, 'castDate' = '2019-08-11', 'castStartTime'= '00:00',
  #' 'castEndTime' = '01:00', 'processedBy' = 'E. Chisholm', 'opticalSetting' =
  #' 'S2', 'imageVolume' = 83663, 'comment' = 'test data')
  #'
  #' oce_dat <- vpr_save(category_conc_n, metadata)
  #' # save(oce_dat, file = vpr_save.RData') # save data
  #'
  # Check that the data is a data frame
  if (!is.data.frame(data)) {
    stop("Data must be a data frame")
  }

  # Check that the metadata is a named list
  if (!is.null(metadata) && !is.list(metadata)) {
    stop("Metadata must be a named list")
  }
  # create oce objects

  oce_data <- as.oce(data)

  # check for metadata in dataframe
  rem_list <- list()
  for (i in seq_len(length(oce_data@data))) {
    if (length(unique(oce_data@data[[i]])) == 1) {
      print(paste('Metadata parameter found in Data object! ', names(oce_data@data)[[i]], 'value of', unique(oce_data@data[[i]]), 'moved to metadata slot. '))
      # add as metadtaa parameter
      oce_data <- oceSetMetadata(oce_data, name = names(oce_data@data)[[i]], value = unique(oce_data@data[[i]]))
      rem_list[[i]] <- i
    }

  }
  # remove data lines
  oce_data@data <- oce_data@data[-unlist(rem_list)]

  # check for other metadata and ask user to supply
  if (missing(metadata)) {
    req_meta <- c('deploymentType',
        'waterDepth',
        'serialNumber',
       'latitudeStart',
        'longitudeStart',
        'castDate',
        'castStartTime',
        'castEndTime',
        'processedBy',
        'opticalSetting',
        'imageVolume',
        'comment')
# TODO include metadata examples or skip # nolint
  # clarify serial number, water depth? # nolint

    for (rm in req_meta) {
    if (is.null(oce_data@metadata[[rm]])) {
      print(paste('Please provide value for Metadata parameter', rm))
      rm_val <- readline(paste('Metadata slot, ', rm, ': '))
      oce_data <- oceSetMetadata(oce_data, name = rm, value = rm_val, note = NULL)
    }

    }
  }else {
# if metadata names and values are provided as list
    for (rm in names(metadata)) {

      rm_val <- metadata[[rm]]
      oce_data <- oceSetMetadata(oce_data, name = rm, value = rm_val, note = NULL)


    }
  }

  return(oce_data)
}

vpr_oce_create <- function(data) {
  #' Create ctd oce object with vpr data
  #'
  #' Formats VPR data frame into \code{oce} format CTD object
  #'
  #' @author E. Chisholm
  #'
  #' @param data data frame of vpr data
  #'
  #' @examples
  #' data('ctd_roi_merge')
  #' oce_dat <- vpr_oce_create(ctd_roi_merge)
  #'
  #' @export
  # create oce objects
  ctd_roi_oce <- oce::as.ctd(data)
  # compare oce vars to df vars
  oce_names <- names(ctd_roi_oce@data)
  df_names <- colnames(data)
  if (length(oce_names) < length(df_names)) {
    warning("oce-ctd object may be missing some data columns!")
  }

  return(ctd_roi_oce)
}


### Core ----

vpr_roi_concentration <- function(data, category_list, station_of_interest, binSize, imageVolume, rev = FALSE) {
  #'Calculate VPR concentrations
  #'
  #' Calculates concentrations for each named category in dataframe
  #'
  #' @param data a VPR dataframe as produced by \code{\link{vpr_ctdroi_merge}}
  #' @param category_list a vector of character strings representing category present in the station being processed
  #' @param station_of_interest The station being processed
  #' @param binSize passed to \code{\link{bin_calculate}}, determines size of depth bins over which data is averaged
  #' @param imageVolume the volume of VPR images used for calculating concentrations (mm^3)
  #' @param rev Logical value defining direction of binning, FALSE (default) - bins will be
  #'   calculated from surface to bottom, TRUE- bins will be calculated bottom to
  #'   surface
  #'
  #' @examples
  #'
  #' data('ctd_roi_merge')
  #' ctd_roi_merge$time_hr <- ctd_roi_merge$time_ms /3.6e+06
  #'
  #' category_list <- c('Calanus', 'krill')
  #' binSize <- 5
  #' station_of_interest <- 'test'
  #' imageVolume <- 83663
  #'
  #' category_conc_n <- vpr_roi_concentration(ctd_roi_merge, category_list,
  #' station_of_interest, binSize, imageVolume)
  #'
  #'@export
  #'
  #'
  # input validation
  # Check that the data argument is a data frame
  if (!is.data.frame(data)) {
    stop("Data must be a data frame")
  }

  # Check that the categories in category_list are valid and contained in the names of data
  valid_categories <- intersect(names(data), unlist(category_list))
  if (length(valid_categories) == 0) {
    stop("Category_list contains no valid categories")
  }

  # Check that the station_of_interest argument is a character vector
  if (!is.character(station_of_interest)) {
    stop("Station_of_interest must be a character vector")
  }

  # Check that the binSize argument is a numeric value greater than 0
  if (!is.numeric(binSize) || binSize <= 0) {
    stop("BinSize must be a numeric value greater than 0")
  }

  # Check that the imageVolume argument is a numeric value greater than 0
  if (!is.numeric(imageVolume) || imageVolume <= 0) {
    stop("ImageVolume must be a numeric value greater than 0")
  }
  # avoid CRAN notes
  . <- NA
  # check that category exist for this station

  category_in_data <- names(data) %in% category_list

  valid_category <- names(data)[category_in_data == TRUE]

  # calculate concentrations
  conc_dat <- list()
  for ( ii in seq_len(length(valid_category))){
    conc_dat[[ii]] <- concentration_category(data, valid_category[ii], binSize, imageVolume, rev = rev) %>%
      dplyr::mutate(., category = valid_category[ii])
  }

  names(conc_dat) <- valid_category

  category_conc <- do.call(rbind, conc_dat)

  category_conc_n <- category_conc %>%
    dplyr::mutate(., station = station_of_interest)

  return(category_conc_n)
}

concentration_category <- function(data, category, binSize, imageVolume, rev = FALSE, breaks = NULL, cutoff = 0.1) {
  #' Binned concentrations
  #'
  #' This function produces depth binned concentrations for a specified category. Similar to \code{\link{bin_cast}} but calculates concentrations for only one category.
  #' Used inside \code{\link{vpr_roi_concentration}}
  #'
  #'
  #' @param data dataframe produced by processing internal to vpr_roi_concentration
  #' @param category name of category isolated
  #' @param binSize passed to \code{\link{bin_calculate}}, determines size of depth bins over which data is averaged
  #' @param imageVolume the volume of VPR images used for calculating concentrations (mm^3)
  #' @param rev Logical value defining direction of binning, FALSE - bins will be
  #'   calculated from surface to bottom, TRUE- bins will be calculated bottom to
  #'   surface
  #' @param cutoff Argument passed to \link[oce]{ctdFindProfiles}
  #' @param breaks Argument passed to \link[oce]{ctdFindProfiles}
  #'
  #' @details Image volume calculations can change based on optical setting of VPR as well as autodeck setting used to process images
  #' For IML2018051 (S2) image volume was calculated as 108155 mm^3 by seascan (6.6 cubic inches)
  #' For COR2019002 S2 image volume was calculated as 83663 mm^3 and S3 image volume was calculated as 366082 mm^3
  #'
  #'
  #' @author E. Chisholm
  #'
  #' @export
  #input validation
  # Check that the data argument is a data frame
  if (!is.data.frame(data)) {
    stop("Data must be a data frame")
  }

  # Check that the category argument is a character string
  if (!is.character(category)) {
    stop("Category must be a character string")
  }

  # Check that the binSize argument is a numeric value greater than 0
  if (!is.numeric(binSize) || binSize <= 0) {
    stop("BinSize must be a numeric value greater than 0")
  }

  # Check that the imageVolume argument is a numeric value greater than 0
  if (!is.numeric(imageVolume) || imageVolume <= 0) {
    stop("ImageVolume must be a numeric value greater than 0")
  }

  # Check that the rev argument is a logical value
  if (!is.logical(rev)) {
    stop("Rev must be a logical value")
  }

  . <- NA # avoid CRAN notes

  # remove other data rows #ADDED BY KS, DAY HOUR CHANGED TO DAY, HOUR
  # TODO remove hardcoding and instead use reference to ctd col_list or use reverse of categories
  # remove other data rows #ADDED BY KS, DAY HOUR CHANGED TO DAY, HOUR
  noncategory <-
    c(
      'time_ms',
      'conductivity',
      'temperature',
      'pressure',
      'salinity',
      'sigmaT',
      'fluor_ref',
      'fluorescence_mv',
      'turbidity_ref',
      'turbidity_mv',
      'altitude_NA',
      'day',
      'hour',
      'station',
      'time_hr',
      'roi',
      'depth'
    )
  dt <- data %>%
    dplyr::select(., any_of(noncategory), all_of(category))


  # get n_roi of only one category
  names(dt) <-
    gsub(names(dt), pattern = category, replacement = 'n_roi')

  # format into oce ctd
  ctd_roi_oce <- vpr_oce_create(dt)

  # bin data
  final <- bin_cast(ctd_roi_oce = ctd_roi_oce, imageVolume = imageVolume, binSize = binSize, rev = rev, breaks = breaks, cutoff = cutoff)

  return(final)
}

bin_cast <- function(ctd_roi_oce, imageVolume, binSize, rev = FALSE, breaks = NULL, cutoff = 0.1) {
  #' Bin vpr data
  #'
  #' Formats \code{oce} style VPR data into depth averaged bins using \code{\link{ctd_cast}} and \code{\link{bin_calculate}}
  #' This function is used inside \code{\link{concentration_category}}
  #'
  #'
  #' @param ctd_roi_oce \code{oce} ctd format VPR data from \code{\link{vpr_oce_create}}
  #' @param binSize passed to \code{\link{bin_calculate}}, determines size of depth bins over which data is averaged
  #' @param imageVolume the volume of VPR images used for calculating concentrations (mm^3)
  #' @param rev logical value,passed to \code{\link{bin_calculate}} if TRUE, binning will begin at bottom of each cast,
  #'   this controls data loss due to uneven binning over depth. If bins begin at
  #'   bottom, small amounts of data may be lost at the surface of each cast, if
  #'   binning begins at surface (rev = FALSE), small amounts of data may be lost
  #'   at bottom of each cast
  #' @param cutoff Argument passed to \link[oce]{ctdFindProfiles}
  #' @param breaks Argument passed to \link[oce]{ctdFindProfiles}
  #'
  #' @details Image volume calculations can change based on optical setting of VPR as well as autodeck setting used to process images
  #' For IML2018051 (S2) image volume was calculated as 108155 mm^3 by seascan (6.6 cubic inches)
  #' For COR2019002 S2 image volume was calculated as 83663 mm^3 and S3 image volume was calculated as 366082 mm^3
  #'
  #'
  #'@return A dataframe of depth averaged bins of VPR data over an entire cast with calculated concentration values
  #' @export
  #'
  #'
  # input validation
  if (!inherits(ctd_roi_oce, "ctd")) {
    stop("ctd_roi_oce must be an object of class 'ctd'")
  }

  # Check that the imageVolume argument is a numeric value greater than 0
  if (!is.numeric(imageVolume) || imageVolume <= 0) {
    stop("imageVolume must be a numeric value greater than 0")
  }

  # Check that the binSize argument is a numeric value greater than 0
  if (!is.numeric(binSize) || binSize <= 0) {
    stop("binSize must be a numeric value greater than 0")
  }

  # Check that the rev argument is a logical value
  if (!is.logical(rev)) {
    stop("rev must be a logical value")
  }
  . <- conc_m3 <- NA
  #browser()
  #find upcasts
  upcast <- ctd_cast(data = ctd_roi_oce, cast_direction = 'ascending', data_type = 'df', breaks = breaks, cutoff = cutoff)
  upcast2 <- lapply(X = upcast, FUN = bin_calculate, binSize = binSize, imageVolume = imageVolume, rev = rev)
  upcast_df <- do.call(rbind, upcast2)

  #find downcasts
  downcast <- ctd_cast(ctd_roi_oce, cast_direction = "descending", data_type = "df", breaks = breaks, cutoff = cutoff)
  downcast2 <- lapply(X = downcast, FUN = bin_calculate, binSize = binSize, imageVolume = imageVolume, rev = rev)
  downcast_df <- do.call(rbind, downcast2)

  #combine_data in bins
  vpr_depth_bin <- rbind(upcast_df, downcast_df)
  vpr_depth_bin <- data.frame(vpr_depth_bin)

  #Remove infinite concentrations (why do these occur again?)
  vpr_depth_bin <- vpr_depth_bin %>%
    # dplyr::mutate(., time_hr = time_hr - min(time_hr)) %>% # this is potentially creating issues where time is not aligned in plots
    dplyr::filter(., is.finite(conc_m3))

  return(vpr_depth_bin)
}

vpr_ctd_read <- function(ctd_files, station_of_interest, day, hour, col_list) {
  #' Read and format CTD VPR data
  #'
  #' Acts as a wrapper for \code{\link{ctd_df_cols}}
  #'
  #' Reads CTD data and adds day, hour, and station information.
  #' Calculates sigma T and depth variables from existing CTD data to supplement raw data.
  #' If there are multiple hours of CTD data, combines them into single dataframe.
  #'
  #' **WARNING** \code{\link{ctd_df_cols}} is hard coded to accept a specific
  #' order of CTD data columns. The names and values in these columns can change
  #' based on the specific instrument and should be updated/confirmed before processing data
  #' from a new VPR.
  #'
  #' @author E. Chisholm & K. Sorochan
  #'
  #'
  #' @param ctd_files full file paths to vpr ctd \code{.dat} files
  #' @param station_of_interest VPR station name
  #' @param day Day of interest, if not provided will be pulled from file path
  #' @param hour Hour of interest, if not provided will be pulled from file path
  #' @param col_list Optional chr vector of CTD data column names
  #'
  #' @examples
  #'
  #' station_of_interest <- 'test'
  #'
  #' ctd_files <- system.file("extdata/COR2019002/rois/vpr5/d222", "h03ctd.dat.gz",
  #' package = "vprr", mustWork = TRUE)
  #'
  #' ctd_dat_combine <- vpr_ctd_read(ctd_files, station_of_interest)
  #'
  #' @export

  # avoid CRAN notes
  . <- NA

  if (length(ctd_files) == 0) {
    stop('No CTD files provided!')
  }
  ctd_dat <- list()
  for (i in seq_len(length(ctd_files))) {

    if (missing(day)) {
      day_id <- unlist(vpr_day(ctd_files[i]))
    }else {
      day_id <- day
    }

    if (missing(hour)) {
      hour_id <- unlist(vpr_hour(ctd_files[i]))
    }else {
      hour_id <- hour
    }


    station_id <- station_of_interest

    if (missing(col_list)) {
      ctd_dat_tmp <- ctd_df_cols(ctd_files[i])
    }else {
      ctd_dat_tmp <- ctd_df_cols(ctd_files[i], col_list)
    }

    ctd_dat[[i]] <- data.frame(ctd_dat_tmp,
                               day = day_id,
                               hour = hour_id,
                               station = station_id,
                               stringsAsFactors = FALSE)
  }


  # combine ctd dat

  ctd_dat_combine <- do.call(rbind, ctd_dat)

  # add calculated vars as default
  # sigma t, depth

  ctd_dat_combine <- ctd_dat_combine %>%
    dplyr::mutate(., sigmaT = oce::swSigmaT(
      ctd_dat_combine$salinity,
      ctd_dat_combine$temperature,
      ctd_dat_combine$pressure
    )) %>%
    dplyr::mutate(., depth = oce::swDepth(ctd_dat_combine$pressure)) # note that default latitude is used (45)


  return(ctd_dat_combine)
}

vpr_ctdroi_merge <- function(ctd_dat_combine, roi_dat_combine) {
  #'Merge CTD and ROI data from VPR
  #'
  #'Combines CTD data (time, hydrographic parameters), with ROI information
  #'(identification number) into single dataframe, aligning ROI identification
  #'numbers and category classifications with time and hydrographic parameters
  #'
  #'@author E. Chisholm & K. Sorochan
  #'
  #'@param ctd_dat_combine a CTD dataframe from VPR processing from \code{\link{vpr_ctd_read}}
  #'@param roi_dat_combine a data frame of roi aid data from \code{\link{vpr_autoid_read}}
  #'
  #'
  #' @examples
  #' data('ctd_dat_combine')
  #' data('roi_dat_combine')
  #'
  #' ctd_roi_merge <- vpr_ctdroi_merge(ctd_dat_combine, roi_dat_combine)
  #'@export
  #'
  # input validation
  # Check that the ctd_dat_combine argument is a data frame
  if (!is.data.frame(ctd_dat_combine)) {
    stop("ctd_dat_combine must be a data frame")
  }

  # Check that the roi_dat_combine argument is a data frame
  if (!is.data.frame(roi_dat_combine)) {
    stop("roi_dat_combine must be a data frame")
  }

  # Check that the ctd_dat_combine argument has a "time_ms" column
  if (!"time_ms" %in% colnames(ctd_dat_combine)) {
    stop("ctd_dat_combine must have a 'time_ms' column")
  }

  # Check that the roi_dat_combine argument has a "time_ms" column
  if (!"time_ms" %in% colnames(roi_dat_combine)) {
    stop("roi_dat_combine must have a 'time_ms' column")
  }

  # avoid CRAN notes
  . <- roi <- time_ms <- NA
  # First subset ctd data by roi id
  ctd_time <- ctd_dat_combine$time_ms
  roi_time <- as.numeric(roi_dat_combine$time_ms)

  roi_index <- which(ctd_time %in% roi_time)
  ctd_subset <- data.frame(ctd_dat_combine[roi_index, ])


  # Get total number of rois per frame
  categories <- colnames(roi_dat_combine)[!(colnames(roi_dat_combine) %in% c('time_ms', 'roi'))]
  category_col_id <- which(colnames(roi_dat_combine) %in% categories)
  category_subset <- roi_dat_combine[, category_col_id]
  n_roi_total <- base::rowSums(category_subset)
  roi_dat_2 <- data.frame(roi_dat_combine, n_roi_total)

  # Combine subsetted CTD and roi data
  ctd_subset_roi <- full_join(ctd_subset, roi_dat_2)

  # combine subsetted roi data and all CTD data such that frames with zero rois are included
  ctd_roi_merge <- ctd_subset_roi %>%
    dplyr::right_join(., ctd_dat_combine)

  ctd_roi_merge[is.na(ctd_roi_merge)] <- 0

  ctd_roi_merge <- ctd_roi_merge %>%
    dplyr::mutate(., roi = ifelse(roi == 0, NA, roi)) %>%
    dplyr::arrange(., time_ms) # ensure that data is sorted by time to avoid processing errors

  return(ctd_roi_merge)
}

vpr_autoid_read <- function(file_list_aid, file_list_aidmeas, export, station_of_interest, opticalSetting, warn = TRUE, categories) {
  #'Read VPR aid files
  #'
  #'Read aid text files containing ROI string information or measurement data and output as a dataframe
  #'
  #'Only outputs either ROI string information OR measurement data
  #'
  #'
  #' @author E. Chisholm & K. Sorochan
  #'
  #'@param  file_list_aid a list object of aid text files, containing ROI strings.
  #'@param file_list_aidmeas  a list object of aidmea text files, containing ROI measurements.
  #'@param export a character string specifying which type of data to output, either 'aid' (roi strings) or 'aidmeas' (measurement data)
  #'@param station_of_interest Station information to be added to ROI data output, use NA if irrelevant
  #'@param opticalSetting Optional argument specifying VPR optical setting. If provided will be used to convert size data into mm from pixels, if missing size data will be output in pixels
  #'@param warn Logical, FALSE silences size data unit warnings
  #'@param categories A list object (of chr strings) with all the potential classification categories
  #'
  #'@note Full paths to each file should be specified
  #'
  #' @examples
  #'
  #' station_of_interest <- 'test'
  #' dayhour <- c('d222.h03', 'd222.h04')
  #' categories <- c("bad_image_blurry", "bad_image_malfunction",
  #' "bad_image_strobe", "Calanus", "chaetognaths","ctenophores","krill",
  #' "marine_snow","Other","small_copepod", "stick")
  #'
  #' #' #VPR OPTICAL SETTING (S0, S1, S2 OR S3)
  #' opticalSetting <- "S2"
  #' imageVolume <- 83663 #mm^3
  #'
  #' auto_id_folder <- system.file('extdata/COR2019002/autoid/', package = 'vprr', mustWork = TRUE)
  #' auto_id_path <- list.files(paste0(auto_id_folder, "/"), full.names = TRUE)
  #'
  #' #'   # Path to aid for each category
  #' aid_path <- paste0(auto_id_path, '/aid/')
  #' # Path to mea for each category
  #' aidmea_path <- paste0(auto_id_path, '/aidmea/')
  #'
  #' # AUTO ID FILES
  #' aid_file_list <- list()
  #' aidmea_file_list <- list()
  #' for (i in 1:length(dayhour)) {
  #'   aid_file_list[[i]] <-
  #'     list.files(aid_path, pattern = dayhour[[i]], full.names = TRUE)
  #'   # SIZE DATA FILES
  #'   aidmea_file_list[[i]] <-
  #'     list.files(aidmea_path, pattern = dayhour[[i]], full.names = TRUE)
  #' }
  #'
  #' aid_file_list_all <- unlist(aid_file_list)
  #' aidmea_file_list_all <- unlist(aidmea_file_list)
  #'
  #'  # ROIs
  #' roi_dat_combine <-
  #'   vpr_autoid_read(
  #'     file_list_aid = aid_file_list_all,
  #'     file_list_aidmeas = aidmea_file_list_all,
  #'     export = 'aid',
  #'     station_of_interest = station_of_interest,
  #'     opticalSetting = opticalSetting,
  #'     warn = FALSE,
  #'     categories = categories
  #'   )
  #'
  #' # MEASUREMENTS
  #' roimeas_dat_combine <-
  #'   vpr_autoid_read(
  #'     file_list_aid = aid_file_list_all,
  #'     file_list_aidmeas = aidmea_file_list_all,
  #'     export = 'aidmeas',
  #'     station_of_interest = station_of_interest,
  #'     opticalSetting = opticalSetting,
  #'     warn = FALSE,
  #'     categories = categories
  #'  )
  #'
  #' @export
  # set-up for only processing aid data
  if (missing(file_list_aidmeas)) {
    export <- 'aid'
  }
  # avoid CRAN notes
  . <- roi <- category <- n_roi <- day_hour <- Perimeter <- Area <- width1 <- width2 <- width3 <- short_axis_length <- long_axis_length <- NA
  if ( export == 'aidmeas') {
    if (missing(opticalSetting)) {
      opticalSetting <- NA
      if (warn != FALSE) {
        warning('No optical setting provided, size data output in pixels!!!')
      }
    }
  }
  # aid


  # check for empty files
  empty_files <- list()
  for (j in seq_len(length(file_list_aid))) {
    mtry <- try(read.table(file_list_aid[j], sep = ",", header = TRUE),
                silent = TRUE)

    if (inherits(mtry, 'try-error')) {
      empty_files[j] <- TRUE
    } else {
      empty_files[j] <- FALSE
    }

  }
  file_list_aid <- file_list_aid[empty_files == FALSE]

  col_names <- "roi"
  dat <- list()

  for (i in seq_len(length(file_list_aid))) {

    data_tmp <- read.table(file = file_list_aid[i], stringsAsFactors = FALSE, col.names = col_names)

    data_tmp$roi <- unlist(vpr_roi(data_tmp$roi))




    data_tmp$category <- unlist(unique(vpr_category(file_list_aid[i], categories)[[1]]))
    day <- unlist(vpr_day(file_list_aid[i]))
    hour <- unlist(vpr_hour(file_list_aid[i]))
    if (length(day) > 1 || length(hour) > 1) {
      stop('Problem detecting day/hour values!')
    }
    data_tmp$day_hour <- paste(day, hour, sep = ".")
    dat[[i]] <- data_tmp

  }


  dat_combine_aid <- do.call(rbind, dat)
  remove(dat, data_tmp, day, hour)

  # format
  dat_combine_aid$id <- row.names(dat_combine_aid)


  # Get tabulated rois per time by category
  roi_df <- dat_combine_aid %>%
    dplyr::mutate(., roi = substr(roi, 1, 8)) %>% # TODO why are we subsetting to 8 digits?
    dplyr::group_by(., category, roi) %>%
    dplyr::summarise(., n_roi = dplyr::n(), .groups = NULL) %>%
    tidyr::spread(., category, n_roi) %>%
    dplyr::mutate(., time_ms = as.numeric(roi))

  roi_dat <- data.frame(roi_df)
  roi_dat[is.na(roi_dat)] <- 0



  # aidmeas
  # TODO: update code so it can run without measurement input
  if (export == 'aidmeas') {

    # check for empty files
    empty_files <- list()
    for (j in seq_len(length(file_list_aidmeas))) {
      mtry <- try(read.table(file_list_aidmeas[j], sep = ",", header = TRUE),
                  silent = TRUE)

      if (inherits(mtry, 'try-error')) {
        empty_files[j] <- TRUE
      } else {
        empty_files[j] <- FALSE
      }
    }
    file_list_aidmeas <- file_list_aidmeas[empty_files == FALSE]

    dat <- list()
    col_names <- c('Perimeter', 'Area', 'width1', 'width2', 'width3', 'short_axis_length', 'long_axis_length')
    for (i in seq_len(length(file_list_aidmeas))) {

      data_tmp <- read.table(file_list_aidmeas[i], stringsAsFactors = FALSE, col.names = col_names)


      if (!is.na(opticalSetting)) {
        data_tmp <- px_to_mm(data_tmp, opticalSetting)
      }




      data_tmp$category <- unlist(vpr_category(file_list_aidmeas[i], categories))
      day <- unlist(vpr_day(file_list_aidmeas[i]))
      hour <- unlist(vpr_hour(file_list_aidmeas[i]))
      data_tmp$day_hour <- paste(day, hour, sep = ".")
      dat[[i]] <- data_tmp

    }

    dat_combine_aidmeas <- do.call(rbind, dat)

    # remove(dat, data_tmp, day, hour)
    dat_combine_aidmeas$id <- row.names(dat_combine_aidmeas)

    # Get roi measurement data frame
    dat_combine_selected <- dat_combine_aidmeas %>%
      dplyr::select(., category, day_hour, id, Perimeter, Area, width1, width2, width3, short_axis_length, long_axis_length) #added all measurement columns EC Jan 28 2020

    roimeas_dat_combine <- right_join(dat_combine_aid, dat_combine_selected, by = c('category', 'day_hour', 'id')) %>%
      dplyr::select(., - id) %>%
      dplyr::mutate(., station = station_of_interest) %>%
      dplyr::mutate(., long_axis_length = as.numeric(long_axis_length)) %>%
      dplyr::mutate(., time_ms = as.numeric(substr(roi, 1, 8)))

  } # end aidmeas section



  # export
  if (export == 'aid') {
    return(roi_dat)
  }

  if (export == 'aidmeas') {
    return(roimeas_dat_combine)
  }
}

bin_calculate <- function(data, binSize = 1, imageVolume, rev = FALSE) {
  #' Get bin averages for VPR and CTD data
  #'
  #' Bins CTD data for an individual cast to avoid depth averaging across tow-yo's
  #'
  #' @author E. Chisholm, K. Sorochan
  #'
  #'

  #' @param data ctd data frame object including scan, salinity, temperature,
  #'   depth, conductivity, time, fluor_ref, turbidity_ref, turbidity_mv,
  #'   altitude, cast_id, n_roi
  #' @param binSize the height of bins over which to average, default is 1 metre
  #' @param imageVolume the volume of VPR images used for calculating concentrations (mm^3)
  #' @param rev logical value, if TRUE, binning will begin at bottom of each cast,
  #'   this controls data loss due to uneven binning over depth. If bins begin at
  #'   bottom, small amounts of data may be lost at the surface of each cast, if
  #'   binning begins at surface (rev = FALSE), small amounts of data may be lost
  #'   at bottom of each cast
  #'
  #'
  #' @details Image volume calculations can change based on optical setting of VPR as well as autodeck setting used to process images
  #' For IML2018051 (S2) image volume was calculated as 108155 mm^3 by seascan (6.6 cubic inches)
  #' For COR2019002 S2 image volume was calculated as 83663 mm^3 and S3 image volume was calculated as 366082 mm^3.
  #' Used internally ( \code{\link{bin_cast}} ) after \code{\link{ctd_cast}} on a single ascending or descending section of VPR cast
  #'
  #'
  #'
  #' @note binSize should be carefully considered for best results
  #' @note Depth is used for calculations! Please ensure depth is included in data frame using \link[oce]{swDepth}
  #'
  #' @export
  #'
  # input validation
  # Check that the data argument is a data frame
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  # Check that the binSize argument is a numeric value greater than 0
  if (!is.numeric(binSize) || binSize <= 0) {
    stop("binSize must be a numeric value greater than 0")
  }

  # Check that the imageVolume argument is a numeric value greater than 0
  if (!is.numeric(imageVolume) || imageVolume <= 0) {
    stop("imageVolume must be a numeric value greater than 0")
  }

  # Check that the rev argument is a logical value
  if (!is.logical(rev)) {
    stop("rev must be a logical value")
  }

  # Check that the data argument has a "depth" column
  if (!"depth" %in% colnames(data)) {
    stop("data must have a 'depth' column")
  }

  # Check that the data argument has a "cast_id" column
  if (!"cast_id" %in% colnames(data)) {
    stop("data must have a 'cast_id' column")
  }

  cast_id <- unique(data$cast_id)
  max_cast_depth <- max(data$depth) # ADDED BY KS TO IDENTIFY EACH TOWYO CHUNK

  p <- data$depth
  max_depth <- max(p, na.rm = TRUE)
  min_depth <- min(p, na.rm = TRUE)
  x_breaks <- seq(from = floor(min_depth), to = ceiling(max_depth), by = binSize)
  if (rev == TRUE) {
    x_breaks <- seq(from = ceiling(max_depth), to = floor(min_depth), by = - binSize) #reversed by KS
  }

  # error when cast is too small
  if (max_depth - min_depth < binSize) {
    warning(paste('Cast', cast_id, 'is too small to calculate information for bins of size', binSize))
    data.frame(NULL)
  } else {


    # Get variables of interest using oce bin functions

    min_time_s <- oce::binApply1D(p, data$time_ms / 1000, xbreaks = x_breaks, min)$result
    max_time_s <- oce::binApply1D(p, data$time_ms / 1000, xbreaks = x_breaks, max)$result
    min_depth <- oce::binApply1D(p, data$depth, xbreaks = x_breaks, min)$result
    max_depth <- oce::binApply1D(p, data$depth, xbreaks = x_breaks, max)$result
    n_roi_bin <- oce::binApply1D(p, data$n_roi, xbreaks = x_breaks, sum)$result
    temperature <- oce::binApply1D(p, data$temperature, xbreaks = x_breaks, mean)$result
    salinity <- oce::binApply1D(p, data$salinity, xbreaks = x_breaks, mean)$result
    density <- oce::binApply1D(p, data$sigmaT, xbreaks = x_breaks, mean)$result
    fluorescence <- oce::binApply1D(p, data$fluorescence_mv, xbreaks = x_breaks, mean)$result
    turbidity <- oce::binApply1D(p, data$turbidity_mv, xbreaks = x_breaks, mean)$result
    time_ms <- oce::binApply1D(p, data$time_ms, xbreaks = x_breaks, mean)$result
    time_hr <- oce::binApply1D(p, data$time_ms / (1000 * 3600), xbreaks = x_breaks, mean)$result # update time naming scheme May 2022
    if (rev == TRUE) {

      depth <- rev(oce::binApply1D(p, data$depth, xbreaks = x_breaks, mean)$xmids)

    } else { # simplify?

      depth <- oce::binApply1D(p, data$salinity, xbreaks = x_breaks, mean)$xmids

    }
    # calculates number of frames captured per depth bin by counting number of pressure observations per bin
    n_frames <- oce::binApply1D(p, data$depth, xbreaks = x_breaks, length)$result # KS edit 10/9/19

    # WARNING
    # binApply1D does not calculate NAs, if there is binned depth range that does
    # not contain any data, the binApply function will not create an empty or NA
    # placeholder bin in that case the result length will be different than the
    # length of midpoints since the variable "pressure" is a mid point calculation it is used to
    # test for non existent empty bins. If there are non existant empty bins,
    # binMean1D will calculate them as NA, this loop finds where the bins would
    # have been located and removes those indexes from the pressure vector so the
    # length of variables is all identical

    if (!(length(depth) == length(salinity))) {

      salinity_mean <- binMean1D(p, data$salinity, xbreaks = x_breaks)$result

      idx_rm <- which(is.na(salinity_mean))

      # informs user where bins were removed due to NAs
      # note if a bin is 'NA' typically because there is no valid data in that depth range,
      # if you have a lot of NA bins, think about increasing your binSize
      message(paste('Removed bins at', depth[idx_rm]))

      lp <- length(depth)
      depth <- depth[-idx_rm]
      if (length(n_frames) == lp) {
        n_frames <- n_frames[-idx_rm]
      }

    }
    # make sure n_frames matches the length of other data frame rows
    if (length(n_frames) > length(depth)) {
      n_frames <- n_frames[-length(n_frames)]
    }
    if (length(n_frames) < length(depth)) {
      n_frames <- c(n_frames, 0)
    }
    if (length(n_frames) != length(depth)) {
      length(n_frames) <- length(depth)
    }
    # Get derived variables

    time_diff_s <- max_time_s - min_time_s

    # calculate concentration based on opticalSetting

    # "Old way" of calculating concentration assuming constant frame rate of 15 fps
    # conc_m3 <- n_roi_bin/((imageVolume/1e09)*(15)*(time_diff_s)) #

    # "New way" of calculating concentration by summing volume associated with frames over depth bin
    vol_sampled_bin_m3 <- (imageVolume / 1e09) * n_frames
    conc_m3 <- n_roi_bin / (vol_sampled_bin_m3) # KS edit 10/9/19

    depth_diff <- max_depth - min_depth

    # Output
    data.frame(depth, min_depth, max_depth, depth_diff, min_time_s, max_time_s, time_diff_s,
               n_roi_bin, conc_m3,
               temperature, salinity, density, fluorescence, turbidity,
               time_hr, n_frames, vol_sampled_bin_m3, time_ms,
               towyo = cast_id, max_cast_depth) # MAX CAST PRESSURE ADDED BY KS
  } # end else loop for size error
}

ctd_cast <- function(data, cast_direction = 'ascending', data_type, cutoff = 0.1, breaks = NULL) {
  #' Isolate ascending or descending section of ctd cast
  #'
  #' This is an internal step required to bin data
  #'
  #'
  #' @author  K Sorochan, E Chisholm
  #'
  #' @param data an \code{oce} ctd object
  #' @param cast_direction 'ascending' or 'descending' depending on desired section
  #' @param data_type specify 'oce' or 'df' depending on class of desired output
  #' @param cutoff Argument passed to \link[oce]{ctdFindProfiles}
  #' @param breaks Argument passed to \link[oce]{ctdFindProfiles}
  #' @return Outputs either data frame or oce ctd object
  #'
  #'
  #'
  #' @note \code{\link{ctdFindProfiles}} arguments for \code{minLength} and \code{cutOff} were updated to
  #' prevent losing data (EC 2019/07/23)
  #'
  #'
  #' @export
  #'
  # input validation
  # Check that the data argument is a valid ctd object
  if (!inherits(data, "ctd")) {
    stop("data must be a valid ctd object")
  }


  cast_updated <- list()
  # browser()
  if (is.null(breaks)) {
    cast <- oce::ctdFindProfiles(data, direction = cast_direction, minLength = 0, cutoff = cutoff)
  }else {
    cast <- oce::ctdFindProfiles(data, breaks = breaks, direction = cast_direction, cutoff = cutoff)

  }



  # append data with 'cast_id' to be able to identify/ combine data frames
  for (i in seq_len(length(cast))) {

    data <- cast[[i]]

    n_obs <- length(data@data$pressure)
    cast_id <- paste(cast_direction, i, sep = "_")
    cast_id_vec <- rep(cast_id, n_obs)

    cast_updated[[i]] <- oce::oceSetData(data, "cast_id", cast_id_vec, "no_unit")

  }

  # output in oce format
  if (data_type == "oce") {

    cast_updated

  }

  # output in dataframe
  if (data_type == "df") {

    getDf <- function(x) {
      data.frame(x@data, stringsAsFactors = FALSE)

    }

    lapply(cast_updated, getDf)

  }

}


### Image helpers ----

vpr_autoid_copy <- function(new_autoid, roi_path, day, hour, cast, station, threshold, org = 'dayhour') {
  #' Copy VPR images into folders
  #'
  #' Organize VPR images into folders based on classifications provided by visual plankton
  #'
  #' @param new_autoid A file path to your autoid folder where data is stored eg. "C:/data/cruise_X/autoid/"
  #' @param day character string representing numeric day of interest (3 chr)
  #' @param hour character string representing hour of interest (2 chr)
  #' @param cast character string, VPR cast number of interest (3 chr)
  #' @param station character string, station name of interest (eg. "Shediac")
  #' @param roi_path (optional) provide if ROI data has been moved since autoid
  #'   files were created (if path strings in aid files do not match where data
  #'   currently exists), a file path where ROI data is stored (up to "rois"
  #'   folder)
  #' @param threshold (optional) a numeric value, supplied only if you are
  #'     copying images based on automated classifications, only images below this
  #'     threshold of confidence will be copied for manual classification
  #' @param org chr value, if 'station', images will be output in folders labelled
  #'   by station, if 'dayhour', images will be output in folders labelled by day
  #'   and hour
  #'
  #' @note this function uses tidy paths, see fs::path_tidy() for more info
  #'
  #' @return organized file directory where VPR images are contained with folders, organized by day, hour and classification,
  #' inside your autoid folder
  #'
  #' @export

  # INPUT VALIDATION
  # Check that the day argument is a character string of length 3
  if (!is.character(day) || nchar(day) != 3) {
    stop("day must be a character string of length 3")
  }

  # Check that the hour argument is a character string of length 2
  if (!is.character(hour) || nchar(hour) != 2) {
    stop("hour must be a character string of length 2")
  }

  # Check that the cast argument is a character string of length 3
  if (!is.character(cast) || nchar(cast) != 3) {
    stop("cast must be a character string of length 3")
  }

  # Check that the station argument is a character vector
  if (!is.character(station)) {
    stop("station must be a character vector")
  }

  # Check that the threshold argument is a numeric value between 0 and 1 (if not NULL)
  if(!missing(threshold)){
    if (!is.null(threshold) && (!is.numeric(threshold) || threshold < 0 || threshold > 1)) {
      stop("threshold must be a numeric value between 0 and 1")
    }
  }

  # Check that the org argument is either 'station' or 'dayhour' (if not NULL)
  if (!is.null(org) && org != 'station' && org != 'dayhour') {
    stop("org must be either 'station' or 'dayhour'")
  }
  #TODO update to use withr::with_dir to avoid CRAN complaints

  # for each dh check which station it should be in
  dh <- paste0('d', day, '.h', hour)
  # pull new_aids per station
  aid_fns <- list.files(new_autoid, pattern = dh, recursive = TRUE, full.names = TRUE)
  # remove empty files
  empty_ind <- list()
  for (ii in seq_len(length(aid_fns))) {
    mtry <- try(read.table(aid_fns[ii], sep = ",", header = TRUE),
                silent = TRUE)
    if (inherits(mtry, 'try-error')) {
      empty_ind[[ii]] <- TRUE
    }else {
      empty_ind[[ii]] <- FALSE
    }
  }

  aid_fns <- aid_fns[unlist(empty_ind) == FALSE]

  # read aid files
  for (ii in seq_len(length(aid_fns))) {
    if (missing(threshold) | is.null(threshold) == TRUE) {
      aid_dat <- read.table(aid_fns[ii])
    }else {
      aid_dat <- read.table(aid_fns[ii], stringsAsFactors = FALSE)
      aid_dat <- subset(aid_dat, aid_dat$V2 < threshold)
    }
    category <- unlist(vprr::vpr_category(aid_fns[ii],
                                          categories = list.files(path = new_autoid, include.dirs = TRUE)))


    # fix file paths so they will copy
    if (!missing(roi_path)) {
      tt <- stringr::str_locate(string = aid_dat$V1[1], pattern = 'rois')
      sub_roi_path <- substr(aid_dat$V1, tt[1], nchar(aid_dat$V1))
      new_roi_path <- paste0(roi_path, sub_roi_path)
    } else {
      new_roi_path <- aid_dat$V1
    }

    # tidy path strings
    new_roi_path <- fs::path_tidy(new_roi_path)

    # copy images in batches

    if (org == 'station') {
      copy_path <- file.path(new_autoid, category,
                             paste0('vpr', cast, '_', station, '_ROIS'))
    }
    if (org == 'dayhour') {
      copy_path <- file.path(new_autoid, category,
                             paste0("aid.", dh, "_ROIS"))
    }
    fs::dir_create(copy_path, recurse = TRUE)

    fs::file_copy(new_roi_path, copy_path, overwrite = TRUE)

    cat(length(new_roi_path), 'images copied to ', copy_path, '\n')
  }
}

### Helpers  & Formatting ----

vpr_ctd_ymd <- function(data, year, offset) {
  #' Add Year/ month/ day hour:minute:second information
  #'
  #' Calculate and record calendar dates for vpr data from day-of-year, hour, and time (in milliseconds) info.
  #' Will also add 'time_hr' parameter if not already present.
  #'
  #' @param data VPR data frame from \code{\link{vpr_ctdroi_merge}}
  #' @param year Year of data collection
  #' @param offset time offset in hours between VPR CPU and processed data times (optional)
  #'
  #' @return a VPR data frame with complete date/time information in a new row named 'ymdhms'
  #'
  #' @examples
  #' year <- 2019
  #' data('ctd_roi_merge')
  #' dat <- vpr_ctd_ymd(ctd_roi_merge, year)
  #'
  #'
  #' @export

  # avoid CRAN notes
  . <- time_ms <- NA

  d <- grep(names(data), pattern = 'time_hr')
  if (length(d) == 0) {
    data <- data %>%
      dplyr::mutate(., time_hr = time_ms / 3.6e+06)
  }

  day_num <- substr(data$day, 2, 4)

  ymdd <- as.Date(as.numeric(day_num), origin = paste0(year, '-01-01'))


  l_per <- round(lubridate::seconds_to_period(data$time_ms / 1000), 0)


  ymdhms_obj <- as.POSIXct(l_per, origin = ymdd, tz = 'UTC')

  if (!missing(offset)) {

    ymdhms_obj <- ymdhms_obj + offset * 3600 # convert hour offset to seconds and adjust times

  }

  data <- data %>%
    dplyr::mutate(., ymdhms = ymdhms_obj)

  return(data)

}

px_to_mm <- function(x, opticalSetting) {
  #'Get conversion factor for pixels to mm for roi measurements
  #'
  #'Used internally
  #'
  #' @details converts pixels to mm using conversion factor specific to optical setting
  #'
  #' @param x an aidmea data frame (standard) to be converted into mm from pixels
  #' @param opticalSetting the VPR setting determining the field of view and conversion factor between mm and pixels
  #'
  #' @details Options for opticalSetting are 'S0', 'S1', 'S2', or 'S3'
  #'
  #' @export


  #find correct conversion factor based on VPR optical setting
  if (opticalSetting == 'S0') {
    #px to mm conversion factor
    frame_mm <- 7
    mm_px <- frame_mm / 1024 #1024 is resolution of VPR images (p.4 DAVPR manual)
  }
  if (opticalSetting == 'S1') {
    #px to mm conversion factor
    frame_mm <- 14
    mm_px <- frame_mm / 1024 #1024 is resolution of VPR images (p.4 DAVPR manual)
  }
  if (opticalSetting == 'S2') {
    #px to mm conversion factor
    frame_mm <- 24
    mm_px <- frame_mm / 1024 #1024 is resolution of VPR images (p.4 DAVPR manual)
  }
  if (opticalSetting == 'S3') {
    #px to mm conversion factor
    frame_mm <- 48
    mm_px <- frame_mm / 1024 #1024 is resolution of VPR images (p.4 DAVPR manual)
  }
  mm2_px2 <- (mm_px)^2

  x[, c(1, 3:7)] <- x[, c(1, 3:7)] * mm_px
  x[, 2] <- x[, 2] * mm2_px2

  return(x)

}

ctd_df_cols <- function(x, col_list) {
  #'Read CTD data (SBE49) from CTD- VPR package
  #'
  #'Internal use \code{\link{vpr_ctd_read}}
  #'
  #'**WARNING** This is hard coded to accept a specific
  #' order of CTD data columns. The names and values in these columns can change
  #' based on the specific instrument and should be updated before processing data
  #' from a new VPR.
  #'
  #'Text file format .dat file
  #'Outputs ctd dataframe with variables time_ms, conductivity, temperature,
  #'pressure, salinity
  #' @author K. Sorochan, E. Chisholm
  #'
  #'
  #'
  #'@param x full filename (ctd .dat file)
  #' @param col_list list of CTD data column names
  #'
  #'@export
  if (missing(col_list)) {
    col_list <- c("time_ms", "conductivity", "temperature", "pressure", "salinity")
    warning('CTD data columns named based on defaults!')
  }




  data <- read.table(textConnection(gsub(":", ",", readLines(x))), sep = ",")
  time <- data[, 1]
  time <- as.numeric(gsub("[^[:digit:]]", "", time))


  data2 <- cbind(time, data[, -1])

  # check that provided names are right length
  if (length(col_list) > ncol(data2)) {
    stop('Column name vector does not match data! Too many column names!')
  }

  colnames(data2) <- col_list
  data2 <- data2[!duplicated(data2), ]
  data2
}

vpr_dayhour <- function(stations, file) {
  #' Find day & hour info to match each station of interest for processing
  #'
  #'
  #'  @author E. Chisholm and K. Sorochan
  #'
  #' @param stations a vector of character values naming stations of interest
  #' @param file CSV file containing 'day', 'hour', 'station', and 'day_hour' columns
  #'
  #' @return Vector of day-hour combinations corresponding to stations of interest
  #'
  #' @export
  # avoid CRAN notes
  . <- station <- NA

  #####DEFINE FOR MULTIPLE STATIONS
  stations_of_interest <- stations

  #USE STATION LIST WITH CORRESPONDING DAY AND HOUR TO MATCH AREA OF INTEREST
  #match hour and day to station
  station_info <- read.csv(file, stringsAsFactors = FALSE)

  soi_info <- station_info %>%
    dplyr::filter(., station %in% stations_of_interest)

  dayhour <- unique(soi_info$day_hour)

  return(dayhour)

}

vpr_ctd_files <- function(castdir, cruise, day_hour) {
  #' Create a list of ctd files to be read
  #'
  #' Searches through typical VP directory structure
  #'
  #' Use with caution
  #'
  #'
  #' @param castdir root directory for ctd cast files
  #' @param cruise cruise name (exactly as in directory structure)
  #' @param day_hour vector of day-hour combinations (e.g, dXXX.hXX)
  #' @author E. Chisholm and K. Sorochan
  #'
  #' @return vector of ctd file paths matching days-hour combinations provided
  #'
  #' @export

  # ADDED BY KS
  vpr_cast_folders <- list.files(castdir, pattern = '')

  # not subset by tow number
  folder <- grep(vpr_cast_folders, pattern = paste0('VPR.', cruise, '*'), value = TRUE) # removed leading period before VPR to fit file naming scheme in COR2019002

  if (length(folder) == 0) {
    stop("No CTD files found!")
    }
  folder_path <- paste0(castdir, folder)

  # grab all days
  full_path <- list.files(folder_path, full.names = TRUE)

  # extract for only specific days
  ctd_files_all <- list.files(full_path, pattern = '*ctd*', full.names = TRUE)

  day_id <- vpr_day(ctd_files_all)
  hour_id <- vpr_hour(ctd_files_all)
  day_hour_id <- paste(day_id, hour_id, sep = ".")

  ctd_files_idx <- which(day_hour_id %in% day_hour)

  ctd_files <- ctd_files_all[ctd_files_idx]

  return(ctd_files)

}

vpr_roi <- function(x) {
  #' Get roi ids from string
  #'
  #' @author K Sorochan
  #' @param x A string specifying directory and file name of roi
  #'
  #' @return A string of only the 10 digit roi identifier
  #'
  #' @examples
  #'
  #' roi_string <- 'roi.0100000000.tif'
  #' vpr_roi(roi_string)
  #'
  #' @seealso \code{\link{vpr_hour}}, \code{\link{vpr_day}}, \code{\link{vpr_category}}
  #' @export
  #'
  #'
  m <- gregexpr("\\d{10}", x)

  y <- regmatches(x, m)

  return(y)

}

vpr_category <- function(x, categories) {
  #' Get category ids from string
  #'
  #' @author K Sorochan
  #'
  #' @param x A chr string which represents file paths from which category should be extracted
  #' @param categories A list object with all the potential classification categories
  #' @return A chr string of only the category id
  #'
  #' @note This function searches for exact matches to categories within '/' file separators. You may encounter errors if
  #'
  #' @examples
  #' category_string <- 'C:/data/cruise/autoid/Calanus/d000/h00'
  #' categories <- list("Calanus", "marine_snow", "blurry", "other_copepod")
  #' vpr_category(category_string, categories)
  #'
  #' @seealso \code{\link{vpr_hour}}, \code{\link{vpr_day}}, \code{\link{vpr_roi}}
  #' @export
  #'
  #'
# updated to vpr_category_ks2 version
  for (i in seq_len(length(categories))) {
    category <- categories[i]
    m_tmp <- gregexpr(category, x)
    if (m_tmp[[1]][1] > 0) {
      m <- m_tmp
    }else {
    }
  }
  y <- regmatches(x, m)
  return(y)

}

vpr_day <- function(x) {
  #' Get day identifier
  #'
  #' @author K Sorochan
  #' @param x A string specifying the directory and file name of the size file
  #'
  #' @return A string of only the day identifier (i.e., "dXXX")
  #'
  #' @examples
  #' day_string <- 'C:/data/cruise/autoid/Calanus/d000/h00'
  #' vpr_day(day_string)
  #'
  #' @seealso \code{\link{vpr_hour}}, \code{\link{vpr_roi}}, \code{\link{vpr_category}}
  #' @export
  #'
  #'
  m <- gregexpr("[d]+\\d{3}", x)

  y <- regmatches(x, m)

  return(y)

}

vpr_hour <- function(x) {

  #' Get hour identifier
  #'
  #' @author K Sorochan
  #' @param x A string specifying the directory and file name of the size file
  #'
  #' @return A string of only the hour identifier (i.e., "hXX")
  #' @seealso \code{\link{vpr_day}}, \code{\link{vpr_roi}}, \code{\link{vpr_category}}
  #'
  #' @examples
  #' hour_string <- 'C:/data/cruise/autoid/Calanus/d000/h00'
  #' vpr_hour(hour_string)
  #'
  #' @export
  #'
  #'

  m <- gregexpr("[h]+\\d{2}", x)

  y <- regmatches(x, m)

  return(y)

}

# Checks ----
vpr_autoid_check <- function(new_autoid, original_autoid, cruise, dayhours) {
  #' Checks manually created aid files for errors
  #'
  #' Checks for empty files, with an option to delete them. Then checks all the
  #' data for duplicated or missing ROIs which would indicate a problem with
  #' `vpr_autoid_create()`
  #'
  #' @author E Chisholm
  #'
  #' @param new_autoid file path to autoid folder eg. C:/data/CRUISENAME/autoid/ (produced by `vpr_autoid_create()`)
  #' @param original_autoid file path to original autoid folder (produced by automated classification)
  #' @param cruise name of cruise which is being checked
  #' @param dayhours chr vector, of unique day and hour values to check through (format d123.h12)
  #'
  #' @return text file (saved in working directory) named CRUISENAME_aid_file_check.txt
  #'
  #'
  #' @export
  #'
  #'
  . <- category <- roi <- NA # remove global variable warnings

  on.exit(closeAllConnections()) # make sure text file gets closed


  category_folders <- list.files(new_autoid, full.names = TRUE)

  withr::with_output_sink(paste0(cruise, '_aid_file_check.txt'), code = {
  # loop through each day.hour

  for (i in seq_len(length(category_folders))) {
    path <- category_folders[i]

    # get all files (aid )
    aid_fns <- list.files(file.path(path, 'aid'), full.names = TRUE)

    #### EMPTY FILE CHECK
    # check for empty files
    empty_ind <- list()
    for (ii in seq_len(length(aid_fns))){
      fn <- readLines(aid_fns[ii])
      if (length(fn) == 0) {
        cat('\n')
        cat(aid_fns[ii], '\n')
        cat('File is empty! \n')
        cat('\n')

        empty_ind[ii] <- TRUE
      }else {
        empty_ind[ii] <- FALSE
      }
    }
    cat('Empty file check complete for', category_folders[i], '\n')
  }

    new_aid_fn <- list.files(path = new_autoid, pattern = 'new_aid', recursive = TRUE, full.names = TRUE)

    categories <- list.files(path = new_autoid)

    cat_list <- lapply(FUN = vpr_category, new_aid_fn, categories)

    new_aids <- data.frame(fn = new_aid_fn,
                           category = unlist(cat_list),
                           day = unlist(vpr_day(new_aid_fn)),
                           hour = unlist(vpr_hour(new_aid_fn))
    )

    # check for and remove empty files
    empty_files <- list()
    for(j in seq_along(new_aids$fn)){
      mtry <- try(read.table(new_aids$fn[j], sep = ",", header = TRUE),
                  silent = TRUE)

      if ( inherits(mtry, 'try-error')) {
        empty_files[j] <- TRUE
      } else {
        empty_files[j] <- FALSE
      }
    }
    new_aids <- new_aids[empty_files == FALSE, ]


    for (i in seq_along(dayhours)) {

      dh <- dayhours[i]

      aid_fns <- new_aids$fn[paste0(new_aids$day, ".", new_aids$hour) == dh]

      if (length(aid_fns) == 0) {
        cat('WARNING: ', dh, 'skipped, no valid data found! \n')
      }else {

        # get test dh  aid cnn data into single table
        all_cnn_aid <- list.files(original_autoid, pattern = dh,
                                  full.names = TRUE, recursive = TRUE)
        aid_dat_cnn <- list()
        for (l in seq_along(all_cnn_aid)) {
          aid_dat_cnn[[l]] <- read_aid_cnn(all_cnn_aid[l])
          cn <- stringr::str_split(all_cnn_aid[l], pattern = '/')
          cn <- cn[[1]][6]
          names(aid_dat_cnn)[l] <- cn
        }

        aid_dat_cnn <- data.table::rbindlist(aid_dat_cnn, idcol = TRUE) %>%
          dplyr::rename(., 'category' = '.id')
        aid_dat_cnn <- aid_dat_cnn %>%
          dplyr::mutate(roi_num = unlist(vpr_roi(aid_dat_cnn$roi)))



        # read all aid files in
        aid_dat <- list()
        for (l in seq_along(aid_fns)) {
          aid_dat[[l]] <- read.table(aid_fns[l], sep = " ")
          names(aid_dat[[l]]) <- 'file_path'
          names(aid_dat)[l] <- vpr_category(aid_fns[l], categories)
          aid_dat[[l]] <- aid_dat[[l]] %>%
            mutate(., roi = unlist(vpr_roi(aid_dat[[l]]$file_path)))
        }

        aid_dat_c <- data.table::rbindlist(aid_dat, idcol = TRUE, fill = TRUE) %>%
          dplyr::rename(., 'category' = '.id')
        aid_dat_c <- aid_dat_c %>%
          dplyr::select(., category, roi)

        #### check for duplicated ROIs ----
        dupcheck <- duplicated(aid_dat_c$roi)

        cat(length(dupcheck[dupcheck == TRUE]), "/", length(dupcheck), "ROIs are duplicated in ", dh, "\n")


        #### check for missing ROIs ----
        newaidrois <- unique(aid_dat_c$roi)

        cnnaidrois <- aid_dat_cnn$roi

        missingrois <- length(cnnaidrois) - length(newaidrois)

        cat(missingrois, "ROIs are missing in ", dh, "\n")
      }
    }

  }) # end sink output

}

# internal (not exported) ----
insertRow <- function(existingDF, newrow, r) {
  #' INTERNAL USE ONLY
  #' quick data frame function from github to insert row inside dat frame
  #'
  #'
  #' @param existingDF data frame
  #' @param newrow new row of data
  #' @param r index of new row
  #'

  existingDF[seq(r + 1, nrow(existingDF) + 1), ] <- existingDF[seq(r, nrow(existingDF)), ]
  existingDF[r, ] <- newrow
  existingDF
}

normalize_matrix <- function(mat) {
  #' Normalize a matrix
  #'
  #' take each element of matrix dived by column total
  #'
  #' Make sure to remove total rows before using with VP data
  #'
  #' @note used internally for visualization of confusion matrices
  #'
  #' @param mat a matrix to normalize
  #'
  #'

  nm <- matrix(nrow = dim(mat)[1], ncol = dim(mat)[2])
  for (i in seq_len(length(nm[, 1]))) { # 1:length(nm[,1])
    for (j in seq_len(length(nm[1, ]))) { # 1:length(nm[1,])
      nm[i, j] <- mat[i, j] / colSums(mat)[j]
    }
  }
  return(nm)

}

### Size (not exported) ----

vpr_ctdroisize_merge <- function(data, data_mea, category_of_interest) {
  #' Format CTD and Size data from VPR
  #'
  #' Format CTD and Meas data frames into combined data frame for analysis and plotting of size data
  #'
  #' @param data VPR dataframe from \code{\link{vpr_ctdroi_merge}}, with calculated variable sigmaT
  #' @param data_mea VPR size data frame from \code{\link{vpr_autoid_read}}
  #' @param category_of_interest a list of category of interest to be included in output dataframe
  #'
  #' @return A dataframe containing VPR CTD and size data
  #'
  #' @examples
  #' \dontrun{
  #' data("ctd_roi_merge")
  #' data("roimeas_dat_combine")
  #' category_of_interest = 'Calanus'
  #'
  #'ctd_roi_merge$time_hr <- ctd_roi_merge$time_ms /3.6e+06
  #'
  #' size_df_f <- vpr_ctdroisize_merge(ctd_roi_merge, data_mea = roimeas_dat_combine,
  #'  category_of_interest = category_of_interest)
  #'}
  #'
  #'
  # INPUT VALIDATION
  # Check that the data argument is a data frame
  if (!is.data.frame(data)) {
    stop("Data must be a data frame")
  }

  # Check that the data_mea argument is a data frame
  if (!is.data.frame(data_mea)) {
    stop("Data_mea must be a data frame")
  }

  # Check that the category_of_interest argument is a character vector
  if (!is.character(category_of_interest)) {
    stop("Category_of_interest must be a character vector")
  }

  # Check that the category_of_interest argument is not empty
  if (length(category_of_interest) == 0) {
    stop("Category_of_interest cannot be empty")
  }

  # Check that the category_of_interest argument contains valid categories
  valid_categories <- unique(data_mea$category)
  invalid_categories <- setdiff(category_of_interest, valid_categories)
  if (length(invalid_categories) > 0) {
    stop(paste("Invalid categories:", paste(invalid_categories, collapse = ", ")))
  }

  # avoid CRAN notes
  . <- time_ms <- day <- hour <- roi_ID <- day_hour <- frame_ID <- pressure <- temperature <- salinity <- sigmaT <- fluorescence_mv <- turbidity_mv <- Perimeter <- Area <- width1 <- width2 <- width3 <- short_axis_length <- long_axis_length <- category <- NA

  data <- data[!duplicated(data$time_ms), ]

  #get CTD data and format
  data_ctd <- data %>%
    dplyr::mutate(., roi_ID = as.character(time_ms)) %>%
    dplyr::mutate(., day_hour = paste(day, hour, sep = ".")) %>%
    dplyr::mutate(., frame_ID = paste(roi_ID, day_hour, sep = "_")) %>%
    dplyr::select(., frame_ID, pressure, temperature, salinity, sigmaT, fluorescence_mv, turbidity_mv)

  #get measurement data
  data_mea <- data_mea %>%
    dplyr::mutate(., roi_ID = as.character(time_ms)) %>%
    dplyr::mutate(., frame_ID = paste(roi_ID, day_hour, sep = "_")) %>%
    dplyr::select(., -Perimeter, -Area, -width1, -width2, -width3, -short_axis_length)

  #combine measurement and ctd data
  data_all <- right_join(data_ctd, data_mea) %>%
    dplyr::filter(., !(is.na(pressure))) %>% #There are NAs at the beginning of CAP3.1 (i.e. measurements that are not in the ctd data)
    dplyr::mutate(., long_axis_length = as.numeric(long_axis_length)) %>%
    dplyr::filter(., category %in% category_of_interest)

  return(data_all)

}

vpr_trrois_size <- function(directory, category, opticalSetting) {
  #' Get size data from idsize files
  #'
  #'
  #' useful for getting size distribution of known rois from each category. gathers
  #' size information from idsize text files produced when training a new
  #' classifier in VP (Visual Plankton)
  #'
  #'
  #'@param directory cruise directory eg. 'C:/data/IML2018051/'
  #'@param category list of character elements containing category of interest
  #'@param opticalSetting VPR optical setting determining conversion between pixels and millimetres (options are 'S0', 'S1', 'S2', or 'S3')
  #'
  #'


  # @examples
  # \dontrun{
  #
  # }

  #loop for each category of interest
  for (t in category) {
    size_file <- list.files(path = paste0(directory, '/idsize'), pattern = paste0('mea.', t))
    #Get info
    auto_measure_px <- read.table(paste0(directory, '/idsize/', size_file), stringsAsFactors = FALSE,
                                  col.names = c('Perimeter', 'Area', 'width1', 'width2', 'width3', 'short_axis_length', 'long_axis_length'))
    eval(parse(text = paste0('auto_measure_', t,'_mm <- px_to_mm(auto_measure_px, opticalSetting )'))) #Convert to mm
  }
  #returns a data frame with size information and named columns
  eval(parse(text = paste0('return(auto_measure_', t, '_mm)')))
}

vpr_size_bin <- function(data_all, bin_mea) {
  #' Bin VPR size data
  #'
  #' Calculates statistics for VPR measurement data in depth averaged bins for analysis and visualization
  #'
  #' @param data_all a VPR CTD and measurement dataframe from \code{\link{vpr_ctdroisize_merge}}
  #' @param bin_mea Numerical value representing size of depth bins over which data will be combined, unit is metres, typical values range from 1 - 5
  #'
  #' @return a dataframe of binned VPR size data statistics including number of observations, median, interquartile ranges, salinity and pressure, useful for making boxplots
  #'
  #' @examples
  #' \dontrun{
  #' data('size_df_f')
  #' vpr_size_bin(size_df_f, bin_mea = 5)
  #' }
  #'
  #'
  #'
  #'
  #Bin by depth
  p <- data_all$pressure
  max_pressure <- max(p, na.rm = TRUE)
  min_pressure <- min(p, na.rm = TRUE)
  x_breaks <- seq(from = floor(min_pressure), to = ceiling(max_pressure), by = bin_mea)

  #Get variables of interest using oce bin functions

  med <- oce::binApply1D(p, data_all$long_axis_length, xbreaks = x_breaks, median)$result
  iqr3 <- oce::binApply1D(p, data_all$long_axis_length,  xbreaks = x_breaks, quantile, probs = 0.75)$result
  iqr1 <- oce::binApply1D(p, data_all$long_axis_length,  xbreaks = x_breaks, quantile, probs = 0.25)$result
  n_obs <- oce::binApply1D(p, data_all$long_axis_length, xbreaks = x_breaks, length)$result
  temperature <- oce::binApply1D(p, data_all$temperature, xbreaks = x_breaks, mean)$result
  salinity <- oce::binApply1D(p, data$salinity, xbreaks = x_breaks, mean)$result
  pressure <- oce::binApply1D(p, data$salinity, xbreaks = x_breaks, mean)$xmids #Could be any of the variables computed, but I just went with salinity

  if (!(length(pressure) == length(salinity))) {

    salinity_mean <- binMean1D(p, data_all$salinity, xbreaks = x_breaks)$result

    idx_rm <- which(is.na(salinity_mean))

    #informs user where bins were removed due to NAs
    #note if a bin is 'NA' typically because there is no valid data in that depth range,
    #if you have a lot of NA bins, think about increasing your binSize
    print(paste('Removed bins at', pressure[idx_rm]))

    pressure <- pressure[-idx_rm]

  }

  dfs <- data.frame('median' = med, 'IQR1' = iqr1,
                    'IQR3' = iqr3, 'n_obs' = n_obs,
                    'temperature' = temperature, 'salinity' = salinity,
                    'pressure' = pressure)
  return(dfs)
}


#####PLOTTING FUNCTIONS#####
#### not exported ####

isopycnal_calculate <- function(sal, pot.temp, reference.p = 0) {
  #' Get vector to draw isopycnal lines on TS plot
  #' Used internally to create TS plots
  #' @author E. Chisholm
  #'
  #' @param sal salinity vector
  #' @param pot.temp temperature vector in deg C
  #' @param reference.p reference pressure for calculation, set to 0
  #'
  #'
  #' @note: modified from source:\url{https://github.com/Davidatlarge/ggTS/blob/master/ggTS_DK.R}
  #'

  # avoid CRAN notes
  density <- NA

  TS <- expand.grid(
    sal = seq(floor(min(sal, na.rm = TRUE)), ceiling(max(sal, na.rm = TRUE)), length.out = 100),
    pot.temp = seq(floor(min(pot.temp, na.rm = TRUE)), ceiling(max(pot.temp, na.rm = TRUE)), length.out = 100)
  )
  TS$density <- gsw::gsw_rho_t_exact(SA = TS$sal, t = TS$pot.temp, p = reference.p) - 1000 # the function calculates in-situ density, but because potential temperature and a single reference pressure is used the result equals potential density at reference pressure

  # isopycnal labels
  # +- horizontal isopycnals
  h.isopycnals <- subset(TS,
                         sal == ceiling(max(TS$sal)) & # selects all rows where "sal" is the max limit of the x axis
                           round(density, 1) %in% seq(min(round(TS$density * 2) / 2, na.rm = TRUE),
                                                     max(round(TS$density * 2) / 2, na.rm = TRUE),
                                                     by = .5)) # selects any line where the rounded denisty is equal to density represented by any isopycnal in the plot
  if (nrow(h.isopycnals) > 0) {
    h.isopycnals$density <- round(h.isopycnals$density, 1) # rounds the density
    h.isopycnals <- aggregate(pot.temp ~ density, h.isopycnals, mean) # reduces number of "pot.temp" values to 1 per each unique "density" value
  }

  # +- vertical isopycnals
  if (nrow(h.isopycnals) == 0) { # if the isopycnals are not +- horizontal then the df will have no rows
    rm(h.isopycnals) # remove the no-line df

    v.isopycnals <- subset(TS, # make a df for labeling vertical isopycnals
                           pot.temp == ceiling(max(TS$pot.temp)) & # selects all rows where "sal" is the max limit of the x axis
                             round(density, 1) %in% seq(min(round(TS$density * 2) / 2),
                                                       max(round(TS$density * 2) / 2),
                                                       by = .5)) # selects any line where the rounded denisty is equal to density represented by any isopycnal in the plot
    v.isopycnals$density <- round(v.isopycnals$density, 1) # rounds the density
    v.isopycnals <- aggregate(sal ~ density, v.isopycnals, mean) # reduces number of "pot.temp" values to 1 per each unique "density" value
  }


  return(TS)
}

vpr_plot_TS <- function(x, reference.p = 0, var){

  #' Make a balloon plot against a TS plot
  #'
  #' TS balloon plot with ROI concentration, sorted by category
  #' includes isopycnal line calculations
  #'
  #' @author E. Chisholm
  #'
  #' @param x dataframe with temperature, salinity, number of rois (n_roi_bin)
  #' @param reference.p reference pressure (default at 0 for surface)- used to calculate isopycnals
  #' @param var variable on which size of points will be based, eg conc_m3 or n_roi_bin
  #'
  #'
  #'
  #' @note modified from source: \url{https://github.com/Davidatlarge/ggTS/blob/master/ggTS_DK.R}
  #'
  #'

# avoid CRAN notes
  p <- NA

  #get isopycnal lines
  sal <-  x$salinity
  pot.temp <-  x$temperature

  # make TS long table
  TS <- expand.grid(
    sal = seq(floor(min(sal, na.rm = TRUE)), ceiling(max(sal, na.rm = TRUE)), length.out = 100),
    pot.temp = seq(floor(min(pot.temp, na.rm = TRUE)), ceiling(max(pot.temp, na.rm = TRUE)), length.out = 100)
  )
  TS$density <- gsw::gsw_rho_t_exact(SA = TS$sal, t = TS$pot.temp, p = reference.p) - 1000 # the function calculates in-situ density, but because potential temperature and a single reference pressure is used the result equals potential density at reference pressure

  #removed isopycnal line labelling scheme so that every isopycnal line could be labelled using different method
  # isopycnal labels for plotting
  # +- horizontal isopycnals
  # h.isopycnals <- subset(TS,
  #                        sal == ceiling(max(TS$sal)) & # selects all rows where "sal" is the max limit of the x axis
  #                          round(density,2) %in% seq(min(round(TS$density*2)/2, na.rm = TRUE),
  #                                                    max(round(TS$density*2)/2, na.rm = TRUE),
  #                                                    by = .5)) # selects any line where the rounded denisty is equal to density represented by any isopycnal in the plot
  # if(nrow(h.isopycnals)>0){
  #   h.isopycnals$density <- round(h.isopycnals$density, 2) # rounds the density
  #   h.isopycnals <- aggregate(pot.temp~density, h.isopycnals, mean) # reduces number of "pot.temp" values to 1 per each unique "density" value
  # }
  #
  # #removing this calculates more isopycnals to be labelled
  # #otherwsie only labels one isopycnal line on plot
  # # +- vertical isopycnals (labels for plotting)
  # #if(nrow(h.isopycnals)==0){ # if the isopycnals are not +- horizontal then the df will have no rows
  #  # rm(h.isopycnals) # remove the no-line df
  #
  #   v.isopycnals <- subset(TS, # make a df for labeling vertical isopycnals
  #                          pot.temp == ceiling(max(TS$pot.temp)) & # selects all rows where "sal" is the max limit of the x axis
  #                            round(density,2) %in% seq(min(round(TS$density*2)/2),
  #                                                      max(round(TS$density*2)/2),
  #                                                      by = .5)) # selects any line where the rounded denisty is equal to density represented by any isopycnal in the plot
  #   v.isopycnals$density <- round(v.isopycnals$density, 2) # rounds the density
  #   v.isopycnals <- aggregate(sal~density, v.isopycnals, mean) # reduces number of "pot.temp" values to 1 per each unique "density" value
  # #}

  #plot
  eval(parse(text = paste0("p <- ggplot()+
                           #isopycnal lines
                           geom_contour(data = TS, aes(x = sal, y = pot.temp, z = density), col = 'grey', linetype = 'solid',
                           breaks = seq(min(round(TS$density*2)/2, na.rm = TRUE), # taking density times 2, rounding and dividing by 2 rounds it to the neares 0.5
                           max(round(TS$density*2)/2, na.rm = TRUE),
                           by = .5)) +
                           geom_text_contour(data = TS, aes(x = sal, y =pot.temp, z= density),binwidth = 0.5, col = 'grey', nudge_x = 0.1)+ #CONTOUR LABELS
                           #roi data sorted by number of rois and category
                           geom_point(data = x, aes(x = salinity, y = temperature, size = ", var, "), shape = 21) +
                           scale_size_area(max_size=10)+ #make balloons bigger
                           #label legends
                           labs(size = expression('Concentration /m'^3)) +
                           labs(col = 'category')+
                           #set x axis (ensure scaling to data)
                           scale_x_continuous(name = 'Salinity [PSU]', expand = c(0,0),
                           limits = c(floor(min(x$salinity, na.rm = TRUE)), ceiling(max(x$salinity, na.rm = TRUE)))) + # use range of 'sal' for x axis
                           #set y axis (esure scaled to data)
                           scale_y_continuous(name = expression(paste('potential temperature [ ',degree,' C]')),
                           limits = c(floor(min(x$temperature, na.rm = TRUE)), ceiling(max(x$temperature, na.rm = TRUE)))) +
                           #get rid of grid lines and text formatting
                           theme_classic() + theme(text = element_text(size=14)) "
  )))

  #using geom_text_contour to label isopycnals instead
  # add isopycnal labels if isopycnals run +- horizontal
  # if(exists("h.isopycnals")){
  #   p <- p + geom_text(data = h.isopycnals,
  #                      aes(x = ceiling(max(TS$sal)), y = pot.temp, label = density),
  #                      hjust = "inward", vjust = 0, col = "grey")
  # }
  #
  # # add isopycnal labels if isopycnals run +- vertical
  # if(exists("v.isopycnals")){
  #   p <- p + geom_text(data = v.isopycnals,
  #                      aes(x = sal, y = ceiling(max(TS$pot.temp)), label = density),
  #                      vjust = "inward", hjust = 0, col = "grey")
  # }

  return(p)
}

vpr_plot_TScat <- function(x, reference.p = 0){

  #' Make a balloon plot
  #'
  #' Balloon plot against a TS plot with ROI concentration and sorted by category
  #' includes isopycnal line calculations. Version of \code{\link{vpr_plot_TS}}, with only relevant* category specified.
  #' *to current analysis and research objectives (See note).
  #'
  #'
  #' @note \strong{WARNING HARD CODED} FOR  5 category, CALANUS, KRILL, ECHINODERM LARVAE, SMALL COPEPOD, CHAETOGNATHS
  #' !! Uses isopycnal labelling method which does not label every contour
  #'
  #' @param x dataframe with temperature, salinity, number of rois named by category
  #' @param reference.p reference pressure (default at 0 for surface)- used to calculate isopycnals
  #'
  #'
  #'
  #' @note modified from source: \url{https://github.com/Davidatlarge/ggTS/blob/master/ggTS_DK.R}
  #'
  #'

# avoid CRAN notes
  density <- salinity <- temperature <- calanus <- chaetognaths <- small_copepod <- krill <- echinoderm_larvae <- NA

  #get isopycnal lines
  sal <-  x$salinity
  pot.temp <-  x$temperature

  # make TS long table
  TS <- expand.grid(
    sal = seq(floor(min(sal, na.rm = TRUE)), ceiling(max(sal, na.rm = TRUE)), length.out = 100),
    pot.temp = seq(floor(min(pot.temp, na.rm = TRUE)), ceiling(max(pot.temp, na.rm = TRUE)), length.out = 100)
  )
  TS$density <- gsw::gsw_rho_t_exact(SA = TS$sal, t = TS$pot.temp, p = reference.p) - 1000 # the function calculates in-situ density, but because potential temperature and a single reference pressure is used the result equals potential density at reference pressure

  # isopycnal labels for plotting
  # +- horizontal isopycnals
  h.isopycnals <- subset(TS,
                         sal == ceiling(max(TS$sal)) & # selects all rows where "sal" is the max limit of the x axis
                           round(density,1) %in% seq(min(round(TS$density*2)/2, na.rm = TRUE),
                                                     max(round(TS$density*2)/2, na.rm = TRUE),
                                                     by = .5)) # selects any line where the rounded denisty is equal to density represented by any isopycnal in the plot
  if(nrow(h.isopycnals)>0){
    h.isopycnals$density <- round(h.isopycnals$density, 1) # rounds the density
    h.isopycnals <- aggregate(pot.temp~density, h.isopycnals, mean) # reduces number of "pot.temp" values to 1 per each unique "density" value
  }

  # +- vertical isopycnals (labels for plotting)
  if(nrow(h.isopycnals)==0){ # if the isopycnals are not +- horizontal then the df will have no rows
    rm(h.isopycnals) # remove the no-line df

    v.isopycnals <- subset(TS, # make a df for labeling vertical isopycnals
                           pot.temp == ceiling(max(TS$pot.temp)) & # selects all rows where "sal" is the max limit of the x axis
                             round(density,1) %in% seq(min(round(TS$density*2)/2),
                                                       max(round(TS$density*2)/2),
                                                       by = .5)) # selects any line where the rounded denisty is equal to density represented by any isopycnal in the plot
    v.isopycnals$density <- round(v.isopycnals$density, 1) # rounds the density
    v.isopycnals <- aggregate(sal~density, v.isopycnals, mean) # reduces number of "pot.temp" values to 1 per each unique "density" value
  }

  #initialize category
  #WARNING HARD CODING, 5 category
  cols <- c('t1' = 'darkorchid3', 't2' = 'deeppink3', 't3' = 'dodgerblue3', 't4' = 'tomato3', 't5' = 'gold3')
  categories <- c('calanus', 'chaetognaths', 'small_copepod', 'krill', 'echinoderm_larvae')
  #plot
  p <- ggplot()+
    #isopycnal lines
    geom_contour(data = TS, aes(x = sal, y = pot.temp, z = density), col = "grey", linetype = "solid",
                 breaks = seq(min(round(TS$density*2)/2, na.rm = TRUE), # taking density times 2, rounding and dividing by 2 rounds it to the neares 0.5
                              max(round(TS$density*2)/2, na.rm = TRUE),
                              by = .5)) +
    #roi data sorted by number of rois and category
    geom_point(data = x, aes(x = salinity, y = temperature, size = calanus, col = 't1' ), shape = 21) +
    #geom_point(data = x, aes(x = salinity, y = temperature, size = marine_snow), shape = 21) +
    #geom_point(data = x, aes(x = salinity, y = temperature, size = stick), shape = 21) +
    geom_point(data = x, aes(x = salinity, y = temperature, size = chaetognaths, col = 't2'), shape = 21) +
    geom_point(data = x, aes(x = salinity, y = temperature, size = small_copepod, col = 't3'), shape = 21) +
    geom_point(data = x, aes(x = salinity, y = temperature, size = krill, col = 't4'), shape = 21) +
    geom_point(data = x, aes(x = salinity, y = temperature, size = echinoderm_larvae, col = 't5'), shape = 21) +
    scale_colour_manual(name = 'categories', values = cols, guide = guide_legend(), labels = categories) +
    scale_size_area(max_size=10)+ #make balloons bigger
    #label legends
    labs(size = 'Number of \n ROIs') +
    #labs(col = 'category')+
    #set x axis (ensure scaling to data)
    scale_x_continuous(name = "salinity", expand = c(0,0),
                       limits = c(floor(min(x$salinity, na.rm = TRUE)), ceiling(max(x$salinity, na.rm = TRUE)))) + # use range of "sal" for x axis
    #set y axis (esure scaled to data)
    scale_y_continuous(name = "potential temperature [C]",
                       limits = c(floor(min(x$temperature, na.rm = TRUE)), ceiling(max(x$temperature, na.rm = TRUE)))) +
    #get rid of grid lines and text formatting
    theme_classic() + theme(text = element_text(size=14))

  # add isopycnal labels if isopycnals run +- horizontal
  if(exists("h.isopycnals")){
    p <- p + geom_text(data = h.isopycnals,
                       aes(x = ceiling(max(TS$sal)), y = pot.temp, label = density),
                       hjust = "inward", vjust = 0, col = "grey")
  }

  # add isopycnal labels if isopycnals run +- vertical
  if(exists("v.isopycnals")){
    p <- p + geom_text(data = v.isopycnals,
                       aes(x = sal, y = ceiling(max(TS$pot.temp)), label = density),
                       vjust = "inward", hjust = 0, col = "grey")
  }

  return(p)
}

interp2xyz <- function(al, data.frame = FALSE) {
  stopifnot(is.list(al), identical(names(al), c("x","y","z")))
  xy <- expand.grid(x = al[["x"]], y = al[["y"]], KEEP.OUT.ATTRS=FALSE)
  cbind(if(!data.frame) data.matrix(xy) else xy,
        z = as.vector(al[["z"]]))
}

vpr_plot_contour <- function(data, var, dup= 'mean', method = 'interp', labels = TRUE, bw = 1, cmo){

  #' Interpolated contour plot of particular variable
  #'
  #' Creates interpolated contour plot, can be used as a background for ROI or tow yo information
  #'
  #' @author E. Chisholm & Kevin Sorochan
  #'
  #' @param data data frame needs to include time_hr, depth, and variable of
  #'   choice (var)
  #' @param var variable in dataframe which will be interpolated and plotted
  #' @param dup if method == 'interp'. Method of handling duplicates in interpolation, passed to interp function (options: 'mean', 'strip', 'error')
  #' @param method Specifies interpolation method, options are 'interp' or
  #'   'oce', oce uses slightly different method (oce is least error prone)
  #' @param labels logical value indicating whether or not to plot contour labels
  #' @param bw bin width defining interval at which contours are labelled
  #' @param cmo name of a `cmocean` plotting theme, see `?cmocean` for more information
  #'
  #'


 # avoid CRAN notes
  x <- y <- z <- NA
  #interpolate
  #use interp package rather than akima to avoid breaking R
  #ref: https://www.user2017.brussels/uploads/bivand_gebhardt_user17_a0.pdf
  # browser()

  # akima method deprecated 2022 - due to licensing issues
  # if(method == 'akima'){
  #   interpdf <- akima::interp(x = data$time_hr, y = data$depth, z = data[[var]], duplicate = dup ,linear = TRUE  )
  # }

  if(method == 'interp'){

    interpdf <- try(interp::interp(x = data$time_hr, y = data$depth, z = data[[var]], duplicate = dup ,linear = TRUE  ))
    if(inherits(interpdf, 'try-error'))
      stop("Interpolation failed, try method = 'oce'")
  }
  if(method == 'oce'){
    interpdf_oce <- oce::interpBarnes(x = data$time_hr, y = data$depth, z = data[[var]] )
    interpdf <- NULL
    interpdf$x <- interpdf_oce$xg
    interpdf$y <- interpdf_oce$yg
    interpdf$z <- interpdf_oce$zg
  }
  #convert to dataframe
  df <- interp2xyz(interpdf, data.frame = TRUE)

  #zero time values
  df$x <- df$x - min(df$x)

  if(missing(cmo)){
    # default to gray
    cmof <- cmocean::cmocean('gray')
    # set default col scheme based on variable name
    if(var %in% c('temperature', 'conc_m3')){
      cmof <- cmocean::cmocean('thermal')
    }
    if(var == 'salinity') {
      cmof <- cmocean::cmocean('haline')
    }
    if(var == 'density') {
      cmof <- cmocean::cmocean('dense')
    }
    if(var == 'fluorescence') {
      cmof <- cmocean::cmocean('algae')
    }
    if(var == 'turbidity') {
      cmof <- cmocean::cmocean('turbid')
    }} else{
      cmof <- cmocean::cmocean(cmo)
    }

  # theme_col <- cmocean::cmocean(cmo)
  cmo_data <- cmof(100)

  if(labels == TRUE){
    # updated plotting from KS
    p <- ggplot(df) +
      geom_tile(aes(x = x, y = y, fill = z)) +
      labs(fill = var) +
      scale_y_reverse(name = "Depth [m]") +
      scale_x_continuous(name = "Time [h]") +
      theme_classic() +
      geom_contour(aes(x = x, y = y, z = z), col = "black") +
      geom_text_contour(aes(x = x, y = y, z = z), binwidth = bw,
                        col = "white", check_overlap = TRUE, size = 8) +
      scale_fill_gradientn(colours = cmo_data, na.value = 'gray')
    # p <- ggplot(df) +
    #   geom_tile(aes(x = x, y = y, fill = z)) +
    #   labs(fill = var) +
    #   scale_y_reverse(name = 'Depth [m]') +
    #   scale_x_continuous(name = 'Time [h]') +
    #   theme_classic() +
    #   geom_contour(aes(x = x, y = y, z= z), col = 'black') +
    #   geom_text_contour(aes(x = x, y = y, z= z),binwidth = bw, col = 'white', check_overlap = TRUE, size = 8)+ #CONTOUR LABELS
    #   scale_fill_continuous(na.value = 'white')
  }else{
    #  updated plotting from KS
    p <- ggplot(df) +
      geom_tile(aes(x = x, y = y, fill = z)) +
      labs(fill = var) + scale_y_reverse(name = "Depth [m]") +
      scale_x_continuous(name = "Time [h]") + theme_classic() +
      geom_contour(aes(x = x, y = y, z = z), col = "black") +
      scale_fill_gradientn(colours = cmo_data, na.value = "gray")
    # p <- ggplot(df) +
    #   geom_tile(aes(x = x, y = y, fill = z)) +
    #   labs(fill = var) +
    #   scale_y_reverse(name = 'Depth [m]') +
    #   scale_x_continuous(name = 'Time [h]') +
    #   theme_classic() +
    #   geom_contour(aes(x = x, y = y, z= z), col = 'black') +
    #   scale_fill_continuous(na.value = 'white')
  }
  return(p)
}

vpr_plot_profile <- function(category_conc_n, category_to_plot, plot_conc) {

  #' Plots VPR profiles of temperature, salinity, density, fluorescence and concentration (by classification group)
  #'
  #'
  #' This plot allows a good overview of vertical distribution of individual classification groups along with reference to hydrographic parameters.
  #' Facet wrap is used to create distinct panels for each category provided
  #'
  #' @param category_conc_n A VPR data frame with hydrographic and concentration data separated by category (from \code{\link{vpr_roi_concentration}})
  #' @param category_to_plot The specific classification groups which will be plotted, if NULL, will plot all category combined
  #' @param plot_conc Logical value whether or not to include a concentration plot (FALSE just shows CTD data)
  #'
  #' @return A gridded object of at least 3 ggplot objects
  #'

  # check that depth is present
  if(!'depth' %in% names(category_conc_n)){
    stop("These plots require a 'depth' variable!")
  }

  # avoid CRAN notes
  temperature <- depth <- salinity <- fluorescence <- density <- conc_m3  <- NA
# plot temp
p <- ggplot(category_conc_n) +
  geom_point(aes(x = temperature, y = depth), col = 'red') +
  scale_y_reverse(name = 'Depth [m]')
# plot salinity
p_TS <- p + geom_point(aes(x = (salinity -25), y = depth), col = 'blue') +
  scale_x_continuous(name = expression(paste("Temperature [",degree,"C]")),sec.axis = sec_axis(~ . +25, name = 'Salinity [PSU]')) +
  theme(axis.line.x.bottom = element_line(colour = 'red'),
        axis.ticks.x.bottom = element_line(colour = 'red'),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line.y = element_line(linetype = 'solid'),
        axis.line.x.top = element_line(colour = 'blue'),
        axis.ticks.x.top = element_line(colour = 'blue'),
        axis.title = element_text(size = 20)
  )
# have to force rescale for multi axes ggplot

# plot fluorescence
p <- ggplot(category_conc_n) +
  geom_point(aes(x = fluorescence, y = depth), col = 'green') +
  scale_y_reverse(name = 'Depth [m]')

# plot density
p_FD <- p + geom_point(aes(x = (density  -20) *20, y = depth)) +
  scale_x_continuous(name = 'Fluorescence [mv]',sec.axis = sec_axis(~. /20  +20, name = 'Density')) +
  theme(axis.line.x.bottom = element_line(colour = 'green'),
        axis.ticks.x.bottom = element_line(colour = 'green'),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line.y = element_line(linetype = 'solid'),
        axis.line.x.top = element_line(colour = 'black'),
        axis.title = element_text(size = 20)
  )
# manual rescale


if(is.null(category_to_plot)){
  pp <- ggplot(category_conc_n) +
    geom_point(aes(x = depth, y = conc_m3/1000)) + #conversion of m3 to L using default density
    stat_summary_bin(aes(x = depth, y = conc_m3/1000), fun = 'mean', col = 'red', geom = 'line', size = 3)  +
    scale_x_reverse(name = 'Depth [m]') +
    scale_y_continuous(name = expression('ROI L'^'-1')) +
    # ggtitle('Concentrations') +
    theme_classic() +
    theme(strip.text = element_text(size = 18),
          plot.title = element_text(size = 25),
          axis.title = element_text(size = 20))+
    coord_flip()
}else{
# facet wrap plot all category, if only one category, comment out facet wrap
pp <- ggplot(category_conc_n[category_conc_n$category %in% c(category_to_plot),]) +
  geom_point(aes(x = depth, y = conc_m3/1000)) + #conversion of m3 to L using default density
  stat_summary_bin(aes(x = depth, y = conc_m3/1000), fun = 'mean', col = 'red', geom = 'line', size = 3)  +
  scale_x_reverse(name = 'Depth [m]') +
  scale_y_continuous(name = expression('ROI L'^'-1')) +
  # ggtitle('Concentrations') +
  facet_wrap(~category, nrow = 1, ncol = length(unique(category_conc_n$category)), scales = 'free_x')+
  theme_classic() +
  theme(strip.text = element_text(size = 18),
        plot.title = element_text(size = 25),
        axis.title = element_text(size = 20))+
  coord_flip()
}

if(plot_conc == TRUE){
p <- grid.arrange(p_TS, p_FD, pp , widths = c(1, 1, 2), heights = 2, nrow = 1, ncol = 3)
}else{
  p <- grid.arrange(p_TS, p_FD, widths = c(1, 1), heights = 2, nrow = 1, ncol = 2)

}
return(p)
}

#### IMAGE ANALYSIS FUNCTIONS ####
#### not exported ####

vpr_img_reclassified <- function(day, hour, base_dir, category_of_interest, image_dir){
  #' Explore reclassified images
  #'
  #' Pull image from reclassified or misclassified files produced during \code{\link{vpr_manual_classification}}
  #'
  #' @param day Character string, 3 digit day of interest of VPR data
  #' @param hour Character string, 2 digit hour of interest of VPR data
  #' @param base_dir directory path to folder containing day/hour folders in which misclassified and reclassified files are organized (eg.'C:/VPR_PROJECT/r_project_data_vis/classification files/') which would contain 'd123.h01/reclassified_krill.txt' )
  #' @param category_of_interest Classification group from which to pull images
  #' @param image_dir directory path to ROI images, eg. "E:\\\\data\\\\cruise_IML2018051\\\\", file separator MUST BE "\\\\" in order to be recognized
  #'
  #' @return folders of misclassified or reclassified images inside image_dir
  #'
  #'
  #'
  ####directory where misclassified/reclassified files
  ####base_dir <- 'C:/VPR_PROJECT/r_project_data_vis/classification files/'


  #basepath where ROI images are
  ##image_dir <- 'E:\\data\\cruise_IML2018051\\'
  ##setwd(image_dir)

  #get misclassified/ reclassified images

  day_hour <- paste0('d', day, '.h', hour)


  folder <- list.files(base_dir, pattern = day_hour, full.names = TRUE)

  files <- list.files(folder, pattern = category_of_interest)

  #pulls out missclassified files
  #(you only want to look at images
  #you have reclassified as specific category - select "n" if pop up appears)

  #only exception (reason to select "y",
  #would be to look specifically at images that you pulled out of a category,
  #for example to check the accuracy of an automatic ID scheme)
  tt <- grep(files, pattern = 'misclassified')
  if (length(tt) > 0 ){
    warning(paste('misclassified files found for ', category_of_interest))
    ans <- readline('Would you like to include these files? (y/n)
                    *NOTE, looking at misclassified images will show you images that were removed from you category of interest
                    during reclassification, this may be useful to get an idea of the accuracy of your automatic
                    classification or which images are confusing your automatic classification.')
    if(ans == 'n'){
      files <- files[-tt]
      folder_name <- "reclassified_ROIS"
    }else{
      files <- files[tt]
      folder_name <- "misclassified_ROIS"
    }
  }

  message(paste('>>>>>', files, 'found for', category_of_interest, ' in ', day_hour, '!'))
  message('>>>>> Copying images now!')



  #runs through reclassified files (should only be one)
  for(ii in files) {


    #reads in roi strings
    roi_path_str <- read.table(paste0(base_dir, '/', day_hour, '/', ii), stringsAsFactors = FALSE)

    #sub out for new basepath where rois are located
    #note this is an extra step because I moved the "data" folder from my C drive
    tt<- stringr::str_locate(string = roi_path_str$V1[1], pattern = 'rois')
    sub_roi_path <- substr(roi_path_str$V1, tt[1], nchar(roi_path_str))
    new_roi_path <- file.path(image_dir, sub_roi_path, fsep = "\\")

    #Create a new folder for autoid rois (where images will be stored)
    roi_folder <- file.path(image_dir, category_of_interest, folder_name, fsep = "\\")
    command1 <- paste('mkdir', roi_folder, sep = " ")
    shell(command1)

    #Copy rois to this directory
    for (iii in seq_len(length(new_roi_path))) {

      dir_tmp <- as.character(new_roi_path[iii])
      command2 <- paste("copy", dir_tmp, roi_folder, sep = " ")
      shell(command2)

      print(paste(iii, '/', length(new_roi_path),' completed!'))
    }
message(paste('Images saved to ', roi_folder))
  }






}

vpr_img_depth <- function(data, min.depth , max.depth, roiFolder , format = 'list'){

  #' Explore VPR images by depth bin
  #'
  #' Allows user to pull VPR images from specific depth ranges, to investigate
  #' trends before classification of images into category groups
  #'
  #'
  #'
  #' @param data data frame containing CTD and ROI data from
  #'   \code{\link{vpr_ctdroi_merge}}, which also contains calculated variables
  #'   sigmaT and time_hr
  #' @param min.depth minimum depth of ROIs you are interested in looking at
  #' @param max.depth maximum depth of ROIs you are interested in exploring
  #' @param roiFolder directory that ROIs are within (can be very general eg.
  #'   C:/data, but will be quicker to process with more specific file path)
  #' @param format option of how images will be output, either as 'list' a list
  #'   of file names or 'image' where images will be displayed
  #'
  #'
  #'


  #      #### examples
  # #determine range of interest
  # mid <- as.numeric(readline('Minimum depth of interest? '))
  # mad <- as.numeric(readline('Maximum depth of interest? '))
  # #run image exploration
  # roi_files <- vpr_img_depth(all_dat, min.depth = mid, max.depth = mad,
  # roiFolder = paste0('E:/data/IML2018051/rois/vpr', tow ), format = 'list')
  #
  # #copy image files into new directory to be browsed
  # roi_file_unlist <- unlist(roi_files)
  # newdir <- file.path(plotdir, paste0('vpr', tow, 'images_', mid, '_', mad, ''))
  # dir.create(newdir)
  # file.copy(roi_file_unlist, newdir)
  #

  # avoid CRAN notes
  . <- pressure <- roi <- NA

  data_filtered <- data %>%
    dplyr::filter(., pressure >= min.depth) %>%
    dplyr::filter(., pressure <= max.depth)

  if(length(data_filtered$roi) < 1){
    stop('No data exists within this depth range!')
  }


  #search for ROI files based on data
  roi_files <- paste0('roi.', sprintf('%08d', data_filtered$roi), '*')
  roi_file_list <- list()
  #options(warn = 1)
  for (i in seq_len(length(roi_files))){
    roi_file_list[[i]] <- list.files(roiFolder, pattern = roi_files[i], recursive = TRUE, full.names = TRUE)
    if (length(roi_file_list[[i]]) >= 1){
      message(paste('Found', length(roi_file_list[[i]]),' files for ',roi_files[i] ))
    }else{
      warning('No file found in directory (', roiFolder, ')  matching ', roi_files[i])
    }
  }

  if( format == 'list'){
    return(roi_file_list)
  }
  if (format == 'image'){
    for(i in seq_len(length(roi_file_list))){
      for(ii in seq_len(length(roi_file_list[[i]]))){
        data_roi <- data_filtered %>%
          dplyr::filter(., roi == roi[i])
        meta_str <- paste0('time (hr): ', data_roi$time_hr[1], '\n temperature: ', data_roi$temperature[1], '\n pressure: ', data_roi$pressure[1], '\n salinity: ', data_roi$salinity[1], '\n')
        pp <-  magick::image_read(roi_file_list[[i]][ii]) %>%
          #print metadata on image
          #image_annotate(text = roi_files[i], color = 'white', size = 10) %>%
          #image_annotate(text = meta_str, color = 'white', location = '-100') %>%
          magick::image_scale(geometry = 'x300')

        print(pp)
        #print metadata
        #cat(paste0(roi_files[i], '\n'))
        #cat( paste0('time (hr): ', data_roi$time_hr, '\n temperature: ', data_roi$temperature, '\n pressure: ', data_roi$pressure, '\n salinity: ', data_roi$salinity, '\n'))

      }
    }
  }




  #
}

vpr_img_category <- function(data, min.depth , max.depth, roiFolder , format = 'list', category_of_interest){

  #' Explore images by depth and classification
  #'
  #' Pulls images from specific depth ranges in specific classification group
  #'
  #'
  #'
  #'
  #'
  #' @param data data frame containing CTD and ROI data from
  #'   \code{\link{vpr_ctdroi_merge}}, which also contains calculated variables
  #'   sigmaT and time_hr
  #' @param min.depth minimum depth of ROIs you are interested in looking at
  #' @param max.depth maximum depth of ROIs you are interested in exploring
  #' @param roiFolder directory that ROIs are within (can be very general eg.
  #'   C:/data, but will be quicker to process with more specific file path)
  #' @param format option of how images will be output, either as 'list' a list
  #'   of file names or 'image' where images will be displayed
  #' @param category_of_interest character string of classification group from which
  #'   to pull images
  #'
  #'
  #'
  #  ### examples
  # #determine range of interest
  # mid <- as.numeric(readline('Minimum depth of interest? '))
  # mad <- as.numeric(readline('Maximum depth of interest? '))
  # #run image exploration
  # roi_files <- vpr_img_category(all_dat, min.depth = mid, max.depth = mad,
  # roiFolder = paste0('E:/data/IML2018051/rois/vpr', tow ), format = 'list',
  # category = 'Calanus')
  #
  # #copy image files into new directory to be browsed
  # roi_file_unlist <- unlist(roi_files)
  # newdir <- file.path(plotdir, paste0('vpr', tow, 'images_', mid, '_', mad, '_', category))
  # dir.create(newdir)
  # file.copy(roi_file_unlist, newdir)
  #
  #

  # avoid CRAN notes
  . <- pressure <- category <- roi <- NA
  if(length(category_of_interest) > 1){
    stop("Only explore one category at a time!")
  }

  data_filtered <- data %>%
    dplyr::filter(., pressure >= min.depth) %>%
    dplyr::filter(., pressure <= max.depth) %>%
    dplyr::filter(., category %in% category_of_interest)

  if(length(data_filtered$roi) < 1){
    stop('No data exists within this depth and category range!')
  }


  #search for ROI files based on data
  roi_files <- paste0('roi.', sprintf('%08d', data_filtered$roi), '*')
  roi_file_list <- list()
  # options(warn = 1) # is this needed?
  for (i in seq_len(length(roi_files))){
    roi_file_list[[i]] <- list.files(roiFolder, pattern = roi_files[i], recursive = TRUE, full.names = TRUE)
    if (length(roi_file_list[[i]]) >= 1){
      print(paste('Found', length(roi_file_list[[i]]),' files for ',roi_files[i] ))
    }else{
      warning('No file found in directory (', roiFolder, ')  matching ', roi_files[i])
    }
  }

  if( format == 'list'){
    return(roi_file_list)
  }
  if (format == 'image'){
    for(i in seq_len(length(roi_file_list))){
      for(ii in seq_len(length(roi_file_list[[i]]))){
        data_roi <- data_filtered %>%
          dplyr::filter(., roi == roi[i])
        meta_str <- paste0('time (hr): ', data_roi$time_hr[1], '\n temperature: ', data_roi$temperature[1], '\n pressure: ', data_roi$pressure[1], '\n salinity: ', data_roi$salinity[1], '\n')
        pp <-  magick::image_read(roi_file_list[[i]][ii]) %>%
          magick::image_scale(geometry = 'x300')

        print(pp)
        #print metadata
        cat(paste0(roi_files[i], '\n'))
        cat( paste0('time (hr): ', data_roi$time_hr[1], '\n temperature: ', data_roi$temperature[1], '\n pressure: ', data_roi$pressure[1], '\n salinity: ', data_roi$salinity[1], '\n'))

      }
    }
  }




  #
}

vpr_img_copy <- function(auto_id_folder, categories.of.interest, day, hour){
  #' Image copying function for specific category of interest
  #'
  #' This function can be used to copy images from a particular category, day
  #' and hour into distinct folders within the auto id directory
  #' This is useful for visualizing the ROIs of a particular classification
  #' group or for performing manual tertiary checks to remove
  #' images not matching classification group descriptions.
  #'
  #'
  #'
  #' @param auto_id_folder eg "D:/VP_data/IML2018051/autoid"
  #' @param categories.of.interest eg. categories.of.interest <- c('Calanus')
  #' @param day character, day of interest
  #' @param hour character, hour of interest
  #'
  #'

  #This code extracts ROIs from the VPR cast folder into folders corresponding to their autoID (from Visual Plankton)


  #modified for pulling reclassigfied images
  folder_names <- list.files(auto_id_folder)


  folder_names <- folder_names[folder_names %in% categories.of.interest]



  day_hour <- paste0('d', day, '.h', hour)
  aid_day_hour <- paste0('aid.', day_hour)


  for (i in folder_names) {

    #Get name of folder containing .txt files with roi paths within a category

    dir_roi <- file.path(auto_id_folder, i, "aid", fsep = "\\")

    #Get names of text files
    txt_roi <- list.files(dir_roi)

    subtxt <- grep(txt_roi, pattern = aid_day_hour, value = TRUE)
    txt_roi <- subtxt

    for(ii in txt_roi) {

      withr::with_dir(dir_roi, code = {

      roi_path_str <- read.table(ii, stringsAsFactors = FALSE)

      path_parts <- stringr::str_split(auto_id_folder, pattern = '/')


      auto_ind <- grep(path_parts[[1]], pattern = 'autoid')

      base_path_parts <- path_parts[[1]][-auto_ind]

      basepath <- stringr::str_c(base_path_parts,  collapse = '\\')

      auto_path <- stringr::str_c(path_parts[[1]], collapse = '\\')

      tt<- stringr::str_locate(string = roi_path_str$V1[1], pattern = 'rois')
      sub_roi_path <- substr(roi_path_str$V1, tt[1], nchar(roi_path_str))
      new_roi_path <- file.path(basepath, sub_roi_path, fsep = '\\')

      #Create a new folder for autoid rois
      roi_folder <- file.path(auto_path, i, paste0(ii, "_ROIS"), fsep = "\\")
      command1 <- paste('mkdir', roi_folder, sep = " ")
      shell(command1)

      #Copy rois to this directory
      for (iii in seq_len(length(new_roi_path))) {

        dir_tmp <- as.character(new_roi_path[iii])
        command2 <- paste("copy", dir_tmp, roi_folder, sep = " ")
        shell(command2)

        print(paste(iii, '/', length(new_roi_path),' completed!'))
      }
      }) # close dir
      }


    print(paste(i, 'completed!'))
  }

  print(paste('Day ', day, ', Hour ', hour, 'completed!'))
  # }

}

vpr_img_check <- function(folder_dir, basepath){

  #' Remove ROI strings from aid and aidmeas files based on a manually organized folder of images
  #'
  #' Should be used after \code{\link{vpr_img_copy}}, and manual image removal from created folders
  #'
  #'
  #'
  #' @param folder_dir directory path to day hour folders containing manually
  #'   reorganized images of a specific category eg.
  #'   'C:/data/cruise_IML2018051/krill/images/' where that folder contains
  #'   '......d123.h01/' which contains manually sorted images of krill
  #' @param basepath directory path to original Visual Plankton files, specified
  #'   down to the classification group. eg.
  #'   'C:/data/cruise_IML2018051/autoid/krill'
  #'
  #'
# this function can be used to edit aid and aidmeas files based on the images contained in a folder
  #useful if images were reorganized into classifciation groups manually in file explorer and then
  #user wants to translate this reorganization into data files


##part two of krill check to remove erroneus images

#after having copied krill images into folder using get_autoid_mod.R

#then manually removing any images which were not Krill

#once this is run, processing and plotting can be done

#E. Chisholm Sept 2019

  #if not supplied, assume folders are the same
  if(missing(basepath)){basepath <- folder_dir}

stfolders <- list.files(folder_dir, full.names = TRUE)

for (i in seq_len(length(stfolders))){ #for each day/hour loop

  krill_ver <- list.files(stfolders[i])


  #find matching original files
  #dh_ind <- stringr::str_locate(stfolders[i], pattern = 'aid.d')

  #dayhr <- substr(stfolders[i], dh_ind[2], nchar(stfolders[i])-5 )

  day <- vpr_day(stfolders[i])
  hour <- vpr_hour(stfolders[i])

  dayhr <- paste0('d', day, '.h', hour)

  #aid and aidmea
  aid_fns <- list.files(paste0(basepath, '/aid'), pattern = dayhr, full.names = TRUE)

  aidmeas_fns <- list.files(paste0(basepath, '/aidmea'), pattern =  dayhr, full.names = TRUE)

  #get indexes of aids which are not matched in krill_check and remove

  #read in aid file

  aid <- read.table(aid_fns ,stringsAsFactors = FALSE)

  #get roi substring
  #aid_old_gen <- substr(aid$V1, nchar(aid$V1) - 17, nchar(aid$V1))
  #aid_old_gen <- trimws(aid_old_gen, which = 'both')

  aid_old_gen <- vpr_roi(aid$V1)

  #find index of images which are in maually verified folder

  ver_ind <- aid_old_gen %in% krill_ver


  ver_ind_unl <- grep(ver_ind, pattern = 'FALSE')

  #remove any ROI number which do not match verified index
  aid_new <- aid$V1[ -ver_ind_unl]



  #read in aidmea file
  aidMea_old <- read.table(aidmeas_fns)

  #use same verified index to subset aidmea file
  aidMea_new <- aidMea_old[-ver_ind_unl, ]


  #check that files match

  if( length(aidMea_new$V1) != length(aid_new)){
    stop('roi and size files do not match!')
  }

  #print new aid and aidmea files

  write.table(file = aidmeas_fns, aidMea_new, sep = "    ", quote = FALSE, col.names = FALSE, row.names = FALSE)

  write.table(file = aid_fns, aid_new, quote = FALSE, col.names = FALSE, row.names = FALSE)

  cat(' \n')
  cat(aidmeas_fns, 'updated! \n')
  cat(aid_fns, 'updated! \n')
  cat('\n')

}
}

