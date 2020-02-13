
#functions for data analysis and visualization
#E Chisholm
#May 2019

#####PROCESSING FUNCTIONS####
#source('r_project_data_vis/get_vpr_summary_functions.R')


#Get time and number of rois from ROI id
##!!! use version of function in get_vpr_summary_functions_taxa from RK
#' get_roi_data <- function(x) {
#'   
#'   #' Get time and number of ROIs from IDs
#'   #' 
#'   #' 
#'   #' @details pulls time stamp and amount of rois from character string data
#'   #'   using substr(), outputs a new data frame
#'   #' 
#'   #' @param x character vector with roi Id
#'   #' 
#'   #' @export
#'   #' 
#'   #' 
#'   
#'   roi_x <- substr(x, 1, 8)
#'   
#'   
#'   roi_df <- data.frame(roi = as.numeric(roi_x))
#'   
#'   
#'   roi_df2 <- roi_df %>%
#'     dplyr::group_by(., roi) %>%
#'     dplyr::summarise(., n_roi = n())
#'   
#'   data.frame(roi_df2)
#'   
#' }

##subset CTD data to trim of bad data at start of cast
#note: use this function before aligning CTD and ROi data to avoid mismatch

trim_ctd <- function(ctd, trimLength, end = F, endTrim = NULL){
  #' subset CTD data to remove bad data at start of cast
  #' @author E. Chisholm
  #'
  #' @details Use this function to remove any anomolous data collected while CTd
  #' was soaking or on deck before cast began Not if aligning ctd and ROI data
  #' ensure that ctd data is trimmed beofre aligning ri data to avoid mismatch
  #' Note that this method only works for towed or moored ctd data where time is
  #' continuously sampled not profile CTD data without time recording. Currently
  #' requires you to be consious of time sampling intervals and choose a trim
  #' length which is a multiple of that so function is able to match time
  #' values.
  #'
  #' @param ctd  oce CTD object
  #' @param trimLength index of cut off point for trimming
  #' @param end logical value (default FALSE) indicating whether to trim end of cast
  #' @param endTrim defaults to NULL, supply number of values for trimming end of cast if end == TRUE
  #' eg, endTrim = 5 will remove the last 5 measurmeents off the cast
  #'
  #'   
  #'   
  
  # time <- ctd[['time_ms']][ctd[['time_ms']] == ctd[['time_ms']][1] + trimLength]
  # ml <- match(time, ctd[['time_ms']], nomatch = 0)
  # if (ml == 0){
  #   warnings(print('Time values failed to match, please check that trim length is a multiple of sampling interval!'))
  #   stop()
  # }
  # trimLength <- length(ctd[['time_ms']][1:ml])
  # 
  
  data_names <- names(ctd@data)
  for (dn in data_names){
    ctd <- oceSetData(object = ctd, name = dn, value =  ctd@data[[dn]][trimLength: length(ctd@data[[dn]])])
  }
  
  if(end == T){
    
    for (dn in data_names){
      tend <- length(ctd@data[[dn]]) - endTrim
      ctd <- oceSetData(object = ctd, name = dn, value = ctd@data[[dn]][1: tend])
    }
  }
  
  return(ctd)
}


px_to_mm <- function(x) {
  #'get converstion factor for pixels to mm for roi measurements
  #'
  #' @details converts pixels to mm using 24mm = 1024 pixels factor
  #' 
  #' @param x pixel data to be converted to mm
  #' 
  #' @export
  #' 
  #' @note EC: (to do) can be simplified
  
  mm_px <- 24/1024 #mm/pixel
  mm2_px2 <- (mm_px)^2 
  
  x[, c(1, 3:7)] <- x[, c(1, 3:7)]*mm_px
  x[,2] <- x[,2]*mm2_px2
  
  return(x)
  
}


#Write function to read in CTD data (SBE49) and Fluorometer data from CTD-VPR package
read.ctdvpr.data <- function(x) {
  #'Read CTD data (SBE49) and Fluorometer data from CTD- VPR package
  #'Text file format .dat file
  #'Outputs ctd dataframe with variables time_ms, conductivity, temperature, 
  #'pressure, salinity, fluor_ref, fluorescence_mv, turbidity_ref, 
  #'turbidity_mv, altitude_NA
  #' @author K. Sorochan, R. Klaver, E. Chisholm
  #'
  #'
  #'
  #'@param x full filename (ctd .dat file)
  #'
  
  
  data <- read.table(textConnection(gsub(":", ",", readLines(x))), sep = ",")
  time <- data[,1]
  time <- as.numeric(gsub("[^[:digit:]]", "", time))
  
  
  data2 <- cbind(time, data[,-c(1)])
  colnames(data2) <- c("time_ms", "conductivity", "temperature", "pressure", "salinity", "fluor_ref", "fluorescence_mv", 
                       "turbidity_ref", "turbidity_mv", "altitude_NA")
  data2 <- data2[!duplicated(data2), ]
  data2
}





#normalize function

normalize <- function(mat){
  #' scale matrix between 0-1
  #' take each element of matrix dived by column total
  #' Make sure to remove total rows before using
  #' @param mat a matrix to normalize
  
  nm <- matrix(nrow = dim(mat)[1], ncol = dim(mat)[2])
  for(i in 1:length(nm[,1])){
    for (j in 1:length(nm[1,])){
      nm[i,j] <- mat[i,j]/colSums(mat)[j]
    }
  }
  return(nm)
  
}




#=======================#
get_trrois_size <- function(directory, taxa){
  
  #' get size data from idsize fdiles produced when training classifier
  #' useful for getting size distribution of known rois from each taxa
  #'
  #'
  #'@param directory cruise directory eg. 'C:/data/IML2018051/'
  #'@param taxa list of character elements containing taxa of interest
  #'
  #'
  #'
  
  
  
  #loop for each taxa of interest
  for (t in taxa){
    #check
    # g <- grep(taxa_names, pattern = t)
    # if (length(g) == 0){
    #   stop(paste('Taxa of interest, ', t, 'not found in data provided!'))
    # }
    # 
    size_file <- list.files(path = paste0(directory,'/idsize'), pattern = paste0('mea.', t))
    #roi_file <- list.files(path = paste0(directory,'/idsize'), pattern = paste0('hid.v0.',t))
    
    #Get info
    #roi_ID <- read.table(paste0(directory,'/idsize/', roi_file), stringsAsFactors = F)
    auto_measure_px <- read.table(paste0(directory, '/idsize/', size_file), stringsAsFactors = F, col.names = c('Perimeter','Area','width1','width2','width3','short_axis_length','long_axis_length'))
    
    eval(parse(text = paste0('auto_measure_', t,'_mm <- px_to_mm(auto_measure_px)'))) #Convert to mm
    
    
  }
  
  eval(parse(text = paste0('return(auto_measure_', t,'_mm)')))
}









##Kevin's method modified by EC
bin_average_vpr_EC <- function(data, binSize = 1){
  
  #' get bin averages for VPR and CTD data
  #' @author E. Chisholm, K. Sorochan
  #' June 18 2019
  #' 
  #' used after getCast function on a single ascending or descending section
  #' 
  #' @param data ctd data frame object including scan, salinity, temperature,
  #'   pressure, conductivity, time, fluor_ref, turbidity_ref, turbidity_mv,
  #'   altitude, cast_id, n_roi
  #'   @param binSize the height of bins over which to average, default is 1 metre
  #'   
  #'   
  #'   
  
  
  p <- data$pressure
  max_pressure <- max(p)
  min_pressure <- min(p)
  x_breaks <- seq(from = floor(min_pressure), to = ceiling(max_pressure), by = binSize) 
  
  #Get variables of interest using oce bin functions
  min_time_s <- binApply1D(p, data$time/1000, xbreaks = x_breaks, min)$result
  max_time_s <- binApply1D(p, data$time/1000, xbreaks = x_breaks, max)$result
  min_pressure <- binApply1D(p, data$pressure, xbreaks = x_breaks, min)$result
  max_pressure <- binApply1D(p, data$pressure, xbreaks = x_breaks, max)$result
  n_roi_bin <- binApply1D(p, data$n_roi, xbreaks = x_breaks, sum)$result
  temperature <- binApply1D(p, data$temperature, xbreaks = x_breaks, mean)$result
  salinity <- binApply1D(p, data$salinity, xbreaks = x_breaks, mean)$result
  density <- binApply1D(p, data$sigmaT, xbreaks = x_breaks, mean)$result
  fluorescence <- binApply1D(p, data$fluorescence_mv, xbreaks = x_breaks, mean)$result
  turbidity <- binApply1D(p, data$turbidity_mv, xbreaks = x_breaks, mean)$result
  avg_time_h <- binApply1D(p, data$time/(1000*3600), xbreaks = x_breaks, mean)$result
  pressure <- binApply1D(p, data$temperature, xbreaks = x_breaks, mean)$xmids #Could be any of the variables computed, but I just went with min_time
  
  if (!(length(pressure) == length(salinity))) {
    
    salinity_mean <- binMean1D(p, data$salinity, xbreaks = x_breaks)$result
    
    idx_rm <- which(is.na(salinity_mean))
    pressure <- pressure[-idx_rm]
    
  }
  
  #Get derived variables
  time_diff_s <- max_time_s - min_time_s
  conc_m3 <- n_roi_bin/((0.000108155)*(13)*(time_diff_s)) #Assumes cubic image volume and 13 fps image aquisition
    #image volume updated with results from seascan (6.6 cubic inch = 0.000108155m^3)
  pressure_diff <- max_pressure - min_pressure
  
  #Output is data frame
  data.frame(pressure, min_pressure, max_pressure, pressure_diff, min_time_s, max_time_s, time_diff_s, 
             n_roi_bin, conc_m3,
             temperature, salinity, density, fluorescence, turbidity, avg_time_h)
  
}


getCast_EC <- function(data, cast_direction, data_type) {

  #' isolate ascending or descending section of ctd cast
  #' required for binning data
  #' 
  #' @param data an oce ctd object 
  #' @param cast_direction 'ascendcing' or 'descending' depending on desired section
  #' @param data_type specify 'oce' or 'df' depending on class of desired output
  #' 
  #' can output either data frame or oce ctd object, input must be oce object
  #' Updated from original K. Sorochan / R. Klaver function by E. Chisholm
  #' 
  #' 
  #' NOTE: ctdFindProfiles arguments for minLength and cutOff were updated to
  #' prevent losing data (EC 2019/07/23)
  
  cast_updated <- list()
  cast <- ctdFindProfiles(data, direction = cast_direction, minLength = 0, cutoff = 0.1)
  
  for(i in 1:length(cast)) {
    
    data <- cast[[i]]
    
    n_obs <- length(data@data$pressure)
    cast_number <- i
    cast_id <- paste(cast_direction, i, sep = "_")
    cast_id_vec <- rep(cast_id, n_obs)
    
    cast_updated[[i]] <- oceSetData(data, "cast_id", cast_id_vec, "no_unit")
    
  }
  
  if(data_type == "oce") {
    
    cast_updated
    
  }
  
  if(data_type == "df") {
    
    getDf <- function(x) {
      
      data.frame(x@data, stringsAsFactors = F)
      
    }
    
    lapply(cast_updated, getDf)
    
  }
  
}



bin_profile_taxa <- function(data, taxa){
  
  #' bin vpr data for a specific taxa
  #' 
  #' 
  #' @param data a dataframe object with rows, pressure, time_ms, and number of roi data for taxa specified
  #' @param taxa the taxa for which you want to bin data
  #' 
  #' bins data based on KS/RK method
  #' bin size set to 1m
  
  x_breaks <- seq(from = floor(min(data$pressure)), to = ceiling(max(data$pressure)), by = 1) 
  min_time_s <- binApply1D(data$pressure, data$time_ms/1000, xbreaks = x_breaks, min)$result
  max_time_s <- binApply1D(data$pressure, data$time_ms/1000, xbreaks = x_breaks, max)$result
  time_diff_s <- max_time_s - min_time_s
  n_roi_bin <- binApply1D(data$pressure, data[[taxa]], xbreaks = x_breaks, sum)$result
  min_pressure <- binApply1D(data$pressure, data$pressure, xbreaks = x_breaks, min)$result
  max_pressure <- binApply1D(data$pressure, data$pressure, xbreaks = x_breaks, max)$result
  #bin temp and salinity
  temperature <- binApply1D(data$pressure, data$temperature, xbreaks = x_breaks, mean)$result
  salinity <- binApply1D(data$pressure, data$salinity, xbreaks = x_breaks, mean)$result
  

  df <- data.frame(time_diff_s, min_pressure, max_pressure, n_roi_bin, temperature, salinity)
return (df)
    
}




binQC <- function(data){
  
  #' remove bins where less than 0.8 seconds was spent
  #' removes spikes in binned data
  #' based on  analysis done by R. Klaver
  #' 
  #' @param data binned data (must include 'time_diff_s' variable)
  #' 
  
  data %>%
    dplyr::filter(., time_diff_s >= 0.8)
  
}




get_dayhour <- function(stations){
  
  #' Find day/ hour info to match each station of interest for processing
  #' Relies on station names list (csv file) -- CAUTION full path is specified for csv
  #' will require update if file is moved
  #' 
  #'  @author E. Chisholm
  #' 
  #' @param stations a list of character values naming stations of interest
  #' 
  #' 
  #' @return List of day and hour values which make up stations of interest
  #' @example 
  #' #define stations
  #' stations <- c('7.6', '7.45', '6.6')
  #' get_dayhour(stations)
  #' 
#DEFINE BY STATION

#####DEFINE FOR MULTIPLE STATIONS 
stations_of_interest <- stations

#USE STATION LIST WITH CORRESPONDING DAY AND HOUR TO MATCH AREA OF INTEREST
#match hour and day to station
station_list <- read.csv('C:/VPR_PROJECT/vp_info/station_names_IML2018051.csv', stringsAsFactors = FALSE)

#get day and hour for each station
if (length(stations_of_interest) > 1){
  day <- list()
  hour <- list()
  
  for (i in 1:length(stations_of_interest)){
    station <- stations_of_interest[[i]]
    
    ind <- grep(station, station_list$station)
    
    day[[i]] <- station_list$day[ind]
    hour[[i]] <- station_list$hour[ind]
    
  }
  
  day <- unlist(day)
  hour <- sprintf('%02d',unlist(hour))
  
}else{
  #get day and hour if there is only one station
  station <- stations_of_interest
  ind <- grep(station, station_list$station)
  day <- station_list$day[ind]
  hour <- station_list$hour[ind]
  
}

list(day = day, hour = hour)

}



list_ctd_files <- function(castdir,cruise, day, hour){
  
  
  #' Create a list of ctd files to be read
  #' Searches through typical VP directory structure
  #' Use with caution
  #' 
  #' 
  #' @param castdir root directory for ctd cast files
  #' @param cruise cruise name (exactly as in directory structure)
  #' @param day list of days of interest
  #' @param hour list of hours of interest
  #' @author E. Chisholm
  #' 
  #' @return list of ctd file paths matching days and hours provided
  #' 
  #' @example 
  #' #get day hour for stations
  #' stations <- c('7.6', '7.45', '6.6')
  #' dayhour <- get_dayhour(stations)
  #' #specify directory
  #' castdir <- 'E:/VPR_full_casts/'
  #' #get ctd files
  #' list_ctd_files(castdir, cruise, dayhour$day, dayhour$hour)
  #' 
  vpr_cast_folders <- list.files(castdir, pattern = '')
  
  #find right vpr cast -- subset by tow number #
  #folder <- grep(vpr_cast_folders, pattern = paste0('AD_', vprnum,'.VPR.', cruise,'*'), value = T)
  
  #not subset by tow number
  folder <- grep(vpr_cast_folders, pattern = paste0('.VPR.', cruise,'*'), value = T)
  
  if (length(folder) == 0){stop("No CTD files found!")}
  folder_path <- paste0(castdir, folder)
  
  
  #grab all days
  full_path <- list.files(folder_path, full.names = TRUE)
  
  
  day_folder <- list()
  for (i in 1:length(full_path)){
    day_folder[[i]] <- substr(full_path[[i]], start = (nchar(full_path[[i]]) - 2), stop =  nchar(full_path[[i]]) )
  }
  
  #subset by station/ day
  day_sub <- day_folder %in% day
  
  full_path <- full_path[day_sub == TRUE]
  
  #extract for only specific days 
  ctd_files <- list.files(full_path, pattern = '*ctd*', full.names = TRUE)
  
  
  #find desired hour
  if(length(hour) == 1){
    ctd_file <- grep(ctd_files, pattern = hour, value = T)
    loop <- FALSE
  }else{
    loop <- TRUE
    #check only pulling right hours
    hour_folder <- list()
    for(i in 1:length(ctd_files)){
      hour_folder[[i]] <- substr(ctd_files[[i]], start = (nchar(ctd_files[[i]]) -8), stop = (nchar(ctd_files[[i]]) - 7))
    }
    hour_folder <- unlist(hour_folder)
    hour_sub <- hour_folder %in% hour
    
    
    ctd_files <- unique(ctd_files[hour_sub == TRUE]) #get rid of duplicate pulls
    
    
  }
  
  return(ctd_files)
  #results in output of listed CTD files, subset by day/hour/station as chosen
  #with full file path described in ctd_files, ready to be read
  #loop variable indicates multiple CTD files to be read in (T/F)
}


format_oce <- function(dat){
  
  #' Formats list of ctd data into oce ctd objects
  #' Input: list of ctd data frames (including -- salinity, temperature, pressure, conductivity, time_ms)
  #' plus any other variables
  #' 
  #' Output: oce format ctd objects (in list form)
  #' 
  #' @author E. Chisholm
  #' 
  #' @param dat list of ctd data frames
  #' 
  #' @example 
  #' #READ CTD DATA
  #' ctd_dat <- read.ctdvpr.data(ctd_file)
  #' #CHANGE TO OCE FORMAT
  #' ctd <- format_oce(ctd_dat)
  #' 
  require(oce)
  
  if(length(names(dat)) > 1){loop = FALSE}else{loop = TRUE}
  
  #make oce ctd object
  if (loop == TRUE){
    #IF for multiple hours at a time, creates CTD list
    ctd <- list()
   
    for(i in 1:length(dat)){
      ctd[[i]] <- as.ctd(salinity = dat[[i]]$salinity, temperature = dat[[i]]$temperature, pressure = dat[[i]]$pressure, conductivity = dat[[i]]$conductivity, time = dat[[i]]$time_ms)
      otherVars <- names(dat[[i]])[!(names(dat[[i]]) %in% c('salinity', 'temperature', 'pressure', 'conductivity', 'time_ms'))]
      
      for ( o in otherVars){
        eval(parse(text = paste0("ctd[[i]] <- oceSetData(ctd[[i]], name = '",o,"', value = dat[[i]]$",o,")")))
      }
      
    }
  }else{
    
    ctd <- as.ctd(salinity = dat$salinity, temperature = dat$temperature, pressure = dat$pressure, conductivity = dat$conductivity, time = dat$time_ms)
    otherVars <- names(dat)[!(names(dat) %in% c('salinity', 'temperature', 'pressure', 'conductivity', 'time_ms'))]
    
    for ( o in otherVars){
      eval(parse(text = paste0("ctd <- oceSetData(ctd, name = '",o,"', value = dat$",o,")")))
    }
  }
  
  return(ctd)
}


calc_roiL <- function(data, taxa){
  #' calculate binned roi L-1 concentrion for a specific taxa
  #' @author E. Chisholm
  #' @param data data frame to be run throughbin_profile_taxa
  #' @param taxa taxa of interest
  #' 
  #' 
  #' 
  

  bin <- bin_profile_taxa(data, taxa)
  bin <- binQC(bin)
  (bin$n_roi_bin/((0.108155)*(13)*(bin$time_diff_s)))
}



getRoiMeasurements <- function(taxafolder, nchar_folder, unit = 'mm') {
  
  #' pull roi measurements from all taxa, all files
  #' 
  #' @param taxafolder path to taxa folder (base -- autoid folder)
  #' @param nchar_folder number of characters in basepath
  #' @param unit unit data will be output in, 'mm' (default -- millimetres) or 'px' (pixels)
  
  
  auto_measure_mm_ls <- list()
  auto_measure_mm_alltaxa_ls <- list()
  
  for (i in 1:length(taxafolder)) {
    
    sizefiles <- list.files(paste(taxafolder[i],'aidmea',sep='\\'), full.names = T)
    roifiles <- list.files(paste(taxafolder[i],'aid',sep='\\'), full.names=T)
    
    for(j in 1:length(sizefiles)) {
      
      sizefile <- sizefiles[j]
      roifile <- roifiles[j]
      
      #Get info
      roi_ID <- read.table(roifile, stringsAsFactors = F)
      auto_measure_px <- read.table(sizefile, stringsAsFactors = F, col.names = c('Perimeter','Area','width1','width2','width3','short_axis_length','long_axis_length'))
      
      if (unit == 'mm'){
      auto_measure_mm <- px_to_mm(auto_measure_px) #Convert to mm
      }else{
        auto_measure_mm <- auto_measure_px
      }
      
      auto_measure_mm$roi_ID <- (roi_ID$V1) #Get roi ids
      #!! WARNING HARD CODING !!#
      auto_measure_mm$roi_ID <- substr(auto_measure_mm$roi_ID, nchar(auto_measure_mm$roi_ID)-13, nchar(auto_measure_mm$roi_ID)-4) #Remove path information for rois
      
      taxa <- substr(taxafolder[i], nchar_folder + 2, nchar(taxafolder[i])) #Get taxa label
      auto_measure_mm$taxa <- rep(taxa, nrow(auto_measure_mm)) #add taxa to dataset
      
      day_hour <- substr(sizefile, nchar(sizefile) - 7, nchar(sizefile))
      auto_measure_mm$day_hour <- rep(day_hour, nrow(auto_measure_mm))
      #saveRDS(auto_measure_mm, paste(taxafolder, "/", "measurements_mm_", taxa[i], ".RDS", sep=""))
      
      auto_measure_mm_ls[[j]] <- auto_measure_mm
      
    }
    
    auto_measure_mm_alltaxa_ls[[i]] <- do.call(rbind, auto_measure_mm_ls)
    
  }
  
  auto_measure_mm_alltaxa_df <- do.call(rbind, auto_measure_mm_alltaxa_ls)
  auto_measure_mm_alltaxa_df
  
}



conc_byTaxa <- function(data, taxa){
  
  #' get depth binned concentrations for specific taxa
  #' 
  #' 
  #' @param data dataframe produced by processing
  #' @param taxa name of taxa isolated
  #' 
  #' 
  #' @author E. Chisholm
  #' 
  #' 
  
  
  dt <- data %>%
    dplyr::select(., names(data)[1:10], names(data)[25:28], taxa, sigmaT)
  
  
 names(dt)<-  gsub(names(dt), pattern = taxa, replacement = 'n_roi')
  
  ctd_roi_oce <- as.ctd(dt)
  otherVars<-  c('time_ms', 'fluorescence_mv', 'turbidity_mv', 'n_roi', 'sigmaT')
  for ( o in otherVars){
    eval(parse(text = paste0("ctd_roi_oce <- oceSetData(ctd_roi_oce, name = '",o,"', value = dt$",o,")")))
  }
  
  #find upcasts
  upcast <- getCast_EC(data = ctd_roi_oce, cast_direction = 'ascending', data_type = 'df')
  upcast2 <- lapply(upcast, bin_average_vpr_EC)
  upcast_df <- do.call(rbind, upcast2)
  #find downcasts
  downcast <- getCast_EC(ctd_roi_oce, "descending", "df")
  downcast2 <- lapply(downcast, bin_average_vpr_EC)
  downcast_df <- do.call(rbind, downcast2)
  #combine_data
  vpr_depth_bin <- rbind(upcast_df, downcast_df)
  vpr_depth_bin <- data.frame(vpr_depth_bin)
  
  #remove outliers from concentration 
  #(spikes when short time is spent in one depth bin)
  
  vpr_depth_bin %>%
    dplyr::mutate(., avg_time_h = avg_time_h - min(avg_time_h)) %>%
    dplyr::filter(., time_diff_s >= 0.8)
  
}



dataSummary <- function(all_dat, fn, station, event, sal_range, temp_range, pres_range){
  
  sink(fn)
  
  cat('Data Summary Report \n')
  cat('Report processed:', as.character(Sys.time()), '\n')
  cat('Cast: ', tow, '   Day: ', day, '   Hour: ', hour, '\n')
  cat('Station: ', station, '\n')
  cat('Event: ', event, '\n')
  cat('\n')
  cat('\n')
  cat(' >>>>  Time \n')
  cat('Data points: ', length(all_dat$time_ms),'\n')
  cat('Range: ', min(all_dat$time_ms),' - ', max(all_dat$time_ms), ' (ms) \n')
  cat('Range: ', min(all_dat$avg_hr),' - ', max(all_dat$avg_hr), ' (hr) \n')
  cat('\n')
  cat('\n')
  cat(' >>>>  Conductivity \n')
  cat('Data points: ', length(all_dat$conductivity),'\n')
  cat('Range: ', min(all_dat$conductivity),' - ', max(all_dat$conductivity), '  \n')
  cat('\n')
  cat('\n')
  cat(' >>>>  Temperature \n')
  cat('Data points: ', length(all_dat$temperature),'\n')
  cat('Range: ', min(all_dat$temperature),' - ', max(all_dat$temperature), ' (c) \n')
  cat('QC: ', length(all_dat$temperature[all_dat$temperature < min(temp_range) ]), 'points below ', min(temp_range), 'deg c \n')
  cat('QC: ', length(all_dat$temperature[all_dat$temperature > max(temp_range)]), 'points above ', max(temp_range), 'deg c \n')
  cat('\n')
  cat('\n')
  cat(' >>>>  Pressure \n')
  cat('Data points: ', length(all_dat$pressure),'\n')
  cat('Range: ', min(all_dat$pressure),' - ', max(all_dat$pressure), ' (db) \n')
  cat('QC: ', length(all_dat$pressure[all_dat$pressure < 0 ]), 'below zero db \n')
  cat('\n')
  cat('\n')
  cat(' >>>>  Salinity \n')
  cat('Data points: ', length(all_dat$salinity),'\n')
  cat('Range: ', min(all_dat$salinity),' - ', max(all_dat$salinity), ' (PSU) \n')
  cat('QC: ', length(all_dat$salinity[all_dat$salinity < min(sal_range) ]), 'points below ', min(sal_range), 'PSU \n')
  cat('QC: ', length(all_dat$salinity[all_dat$salinity > max(sal_range) ]), 'points above ', max(sal_range), 'PSU \n')
  cat('\n')
  cat('\n')
  cat(' >>>>  Fluorescence \n')
  cat('Data points: ', length(all_dat$fluorescence_mv),'\n')
  cat('Range: ', min(all_dat$fluorescence_mv),' - ', max(all_dat$fluorescence_mv), ' (mv) \n')
  cat('\n')
  cat('\n')
  cat(' >>>>  Turbidity \n')
  cat('Data points: ', length(all_dat$turbidity_mv),'\n')
  cat('Range: ', min(all_dat$turbidity_mv),' - ', max(all_dat$turbidity_mv), ' (mv) \n')
  cat('\n')
  cat('\n')
  cat(' >>>>  ROI count \n')
  cat('Data points: ', length(all_dat$n_roi),'\n')
  cat('Range: ', min(all_dat$n_roi),' - ', max(all_dat$n_roi), ' (counts) \n')
  cat('\n')
  cat('\n')
  cat(' >>>>  Sigma T \n')
  cat('Data points: ', length(all_dat$sigmaT),'\n')
  cat('Range: ', min(all_dat$sigmaT),' - ', max(all_dat$sigmaT), '  \n')
  cat('QC: ', length(all_dat$sigmaT[all_dat$sigmaT < 22 ]), 'points below twenty-two  \n')
  cat('QC: ', length(all_dat$sigmaT[all_dat$sigmaT > 28 ]), 'points above twenty-eight  \n')
  
  sink()
  
}
#####PLOTTING FUNCTIONS#####

#balloon plot with isopycnals final

#create TS data frame
get_isopycnals<- function(sal, pot.temp, reference.p = 0){
  #' get vector to draw isopycnal lines on TS plot
  #' Used internally to create TS plots
  #' @author E. Chisholm
  #'  
  #'    @param sal salinity vector
  #' @param pot.temp temperature vector in deg C
  #' @param reference.p reference pressure for calculation, set to 0
  #' 
  #' 
  #' modified from source:https://github.com/Davidatlarge/ggTS/blob/master/ggTS_DK.R
  
  require(gsw)
  TS <- expand.grid(
    sal = seq(floor(min(sal, na.rm = TRUE)), ceiling(max(sal, na.rm = TRUE)), length.out = 100),
    pot.temp = seq(floor(min(pot.temp, na.rm = TRUE)), ceiling(max(pot.temp, na.rm = TRUE)), length.out = 100)
  )
  TS$density <- gsw_rho_t_exact(SA = TS$sal, t = TS$pot.temp, p = reference.p) - 1000 # the function calculates in-situ density, but because potential temperature and a single reference pressure is used the result equals potential density at reference pressure
  
  # isopycnal labels 
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
  
  # +- vertical isopycnals
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
  
  
  return(TS)
}

plotTS_balloon <- function(x, reference.p = 0, var){
  
  #' Make a balloon plot against a TS plot with ROI concentration and sort by taxa
  #' includes isopycnal line calculations
  #' @author E. Chisholm
  #' 
  #' @param x dataframe with temperature, salinity, number of rois (n_roi_bin)
  #' @param reference.p reference pressure (default at 0 for surface)- used to calculate ispycnals
  #' @param var variable on which of points will be based, eg conc_m3 or n_roi_bin
  #' @example 
  #' p <- plotTS_balloon(x)
  #' p + ggtitle('Concentration by taxa')
  #'
  #'  @export
  #' 
  #' @note modified from source: https://github.com/Davidatlarge/ggTS/blob/master/ggTS_DK.R
  #' 
  #' 
  
  require(ggplot2)
  require(gsw)
  
  
  #get isopycnal lines
  sal <-  x$salinity
  pot.temp <-  x$temperature
  
  # make TS long table
  TS <- expand.grid(
    sal = seq(floor(min(sal, na.rm = TRUE)), ceiling(max(sal, na.rm = TRUE)), length.out = 100),
    pot.temp = seq(floor(min(pot.temp, na.rm = TRUE)), ceiling(max(pot.temp, na.rm = TRUE)), length.out = 100)
  )
  TS$density <- gsw_rho_t_exact(SA = TS$sal, t = TS$pot.temp, p = reference.p) - 1000 # the function calculates in-situ density, but because potential temperature and a single reference pressure is used the result equals potential density at reference pressure
  
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
  
  #plot
  eval(parse(text = paste0("p <- ggplot()+
                           #isopycnal lines
                           geom_contour(data = TS, aes(x = sal, y = pot.temp, z = density), col = 'grey', linetype = 'solid',
                           breaks = seq(min(round(TS$density*2)/2, na.rm = TRUE), # taking density times 2, rounding and dividing by 2 rounds it to the neares 0.5
                           max(round(TS$density*2)/2, na.rm = TRUE), 
                           by = .5)) +
                           #roi data sorted by number of rois and taxa
                           geom_point(data = x, aes(x = salinity, y = temperature, size = ", var, "), shape = 21) +
                           scale_size_area(max_size=10)+ #make balloons bigger
                           #label legends
                           labs(size = 'ROIs') +
                           labs(col = 'Taxa')+
                           #set x axis (ensure scaling to data)
                           scale_x_continuous(name = 'salinity', expand = c(0,0), 
                           limits = c(floor(min(x$salinity, na.rm = TRUE)), ceiling(max(x$salinity, na.rm = TRUE)))) + # use range of 'sal' for x axis
                           #set y axis (esure scaled to data)
                           scale_y_continuous(name = 'potential temperature [C]', 
                           limits = c(floor(min(x$temperature, na.rm = TRUE)), ceiling(max(x$temperature, na.rm = TRUE)))) +
                           #get rid of grid lines and text formatting
                           theme_classic() + theme(text = element_text(size=14)) "
  )))
  
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


plotTS_balloon_EC <- function(x, reference.p = 0){
  
  #' Make a balloon plot against a TS plot with ROI concentration and sort by taxa
  #' includes isopycnal line calculations
  #' 
  #' 
  #' WARNING HARD CODED FOR  5 TAXA, CALANUS, KRILL, ECHINODERM LARVAE, SMALL COPEPOD, CHAETOGNATHS
  #' 
  #' 
  #' @param x dataframe with temperature, salinity, number of rois named by taxa
  #' @param reference.p reference pressure (default at 0 for surface)- used to calculate ispycnals
  #' 
  #' @example 
  #' p <- plotTS_balloon(x)
  #' p + ggtitle('Concentration by taxa')
  #'
  #'  @export
  #' 
  #' @note modified from source: https://github.com/Davidatlarge/ggTS/blob/master/ggTS_DK.R
  #' 
  #' 
  
  require(ggplot2)
  require(gsw)
  
  
  #get isopycnal lines
  sal <-  x$salinity
  pot.temp <-  x$temperature
  
  # make TS long table
  TS <- expand.grid(
    sal = seq(floor(min(sal, na.rm = TRUE)), ceiling(max(sal, na.rm = TRUE)), length.out = 100),
    pot.temp = seq(floor(min(pot.temp, na.rm = TRUE)), ceiling(max(pot.temp, na.rm = TRUE)), length.out = 100)
  )
  TS$density <- gsw_rho_t_exact(SA = TS$sal, t = TS$pot.temp, p = reference.p) - 1000 # the function calculates in-situ density, but because potential temperature and a single reference pressure is used the result equals potential density at reference pressure
  
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
  
  #initialize taxa
  #WARNING HARD CODING, 5 TAXA
  cols <- c('t1' = 'darkorchid3', 't2' = 'deeppink3', 't3' = 'dodgerblue3', 't4' = 'tomato3', 't5' = 'gold3')
  taxas <- c('calanus', 'chaetognaths', 'small_copepod', 'krill', 'echinoderm_larvae')
  #plot
  p <- ggplot()+
    #isopycnal lines
    geom_contour(data = TS, aes(x = sal, y = pot.temp, z = density), col = "grey", linetype = "solid",
                 breaks = seq(min(round(TS$density*2)/2, na.rm = TRUE), # taking density times 2, rounding and dividing by 2 rounds it to the neares 0.5
                              max(round(TS$density*2)/2, na.rm = TRUE), 
                              by = .5)) +
    #roi data sorted by number of rois and taxa
    geom_point(data = x, aes(x = salinity, y = temperature, size = calanus, col = 't1' ), shape = 21) +
    #geom_point(data = x, aes(x = salinity, y = temperature, size = marine_snow), shape = 21) +
    #geom_point(data = x, aes(x = salinity, y = temperature, size = stick), shape = 21) +
    geom_point(data = x, aes(x = salinity, y = temperature, size = chaetognaths, col = 't2'), shape = 21) +
    geom_point(data = x, aes(x = salinity, y = temperature, size = small_copepod, col = 't3'), shape = 21) +
    geom_point(data = x, aes(x = salinity, y = temperature, size = krill, col = 't4'), shape = 21) +
    geom_point(data = x, aes(x = salinity, y = temperature, size = echinoderm_larvae, col = 't5'), shape = 21) +
    scale_colour_manual(name = 'Taxas', values = cols, guide = guide_legend(), labels = taxas) +
    scale_size_area(max_size=10)+ #make balloons bigger
    #label legends
    labs(size = 'Number of \n ROIs') +
    #labs(col = 'Taxa')+
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



vis_cm <- function(cm, classes, type, addLabels = T, threshold = NULL){
  #' Plots normalized confusion matrix
  #' @author E. Chisholm
  #'
  #' @param cm Confusion matrix (numeric)
  #' @param classes character list of classes present in confusion matrix
  #'   (ordered)
  #' @param type character value 'NN', 'SVM' or 'Dual', appended to 'Confusion
  #'   Matrix' to create title
  #' @param addLabels logical value whetehr to add percentage accuracy labels to
  #'   plot (defaults to TRUE)
  #' @param threshold numeric value which determines the minimum value of
  #'   frequency labelled on the plot on a normalized scale of 0-1 (useful for highliighting significant
  #'   disagreement)
  #'
  #' @output a visualization of the confusion matrix, normalized
  #'
  #' @export
  
  #check dimensions
  
  
  require(ggplot2)
  require(stringr)
  
  dimcm <- dim(cm)
  if (dimcm[1] != length(classes) +1){
    stop(' Incorrect dimensions, matrix does not match classes given!')
  }
  
  
  
  #remove total columns
  conf <- cm[1:dimcm[1]-1,1:dimcm[2]-1]
  #create matrix and normalize
  input.matrix.normalized <- data.matrix(normalize(conf))
  
  
  #add labels
  colnames(input.matrix.normalized) = classes
  rownames(input.matrix.normalized) = classes
  
  #build conf mat
  confusion <- as.data.frame(as.table(input.matrix.normalized))
  
  #basic plot
  plot <- ggplot(confusion) 
  
  #add data
  p <- plot + geom_tile(aes(x=Var1, y=Var2, fill=Freq)) +  #adds data fill
    theme(axis.text.x=element_text(angle=45, hjust=1)) + #fixes x axis labels
    scale_x_discrete(name="Actual Class") + #names x axis
    scale_y_discrete(name="Predicted Class") + #names y axis
    scale_fill_gradient(breaks=seq(from=-.5, to=4, by=.2)) + #creates colour scale
    labs(fill="Normalized\nFrequency") + #legend title
    #theme(legend.position = "none" ) +  #removes legend
    ggtitle(label = paste(type, 'Confusion Matrix')) 
  
  #accuracy labels along diagonal
  
  if (addLabels == T){
    #find diagonal values
    acc <- confusion$Freq[confusion$Var1 == confusion$Var2]
    #for each taxa
    for (i in 1:length(unique(confusion$Var1))){
      #add text label
      p <- p + annotate('text', x = i, y = i, #position on diagonal
                        #label with frequency as percent rounded to 2 digits
                        label = paste0(round(acc[i], digits = 2)*100, '%'), 
                        #text formatting
                        colour = 'white',
                        size = 3)
    }
  }
  
  #threshold labels
  
  if ( !is.null(threshold)){
    for (i in 1:length(confusion$Var1)){
      #if frequency is above threshold
      if (confusion$Freq[i] > threshold ){
        #find x and y locations to plot
        x <- grep(levels(confusion$Var1), pattern = as.character(confusion$Var1[i]) )
        y <- grep(levels(confusion$Var2), pattern = as.character(confusion$Var2[i]) )
        #not already labelled on diagonal
        if( x != y){
          #add text
          p <- p + annotate('text', x = x, y = y,
                            #label - frequency as percent, rounded
                            label = paste0(round(confusion$Freq[i], digits = 2)*100, '%'),
                            #text formatting
                            colour = 'white',
                            size = 3)
        }
      }
    }
  }
  
  return(p)
  
  
  #end of function
}


###size histogram function for future use ###
#===========================================#


size_histogram <- function(data, param, title = NULL , bw = 0.1, xlim = NULL){
  #' Plot size frequency histogram
  #' 
  #' @param param size parameter of interest (corresponds to sub lists within data argument)
  #' @param data  size data from auto_measure_mm subset into taxas
  #' @param title main title for plot, if left null will default based on parameter and taxa   
  #' @param bw bin width, defines width of bars on histogram, defaults to 0.1, decrease for more detail
  #' @param xlim plot xlimit, defaults to min max of data if not provided
  #' 
  #' 
  #' @details param options are typically 'Perimeter', 'Area', 'width1','width2',
  #'   'width3', 'short_axis_length', 'long_axis_length'
  #' 
  #' @export
  #' @author E. Chisholm
  
  require(ggplot2)
  
  if (is.null(title)){
    title <- paste(param , ':', data$taxa[1])
  }
  if(is.null(xlim)){
    xlim <- c(min(data[[param]]), max(data[[param]]))
  }
  qplot(data[[param]], geom = 'histogram', 
        binwidth = bw, 
        main = title, 
        xlab = 'length (mm)', 
        ylab = 'Frequency', 
        fill = I('green'), 
        col = I('green'), 
        alpha = I(.2), 
        xlim = xlim)
}


vis_unkn <- function(cm, classes, threshold = 0, summary = T){
  
  #' Function to visualize losses to unknown category due to disagreement in Dual classifier
  #' 
  #' Makes confusion matrix like plot, where x axis represent SVM classification, y axis represent NN classification
  #' Allows visual summary of data lost to unknown category
  #' 
  #' 
  #' @param cm dual unknown confusion matrix from VP
  #' @param classes taxa groups in order, from VP
  #' @param threshold minimum value which will be labelled in plot
  #' 
  #' E. Chisholm May 2019
  #' 
  require(ggplot2)
  require(gridExtra)
  
  dimcm <- dim(cm)
  #remove total columns
  conf <- cm[1:dimcm[1]-1,1:dimcm[2]-1]
  #create matrix and normalize
  input.matrix<- data.matrix(conf)
  
  
  #add labels
  colnames(input.matrix) = taxas
  rownames(input.matrix) = taxas
  
  #build conf mat
  confusion <- as.data.frame(as.table(input.matrix))
  
  #basic plot
  plot <- ggplot(confusion) 
  
  #add data
  p<- plot + geom_tile(aes(x=Var1, y=Var2, fill=Freq, col = 'black')) +  #adds data fill
    theme(axis.text.x=element_text(angle=90, hjust=1)) + #fixes x axis labels
    scale_x_discrete(name="SVM Class") + #names x axis
    scale_y_discrete(name="NN Class") + #names y axis
    scale_fill_gradient(low = 'orchid', high = 'darkorchid4',breaks=seq(from=-.5, to=4, by=.2)) + #creates colour scale
    #labs(fill="Normalized\nFrequency") + #legend title
    theme(legend.position = "none" ) +  #removes legend
    ggtitle(label = 'Disagreement in Dual Classifier') 
  
  
  threshold<- 0
  #labels
  for (i in 1:length(confusion$Var1)){
    #if frequency is above threshold
    if (confusion$Freq[i] > threshold ){
      #find x and y locations to plot
      x <- grep(levels(confusion$Var1), pattern = as.character(confusion$Var1[i]) )
      y <- grep(levels(confusion$Var2), pattern = as.character(confusion$Var2[i]) )
      #not already labelled on diagonal
      
      #add text
      p <- p + annotate('text', x = x, y = y,
                        #label - frequency as percent, rounded
                        label = round(confusion$Freq[i], digits = 2),
                        #text formatting
                        colour = 'white',
                        size = 3)
      
    }
  }
  
  #add summary text
  if (summary == TRUE){
    tab <- as.data.frame(
      c(
        'Sample Size' = '40:80' , #update for different sizes
        'Total Disagreement' = sum(confusion$Freq),
        'Average loss per taxa' = round(sum(confusion$Freq)/length(taxas), digits = 0)
      )
    )
    
    # using gridExtra
    
    p_tab <- tableGrob(unname(tab))
    grid.arrange(p, p_tab, heights = c(1, 0.2))
  }
  return(p)
  
}




#contour plot with interpolation

conPlot_EC <- function(data, var, dup= 'mean'){
  
  #' make interpolated contour plot of particular variable
  #' creates ggplot object onto which you can add binned number of rois
  #' automatically zeros time axis
  #' @author E. Chisholm
  #' 
  #' @param data data frame needs to include avg_hr, pressure, and variable of choice
  #' @param var variable in dataframe which will be interpolated and plotted
  
  
  require(metR)
  require(ggplot2)
  require(interp)
  require(akima)
  #interpolate
  #use interp package rather than akima to avoid breaking R
  #ref: https://www.user2017.brussels/uploads/bivand_gebhardt_user17_a0.pdf
  interpdf <- akima::interp(x = data$avg_hr, y = data$pressure, z = data[[var]], duplicate = dup )
  #convert to dataframe
  df <- interp2xyz(interpdf, data.frame = TRUE)
  
  #zero time values
  df$x <- df$x - min(df$x)
  
  p <- ggplot(df) +
    geom_tile(aes(x = x, y = y, fill = z)) +
    labs(fill = var) +
    scale_y_reverse(name = 'Pressure (db)') +
    scale_x_continuous(name = 'Time (hr)') +
    theme_classic() +
    geom_contour(aes(x = x, y = y, z= z), col = 'black') +
    geom_text_contour(aes(x = x, y = y, z= z),binwidth = 1, col = 'white')+ #CONTOUR LABELS
    scale_fill_continuous(na.value = 'white')
  
  return(p)
}

#TRIM CTD DATA#
trim_ctd_plot <- function(ctd){
  
  
  #' 
  #' 
  #' Plot a list of CTD casts to determine if they require trimming
  #' Internally calls trim_ctd to trim casts based on user input
  #' 
  #' @param ctd a list of oce CTD objects to be trimmed
  #' 
  #' @note If each ctd object contains multiple up/down casts, 
  #' different procedure is required
  #' this function only trims based on the start/end index of entire data
  #' 
  
  
  
  for (i in 1 :length(ctd)){
    
    #trim ctd object
    plot(ctd[[i]], which = 1)
    par(new = TRUE)
    title(main = i)
    
    trim <- readline('Does this cast require trimming? (y/n)')
    end_Trim <- readline('Does this cast require trimming at the end? (y/n)')
    
    if(trim == 'y'){
      plot(ctd[[i]][['salinity', 'data']][1:10000])
      abline(v = 100, col = 'red')
      abline(v = 200, col = 'orange')
      abline(v = 300, col = 'yellow')
      abline(v = 400, col = 'green')
      abline(v = 500, col = 'blue')
      abline(v = 600, col = 'orchid')
      abline(v = 700, col = 'pink')
      abline(v = 800, col = 'purple')
      trimIndex <- readline('Provide desired index for trimming (numeric)')
      trim_ctd(ctd[[i]], trimLength = trimIndex)
      
    }
    
    if (trim  == 'y' & end_Trim=='y'){
      plot(ctd[[i]][['salinity', 'data']][1:10000])
      abline(v = 100, col = 'red')
      abline(v = 200, col = 'orange')
      abline(v = 300, col = 'yellow')
      abline(v = 400, col = 'green')
      abline(v = 500, col = 'blue')
      abline(v = 600, col = 'orchid')
      abline(v = 700, col = 'pink')
      abline(v = 800, col = 'purple')
      trimIndex <- readline('Provide desired index for trimming (numeric)')
      
      plot(ctd[[i]][['salinity', 'data']][(length(ctd[[i]][['salinity', 'data']])-10000): length(ctd[[i]][['salinity', 'data']])], ylab = '' )
      abline(v = 10000, col = 'red')
      abline(v = 9900, col = 'orange')
      abline(v = 9800, col = 'yellow')
      abline(v = 9700, col = 'green')
      abline(v = 9600, col = 'blue')
      abline(v = 9500, col = 'orchid')
      abline(v = 9400, col = 'pink')
      abline(v = 9300, col = 'purple')
      endIndex <- readline('Provide desired index for trimming end (numeric)')
      
      trim_ctd(ctd[[i]], trimLength = as.numeric(trimIndex), end = TRUE, endTrim = as.numeric(endIndex))
      
    }
  }
}


plot_profile_conc <- function(data, taxa){
  
  #' plot a profile of taxa specific ROI concetntration
  #' @author E. Chisholm
  #' 
  #' @param data dataframe which will be run through bin_profile_taxa (see function requirements)
  #' @param taxa taxa of interest
  #' 
  #' 
  #create binned profile summary
  
  require(ggplot2)
  bin <- bin_profile_taxa(data, taxa)
  bin <- binQC(bin)
  roiL <- (bin$n_roi_bin/((0.108155^3)*(13)*(bin$time_diff_s)))
  #subset to less than 100 roi/L
  p <- ggplot(bin[roiL < 100, ]) +
    geom_point(aes(y = roiL[roiL < 100], x = min_pressure[roiL< 100])) +
    scale_x_reverse(name = 'Pressure (db)') +
    scale_y_continuous(name = 'ROI L^-1') +
    ggtitle(taxa) + 
    geom_smooth(aes(y = roiL[roiL < 100], x = min_pressure[roiL < 100])) +
    coord_flip() +
    theme_classic()
  
  return(p)
}


conPlot_conc <- function(data, dup = 'mean'){
  #contour concentration plots
  
  #' @param data vpr depth binned data, with parmaeters avg_time_h , pressure, and conc_m3
  #' 
  #' 
  #' makes a contour plot of ROI concentration, interpolated over time and depth
  #' 
  #' 
  
  require(metR)
  require(ggplot2)
  require(interp)
  require(akima)
  
  bindat <- data
  
  interpdf <- akima::interp(x = bindat$avg_time_h, y = bindat$pressure, z = bindat$conc_m3, duplicate = dup)
  #convert to dataframe
  df <- interp2xyz(interpdf, data.frame = TRUE)
  
  #zero time values
  df$x <- df$x - min(df$x)
  
  p <- ggplot(df) +
    geom_tile(aes(x = x, y = y, fill = z)) +
    labs(fill = 'concentration \n (/m3)') +
    scale_y_reverse(name = 'Pressure (db)') +
    scale_x_continuous(name = 'Time (hr)') +
    theme_classic() +
    geom_contour(aes(x = x, y = y, z= z), col = 'black') +
    geom_text_contour(aes(x = x, y = y, z= z), col = 'white')+
    scale_fill_continuous(na.value = 'white')
  
  return(p)
  
}

