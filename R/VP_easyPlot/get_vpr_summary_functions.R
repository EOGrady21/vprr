#Functions for data processing and plotting VPR data

#
read.ctdvpr.data <- function(x) {
  #' Read in CTD data (SBE49) and Fluorometer data from CTD-VPR package
  #' 
  #'  
  #'   @param x text file with ctd data from VPR package
  #'   
  #'    
  #'     @export 
  
  data <- read.table(textConnection(gsub(":", ",", readLines(x))), sep = ",")
  time <- data[,1]
  time <- as.numeric(gsub("[^[:digit:]]", "", time))
  
  
  data2 <- cbind(time, data[,-c(1)])
  colnames(data2) <- c("time_ms", "conductivity", "temperature", "pressure", "salinity", "flour_ref", "fluorescence_mv", 
                       "turbidity_ref", "turbidity_mv", "altitude_NA")
  data2 <- data2[!duplicated(data2), ]
  data2
}


subset.ctdvpr.data <- function(x) {
  #' Get subset of data for hydrography summary
  #' 
  #' 
  #' @param x ctd data in matrix form
  #' 
  #' Takes every 10th row of data from ctd object
  #' 
  #' @export

  row_id_ctd <- seq(from = 1, to = nrow(x), by = 10)
  x[row_id_ctd,]

}

#
myCTDplot <- function(x, x_pos, y_pos) {
  
  #' Plot map, TS, and vertical profiles - CTD data
  #' 
  #' @param x oce CTD object
  #' @param x_pos longitude of ctd cast
  #' @param y_pos latitude of ctd cast
  #' 
  #' 
  #' @details returns a series of plots for summarizing ctd data, 
  #'   including position, and profiles of temperature, salinity, 
  #'   sigmaTheta and fluorescence
  #'
  #' 
  #'   @export
  #' 
  #' @notes EC: (to do) Improvement could be made by using lat/lon values within ctd
  #'   object rather than inputting arguments
  #' 
  #' 

  
  par(mfrow=c(1, 2))
  plotTS(x)
  plot(coastlineWorldFine, clongitude = x_pos[1], clatitude = y_pos[1], span = 150)
  #contour(bx, by, bz, levels = c(-100))
  lines(x_pos, y_pos, col = "red", lwd = 2)
  points(x_pos[1], y_pos[1], pch = 12)
  
  plotProfile(x, xtype='temperature', type = 'p', col = 'red')
  plotProfile(x, xtype='salinity', type = 'p', col = 'blue')
  plotProfile(x, xtype='sigmaTheta', type = 'p', col = 'magenta')
  plotProfile(x, xtype='fluorescence', type = 'p', col = 'green')
}

#
bubble_plotTSnroi <- function(x, r) {
  #' Get TS bubble plot for rois - number of rois
  #' 
  #' @param x oce CTD object
  #' @param r ??? (a list object with salinity, temperature, n_roi )
  #' 
  #' 
  #' @note EC: (to do) not used in either Rmd file (deprecated older version?) 
  #' not exported
  
  par(mfrow = c(1, 2))
  slim = range(x@data$salinity)
  tlim = range(x@data$temperature)
  plotTS(x, Slim = slim, Tlim = tlim, type = "n")
  par(new = T)
  symbols(x = r$salinity, y = r$temperature, circles = r$n_roi, 
          xlab  = "", ylab = "", fg = "blue",
          inches = 0.1, xlim = slim, ylim = tlim)
  plot.new()
}

#
bubble_plotTSconc <- function(x, r,lt, pos, inches, levels, round) {
  #' Get TS bubble plot for rois - concentration
  #' 
  #' @param pos position of legend (following legend() rules)
  #' @param x oce CTD object
  #' @param r ? list object with salinity, temperature, concentration, seems to be ctd data pulled to 'concentration' object
  #' @param lt legend title
  #' @param inches scales pt.cex of legend (set to 0.1)
  #' @param levels number of legend intervals
  #' @param round number of significant figures desired for size intervals
  #' 
  #' 
  #' @export
  #' 
  #' @note EC: (to do) Arguments could be reduced , try using ggplot with data classed
  #'   with size, might be cleaner code (ref =
  #'   https://r4ds.had.co.nz/data-visualisation.html , aes scaling... 
  #'   aes(size = class))
  #'   
  #'   
  #'   

  #l is legend title, p is position of legend,
  #scaling is ratio of pt.cex size to inches (might always be 2.7, not sure), 
  #levels how many levels of legend you want.
  # this will not work if mfrow = c(1,2)
  #round is #sigfigs rounded to (14938 to 2 sigs = 150000)
  par(mfrow = c(1, 2))
  slim = c(min(x@data$salinity),max(x@data$salinity)) #removed hard coded minimum for better scales (EC) 
  tlim = range(x@data$temperature)
  plotTS(x, Slim = slim, Tlim = tlim, type = "n")
  par(new = T)
  symbols(x = r$salinity, y = r$temperature, circles = r$concentration, 
          xlab  = "", ylab = "", fg = "grey22",
          inches = inches, xlim = slim, ylim = tlim)
  max <- max(r$concentration)
  bigbubble <- signif(max,round)
  leglev <- seq(0,bigbubble,bigbubble/levels)
  leglev <- leglev[-1]
  par(new = T)
  legend(pos, legend=c(leglev), pt.cex=c((leglev/max)*((inches/0.1)*2.629)),
         pch=1, title=lt)
  
  plot.new()
}

#
bin_average_vpr <- function(data) {
  #' Get bin averaged data
  #' 
  #' @param data ctd data in list format 
  #' 
  #' @output Data frame with fields pressure, min_pressure, max_pressure,
  #'   pressure_diff, min_time_s, max_time_s, time_diff_s, n_roi_bin,
  #'   conc_m3,temperature, salinity, density, fluorescence, turbidity,
  #'   avg_time_h
  #'   
  #' @details list of data must include elements pressure, time_ms, n_roi,
  #'   temperature, salinity, sigmaTheta, fluorescence turbidity. Produces data frame with parameters of interest.
  #'   Warning this function assumes 0.024 cubic inch image volume and 13 fps (hard coded)
  #'   
  #'   @export
  #'   
  #'   @note EC: (to do) update to remove hard coded values , use loop through names of
  #'     list elements rather than hard coding names (control for varying naming
  #'     conventions)
  #'   
  
  p <- data$pressure
  max_pressure <- max(p)
  min_pressure <- min(p)
  x_breaks <- seq(from = floor(min_pressure), to = ceiling(max_pressure), by = depth_bin_size)  #Change the by argument if you want to change the size of the depth bins from 1 m
  
  #Get variables of interest using oce bin functions
  min_time_s <- binApply1D(p, data$time_ms/1000, xbreaks = x_breaks, min)$result
  max_time_s <- binApply1D(p, data$time_ms/1000, xbreaks = x_breaks, max)$result
  min_pressure <- binApply1D(p, data$pressure, xbreaks = x_breaks, min)$result
  max_pressure <- binApply1D(p, data$pressure, xbreaks = x_breaks, max)$result
  n_roi_bin <- binApply1D(p, data$n_roi, xbreaks = x_breaks, sum)$result
  #temperature <- binMean1D(p, data$temperature, xbreaks = x_breaks)$result
  temperature <- binApply1D(p, data$temperature, xbreaks = x_breaks, mean)$result
  #salinity <- binMean1D(p, data$salinity, xbreaks = x_breaks)$result
  salinity <- binApply1D(p, data$salinity, xbreaks = x_breaks, mean)$result
  #density <- binMean1D(p, data$sigmaTheta, xbreaks = x_breaks)$result
  density <- binApply1D(p, data$sigmaTheta, xbreaks = x_breaks, mean)$result
  #fluorescence <- binMean1D(p, data$fluorescence, xbreaks = x_breaks)$result
  fluorescence <- binApply1D(p, data$fluorescence, xbreaks = x_breaks, mean)$result
  #turbidity <- binMean1D(p, data$turbidity, xbreaks = x_breaks)$result
  turbidity <- binApply1D(p, data$turbidity, xbreaks = x_breaks, mean)$result
  #avg_time_h <- binMean1D(p, data$time_ms/(1000*3600), xbreaks = x_breaks)$result
  avg_time_h <- binApply1D(p, data$time_ms/(1000*3600), xbreaks = x_breaks, mean)$result
  pressure <- binApply1D(p, data$temperature, xbreaks = x_breaks, mean)$xmids #Could be any of the variables computed, but I just went with min_time
  
  if (!(length(pressure) == length(salinity))) {
    
    salinity_mean <- binMean1D(p, data$salinity, xbreaks = x_breaks)$result
    
    idx_rm <- which(is.na(salinity_mean))
    pressure <- pressure[-idx_rm]
    
  }
  
  #Get derived variables
  time_diff_s <- max_time_s - min_time_s
  conc_m3 <- n_roi_bin/((0.024^3)*(13)*(time_diff_s)) #Assumes cubic image volume and 13 fps image aquisition
  
  pressure_diff <- max_pressure - min_pressure
  
  #Output is data frame
  data.frame(pressure, min_pressure, max_pressure, pressure_diff, min_time_s, max_time_s, time_diff_s, 
             n_roi_bin, conc_m3,
             temperature, salinity, density, fluorescence, turbidity, avg_time_h)
  
}




#Plot roi concentration
roi_concentration_plot <- function(x) {
  #' Plot ROI concentration template
  #' 
  #' @details plots concentration profile at mfrow = c(1,2)
  #' 
  #' 
  #' @param x concentration data to plot in profile
  #' 
  #' @export
  
  par(mfrow = c(1,2))
  plotProfile(x, xtype = "concentration", type = 'p', colour = "black")
  
}





#Plot contour plots
contour_plots <- function(contour_data, ctd_cont_tmp, roi_cont_tmp) {
  
  #' Plot contours
  #' 
  #' @details makes section contour plot best for roi concentration over hydro data
  #' Might be for profile data??? 
  #' 
  #' 
  #' @param contour_data list with j elements, each sub list has components x
  #'   and y and z will make up background hydro data of contour plot
  #' @param ctd_cont_tmp CTD data list with field time_h and pressure to make plot axes
  #' @param roi_cont_tmp ROI data list with fields time_h, pressure and n_roi
  #' 
  #' @export
  #' 
  #' @note EC: (to do) couold be tidied, could use ggplot
  
  # plot_coords <- list("top_left" = c(0.1,0.4,0.60,0.95), "top_right" = c(0.5,0.8,0.60,0.95),
  #                     "bottom_left" = c(0.1,0.4,0.15,0.5), "bottom_right" = c(0.5,0.8,0.15,0.5))
  
  #contour_data <- contour_data[[-5]]
  for(j in 2:length(contour_data)) {
    
    c_tmp <- contour_data[[j]]
    y_limits <- rev(range(c_tmp$y))
    x_limits <- range(c_tmp$x)
    hydro <- var_ts[j] #where does this object come from?
    
    #Get panel with contour plots ()
    
    # par(new = "TRUE",
    # plt = plot_coords[[j-1]],  # defining window for second plot
    # las = 1)
    
    print(hydro)
    
    par(mar = c(4, 4, 2, 2) + 0.1)
    
    filled.contour(c_tmp$x, c_tmp$y, c_tmp$z, nlevels = 50,
                   color.palette = colorRampPalette(c("white", "blue")),
                   ylim = y_limits, xlab = "Time (h)", ylab = "Depth (m)",
                   
                   plot.axes = {
                     points(ctd_cont_tmp$time_h, ctd_cont_tmp$pressure, pch = ".")
                     symbols(roi_cont_tmp$time_h, roi_cont_tmp$pressure, circles = roi_cont_tmp$n_roi, 
                             fg = "darkgrey", bg = "grey", inches = 0.1, add = T)
                     axis(1)
                     axis(2)
                     contour(c_tmp$x, c_tmp$y, c_tmp$z, nlevels = 10, add = T)
                     
                     
                   }) 
  }
  
}




#Plot contour plots from binned data
contour_plots_binned <- function(contour_binned_data, binned_cont_tmp, binned_cont_tmp_subset) {
  
  #' Contour plots for binned data
  #'
  #' @details  Makes filled contour plots for binned (towyo) data
  #'
  #' @param contour_binned_data hydro(?) data in list form, with each sub list
  #'   containing elements x, y, and z used to create background contour fill
  #'
  #' @param binned_cont_tmp data list with element avg_time_h and pressure used
  #'   to plot points ver contour plot?
  #'
  #' @param binned_cont_tmp_subset data list with elements avg_time_h, pressure
  #'   and conc_m3 that plots as bubbles where size is proportional to
  #'   concentration over contours
  #'
  #'   @export
  #'   
  #'   @note EC: (to do) make sure these data structures are stable add in ... for other plotting arguments
  for(j in 1:length(contour_binned_data)) {
    
    c_tmp <- contour_binned_data[[j]]
    y_limits <- rev(range(c_tmp$y))
    x_limits <- range(c_tmp$x)
    hydro <- binned_vars[j] #where is this object coming from?!
    
    print(hydro)
    
    par(mar = c(4, 4, 2, 2) + 0.1)
    
    
    if (hydro == 'conc_m3') {
      #hydro=conc
      x <- 10
    }  else if (hydro == 'density') {
      x <- length(seq(floor(min(c_tmp$z, na.rm =T)),ceiling(max(c_tmp$z, na.rm=T)),0.25))
     
      
    } else {
      
      x <-  length(seq(floor(min(c_tmp$z, na.rm =T)),ceiling(max(c_tmp$z, na.rm = T)),0.5))
      
    }
    
    filled.contour(c_tmp$x, c_tmp$y, c_tmp$z, nlevels = 50,
                   color.palette = colorRampPalette(c("white", "blue")),
                   ylim = y_limits, xlab = "Time (h)", ylab = "Depth (m)",
                   
                   plot.axes = {
                     points(binned_cont_tmp$avg_time_h, binned_cont_tmp$pressure, pch = ".")
                     axis(1)
                     axis(2)
                     contour(c_tmp$x, c_tmp$y, c_tmp$z, nlevels=x, add = T)
                     symbols(binned_cont_tmp_subset$avg_time_h, binned_cont_tmp_subset$pressure, circles = binned_cont_tmp_subset$conc_m3, 
                             fg = "darkgrey", bg = "grey", inches = 0.1, add = T)
                     #x was originally 'nlevels = 10'
                     
                   }) 
  }
  
}

#Plot size frequency of category
size_freq_plots <- function(x, number_of_classes, colour_of_bar) {
  
  #' size frequency plots
  #' 
  #' @details makes histogram of size frequency data for long axis length
  #' 
  #' @param x data with sublists 'taxa', 'long_axis_length'
  #' @param number_of_classes passed to nclass argument of hist()
  #' @param colour_of_bar passed to col argument of hist()
  #' 
  #' @export
  #' 
  #' 
  #' @note EC: (to do) could use '...' for plotting arguments might want to use ggplot
  
  data <- x
  taxa <- unique(data$taxa)
  
  for(i in 1:length(taxa)) {
    
    par(mfrow = c(1,2))
    
    taxa_id <- taxa[i]
    
    data_hist <- data %>%
      dplyr::filter(., taxa == taxa_id)
    
    data_hist2 <- data_hist$long_axis_length
    
    hist(data_hist2, nclass = number_of_classes, col = colour_of_bar, xlab = "Long axis of bug (mm)", main = taxa_id) #Eventually you will want to loop through taxa
    
    if(length(taxa) == 1) {
      
      plot.new()
      
    }
    
  }
  
}

#Get time and number of rois from ROI id
get_roi_data <- function(x) {
  
  #' Get time and number of ROIs from IDs
  #' 
  #' 
  #' @details pulls time stamp and amount of rois from character string data
  #'   using substr(), outputs a new data frame
  #' 
  #' @param x character vector with roi Id
  #' 
  #' @export
  #' 
  #' @note EC: (to do) determine what format input is and purpose of function
  
  roi_x <- substr(x, 1, 8)
  
  
  roi_df <- data.frame(roi = as.numeric(roi_x))
 
  
  roi_df2 <- roi_df %>%
    dplyr::group_by(., roi) %>%
    dplyr::summarise(., n_roi = n())
  
  data.frame(roi_df2)
  
}

#
getRoiMeasurements <- function(taxafolder, nchar_folder) {
  
  #' A data processing function to combine roi ids with their measurements
  #' 
  #' @details pulls size data from classified taxas and combines with roi IDs
  #' uses aidmea files containing size data and aid files containg roi ids in each taxa
  #' internally uses px_to_mm() to convert from pixels to mm for size data
  #' 
  #' @param taxafolder the path of folder which contains data for taxa of
  #'   interest (can be a list) eg.
  #'   E:\\data\\cruise_IML2018051\\autoid/Temora_Triconia
  #'   
  #'   @param nchar_folder length of character vector describing path to folder
  #'     containg taxa folders (?)
  #'     
  #'     @export
  #'     
  #'     @note EC: (to do) improve robustness and naming scheme
  #'     
  
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
      
      auto_measure_mm <- px_to_mm(auto_measure_px) #Convert to mm
      
      auto_measure_mm$roi_ID <- (roi_ID$V1) #Get roi ids
      auto_measure_mm$roi_ID <- substr(auto_measure_mm$roi_ID, 50, nchar(auto_measure_mm$roi_ID)-4) #Remove path information for rois
      
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
  
  y <- x #? not necessary
  
  y #return()
  
}



#get a rough idea of the minimum ship speed required such that VPR frames do not overlap
getMinShipSpeed_knots <- function(frame_diff, tow_duration, x_dim) {
  
  #' estimate minimum ship speed to ensure no verlap of VPR frames
  #' 
  #' @details calculates minimum ship speed requires to avoid overlap of frames
  #'   based on frame rate and image volume. Output is speed in knots
  #'   
  #'   @param frame_diff difference in seconds between frames (?)
  #'   @param tow_duration not used in function (!!!)
  #'   @param x_dim image volume (?)
  #'   
  #'   @export
  #'   
  #'   @note EC: (to do) remove unused arguments, clean up, make sure x_dim is correct input
  
  fps <- frame_diff/time_sec #where does this object come from???
  minspeed_m_s <- x_dim*fps #Min ship speed; units: m/s
  minspeed_knots <- minspeed_m_s/0.514 #reference for conversion factor?
  
  minspeed_knots #return()
  
}





#Get upcast or downcast from towyo
getCast <- function(data_towyo, cast_direction, data_type) {
  
  #' isolate up or down cast from towyo ctd data
  #'
  #' @details pull up or down cast from data in various formats
  #'
  #' @param data_towyo not used in function (!!!) meant to be ctd_oce? or data?
  #'
  #' @param cast_direction define up or down cast (passed to direction argument
  #'   of oce::ctdFindProfiles())
  #'
  #' @param data_type format of input data, string of 'oce' for oce ctd object
  #'   or 'df' for data frame
  #'
  #'   @export
  #'   
  #'   @note EC: (to do) Do not think this function will run, fix arguments and
  #'     format to be able to accept both data types
  
  cast_updated <- list()
  cast <- ctdFindProfiles(ctd_oce, direction = cast_direction) #where is ctd_oce?
  
  for(i in 1:length(cast)) { 
    
    data <- cast[[i]]
    
    n_obs <- length(data@data$pressure) #is this data_towyo?
    cast_number <- i
    cast_id <- paste(cast_direction, i, sep = "_")
    cast_id_vec <- rep(cast_id, n_obs)
    
    cast_updated[[i]] <- oceSetData(data, "cast_id", cast_id_vec, "no_unit")
    
  }
  
  if(data_type == "oce") {
    
    cast_updated #return()
    
  }
  
  if(data_type == "df") {
    
    getDf <- function(x) { #where is x coming from?
      
      data.frame(x@data, stringsAsFactors = F)
      
    }
    
    lapply(cast_updated, getDf)
    
  }
  
}



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
  #'   @note    not yet tested
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
