##EXPLORE IMAGES BY DEPTH BIN


require(magick)
require(dplyr)

exploreImages_depth <- function(data, min.depth , max.depth, roiFolder , format = 'list'){
  
  #' @param data data frame containing CTD and ROI data that has been processed through VP easy plot
  #' @param min.depth minimum depth of ROIs you are interested in looking at
  #' @param max.depth maximum depth of ROIs you are interested in exploring
  #' @param roiFolder directory that ROIs are within (can be very general eg. C:/data, but will be quicker to process with more specific file path)
  #' @param format option of how images will be output, either as ('list') 
  #' a list of file names or 'image' where images will be displayed
  
  
  data_filtered <- data %>%
    dplyr::filter(., pressure >= min.depth) %>%
    dplyr::filter(., pressure <= max.depth)
  
  if(length(data_filtered$roi) < 1){
    stop('No data exists within this depth range!')
  }

  
  #search for ROI files based on data
  roi_files <- paste0('roi.', sprintf('%08d', data_filtered$roi), '*')
  roi_file_list <- list()
  options(warn = 1)
  for (i in 1:length(roi_files)){
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
    for(i in 1:length(roi_file_list)){
      for(ii in 1:length(roi_file_list[[i]])){
        data_roi <- data_filtered %>%
          dplyr::filter(., roi == roi[i])
        meta_str <- paste0('time (hr): ', data_roi$avg_hr[1], '\n temperature: ', data_roi$temperature[1], '\n pressure: ', data_roi$pressure[1], '\n salinity: ', data_roi$salinity[1], '\n')
     pp <-  magick::image_read(roi_file_list[[i]][ii]) %>%
       #print metadata on image 
       #image_annotate(text = roi_files[i], color = 'white', size = 10) %>%
       #image_annotate(text = meta_str, color = 'white', location = '-100') %>%
       image_scale(geometry = 'x300')

      print(pp)
      #print metadata
      #cat(paste0(roi_files[i], '\n'))
        #cat( paste0('time (hr): ', data_roi$avg_hr, '\n temperature: ', data_roi$temperature, '\n pressure: ', data_roi$pressure, '\n salinity: ', data_roi$salinity, '\n'))
      
        }
        }
  }

  
  
  
 # 
  }

