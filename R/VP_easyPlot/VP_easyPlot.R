####VPR PLOTTING####

##E. Chisholm July 2019

##Designed to simply plot VPR data to ensure proper collection and visualize
##initial results while cruise is in progress. Note this script does not classify
##ROIs or distinguish by taxa but gives an overall summary of data collected.


##Input: Data directory path.
##script finds VPR ROIs and CTD data for specified
##    tow number processes and combines data.
##Output: Series of plots and a data
##    summary report (in directory specified 'plotdir')

##To Use: Edit file paths and variables in 
##Step 1 as directed, 
##source entire document "source('VP_easyPlot.R)"
##    (using source command from console rather than R studio 'source' button allows
##    you to see real time outptut from functions)

###STEP 1: Set Up####

#load in all required packages
#first time, packages need to be installed with install.package("package")
library(ggplot2)
library(akima)
library(interp)
library(dplyr)
library(oce)
library(gridExtra)
library(metR)
library(tidyr)


#source functions
source('EC_functions.R')
source('get_vpr_summary_functions.R')
source('imageExplore.R')

####USER INPUT REQUIRED#####
#Choose directory (where plots will be saved - ensure it ends with "/")
plotdir <- "R:/Shared/ChisholmE/VPR_plotting/figures/"

#set defaults
#SHOULD BE UPDATED BY USER FOR SPECIFIC CRUISE OR DESIRED DATA SELECTION

cruise <- 'IML2018051'
tow <- '0' #VPR tow of interest
station <- 'Station Example' #set station metadata
event <- '001' #set event metadata

#location of data directory
basepath <- "E:/data"

##OPTIONAL QC PARAMETERS##
#min and max values of each parameter
sal_range <- c(28, 35) #salinity range (PSU)
temp_range <- c(0, 15) #temperature range (deg C)
pres_range <- c(0, 500) #pressure range (db)

####STEP 2: Load data####

#gets day and hour in cast
day <- list.files(file.path(basepath, cruise, 'rois', paste0('vpr', tow)))
hour <- list.files(file.path(basepath, cruise, 'rois', paste0('vpr', tow), day))
hour <- unique(substr(hour, 1,3))
#check that there is data present
if (length(day) < 1){
  stop("No data found in tow ", tow, '!')
}

#LOAD CTD
ctd_path <- unique(file.path(basepath, cruise, 'rois', paste0('vpr', tow), day))
ctd_files <- list.files(ctd_path, pattern = '.dat', full.names = TRUE )

#check that there is ctd data
if (length(ctd_files) < 1){
  stop(paste("No CTD data found in tow ", tow," " ,day,"! \n"))
}

#check that number of ctd files matches number of hours in cast
if (length(ctd_files) != length(hour)){
  warning(paste(length(ctd_files), 'CTD files found for', length(hour), 'hours!'))
}

cat(paste(">>>>> Now processing VPR", tow, day, hour, '\n'))



cat(paste(">>>>> Now reading CTD files", ctd_files, '\n'))
  #read CTD dat for multiple hours/ files
  ctd_dat <- list()
  for (i in 1:length(ctd_files)){
    ctd_dat[[i]] <- read.ctdvpr.data(ctd_files[i])
  }


#get ROI data

roi_files <- list.files(file.path(ctd_path, hour))
roi_num <- substr(roi_files, 5, nchar(roi_files) - 4)


####STEP 3: Format data####

#make oce ctd object
#easier for plotting
ctd <- format_oce(ctd_dat)

#combine ctd data over hours into single data frame

#set loop variable for reference
if(length(names(ctd_dat)) > 1){loop = FALSE}else{loop = TRUE}

if(loop == TRUE){
  #for multiple hours of data
ctd_dat_all <- do.call(rbind, ctd_dat)
cat(length(ctd_dat), ' hours of CTD data combined!')
}else{
  #for single hour of data
  ctd_dat_all <- ctd_dat
}

#get roi number that will match ctd time_ms
rois <- as.numeric(substr(roi_num, 1, 8))
#format as table to get frequency
roi_table <- as.data.frame(table(rois))


#subset ctd and roi data where time/roi identifier match
ctd_sub <- which(ctd_dat_all$time_ms %in% rois)
roi_sub <- which(rois %in% ctd_dat_all$time_ms)
#subset data individually
ctd_dat_sub <- ctd_dat_all[ctd_sub,]

#EC & KS fix 2019/08/08 due to error producing NA roi numbers
roi_dat_sub <- rois[!duplicated(rois)]
#roi_dat_sub <- rois[ctd_sub]

#if ctd and roi data dont match stop process
#requires review of input files
if(length(roi_dat_sub) != length(ctd_dat_sub$time_ms)){
  stop('CTD and ROI times do not match!!')
}

#combine roi and ctd data
all_dat <- ctd_dat_sub %>%
  dplyr::mutate(., roi = roi_dat_sub) %>%
  dplyr::mutate(., n_roi = roi_table$Freq) #add n_roi (count of rois per second)

#calculated variables

sigmaT <- swSigmaT(salinity = all_dat$salinity, temperature = all_dat$temperature, pressure = all_dat$pressure)

#add sigma t and time(hr) to combined data frame
all_dat <- all_dat %>%
  dplyr::mutate(., sigmaT = sigmaT) %>%
  dplyr::mutate(., avg_hr = time_ms/3.6e+06)


##QC option-------

qc_ans <- readline('Would you like to QC this data before plotting? (y/n) ')

if (qc_ans == 'y'){
  all_dat_o <- all_dat #save original data as seperate object for comparison
  all_dat <- all_dat %>% #filter data based on parameter ranges set in step 1
    dplyr::filter(., salinity > min(sal_range)) %>%
    dplyr::filter(., salinity < max(sal_range)) %>%
    dplyr::filter(., temperature > min(temp_range)) %>%
    dplyr::filter(., temperature < max(temp_range)) %>%
    dplyr::filter(., pressure > min(pres_range)) %>%
    dplyr::filter(., pressure < max(pres_range))
  
  #print of QC summary report
  qc_pts <- length(all_dat_o$time_ms) - length(all_dat$time_ms)
  cat('>>>>>', qc_pts, 'points removed based on QC parameters \n')
  cat('>>>>> Salinity range: ', sal_range, '\n')
  cat('>>>>> Temperature range: ', temp_range, '\n')
  cat('>>>>> Pressure range: ', pres_range, '\n')
  

}else{
  all_dat_o <- all_dat
}
#processing for plotting format
#create oce objects with all variables
ctd_roi_oce <- as.ctd(all_dat)
otherVars<-  c('time_ms', 'fluorescence_mv', 'turbidity_mv', 'n_roi', 'sigmaT')
for ( o in otherVars){
  eval(parse(text = paste0("ctd_roi_oce <- oceSetData(ctd_roi_oce, name = '",o,"', value = all_dat$",o,")")))
}

#seperate into up and down casts before binning data
#find upcasts
upcast <- getCast_EC(data = ctd_roi_oce, cast_direction = 'ascending', data_type = 'df')
upcast2 <- lapply(upcast, bin_average_vpr_EC)
upcast_df <- do.call(rbind, upcast2)
#find downcasts
downcast <- getCast_EC(ctd_roi_oce, "descending", "df")
downcast2 <- lapply(downcast, bin_average_vpr_EC)
downcast_df <- do.call(rbind, downcast2)
#combine_data into bins
vpr_depth_bin <- rbind(upcast_df, downcast_df)
vpr_depth_bin <- data.frame(vpr_depth_bin)

#remove outliers from concentration 
#(spikes when short time is spent in one depth bin)
#zero time values for neat plot axes 
vpr_depth_bin <- vpr_depth_bin %>%
  dplyr::mutate(., avg_time_h = avg_time_h - min(avg_time_h)) %>%
  dplyr::filter(., time_diff_s >= 0.8)

##data report
fn = paste0(plotdir,'vpr', tow, 'dataSummary.txt')
#data summary for original (pre QC) data, 
#to get stats on points removed outside of bounds
dataSummary(all_dat_o, fn = fn, station, event, sal_range, temp_range, pres_range)

cat(paste("Formatting complete! \n"))

####STEP 4: Plotting####

####CTD PROFILE PLOTS###

#set consistent pressure axis between plots
y_limits_p <- c(max(all_dat$pressure), min(all_dat$pressure))
#plot temp
p <- ggplot(all_dat) +
  geom_point(aes(x = temperature, y = pressure), col = 'red') + #adds pts
  scale_y_reverse(name = 'Pressure (db)', limits = y_limits_p) #flips y axis
#plot salinity
p_TS <- p + geom_point(aes(x = (salinity -25), y = pressure), col = 'blue') + 
  #create secondary axis with appropriate scaling to show two variables on plot
  scale_x_continuous(name = 'Temperature (c)',sec.axis = sec_axis(~ . +25, name = 'Salinity (PSU)')) +
  #change colours of axes to match data pts for clarity
  theme(axis.line.x.bottom = element_line(colour = 'red'), 
        axis.ticks.x.bottom = element_line(colour = 'red'), 
        panel.background = element_blank(),
        panel.grid = element_blank(), 
        axis.line.y = element_line(linetype = 'solid'),
        axis.line.x.top = element_line(colour = 'blue'),
        axis.ticks.x.top = element_line(colour = 'blue')
  )


#plot fluorescence
p <- ggplot(all_dat) +
  geom_point(aes(x = fluorescence_mv, y = pressure), col = 'green') + #add pts
  scale_y_reverse(name = 'Pressure (db)', limits = y_limits_p) #flip y axis

#plot density
p_FD <- p + geom_point(aes(x = (sigmaT  -20) *20, y = pressure)) + #add pts
  #set up bopth axes with relative scales
  scale_x_continuous(name = 'Fluorescence (mv)',sec.axis = sec_axis(~. /20  +20, name = 'Density')) +
  #change colour of axes to match data pts
  theme(axis.line.x.bottom = element_line(colour = 'green'), 
        axis.ticks.x.bottom = element_line(colour = 'green'),
        panel.background = element_blank(),
        panel.grid = element_blank(), 
        axis.line.y = element_line(linetype = 'solid'),
        axis.line.x.top = element_line(colour = 'black')
        )


#plot concentration profile
pp <- ggplot(vpr_depth_bin) +
  geom_point(aes(x = pressure, y = conc_m3/1024)) + #conversion of m3 to L using default density
  #create average line (can only be calculated along y so requires flipping plot after calculation)
  stat_summary_bin(aes(x = pressure, y = conc_m3/1024), fun.y = 'mean', col = 'red', geom = 'line', size = 3, alpha = 0.5)  +
  #reverse what will become y axis
  scale_x_reverse(name = 'Pressure (db)', limits = y_limits_p) +
  #set what will become x axis so that dimension match other profile plots (axes on both ends)
  scale_y_continuous(name = 'ROI L^-1', position = 'right', sec.axis = sec_axis(~. , name = 'ROI L^-1')) +
  theme_classic() +
  #flip x and y axes now that calculations are finished
  coord_flip()

#save three panel ctd profile plots
png(paste0(plotdir, 'vpr', tow,'summaryCTDplots.png'))
grid.arrange(p_TS, p_FD,pp, ncol = 3, nrow = 1)
dev.off()
cat(paste('>>>>>>',paste0('vpr', tow,'summaryCTDplots.png'), 'saved! \n'))


#plot VPR path
png(paste0(plotdir, 'vpr', tow,'vprPath.png'))
q <- qplot(vpr_depth_bin$avg_time_h, vpr_depth_bin$pressure, geom = 'line') +
  scale_y_reverse('Pressure') +
  scale_x_continuous('Time') +
  theme_classic()+
  ggtitle('Binned VPR path')

print(q)
dev.off()

cat(paste('>>>>>>',paste0('vpr', tow,'vprPath.png'), 'saved! \n'))


#plot concentration contours
png(paste0(plotdir, 'vpr', tow,'conPlots_conc.png'), width = 1500, height = 500)

p <- conPlot_conc(na.omit(vpr_depth_bin), dup = 'strip') #plot contours
#add concentration bubbles and path line
pp <- p + geom_line(data = vpr_depth_bin, aes(x = avg_time_h, y = min_pressure), col = 'snow4', inherit.aes = FALSE) +
  geom_point(data = vpr_depth_bin, aes(x = avg_time_h, y = min_pressure, size = conc_m3), alpha = 0.1)+
  ggtitle('Concentration') +
  labs(size = "")+ #size scale null (same as contour scale units)
  scale_size_continuous(range = c(0, 10)) #enlarge bubbles


print(pp)
dev.off()
cat(paste('>>>>>>',paste0('vpr', tow,'conPlots_conc.png'), 'saved! \n'))


##contour plots for ctd vars
#sigma t contours
#interpolate data
  vpr_int <- akima::interp(x = vpr_depth_bin$avg_time_h, y = vpr_depth_bin$pressure, z = vpr_depth_bin$density, duplicate = 'strip')
    
#plot
  #set consistent x and y limits
y_limits <- rev(range(vpr_int$y))
x_limits <- range(vpr_int$x)


png(paste0(plotdir, 'vpr', tow, 'sigmaT_contour.png'), width = 2000, height = 500)
#make contour plot
filled.contour(vpr_int$x, vpr_int$y, vpr_int$z, nlevels = 50,
               color.palette = colorRampPalette(c("blue", 'red')),
               ylim = y_limits, xlab = "Time (h)", ylab = "Depth (m)", main = 'Concentration over Density',
               #add axes and annotations
               plot.axes = {
                 #add concentration bubbles
                 points(vpr_depth_bin$avg_time_h, vpr_depth_bin$pressure, pch = ".")
                 #add path line
                 points((all_dat$avg_hr - min(all_dat$avg_hr)), all_dat$pressure, type = 'l')
                 #add axes
                 axis(1)
                 axis(2)
                 #add contour lines
                 contour(vpr_int$x, vpr_int$y, vpr_int$z, nlevels=10, add = T)
                 #enlarge bubbles based on concentrations
                 symbols(vpr_depth_bin$avg_time_h, vpr_depth_bin$pressure, circles = vpr_depth_bin$conc_m3, 
                         fg = "black", bg = "grey", inches = 0.3, add = T)
                
                 
               }) 

dev.off()
cat(paste('>>>>>>',paste0('vpr', tow, 'sigmaT_contour.png'), 'saved! \n'))

#temperature
#interpolate data
vpr_int <- akima::interp(x = vpr_depth_bin$avg_time_h, y = vpr_depth_bin$pressure, z = vpr_depth_bin$temperature, duplicate= 'strip')

#plot
#set consistent x and y limits
y_limits <- rev(range(vpr_int$y))
x_limits <- range(vpr_int$x)

png(paste0(plotdir, 'vpr', tow, 'temp_contour.png'), width = 2000, height = 500)
#make contour plot
filled.contour(vpr_int$x, vpr_int$y, vpr_int$z, nlevels = 50,
               color.palette = colorRampPalette(c( "blue", 'red')),
               ylim = y_limits, xlab = "Time (h)", ylab = "Depth (m)", main = 'Concentration over Temperature',
               #add anotations
               plot.axes = {
                 #add bubbles
                 points(vpr_depth_bin$avg_time_h, vpr_depth_bin$pressure, pch = ".")
                 #add vpr path
                 points(all_dat$avg_hr - min(all_dat$avg_hr), all_dat$pressure, type = 'l')
                 #add axes
                 axis(1)
                 axis(2)
                 #add contour lines
                 contour(vpr_int$x, vpr_int$y, vpr_int$z, nlevels=10, add = T)
                 #enlarge bubble size based on concentration
                 symbols(vpr_depth_bin$avg_time_h, vpr_depth_bin$pressure, circles = vpr_depth_bin$conc_m3, 
                         fg = "darkgrey", bg = "grey", inches = 0.3, add = T)
                 
                 
               }) 

dev.off()
cat(paste('>>>>>>',paste0('vpr', tow, 'temp_contour.png'), 'saved! \n'))


#salinity
#interpolate data
vpr_int <- akima::interp(x = vpr_depth_bin$avg_time_h, y = vpr_depth_bin$pressure, z = vpr_depth_bin$salinity, duplicate = 'strip')

#plot
#set consistent x and y limits
y_limits <- rev(range(vpr_int$y))
x_limits <- range(vpr_int$x)

png(paste0(plotdir, 'vpr', tow, 'sal_contour.png'), width = 2000, height = 500)
#make contour plot
filled.contour(vpr_int$x, vpr_int$y, vpr_int$z, nlevels = 50,
               color.palette = colorRampPalette(c( "blue", 'red')),
               ylim = y_limits, xlab = "Time (h)", ylab = "Depth (m)", main = 'Concentration over Salinity',
               #add annotations
               plot.axes = {
                 #add bubbles
                 points(vpr_depth_bin$avg_time_h, vpr_depth_bin$pressure, pch = ".")
                 #add vpr path
                 points(all_dat$avg_hr - min(all_dat$avg_hr), all_dat$pressure, type = 'l')
                 #add axes
                 axis(1)
                 axis(2)
                 #add contour lines
                 contour(vpr_int$x, vpr_int$y, vpr_int$z, nlevels=10, add = T)
                 #enlarge bubbles based on concentration
                 symbols(vpr_depth_bin$avg_time_h, vpr_depth_bin$pressure, circles = vpr_depth_bin$conc_m3, 
                         fg = "darkgrey", bg = "grey", inches = 0.3, add = T)
                 
                 
               }) 

dev.off()
cat(paste('>>>>>>',paste0('vpr', tow, 'sal_contour.png'), 'saved! \n'))

###EXPLORE IMAGES####

ans <- readline('Would you like to run image exploration? (y/n) ')
if (ans == 'y'){
mid <- as.numeric(readline('Minimum depth of interest? '))
mad <- as.numeric(readline('Maximum depth of interest? '))

roi_files <- exploreImages_depth(all_dat, min.depth = mid, max.depth = mad, roiFolder = paste0('E:/data/IML2018051/rois/vpr', tow ), format = 'list')

roi_file_unlist <- unlist(roi_files)
newdir <- file.path(plotdir, paste0('vpr', tow, 'images_', mid, '_', mad, ''))
dir.create(newdir)
file.copy(roi_file_unlist, newdir)
}else{
  cat('Data exploration complete!')
}
#If more plotting or data exploration is required the object 'all_dat' 
#contains all roi and ctd data, as a data frame, easily called to plot
#eg. plot(x = all_dat$n_roi, y = all_dat$pressure)

#'ctd' object has multiple oce ctd objects, can be individually called eg. ctd[[1]]
#easily plotted ,call ?`plot,ctd-method` to see oce plotting details.


