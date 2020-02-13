

#source function
source('imageExplore.R')


###EXPLORE IMAGES####
mid = 20 #set minimum depth
mad = 40 #set maximum depth
roiFolder = 'E:/data/IML2018051/rois/' #set folder where ROIs are

#note this function is searching through all ROI images for matches, might be a little slow
#to speed it up, try setting the roiFolder path to be more specific, so it is only looking through files from one cast
#eg instead of 'E:/data/IML2018051/rois' use 'E:/data/IML2018051/rois/vpr0/'

roi_files <- exploreImages(all_dat, min.depth = mid, max.depth = mad, roiFolder = roiFolder, format = 'list') #gets list of all ROI file names within specified depth range

roi_file_unlist <- unlist(roi_files) #reformat list for simplicity

#create directory for images, automatically named by vpr tow number, min and max depths
newdir <- file.path(plotdir, paste0('vpr', tow, 'images_', mid, '_', mad, ''))
dir.create(newdir)

#copy ROI images into new directory
file.copy(roi_file_unlist, newdir)

