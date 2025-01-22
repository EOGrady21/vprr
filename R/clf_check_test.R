## manual classifier check

vpr_manual_classification <-
  function(day,
           hour,
           basepath,
           category_of_interest,
           gr = TRUE,
           scale = 'x300',
           opticalSetting = 'S2',
           img_bright = TRUE,
           threshold_score,
           path_score) {

    #' Function to check results of classification manually
    #'
    #'
    #' Displays each image in day hour specified,
    #' prompts user to confirm or deny classification.
    #' If classification is denied, asks for a reclassification
    #'  value based on available category
    #'
    #' @param day day of interest in autoid (3 chr)
    #' @param hour hour of interest in autoid (2 chr)
    #' @param basepath path to folder containing autoid files (e.g., 'extdata/COR2019002/autoid')
    #' @param category_of_interest list of category folders you wish you sort through
    #' @param gr logical indicating whether pop up graphic menus are used (user preference - defaults to TRUE)
    #' @param scale argument passed to \code{\link{image_scale}}, default = 'x300'
    #' @param opticalSetting specifies optical setting of VPR, defining image frame
    #'   size, current options are 'S0', 'S1', 'S2' (default), 'S3', see further
    #'   info in details
    #'@param img_bright logical value indicating whether or not to include a blown
    #'  out high brightness version of image (can be helpful for viewing dark field
    #'  fine appendages)
    #' @param threshold_score (optional) a numeric value defining the minimum confidence
    #'   value, under which automatic classifications will be passed through
    #'   manual reclassification. This argument should match the threshold
    #'   provided in `vpr_autoid_copy()`
    #' @param path_score (optional) file path to folder containing autoid files with confidence values produced by automated
    #'   classification
    #'
    #' @details Optical Setting frame sizes: S0 = 7x7 mm, S1 = 14x14mm, S2 =
    #'   24x24mm, S3 = 48x48 mm. These settings define the conversion factor from
    #'   pixels to millimetres and calculate image size for classification
    #'   reference
    #'
    #'
    #' @section Development:
    #'   \itemize{
    #'       \item Add "undo" functionality to go back on a typing mistake
    #'       \item  Fix scaling/ size issue so images are consistently sized
    #'       }
    #'
    #'@export

    # # if no threshold score is provided, check through all images
    # if(missing(threshold_score)){
    #   threshold_score <- 1
    # }

    day_hour <- paste0('d', day, '.h', hour)
    dirpath <- file.path("manual_reclassification_record",day_hour)
    dir.create(path = dirpath, showWarnings = FALSE, recursive = TRUE)
    existingFiles <- list.files(dirpath, full.names = TRUE)
    ans <-
      menu(
        c('Yes', 'No'),
        graphics = FALSE,
        title = paste(
          'WARNING!!! ALL EXISTING FILES IN', day_hour,
          'ARE ABOUT TO BE DELETED. DO YOU WISH TO PROCEED?'
        )
      )
    if (ans == 1) {
      file.remove(existingFiles)
    } else{
      warning(immediate. = TRUE,
              paste('CAUTION, FILES FOR', day_hour, 'ARE BEING APPENDED!!'))
    }

   categoryFolders_og <- list.files(basepath, full.names = TRUE)
   categoryNames <- list.files(basepath)
    allcategory <- list.files(basepath)

   categoryFolders <-categoryFolders_og[categoryNames %in% category_of_interest]
   categoryNames <- categoryNames[categoryNames %in% category_of_interest]
    if (length(categoryFolders) == 0) {
      stop('No category folders match category of interest!
                                     Caution of capitalization!')
    }

    t_f <- dir.exists(categoryFolders)

    # Make an empty list for reclassficiations with named elements for each category
    reclassified <- vector("list", length(allcategory))
    names(reclassified) <- allcategory

    for (i in seq_len(length(categoryFolders))) {
      misclassified <- vector()

      print(paste('CATEGORY START : ', categoryFolders[i]))
      y <- readline(paste('CONFIRM NEW CATEGORY : ', categoryFolders[i]))
      # clear existing files
      path <- categoryFolders[i]

      # if(!missing(path_score)){
      # #ensure that the autoid and score folders match... can mismatch if extra categories are added to autoid directory in manual re-classification step
      # path_cat <- basename(path)
      # path_scr <- paste(path_score, path_cat, sep = "/")
      # path_scr_logical <- dir.exists(path_scr) # it will not exist if it is a newly added category (after cnn classification)
      # }

      if (t_f[i] == FALSE) {
        print(paste('category : ', categoryFolders[i], 'DOES NOT EXIST!'))
        SKIP = TRUE
      } else{
        dayHrFolders <- list.files(path, full.names = TRUE)

        dayHrFolder <-
          grep(dayHrFolders, pattern = day_hour, value = TRUE)
        # if(!missing(path_score)){
        #   Folders_scr <- list.files(path_scr, full.names = TRUE)
        #   Folders_scr2 <- list.files(Folders_scr, full.names = TRUE)
        #
        # }

        if (length(dayHrFolder) == 0) {
          print(paste('category : ', categoryFolders[i], 'DOES NOT EXIST IN ', day_hour, '!'))
          SKIP = TRUE
        } else{
          SKIP = FALSE

        if(missing(threshold_score)){
          # grab aid file info
          aidFolder <-
            grep(dayHrFolders, pattern = 'aid$', value = TRUE)
          aidFile <-
            list.files(aidFolder, pattern = day_hour, full.names = TRUE)
          aid_dat <- read.table(aidFile, stringsAsFactors = FALSE) # TODO read in pred_results instead of aid
          aid_dat <- unique(aid_dat$V1) # KS added unique to duplicate bug fix

        }else{
          # SKIP = FALSE
          # aidFolder <- grep(dayHrFolders, pattern = "aid$",
          #                   value = TRUE)
          #
          # aidFile <- list.files(aidFolder, pattern = day_hour, full.names = TRUE)
          # aid_dat <- read.table(aidFile, stringsAsFactors = FALSE)
          # rois_all <- list.files(dayHrFolder, full.names = TRUE)
          #
          # aidFile_scr <- grep(Folders_scr2, pattern = day_hour, value = TRUE)
          # if(length(aidFile_scr) == 0){
          #   stop(paste('No CNN aid found for', day_hour, 'in', categoryFolders[i]))
          # }
          #aid_dat_scr <- read.table(aidFile_scr, stringsAsFactors = F)
          #aid_dat_threshold <- subset(aid_dat_scr, aid_dat_scr$V2 < threshold_score)
          #aid_dat_t <- aid_dat_threshold
          #aid_dat_t_roi <- basename(aid_dat_t)
          #aid_scr_t <- aid_dat_threshold$V2

          aid_dat_t <- subset(aid_dat, aid_dat$V2 < threshold_score)
          aid_scr_t <- aid_dat_t$V2

          # rois_all_bn <- basename(rois_all)
          # # remove file extension in case of comparing .tif and .png
          #   if(stringr::str_sub(rois_all_bn, start = -3, end = -1)[[1]] !=
          #     stringr::str_sub(aid_dat_t_roi, start = -3, end = -1)[[1]]){
          #     rois_all_bn <- stringr::str_sub(rois_all_bn, start = 1, end = -5) # remove file extension
          #     aid_dat_t_roi <- stringr::str_sub(aid_dat_t_roi, start = 1, end = -5) # remove file extension
          #     rois_threshold_idx <- which(rois_all_bn %in% aid_dat_t_roi) # compare for index
          #
          #   }else{
          #     rois_threshold_idx <- which(rois_all_bn %in% aid_dat_t_roi)
          #     }
          # rois <- rois_all[rois_threshold_idx]
          # if(length(rois) == 0){
          #   stop('No ROIs found!')
          # }

        }

          rois <- list.files(dayHrFolder, full.names = TRUE)

          # find correct conversion factor based on VPR optical setting
          if (opticalSetting == 'S0') {
            # px to mm conversion factor
            frame_mm <- 7
            mm_px <-
              frame_mm / 1024 # 1024 is resolution of VPR images (p.4 DAVPR manual)
            pxtomm <- 1 / mm_px
          }
          if (opticalSetting == 'S1') {
            # px to mm conversion factor
            frame_mm <- 14
            mm_px <-
              frame_mm / 1024 # 1024 is resolution of VPR images (p.4 DAVPR manual)
            pxtomm <- 1 / mm_px
          }
          if (opticalSetting == 'S2') {
            # px to mm conversion factor
            frame_mm <- 24
            mm_px <-
              frame_mm / 1024 # 1024 is resolution of VPR images (p.4 DAVPR manual)
            pxtomm <- 1 / mm_px
          }
          if (opticalSetting == 'S3') {
            # px to mm conversion factor
            frame_mm <- 42 # correct conversion factor (7/11/2022)
            mm_px <-
              frame_mm / 1024 # 1024 is resolution of VPR images (p.4 DAVPR manual)
            pxtomm <- 1 / mm_px
          }

          for (ii in seq_len(length(rois))) {
            print(paste(ii, '/', length(rois)))

            if(missing(threshold_score)){

              scr_tmp <- "no_score"

            } else {

              scr_tmp <- aid_scr_t[ii] # check to make sure ROI image matches score displayed

              if(unlist(vpr_roi(aid_dat_threshold$V1[ii])) != unlist(vpr_roi(rois[ii]))){

                stop('Mismatch between CNN scores and ROI images, check autoid folder inputs!')

              }

            }


          #     img <- magick::image_read(rois[ii], strip = FALSE) %>%
          #       magick::image_scale(scale) %>%
          #       magick::image_annotate(paste(categoryNames[i], "(roi.", unlist(vpr_roi(rois[ii])), ")"), color = "red", size = 12) %>%
          #       magick::image_annotate(text = paste("scoreCNN = ", round(scr_tmp, digits = 2)),
          #                              location = "+0+20",
          #                              color = "red") %>%
          #       magick::image_annotate(text = paste(round(imgdat$width/pxtomm, digits = 2), "x", round(imgdat$height/pxtomm, digits = 2), "mm"),
          #                              location = "+0+10",
          #                              color = "red")
          #
          #   }
          #
          #     img <- magick::image_read(rois[ii], strip = FALSE) %>%
          #     magick::image_scale(scale) %>%
          #     magick::image_annotate(categoryNames[i], color = 'red', size = 12)
          #
          #     # read in original image without scaling
          #     img_o <- magick::image_read(rois[ii])
          #     imgdat <- magick::image_info(img_o)
          #
          #   # annotate original image size
          #     img <- magick::image_annotate(img, text = paste(round(imgdat$width / pxtomm, digits = 2), 'x', round(imgdat$height / pxtomm, digits = 2),
          #         'mm'
          #       ),
          #       location = '+0+10',
          #       color = 'red'
          #     )
          #
          # #}
          #
          # else{
          #
          #     scr_tmp <- aid_scr_t[ii] # check to make sure ROI image matches score displayed
          #
          #     if(unlist(vpr_roi(aid_dat_threshold$V1[ii])) != unlist(vpr_roi(rois[ii]))){
          #       stop('Mismatch between CNN scores and ROI images, check autoid folder inputs!')
          #     }


              img_tmp <- magick::image_read(rois[ii])
              imgdat <- magick::image_info(img_tmp)


              img <- magick::image_read(rois[ii], strip = FALSE) %>%
                magick::image_scale(scale) %>%
                magick::image_annotate(paste(categoryNames[i], "(roi.", unlist(vpr_roi(rois[ii])), ")"), color = "red", size = 12) %>%
                magick::image_annotate(text = paste("scoreCNN = ", round(scr_tmp, digits = 2)),
                                       location = "+0+20",
                                       color = "red") %>%
                magick::image_annotate(text = paste(round(imgdat$width/pxtomm, digits = 2), "x", round(imgdat$height/pxtomm, digits = 2), "mm"),
                                       location = "+0+10",
                                       color = "red")

          }

            if (img_bright == TRUE) {
              img_n <- magick::image_modulate(img, brightness = 500)

              img_f <- magick::image_append(c(img, img_n))

              print(img_f)

            } else{
              print(img)
            }


            #pop up menu
            ans <-
              menu(
                choices = c('Yes', 'No'),
                graphics = gr,
                title = paste(
                  "Is the classification, ",
                  categoryNames[i],
                  ", accurate? (y/n)"
                )
              )

            if (ans == 1) {


            } else{
              # original method
              # sink(file = paste0(day_hour,'/misclassified_', categoryNames[i], '.txt'), append = TRUE)
              # cat(aid_dat[[ii]], '\n')
              # sink()

              if(missing(path_score)){
                misclassified <- c(misclassified, aid_dat[[ii]])
              }else{
                misclassified <- c(misclassified, aid_dat_t[[ii]]) # if threshold has been applied
                # TODO test that this is pulling correctly
              }


              # update to create generic category options
              # EC 2019 October 30
              ans <-
                menu(c(allcategory),
                     graphics = gr,
                     title = "Appropriate Category Classification?")

              if(missing(path_score)){
              reclassified[[ans]] <-
                c(reclassified[[ans]], aid_dat[[ii]])
              }else{ # if threshold has been applied
                # TODO double check this pulling
                reclassified[[ans]] <- c(reclassified[[ans]],
                                         aid_dat_t[ii])
              }

              # original method
              # sink(file = paste0(day_hour,'/reclassify_', allcategory[[ans]], '.txt'), append = TRUE)
              # cat(aid_dat[ii], '\n')
              # sink()

            }


          }

          # Write information to file
          # sink(
          #   file = paste0(day_hour, '/misclassified_', categoryNames[i], '.txt'),
          #   append = T
          # )
          withr::with_output_sink(paste0(dirpath, '/misclassified_', categoryNames[i], '.txt'),
                                  append = TRUE,
                                  code = {
                                    cat(misclassified, sep = '\n')
                                  })
          #sink()

        }# skip = TRUE loop (category)
      }# skip = TRUE loop (dayhr)

      if (SKIP == TRUE) {
        # creates blank misclassified file if category of interest is not present in specified hour (so images reclassified as this category will be moved)
        # sink(
        #   file = paste0(day_hour, '/misclassified_', categoryNames[i], '.txt'),
        #   append = TRUE
        # )
        # sink()
        withr::with_output_sink(paste0(dirpath, '/misclassified_', categoryNames[i], '.txt'),
                                append = TRUE,
                                code = {
                                  cat('\n')
                                })
      }
    }

    # Write reclassified files for each category
    for (i in seq_len(length(reclassified))) {
      category_id <- names(reclassified[i])
      recl_tmp <- reclassified[[i]]

      # Make a reclassify file only for category that need to be reclassified
      if (length(recl_tmp != 0)) {
        # sink(
        #   file = paste0(day_hour, '/reclassify_', category_id, '.txt'),
        #   append = TRUE
        # )
        withr::with_output_sink(paste0(dirpath, '/reclassify_', category_id, '.txt'), append = TRUE, code = {
          cat(recl_tmp, sep = '\n')
        })
        # sink()

      }

    }

  }

vpr_autoid_create <- function(reclassify, misclassified, basepath, day, hour, mea = TRUE, categories) {
  #' Modifies aid and aid mea files based on manual reclassification
  #' @author E. Chisholm
  #'
  #' @param reclassify list of reclassify files (output from vpr_manual_classification())
  #' @param misclassified list misclassify files (output from vpr_manual_classification())
  #' @param basepath path to folder containing autoid files (e.g., 'extdata/COR2019002/autoid')
  #' @param day day identifier for relevant aid & aidmeas files
  #' @param hour hour identifier for relevant aid & aidmeas files
  #' @param mea logical indicating whether or not there are accompanying measurement files to be created
  #' @param categories A list object with all the potential classification categories
  #'
  #' @examples
  #' \dontrun{
  #' basepath <- 'E:/autoID_EC_07032019/'
  #' day <- '289'
  #' hr <- '08'
  #' categories <-
  #' c("bad_image_blurry", "bad_image_malfunction", "bad_image_strobe", "Calanus", "chaetognaths",
  #' "ctenophores", "krill", "marine_snow", "Other", "small_copepod", "stick")
  #' day_hour_files <-  paste0('d', day, '.h', hr)
  #' misclassified <- list.files(day_hour_files, pattern = 'misclassified_', full.names = TRUE)
  #' reclassify <- list.files(day_hour_files, pattern = 'reclassify_', full.names = TRUE)
  #' vpr_autoid_create(reclassify, misclassified, basepath, categories)
  #' }
  #' @export

  . <- day <- hour <- NA
  # get day and hour values
  day_misclassified <- unique(vpr_day(misclassified))[[1]]
  hour_misclassified <- unique(vpr_hour(misclassified))[[1]]

  if(length(day_misclassified) != 1 | length(hour_misclassified) != 1) {
    stop("MULTIPLE DAYS OR HOURS AMONG MISCLASSIFIED FILES, PLEASE CORRECT.")
  }

  categoryNames <- list.files(basepath)
  categoryFolders <- sort(list.files(basepath, full.names = TRUE))
  misclassified <- sort(misclassified)

  # loop through misclassified folders
  for (i in seq_len(length(misclassified))) {

    if (length(categoryFolders) != length(misclassified)) {
      stop('NUMBER OF MISCLASSIFIED FILES MUST == NUMBER OF CATEGORIES IN BASEPATH')
    }

    # get category value and check
    category <- vpr_category(misclassified[i], categories = categoryNames)
    categoryFolder <- grep(categoryFolders[i], pattern = category, value = TRUE)

    categoryFolder_check <- gsub(pattern = basepath, replacement = "", categoryFolder)
    categoryFolder_check <- gsub(pattern = "/", replacement = "", categoryFolder_check)

    if (category != categoryFolder_check) {
      stop('CATEGORIES ARE BEING CONFUSED - INVESTIGATE')
    }

    if (!category %in% categoryNames) {
      stop(paste(category, "is not a valid category name. Please run vpr_category_create() to create proper file structure within basepath"))
    }
    # read in misclassified ROIs
    mis_roi <- readLines(misclassified[i])

    if(unique(mis_roi)[[1]] == "") {

      mis_roi <- as.character()

    }

    if (length(mis_roi) != 0) { # if there are ROIs that were misclassified
      day_hour <- unique(substr(sub(mis_roi, pattern = ".*d",
                                    replacement = "d"), 1, 8))
      day_hour <- gsub(pattern = "\\\\", replacement = ".",
                       x = day_hour)
      mis_roi_gen <- unlist(vpr_roi(mis_roi))
      mis_roi_df <- data.frame(mis_roi_gen, day_hour, category,
                               stringsAsFactors = FALSE)
      aidFolder <- list.files(categoryFolder, pattern = "^aid$",
                              full.names = TRUE)
      mis_roi_df <- mis_roi_df %>% dplyr::group_by(., day_hour)
      if (length(unique(mis_roi_df$day_hour)) > 1) {
        stop("MULTIPLE HOURS IN ONE FILE, PLEASE CORRECT.")
      }
    }else { # if there were no ROIs misclassified
      print(paste("Blank misclassified file found for",
                  category, "!"))
      day_n <- vpr_day(misclassified[i])
      hr_n <- vpr_hour(misclassified[i])
      day_hour <- paste(day_n, hr_n, sep = ".")
      aidFolder <- list.files(categoryFolder, pattern = "^aid$",
                              full.names = TRUE)
    }
    # read in original aid file
    aids <- list.files(aidFolder, full.names = TRUE)
    # Ensure backward compatibility with files that do not have .txt extension
    # EO 12/12/24
    aid_list_old_fn <- grep(aids, pattern = paste0(day_hour, "$|", day_hour, "\\.txt$"), value = TRUE)
    if (length(aid_list_old_fn) == 0) { # create a dummy file if no aid exists
      aid_list_old_fn <- paste0(aidFolder, "/dummy_aid.", day_hour, ".txt")  # Changed to .txt
      withr::with_output_sink(aid_list_old_fn, code = {
        cat("\n")
      })
      print(paste("DUMMY FILE CREATED FOR", category, " : ", aid_list_old_fn))
      DUMMY = TRUE
      aid_new <- NULL
    } else {
      aid_list_old <- readLines(aid_list_old_fn)
      aid_list_old <- unique(aid_list_old)
      aid_old_gen <- unlist(vpr_roi(aid_list_old))

      if (length(mis_roi) == 0) { # if nothing was misclassified, pull original aid through
        aid_new <- aid_list_old
      } else {
          # remove duplicates
        sub_mis_roi <- mis_roi_df %>% dplyr::filter(.,
                                                    day_hour == unique(mis_roi_df$day_hour))
        mm <- aid_old_gen %in% sub_mis_roi$mis_roi_gen # KS solution 2023/09/27 - fixed ROI duplication error
        ind <- grep(mm, pattern = "TRUE")
        aid_new <- aid_list_old[-ind] # remove misclassified ROIs
        cat(paste(">>>>", length(ind), "ROIs removed from",
                  category, "in", unique(day_hour), "\n>>>> File:",
                  aid_list_old_fn, "\n"))
      }
      DUMMY <- FALSE
    }
    # fix matching meas file
    if (mea == TRUE) {
      aidMeaFolder <- list.files(categoryFolder, pattern = "^aidmea$",
                                 full.names = TRUE)
      aidMeaFile <- list.files(aidMeaFolder, pattern = paste0("*", day_hour, ".txt"), full.names = TRUE)  # Changed to .txt EO 12/12/24
      if (length(aidMeaFile) == 0) {
        aidMeaFile <- paste0(aidMeaFolder, "/dummy_aid.mea.", day_hour, ".txt")  # Changed to .txt EO 12/12/24
        withr::with_output_sink(aidMeaFile, code = {
          cat("\n")
        })
        print(paste("DUMMY FILE CREATED FOR MEAS OF",
                    category, " : ", aidMeaFile))
        aidMea_new <- NULL
        DUMMY = TRUE
      }
      else {
        aidMea_old <- read.table(aidMeaFile)
        aidMea_old <- unique(aidMea_old)
        if (length(mis_roi) == 0) {
          aidMea_new <- aidMea_old
        }
        else {
          aidMea_new <- aidMea_old[-ind, ]
          cat(paste(">>>>", length(ind), "Measurements removed from",
                    category, "in", unique(day_hour), "\n>>>> File:",
                    aidMeaFile, "\n"))
        }
        DUMMY = FALSE
      }
    }

    # check for any ROIs to be reclassified
    reclassify_category <- grep(reclassify,
                            pattern = paste0('reclassify_',category, '.txt'), # FIX [EOG] - Should only pull exact matches to category, will be sensitive to file naming conventions CAUTION
                            value = TRUE)

    if (length(reclassify_category) == 0) { # nothing to be reclassified
      print(paste("No", category, "to be reclassified"))
      if (mea == TRUE) {
        aidMea_final <- aidMea_new
      }
      aid_final <- aid_new
      if (DUMMY == TRUE) {
        warning(print("No original data and no reclassified data, consider removing category."))
      }
    } else { # add ROIs which were reclassified to category
      recl_roi <- readLines(reclassify_category)
      day_hour_re <- paste(day, hour, sep = ".")
      recl_roi_gen <- unlist(vpr_roi(recl_roi))
      if (length(unique(day_hour_re)) > 1) {
        stop(paste(reclassify_category, "has more than one unique hour value!\n
                   Please double check file."))
      }
      recl_roi_df <- data.frame(recl_roi_gen, day_hour_re,
                                recl_roi, stringsAsFactors = FALSE)
      recl_roi_df <- recl_roi_df %>% dplyr::filter(., !duplicated(recl_roi_gen))
      aid_final <- c(aid_new, recl_roi_df$recl_roi)
      cat(paste(">>>>", length(recl_roi_df$recl_roi),
                "ROIs added to", category, "in", unique(day_hour),
                "\n"))
      aid_fn_list <- list()
      for (l in seq_len(length(categoryFolders))) {
        all_aids <- list.files(file.path(categoryFolders[[l]],
                                         "aid"), full.names = TRUE)
        aid_fn_list[[l]] <- grep(all_aids, pattern = day_hour,
                                 value = TRUE)
      }
      # fix matching meas file
      if (mea == TRUE) {
        aidm_fn_list <- list()
        for (l in seq_len(length(categoryFolders))) {
          all_aidms <- list.files(file.path(categoryFolders[[l]],
                                            "aidmea"), full.names = TRUE)
          aidm_fn_list[[l]] <- grep(all_aidms, pattern = day_hour,
                                    value = TRUE)
        }
        roimeas_dat_combine <- vpr_autoid_read(file_list_aid = unlist(aid_fn_list),
                                               file_list_aidmeas = unlist(aidm_fn_list), export = "aidmeas",
                                               station_of_interest = NA, warn = FALSE)
        recl_roi_num <- recl_roi_df$recl_roi_gen
        recl_roi_meas <- roimeas_dat_combine[roimeas_dat_combine$roi %in%
                                               recl_roi_num, ]
        if (length(recl_roi_meas$roi_ID) > length(recl_roi)) {
          print(paste("Warning, duplicate ROI detected! Removing automatically"))
          print(recl_roi_meas[duplicated(recl_roi_meas$roi_ID),
          ])
          recl_roi_meas <- recl_roi_meas %>% dplyr::filter(.,
                                                           !duplicated(recl_roi_meas$roi_ID))
        }
        col_names <- c("Perimeter", "Area",
                       "width1", "width2", "width3",
                       "short_axis_length", "long_axis_length")
        recl_roi_meas <- recl_roi_meas %>% dplyr::select(.,
                                                         col_names)
        aidMea_list <- list()
        for (iii in 1:7) {
          aidMea_list[[iii]] <- c(aidMea_new[, iii],
                                  unname(recl_roi_meas[, iii]))
        }
        aidMea_final <- data.frame(matrix(unlist(aidMea_list),
                                          ncol = length(aidMea_list)))
        cat(paste(">>>>", length(recl_roi_meas$Perimeter),
                  "Measurements added to", category, "in",
                  unique(day_hour), "\n"))
        if (length(recl_roi_meas$Perimeter) != length(recl_roi_df$recl_roi_gen)) {
          warning("Measurements and ROI numbers in reclassification do not match!!!")
        }
      }
    }
    # write new_autoid files
    dirpath <- file.path("new_autoid", category[[1]])
    dir.create(dirpath, showWarnings = FALSE, recursive = TRUE)
    if (mea == TRUE) {
      aidMea_final_nm <- paste0("new_aid.mea.", unique(day_hour), '.txt')
      aidMea_final_fn <- file.path(dirpath, "aidmea",
                                   aidMea_final_nm)
      dir.create(file.path(category, "aidmea"), showWarnings = FALSE,
                 recursive = TRUE)
      write.table(file = aidMea_final_fn, aidMea_final,
                  sep = "    ", quote = FALSE, col.names = FALSE,
                  row.names = FALSE)
      cat(paste(">>>> New aidmea file created for",
                category, "in", unique(day_hour), "\n"))
    }
    aid_final_nm <- paste0("new_aid.", unique(day_hour), '.txt')
    aid_final_fn <- file.path(dirpath, "aid", aid_final_nm)
    dir.create(file.path(dirpath, "aid"), showWarnings = FALSE,
               recursive = TRUE)

    #aid_final <- sub(" .*", "", aid_final) #####


    write.table(file = aid_final_fn, aid_final, quote = FALSE,
                col.names = FALSE, row.names = FALSE)
    cat(paste(">>>> New aid file created for",
              category, "in", unique(day_hour), "\n"))
    if (DUMMY == TRUE & mea == TRUE) {
      atf <- grep(aid_list_old_fn, pattern = "dummy")
      amtf <- grep(aidMeaFile, pattern = "dummy")
      if (length(atf) != 0 & length(amtf) != 0) {
        print(paste("Deleting dummy files!"))
        print(paste(aidMeaFile, " & ", aid_list_old_fn))
        unlink(aid_list_old_fn)
        unlink(aidMeaFile)
      }
    }

    if (DUMMY == TRUE & mea == FALSE) {
      atf <- grep(aid_list_old_fn, pattern = "dummy")
      if (length(atf) != 0) {
        print(paste("Deleting dummy file!"))
        print(aid_list_old_fn)
        unlink(aid_list_old_fn)
      }
    }

  }
}

vpr_category_create <- function(category, basepath) {
  #' Create a new category to be considered for classification after processing with VP
  #'
  #' creates empty directory structure to allow consideration of new category during vpr_manual_classification()
  #'
  #' @param category new category name to be added (can be a list of multiple category names)
  #' @param basepath path to folder containing autoid files (e.g., 'extdata/COR2019002/autoid')
  #'
  #' @return empty directory structure using new category name inside basepath
  #' @export
  #'
  #'
  #'
  #'
  for (i in seq_len(length(category))) {
    # create new category folder
    newcategorypath <- file.path(basepath, category[[i]])
    dir.create(newcategorypath)

    # create blank aid and aidmeas folders

    dir.create(paste0(newcategorypath, '/aid'), showWarnings = FALSE)
    dir.create(paste0(newcategorypath, '/aidmea'), showWarnings = FALSE)

  }
}
