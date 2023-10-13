### BUG FIXES AND ENHANCEMENTS
* [ENHANCEMENT] Generalize vpr_category to better handle duplicate category names
* [BUG FIX] Check for missing category values can accept a list
* [ENHANCEMENT] Update vpr_autoid_check() to reflect better data checks and not touch original files
* [BUG FIX] Major bug fixed in vpr_autoid_create(), ROIs are now correctly pulled from misclassified & reclassified files and inserted into new aid files
* [ENHANCEMENT] vpr_autoid_copy() now has functionality for automated output from CNNs and option to limit copying by a CNN probability threshold (to only copy low confidence images for manual reclassification). Also updates copying method to improve speed. 
* [ENHANCEMENT] Update vpr_manual_classification() to include CNN automated classification functionality and print automated classification confidence and ROI numbers on images 
* [ENHANCEMENT] add organization structure options to vpr_autoid_copy()
* [BUG FIX] fix example in vpr_autoid_create()
* [ENHANCEMENT] Update CTD column naming flexibility and defaults, and include input validation for column names
* [ENHANCEMENT] Update required metadata in vpr_save() to specify start & stop latitude and longitude to match start & end time values
* [ENHANCEMENT] Lintr clean up of formatting

### CHANGES
* taxa replaced with category in all objects & instances
* update to v2 of pkgdown workflow in GH actions
* Remove unessecary hardcoding of variables to be added to oce-ctd object in vpr_oce_create()
* added data.table dependency for quicker handling of large tables
* add read_aid_cnn() function to smooth reading of automated classification output
* Removed references to Visual Plankton, no longer relevant
* implement tidy paths


# vprr 0.2.0
### BUG FIXES AND ENHANCEMENTS
* [ENHANCEMENT] CodeFactor suggested updates to style
* [BUG FIX] export vpr_manual_classification (Issue #23)
* [ENHANCEMENT] update processing method to be flexible enough to run prediction output from non-vp automated classification system
* [BUG FIX] consistency in variable naming for plotting depth/pressure (Issue #22)

### CHANGES
* Removed all reference to akima package (due to licensing issue brought to attention by CRAN)
* Migrate from Travis CI to GH Actions
* clarify directory structure and contain any file output to organized folders
* edits to vignette and documentation for clarity
* clarify time variable names and implement consistency in time processing (Issue #29)

# vprr 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* Initial release
