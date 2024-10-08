% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clf_check_test.R
\name{vpr_manual_classification}
\alias{vpr_manual_classification}
\title{Function to check results of classification manually}
\usage{
vpr_manual_classification(
  day,
  hour,
  basepath,
  category_of_interest,
  gr = TRUE,
  scale = "x300",
  opticalSetting = "S2",
  img_bright = TRUE,
  threshold_score,
  path_score
)
}
\arguments{
\item{day}{day of interest in autoid (3 chr)}

\item{hour}{hour of interest in autoid (2 chr)}

\item{basepath}{path to folder containing autoid files (e.g., 'extdata/COR2019002/autoid')}

\item{category_of_interest}{list of category folders you wish you sort through}

\item{gr}{logical indicating whether pop up graphic menus are used (user preference - defaults to TRUE)}

\item{scale}{argument passed to \code{\link{image_scale}}, default = 'x300'}

\item{opticalSetting}{specifies optical setting of VPR, defining image frame
size, current options are 'S0', 'S1', 'S2' (default), 'S3', see further
info in details}

\item{img_bright}{logical value indicating whether or not to include a blown
out high brightness version of image (can be helpful for viewing dark field
fine appendages)}

\item{threshold_score}{(optional) a numeric value defining the minimum confidence
value, under which automatic classifications will be passed through
manual reclassification. This argument should match the threshold
provided in \code{vpr_autoid_copy()}}

\item{path_score}{(optional) file path to the autoid_cnn_scr folder
(autoid files with confidence values produced by automated
classification)}
}
\description{
Displays each image in day hour specified,
prompts user to confirm or deny classification.
If classification is denied, asks for a reclassification
value based on available category
}
\details{
Optical Setting frame sizes: S0 = 7x7 mm, S1 = 14x14mm, S2 =
24x24mm, S3 = 48x48 mm. These settings define the conversion factor from
pixels to millimetres and calculate image size for classification
reference
}
\section{Development}{

\itemize{
\item Add "undo" functionality to go back on a typing mistake
\item  Fix scaling/ size issue so images are consistently sized
}
}

