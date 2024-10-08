% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EC_functions.R
\name{vpr_oce_create}
\alias{vpr_oce_create}
\title{Create ctd oce object with vpr data}
\usage{
vpr_oce_create(data)
}
\arguments{
\item{data}{data frame of vpr data}
}
\description{
Formats VPR data frame into \code{oce} format CTD object
}
\examples{
data('ctd_roi_merge')
oce_dat <- vpr_oce_create(ctd_roi_merge)

}
\author{
E. Chisholm
}
