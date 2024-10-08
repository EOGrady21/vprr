% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EC_functions.R
\name{vpr_size_bin}
\alias{vpr_size_bin}
\title{Bin VPR size data}
\usage{
vpr_size_bin(data_all, bin_mea)
}
\arguments{
\item{data_all}{a VPR CTD and measurement dataframe from \code{\link{vpr_ctdroisize_merge}}}

\item{bin_mea}{Numerical value representing size of depth bins over which data will be combined, unit is metres, typical values range from 1 - 5}
}
\value{
a dataframe of binned VPR size data statistics including number of observations, median, interquartile ranges, salinity and pressure, useful for making boxplots
}
\description{
Calculates statistics for VPR measurement data in depth averaged bins for analysis and visualization
}
\examples{
\dontrun{
data('size_df_f')
vpr_size_bin(size_df_f, bin_mea = 5)
}




}
