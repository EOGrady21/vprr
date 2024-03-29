% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EC_functions.R
\name{vpr_plot_contour}
\alias{vpr_plot_contour}
\title{Interpolated contour plot of particular variable}
\usage{
vpr_plot_contour(
  data,
  var,
  dup = "mean",
  method = "interp",
  labels = TRUE,
  bw = 1,
  cmo
)
}
\arguments{
\item{data}{data frame needs to include time_hr, depth, and variable of
choice (var)}

\item{var}{variable in dataframe which will be interpolated and plotted}

\item{dup}{if method == 'interp'. Method of handling duplicates in interpolation, passed to interp function (options: 'mean', 'strip', 'error')}

\item{method}{Specifies interpolation method, options are 'interp' or
'oce', oce uses slightly different method (oce is least error prone)}

\item{labels}{logical value indicating whether or not to plot contour labels}

\item{bw}{bin width defining interval at which contours are labelled}

\item{cmo}{name of a \code{cmocean} plotting theme, see \code{?cmocean} for more information}
}
\description{
Creates interpolated contour plot, can be used as a background for ROI or tow yo information
}
\author{
E. Chisholm & Kevin Sorochan
}
