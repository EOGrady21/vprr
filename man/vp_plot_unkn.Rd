% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EC_functions.R
\name{vp_plot_unkn}
\alias{vp_plot_unkn}
\title{Function to visualize losses to unknown category due to disagreement in Dual classifier}
\usage{
vp_plot_unkn(cm, classes, threshold = 0, summary = TRUE, sample_size = NULL)
}
\arguments{
\item{cm}{dual unknown confusion matrix from VP}

\item{classes}{category groups in order, from VP}

\item{threshold}{minimum value which will be labelled in plot}

\item{summary}{logical to add text summary to plot
E. Chisholm May 2019}

\item{sample_size}{character string describes the sample size used to train the model being plotted (optional)}
}
\description{
Makes confusion matrix like plot, where x axis represent SVM classification, y axis represent NN classification
Allows visual summary of data lost to unknown category
}
