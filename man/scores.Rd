% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sandwich-estimator.R
\name{scores}
\alias{scores}
\title{Extract the empirical estimation function}
\usage{
scores(x, ...)
}
\arguments{
\item{x}{A model object of class \code{bgw_mle}}

\item{...}{Additional arguments passed to methods}
}
\description{
This function extracts the empirical estimation function from a model object
of class \code{bgw_mle}. T
scores matrix from a model object. The scores matrix
contains the gradient observations and is used to calculate the robust
variance-covariance matrix.
}
