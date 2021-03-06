% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FitLandDF.R
\name{sdvar}
\alias{sdvar}
\alias{variance}
\alias{sdev}
\title{Get Standard Deviation/Variance of Values in Fitness Landscape}
\usage{
variance(x, ...)

sdev(x, ...)
}
\arguments{
\item{x}{FitLandDF object}

\item{...}{additional parameters (e.g. `na.rm`)}
}
\value{
variance or standard deviation of values in fitness landscape
}
\description{
Get Standard Deviation/Variance of Values in Fitness Landscape
}
\examples{
# create fitness landscape with non-zero variance and standard deviation
values <- array(1:27, dim = rep(3, 3))
my_landscape <- FitLandDF(values)

# calculate variance
variance(my_landscape)

# calculate standard deviation
sdev(my_landscape)

}
