% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/color_utility.R
\name{eccentricity_factor}
\alias{eccentricity_factor}
\title{Calculate eccentricity factor for given hue angle}
\usage{
eccentricity_factor(h)
}
\arguments{
\item{h}{Numeric hue angle in degrees [0, 360)}
}
\value{
Numeric eccentricity factor (typically between 1.0 and 1.2714)
}
\description{
Computes the eccentricity factor which accounts for the non-uniformity
of color perception across different hue angles.
}
\examples{
eccentricity_factor(45)
}
\keyword{internal}
