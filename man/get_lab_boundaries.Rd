% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/color_utility.R
\name{get_lab_boundaries}
\alias{get_lab_boundaries}
\title{Get LAB color space boundaries}
\usage{
get_lab_boundaries(base_colors = NULL)
}
\arguments{
\item{base_colors}{An optional vector of hex color codes (e.g., "#FF0000").
If provided, these colors will influence the LAB space
boundaries. Default is NULL.}
}
\value{
A list containing three elements:
\item{L}{A numeric vector of length 2,
representing the min and max values for the L channel}
\item{a}{A numeric vector of length 2,
representing the min and max values for the a channel}
\item{b}{A numeric vector of length 2,
representing the min and max values for the b channel}
}
\description{
Determines valid boundaries for LAB color space, considering any base colors.
The boundaries ensure:
\itemize{
\item Colors remain in displayable RGB gamut
\item Base colors (if any) influence lightness, a, and b ranges
\item Sufficient space for color variation
}
}
\details{
If base colors are provided, the function calculates boundaries
that encompass these colors with some padding. If no base colors
are given, it uses default ranges that allow for a wide variety of
colors.
}
\keyword{internal}
