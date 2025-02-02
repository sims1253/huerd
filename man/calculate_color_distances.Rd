% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/color_distances.R
\name{calculate_color_distances}
\alias{calculate_color_distances}
\title{Calculate distances between colors}
\usage{
calculate_color_distances(points)
}
\arguments{
\item{points}{A matrix of LAB coordinates where each row represents a color
and columns represent L, A, and B values respectively.}
}
\value{
A list containing two elements:
\item{avg}{The average perceptual distance between all valid
color pairs.}
\item{min}{The minimum perceptual distance found between any two
valid color pairs.}
}
\description{
This function computes perceptual distances between all color pairs in a
given set of colors. It converts LAB coordinates to RGB, checks for valid
RGB colors, and calculates the color distance using the color_distance
function.
}
\details{
The function skips calculations for invalid RGB colors (values
outside 0-255 range) and assigns a distance of 0 to such pairs.
It also excludes fixed base colors from the calculations.
}
\keyword{internal}
