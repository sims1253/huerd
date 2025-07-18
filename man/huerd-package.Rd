% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/huerd-package.R
\docType{package}
\name{huerd-package}
\alias{huerd}
\alias{huerd-package}
\title{huerd: Constrained Color Palette Generation with Aesthetic Guidance}
\description{
The huerd package provides tools for generating perceptually distinct color
palettes with constraints. The main feature is the ability to specify fixed
colors that must be included in the final palette unchanged. Generated
colors are optimized for distinctness and CVD safety, and can optionally
adhere to the aesthetic "vibe" (Lightness and Chroma profile) of the fixed colors.
}
\section{Main Functions}{

\describe{
\item{\code{\link[=generate_palette]{generate_palette()}}}{Generate a color palette with constraints.}
\item{\code{\link[=evaluate_palette]{evaluate_palette()}}}{Evaluate the quality of a color palette.}
\item{\code{\link[=simulate_palette_cvd]{simulate_palette_cvd()}}}{Simulate how a palette appears under CVD.}
\item{\code{\link[=is_cvd_safe]{is_cvd_safe()}}}{Quick check for CVD safety against a threshold.}
}
}

\section{Color Spaces}{

The package uses the OK LAB color space by default for its perceptual uniformity.
Euclidean distance in OK LAB corresponds well to perceived color differences.
A Just Noticeable Difference (JND) in OK LAB is approximately 0.02.
}

\section{Optimization Method}{

\code{huerd} treats palette generation as a box-constrained optimization problem,
handled by the \code{nloptr} package. The goal is to find an arrangement of
colors that maximizes a score based on perceptual distance, CVD safety, and
optional penalties for aesthetic or gamut deviations.
}

\seealso{
Useful links:
\itemize{
  \item \url{https://github.com/sims1253/huerd}
  \item Report bugs at \url{https://github.com/sims1253/huerd/issues}
}

}
\author{
\strong{Maintainer}: Maximilian Scholz \email{dev.scholz@mailbox.org}

}
\keyword{internal}
