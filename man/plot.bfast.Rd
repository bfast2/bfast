\name{plot.bfast}
\alias{plot.bfast}

\title{
Methods for objects of class "bfast".
}
\description{
Plot methods for objects of class "bfast".
}
\usage{
%%## S3 method for class 'bfast':
\method{plot}{bfast}(x, type = c("components", "all", "data", "seasonal",
"trend", "noise"), sim = NULL, largest=FALSE, main, ANOVA = FALSE, ...)
}

\arguments{
  \item{x}{ \code{\link[bfast]{bfast}} object }
  \item{type}{Indicates the type of plot. See details.}
  \item{sim}{Optional \code{\link[stats]{stl}} object containing the original components used when simulating \code{x}. }
  \item{largest}{If TRUE, show the largest jump in the trend component.}
  \item{ANOVA}{if TRUE Derive Slope and Significance values for each identified trend segment}
  \item{main}{an overall title for the plot.}
  \item{...}{further arguments passed to the \code{\link[graphics]{plot}} function.}
}

\details{
This function creates various plots to demonstrate the results of a bfast decomposition. 
The type of plot shown depends on the value of \code{type}.
\itemize{
\item{components}{Shows the final estimated components with breakpoints.}
\item{all}{Plots the estimated components and breakpoints from all iterations.}
\item{data}{Just plots the original time series data.}
\item{seasonal}{Shows the trend component including breakpoints.}
\item{trend}{Shows the trend component including breakpoints.}
\item{noise}{Plots the noise component along with its acf and pacf.}
}
If \code{sim} is not \code{NULL}, the components used in simulation are also shown on each graph.
}

\author{
Jan Verbesselt, Rob Hyndman and Rogier De Jong
}

\examples{
## See \code{\link[bfast]{bfast}} for examples.
}

\keyword{ts}

