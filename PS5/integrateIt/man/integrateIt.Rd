% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/intergrateIt.R
\name{integrateIt}
\alias{integrateIt}
\alias{integrateIt,}
\alias{ANY-method}
\title{Integrating functions}
\usage{
integrateIt(x_in, f_x, range, rule)
}
\arguments{
\item{x_in}{A vector of input values}

\item{f_x}{A function to generate evaluated values y based on input x_in}

\item{range}{Starting and ending values}

\item{rule}{Method of integration: Trapezoid or Simpson}
}
\value{
An object of class Simpson or Trapezoid containing
\item \code{integral} {the numerical approximation of the definite integral}
\item \code{x} {a vector of values}
\item \code{y} {a vector of evaluated values based on some function}
}
\description{
Integrates function via Trapezoidal and Simpson's Rule
}
\note{
This produces an object of a class Trapezoid or Simpson
}
\examples{

integrateIt(1:10, function(x){x+1}, c(2,5), rule = "Trapezoid")
integrateIt(1:10, function(x){x+1}, c(2,5), rule = "Simpson")
integrateIt(seq(1, 10, 0.1), function(x){x+1}, c(2,5), rule = "Simpson")


}
\seealso{
\code{\link{Trapezoid}}, \code{\link{Simpson}}
}
\author{
Cecilia Y. Sui
}
