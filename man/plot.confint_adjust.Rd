% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/confint_adjust-plot.R
\name{plot.confint_adjust}
\alias{plot.confint_adjust}
\title{Plot \code{confint_adjust} x}
\usage{
\method{plot}{confint_adjust}(x, parm, mar = c(5.1, 7.1, 4.1, 2.1), line = mar[2] - 1, ...)
}
\arguments{
\item{x}{An \code{confint_adjust} x produced by the
\code{\link[api2lm]{confint_adjust}} function.}

\item{parm}{a specification of which parameters are to be given
    confidence intervals, either a vector of numbers or a vector of
    names.  If missing, all parameters are considered.}

\item{mar}{A numerical vector of the form c(bottom, left,
top, right) which gives the number of lines of margin
to be specified on the four sides of the plot. The
default is c(5, 7, 4, 2) + 0.1.}

\item{line}{The MARgin line, starting at 0 counting
outwards, to draw the y-axis label. The default is 1
unit less than \code{mar[2]}.}

\item{...}{Additional arguments passed to \code{plot}.}
}
\value{
None.
}
\description{
Plot a \code{confint_adjust} x produced by the
\code{\link[api2lm]{confint_adjust}} function. See
Examples.
}
\details{
The \code{plot} function doesn't automatically adjust the
margins to account for the label names. If you need more
space for your labels, then increase the second element
of \code{mar} from 7.1 upward and line upward.
Alternatively, if you need less space, then you can
decrease both of these values. Or you could use the
\code{autoplot} function that automatically controls the
spacing.
}
\examples{
fit <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars)
# standard intervals
cia <- confint_adjust(fit)
plot(cia)
# plot subset of intervals
plot(cia, parm = c("hp", "disp"))
# adjust margin and line for better formatting
plot(cia, parm = 2:3, mar = c(5.1, 4.1, 4.1, 2.1))
}
\author{
Joshua P. French
}
