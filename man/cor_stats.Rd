% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cor_stats.R
\name{cor_stats}
\alias{cor_stats}
\alias{print.cor_stats}
\title{Compute correlation matrix}
\usage{
cor_stats(x, ..., digits = 2, threshold = 0.7)

\method{print}{cor_stats}(x, ..., digits = 2, threshold = 0.7)
}
\arguments{
\item{x}{A `data.frame` or `lm` object.}

\item{...}{Currently unimplemented.}

\item{digits}{The number of decimal places to print. The
default is \code{2}.}

\item{threshold}{A threshold determining which
correlation statistics to print. Statistics above the
threshold are printed. The default is \code{0.7}.
Otherwise, a "\code{.}" is displayed in place of the
number.}
}
\description{
\code{cor_stats} creates a correlation matrix for an
object. The correlation is only computed for
\code{numeric} variables.
}
\details{
If \code{x} is a \code{data.frame} object, then the
correlation matrix is computed for all columns of
\code{x} that are \code{numeric}. If \code{x} is an
\code{lm} object, then the response variable column is
removed from \code{x$model} and the correlation matrix is
computed for all remaining columns that are
\code{numeric}.
}
\examples{
cor_stats(home_sales)
lmod <- lm(price ~ ., data = home_sales)
cstats <- cor_stats(lmod)
cstats
print(cstats, digits = 3, threshold = 0.5)
cstats_sum <- summary(cstats)
cstats_sum
print(cstats_sum, digits = 3, threshold = 0.5)
}
\author{
Joshua French
}
