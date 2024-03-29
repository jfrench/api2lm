% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rloo.lm.R
\name{rloo.lm}
\alias{rloo.lm}
\alias{rdeleted.lm}
\alias{rjackknife.lm}
\title{Compute leave-one-out residuals for `lm` objects.}
\usage{
\method{rloo}{lm}(
  model,
  infl = stats::lm.influence(model, do.coef = FALSE),
  res = infl$wt.res,
  ...
)

\method{rdeleted}{lm}(
  model,
  infl = stats::lm.influence(model, do.coef = FALSE),
  res = infl$wt.res,
  ...
)

\method{rjackknife}{lm}(
  model,
  infl = stats::lm.influence(model, do.coef = FALSE),
  res = infl$wt.res,
  ...
)
}
\arguments{
\item{model}{a fitted model object from the \code{\link[stats]{lm}} function.}

\item{infl}{influence structure as returned by \code{\link[stats]{lm.influence}}.}

\item{res}{(possibly weighted) residuals, with proper default.}

\item{...}{Currently unimplemented}
}
\description{
\code{rloo.lm} computes the leave-one-out residuals of
the \code{lm} object stored in \code{model}. \cr\cr
\code{rjackknife.lm} and \code{rdeleted.lm} are aliases
for \code{rloo.lm}.
}
\details{
Let \eqn{\hat{\epsilon}_i} denote the residual of the
\eqn{i}th observation and \eqn{h_i} denote the leverage
value of the \eqn{i}th observation The leave-one-out residual for observation \eqn{i} is
computed as

\deqn{l_i = \frac{\hat{\epsilon}_i}{1-h_i}.}
}
\examples{
lmod <- lm(Girth ~ Height, data = trees)
rloo(lmod)
}
\author{
Joshua French
}
