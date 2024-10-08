% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/2014BaileyLopez.R
\name{DSR}
\alias{DSR}
\title{Deflated Sharpe ratio}
\usage{
DSR(
  m,
  rfr = 0,
  SR_bench = 0,
  annualize_by = 1,
  na.rm = FALSE,
  N,
  SR_hat,
  sd_SR
)
}
\arguments{
\item{m}{A matrix of returns.}

\item{rfr}{The risk-free (rate of) return. Only used if m != NULL.}

\item{annualize_by}{The factor used to annualized returns. Only used if m != NULL.}

\item{na.rm}{logical. Should missing values be removed when estimating the Sharpe ratios? Only used if m != NULL.}

\item{N}{The number of strategies/parameter combinations tested. Only used if m = NULL.}

\item{SR_hat}{The IS Sharpe ratio for which the MinBTL should be estimated. Only used if m = NULL.}

\item{sd_SR}{The IS Sharpe ratio's standard deviation. Only used if m = NULL.}
}
\value{
The Deflated Sharpe ratio (DSR).
}
\description{
Computes the Deflated Sharpe ratio (DSR)for a matrix of strategy returns or a number of strategies, a IS Sharpe ratio and the IS Sharpe ratio's standard deviation..
If a matrix (m) is being provided, the function uses its column number as the number of strategies and the maximum Sharpe ratio across its columns as the IS Sharpe ratio. If no matrix is being provided (m = NULL), the arguments N, SR_hat and sd_SR are being used instead.
}
\examples{
data(IS)
DSR(IS[, 2:6])
}
