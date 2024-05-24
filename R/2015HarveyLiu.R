#### Harvey and Liu (2015)
# Based on the contents of Backtesting (Harvey and Liu, 2012).



### Bonferroni adjustment ----
#' Bonferroni adjustment
#'
#' Adjusts a vector of p-values based on Bonferroni's method.
#' @param pvals A vector of p-values.
#' @return The Bonferroni-adjusted vector of p-values.
#' @examples
#' data(IS)
#'
#' TT <- ncol(IS[, 2:6])
#' SR_hat <- apply(IS[, 2:6], 2, SharpeRatio)
#' t_ratios <- SR_hat * sqrt(TT)
#' plot(sort(t_ratios), type = "l")
#'
#' pvals <- 1 - unlist(lapply(t_ratios, pt, df = TT - 1))
#' pvals
#'
#' BonfAdj(pvals)
#' @export
BonfAdj <- function(pvals) {
  return(pmin(pvals * length(pvals), 1))
}


### Holm's adjustment ----
#' Holm's adjustment
#'
#' Adjusts a vector of p-values based on Holm's method.
#' @param pvals A vector of p-values.
#' @return The Holm-adjusted vector of p-values.
#' @examples
#' data(IS)
#'
#' TT <- ncol(IS[, 2:6])
#' SR_hat <- apply(IS[, 2:6], 2, SharpeRatio)
#' t_ratios <- SR_hat * sqrt(TT)
#' plot(sort(t_ratios), type = "l")
#'
#' pvals <- 1 - unlist(lapply(t_ratios, pt, df = TT - 1))
#' pvals
#'
#' HolmAdj(pvals)
#' @export
HolmAdj <- function(pvals) {
  M <- length(pvals)
  indizes <- order(pvals, decreasing = FALSE)
  pvals_adj <- numeric(length = M)
  pvals_adj[indizes[1]] <- min(pvals[indizes[1]] * M, 1)
  for (i in 2:M) {
    pvals_adj[indizes[i]] <- max(
      pvals_adj[indizes[1:(i - 1)]],
      min(pvals[indizes[i]] * (M - i + 1), 1)
    )
  }

  return(pvals_adj)
}


### BHY adjustment ----
#' Benjamini, Hochberg and Yekutieli (BHY)'s adjustment
#'
#' Adjusts a vector of p-values based on BHY's method.
#' @param pvals A vector of p-values.
#' @return The BHY-adjusted vector of p-values.
#' @examples
#' data(IS)
#'
#' TT <- ncol(IS[, 2:6])
#' SR_hat <- apply(IS[, 2:6], 2, SharpeRatio)
#' t_ratios <- SR_hat * sqrt(TT)
#' plot(sort(t_ratios), type = "l")
#'
#' pvals <- 1 - unlist(lapply(t_ratios, pt, df = TT - 1))
#' pvals
#'
#' BHYAdj(pvals)
BHYAdj <- function(pvals) {
  M <- length(pvals)
  indizes <- order(pvals, decreasing = FALSE)
  c <- sum(1 / (1:M))
  pvals_adj <- numeric(length = M)
  pvals_adj[indizes[M]] <- pvals[indizes[M]]
  for (i in (M - 1):1) {
    pvals_adj[indizes[i]] <- min(
      pvals_adj[indizes[i + 1]],
      ((M * c) / i) * pvals[indizes[i]]
    )
  }

  return(pvals_adj)
}
