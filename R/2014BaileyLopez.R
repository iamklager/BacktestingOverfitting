#### Bailey and Lopez (2014)
# Based on the contents of The Deflated Sharpe Ratio: Correcting for Selection Bias, Backtest Overfitting and Non-Normality (Bailey and Lopez, 2014).


### Deflated Sharpe Ratio ----
#' Deflated Sharpe ratio
#'
#' Computes the Deflated Sharpe ratio (DSR)for a matrix of strategy returns or a number of strategies, a IS Sharpe ratio and the IS Sharpe ratio's standard deviation..
#' If a matrix (m) is being provided, the function uses its column number as the number of strategies and the maximum Sharpe ratio across its columns as the IS Sharpe ratio. If no matrix is being provided (m = NULL), the arguments N, SR_hat and sd_SR are being used instead.
#' @param m A matrix of returns.
#' @param rfr The risk-free (rate of) return. Only used if m != NULL.
#' @param annualize_by The factor used to annualized returns. Only used if m != NULL.
#' @param na.rm logical. Should missing values be removed when estimating the Sharpe ratios? Only used if m != NULL.
#' @param N The number of strategies/parameter combinations tested. Only used if m = NULL.
#' @param SR_hat The IS Sharpe ratio for which the MinBTL should be estimated. Only used if m = NULL.
#' @param sd_SR The IS Sharpe ratio's standard deviation. Only used if m = NULL.
#' @return The Deflated Sharpe ratio (DSR).
#' @examples
#' data(IS)
#' DSR(IS[, 2:6])
#' @export
DSR <- function(m, rfr = 0, SR_bench = 0, annualize_by = 1, na.rm = FALSE, N, SR_hat, sd_SR) {
  if (!is.null(m)) {
    N <- ncol(m)
    SR_hat <- apply(m, 2, SharpeRatio, rfr = rfr, annualize_by = annualize_by, na.rm = na.rm)
    sd_SR <- sd(SR_hat)
  }

  if (N < 5) {
    return("N is too small. N must be >> 1.")
  }

  eul_masch <- -digamma(1)
  E_max <- (1 - eul_masch) * qnorm(p = 1 - (1 / N)) + eul_masch * qnorm(p = 1 - (exp(-1)/N))

  SR0_hat <- sd_SR * max(SR_hat) * E_max
  DSR <- apply(m, 2, PSR, SR_bench = SR0_hat, annualize_by = annualize_by, na.rm = na.rm)

  if (!is.null(colnames(m))) {
    names(DSR) <- colnames(m)
  }

  return(PSR_hat)
}
