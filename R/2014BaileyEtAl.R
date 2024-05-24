#### Bailey et al. (2014)
# Based on the contents of Pseudo-Mathematics and Financial Charlatanism: The Effects of Backtest Overfitting on Out-of-Sample Performance (Bailey el al., 2014).


### Minimum Backtest Length (MinBTL) ----
#' Minimum Backtest Length
#'
#' Estimates the Minimum Backtest Length (MinBTL) and its upper bound for a matrix of strategy returns or a number of strategies and a IS Sharpe ratio.
#' If a matrix (m) is being provided, the function uses its column number as the number of strategies and the maximum Sharpe ratio across its columns as the IS Sharpe ratio. If no matrix is being provided (m = NULL), the arguments N and SR_hat are being used instead.
#' @param m A matrix of returns.
#' @param rfr The risk-free (rate of) return. Only used if m != NULL.
#' @param annualize_by The factor used to annualized returns. Only used if m != NULL.
#' @param na.rm logical. Should missing values be removed when estimating the Sharpe ratios? Only used if m != NULL.
#' @param N The number of strategies/parameter combinations tested. Only used if m = NULL.
#' @param SR_hat The IS Sharpe ratio for which the MinBTL should be estimated. Only used if m = NULL.
#' @return The Minimum Backtest Length (MinBTL) and its upper bound (UpperBound).
#' @examples
#' data(IS)
#' MinBTL(IS[, 2:6], rfr = 0, annualize_by = 1, na.rm = FALSE)
#' MinBTL(m = NULL, n = 10, SR_hat = 1.5)
#' @export
MinBTL <- function(m = NULL, rfr = 0, annualize_by = 1, na.rm = FALSE, N, SR_hat) {
  if (!is.null(m)) {
    N <- ncol(m) # Number of strategies
    SR_hat <- max(apply(m, 2, SharpeRatio, rfr = rfr, annualize_by = annualize_by, na.rm = na.rm))
  }

  if (N < 5) {
    return("N is too small. N must be >> 1.")
  }

  eul_masch <- -digamma(1)
  E_max <- (1 - eul_masch) * qnorm(p = 1 - (1 / N)) + eul_masch * qnorm(p = 1 - (exp(-1)/N))

  MinBTL <- (E_max / SR_hat)^2
  UpperBound <- (2 * log(N, base = exp(1))) / (SR_hat^2)

  res <- c(MinBTL, UpperBound)
  names(res) <- c("MinBTL", "UpperBound")

  return(res)
}

