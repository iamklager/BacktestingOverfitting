#### Basic functions
# Functions not specific to backtesting overfitting.


### Sharpe ratio ----
#' Sharpe ratio
#'
#' Computes the Sharpe ratio (SR) for a vector of returns.
#' @param r A vector of returns.
#' @param rfr The risk-free rate of return.
#' @param annualize_by The factor used to annualized returns.
#' @param na.rm logical. Should missing values be removed?
#' @return The (annualized) Sharpe ratio.
#' @examples
#' SharpeRatio(rnorm(252), 0, 1)
#' @export
SharpeRatio <- function(r, rfr = 0, annualize_by = 1, na.rm = FALSE) {
  return(
    sqrt(annualize_by)*(mean(x = r - rfr, na.rm = na.rm)/sd(x = r, na.rm = na.rm))
  )
}
