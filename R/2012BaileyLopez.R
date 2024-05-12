#### Bailey and Lopez (2012)
# Based on the contents of The sharpe ratio efficient frontier (Bailey and Lopez, 2012).


### Probabilistic Sharpe Ratio ----
#' Probabilistic Sharpe ratio
#'
#' Computes the probabilistic Sharpe ratio (PSR) for a vector of returns.
#' @param r A vector of returns.
#' @param rfr The risk-free rate of return.
#' @param SR_bench A benchmark Sharpe ratio
#' @param annualize_by The factor used to annualized returns.
#' @param na.rm logical. Should missing values be removed?
#' @return The (annualized) Sharpe ratio.
#' @examples
#' data(STRATEGIES)
#' PSR(STRATEGIES[, 2], 0, 0, 1)
#' @export
PSR <- function(r, rfr = 0, SR_bench = 0, annualize_by = 1, na.rm = TRUE) {
  if (na.rm) {
    r <- na.omit(r)
  }
  n <- length(r)
  SR_hat <- SharpeRatio(r = r, rfr = rfr, annualize_by = annualize_by)
  gamma3_hat <- PerformanceAnalytics::skewness(x = r, method = "moment")
  gamma4_hat <- PerformanceAnalytics::kurtosis(x = r, method = "moment")
  q <- ( (SR_hat - SR_bench)*sqrt(n-1) ) / sqrt( 1 - gamma3_hat*SR_hat + ((gamma4_hat-1)/4)*(SR_hat^2) )
  PSR_hat <- pnorm(q = q, mean = 0, sd = 1)

  return(PSR_hat)
}


### Minimum Track Record Length ----
#' Minimum Track Record Length
#'
#' Computes the Minimum Track Record Length (MinTRL) for a vector of returns.
#' @param r A vector of returns.
#' @param rfr The risk-free rate of return.
#' @param SR_bench A benchmark Sharpe ratio
#' @param alpha The significance level.
#' @param annualize_by The factor used to annualized returns.
#' @param round_up Whether the resulting MinTRL should be rounded up or not.
#' @param na.rm logical. Should missing values be removed?
#' @return The (annualized) Sharpe ratio.
#' @examples
#' data(STRATEGIES)
#' MinTRL(STRATEGIES[, 2], 0, 0, 0.05, 1)
#' @export
MinTRL <- function(r, rfr = 0, SR_bench = 0, alpha = 0.05, annualize_by = 1, round_up = TRUE, na.rm = FALSE) {
  if (na.rm) {
    r <- na.omit(r)
  }
  if ( length(r) <= 30 ) {
    warning(paste0(
      "Only ", length(r), " observations provided. Results based upon 30 or less observations are not reliable!"
    ))
  }
  SR_hat <- SharpeRatio(r = r, rfr = rfr, annualize_by = annualize_by)
  gamma3_hat <- PerformanceAnalytics::skewness(x = r, method = "moment")
  gamma4_hat <- PerformanceAnalytics::kurtosis(x = r, method = "moment")
  Z_alpha <- qnorm(p = alpha, lower.tail = FALSE)
  MinTRL <- 1 + ( 1 - gamma3_hat*SR_hat + ((gamma4_hat-1)/4)(SR_hat^2) )( (Z_alpha/(SR_hat - SR_bench))^2 )
  MinTRL <- ifelse(round_up, round(MinTRL, 0), MinTRL)

  return(MinTRL)
}


### Sharpe Ratio Efficient Frontier ----
#' Sharpe Ratio Efficient Frontier
#'
#' Computes the Sharpe Ratio Efficient Frontier (SEF) for a vector of returns.
#' @param m A matrix of strategy returns.
#' @param rfr The risk-free rate of return.
#' @param annualize_by The factor used to annualized returns.
#' @param na.rm logical. Should missing values be removed?
#' @return The (annualized) Sharpe ratio.
#' @examples
#' data(STRATEGIES)
#' SEF(STRATEGIES[,-1], 0, 1)
#' @export
SEF <- function(m, rfr = 0, annualize_by = 1, na.rm = FALSE) {
  SR_hats <- apply(m, 2, SharpeRatio, annualize_by = annualize_by, na.rm = na.rm)
  PSR_hats <- apply(m, 2, PSR, SR_bench = 0, rfr = rfr, annualize_by = annualize_by, na.rm = na.rm)

  opt_strat <- which(PSR_hats == max(PSR_hats))
  if ( !is.null(colnames(m)) ) {
    opt_strat_names <- colnames(m)[opt_strat]
  } else {
    opt_strat_names <- opt_strat
  }
  opt_strat_name <-
    SEF <- list(
      Optimal_Strategy = opt_strat_names,
      PSR_hat  = PSR_hats[opt_strat],
      SR_hat   = SR_hats[opt_strat]
    )

  return(SEF)
}
