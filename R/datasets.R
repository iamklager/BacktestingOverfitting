#### Datasets
# The datasets included in the package.


#### S&P 500 index ----
#' S&P 500 index price data
#'
#' Price data of the S&P 500 index between the 2012-12-19 and 2023-12-31.
#'
#' @docType data
#'
#' @usage data(SP500)
#'
#' @format An object of class \code{"cross"}; see \code{\link[qtl]{read.cross}}.
#'
#' @keywords datasets
#'
#' @source \href{https://finance.yahoo.com/quote/%5EGSPC/}{yahoo! finance}
#'
#' @examples
#' data(SP500)
#' plot(SP500$Adjusted, type = "l")
"SP500"


#### Strategy returns ----
#' Strategy returns
#'
#' Returns of 100 momentum strategies applied to the SP500 using different parameter combinations.
#'
#' @docType data
#'
#' @usage data(STRATEGIES)
#'
#' @format An object of class \code{"cross"}; see \code{\link[qtl]{read.cross}}.
#'
#' @keywords datasets
#'
#' @examples
#' data(STRATEGIES)
#'
#' apply(STRATEGIES, 2, SharpeRatio, rfr = 0.07)
"STRATEGIES"


#### In-sample strategy returns ----
#' In-sample strategy returns
#'
#' In-sample returns of 100 momentum strategies applied to the SP500 using different parameter combinations.
#'
#' @docType data
#'
#' @usage data(IS)
#'
#' @format An object of class \code{"cross"}; see \code{\link[qtl]{read.cross}}.
#'
#' @keywords datasets
#'
#' @examples
#' data(IS)
#'
#' apply(IS, 2, SharpeRatio, rfr = 0.07)
"IS"


#### Out-of-sample strategy returns ----
#' Out-of-sample strategy returns
#'
#' Out-of-sample returns of 100 momentum strategies applied to the SP500 dataset using different parameter combinations.
#'
#' @docType data
#'
#' @usage data(OOS)
#'
#' @format An object of class \code{"cross"}; see \code{\link[qtl]{read.cross}}.
#'
#' @keywords datasets
#'
#' @examples
#' data(OOS)
#'
#' apply(OOS, 2, SharpeRatio, rfr = 0.07)
"OOS"

