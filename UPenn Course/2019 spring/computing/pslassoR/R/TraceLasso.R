#' fit a model with given X and y
#'  with trace lasso method
#'
#'
#' @param X  auto correlation structure
#' @param y  dimension of the data
#' @param logistic indicator for using logistic or not
#'
#' @return coefficients generated
#'
#' @examples
#'  PSlasso(X, y, logistic = TRUE)
#'
#' @export
#'


library(reticulate)
use_python("/usr/local/bin/python2", required = FALSE)

TraceLasso = function(X,y, logistic = TRUE){

  model = models$PrecisionLasso()

  model$setLogisticFlag(logistic)

  model$setLambda(1)

  model$setLearningRate(1e-6)

  X = matrix(rnorm(100000), nrow = 100)

  y = rnorm(100)

  model$setGamma(1) # Calculate gamma

  model$fit(X,y) # Calculate gamma

  H = model$getBeta()

  return(H)
}
