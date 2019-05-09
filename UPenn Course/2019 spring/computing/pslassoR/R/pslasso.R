#' fit a model with given X and y with
#' precision lasso method
#'
#'
#' @param X  auto correlation structure
#' @param y  dimension of the data
#' @param logistic indicator for doing regression on binary response
#'
#' @return coefficients generated
#'
#' @examples
#'  PSlasso(X, y, logistic)
#'
#' @export
#'


PSlasso = function(X,y, logistic = TRUE){

  if(!is.vector(y)){
    y = as.vector(y)
  }

  models <- import_from_path("models", path = "/Users/w/Desktop/UPenn\ Course/2019\ spring/computing/")

  model = models$PrecisionLasso()

  model$setLogisticFlag(logistic)

  model$setLambda(1)

  model$setLearningRate(1e-6)

  model$calculateGamma(X) # Calculate gamma

  model$fit(X,y) # Calculate gamma

  H = model$getBeta()

  return(H)
}
