#' fit a model with given X and y with
#' precision lasso method
#'
#'
#' @param X  auto correlation structure
#' @param y  dimension of the data
#' @param logistic indicator for doing regression on binary response
#' @param method choose the method: lasso, ridge or elasticnet
#' @return coefficients generated
#'
#' @examples
#'  PSlasso(X, y, logistic)
#'
#' @export
#'


glmreg = function(X, y, logistic = TRUE, method = c("lasso", "ridge", "elasticnet")){

  alpha = switch(method,
                 lasso = 1,
                 ridge = 0,
                 elasticnet = 0.5)

  if(logistic){
    dist = "binomial"
  }else{
    dist = "gaussian"
  }

  fit = cv.glmnet(X, y, family = dist, alpha = alpha, nlambda = 100, intercept=F)

  small.lambda.betas <- coef(fit, s = "lambda.min")

  return(small.lambda.betas[-1])
}
