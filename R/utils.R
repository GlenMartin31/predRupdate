#' Apply a logit transformation to an input
#'
#' \code{logit} applies the logit transformation to convert a vector of values
#' between 0 and 1, to values between -Inf and Inf. Used to convert a
#' probability from a logistic regression model onto the linear predictor scale.
#'
#' @param p Numeric vector of probabilities (i.e. values between 0 and 1) that
#'   will be transformed.
#'
#' @return A numeric vector, with values between -Inf and Inf
#' @export
#'
#' @seealso \code{\link{inv_logit}}
#'
#' @examples
#' logit(0.5)
#' logit(c(0.1, 0.2, 0.3))
logit <- function(p) {
  if( any(p < 0 | p > 1) ) stop('p not between 0 and 1')
  log(p / (1 - p))
}


#' Apply the inverse logit function to an input
#'
#' \code{inv_logit} applies the inverse-logit transformation (expit/ logistic
#' function) to convert a vector of values between -Inf and Inf, to values
#' between 0 and 1. Used to convert the linear predictor of a logistic
#' regression model into a probability.
#'
#' @param x Numeric vector with values between -Inf and Inf.
#'
#' @return Numeric vector of probabilities (i.e. values between 0 and 1)
#' @export
#'
#' @seealso \code{\link{logit}}
#'
#' @examples
#' inv_logit(-2)
#' inv_logit(c(-2,-1,0,1,2))
inv_logit <- function(x) {
  1 / (1 + exp(-x))
}



#' Create dummy variables for all categorical/factor variables in a data.frame
#'
#' Create dummy/indicator variables for all categorical variables in a
#' data.frame. Can be used as a pre-processing step before calling other
#' functions within the package.
#'
#' @param df a data.frame on which to make dummy variables for each
#'   categorical/factor variable, based on contrasts.
#'
#' @return a data.frame matching \code{df} but where each categorical
#'   variable in \code{df} is replaced with indicator variables. All
#'   combinations of the indicator/dummy variable are returned. Naming
#'   convention of the new dummy variables is variable_level. For example, a
#'   factor variable in \code{df} named "colour" with levels "red", "green" and
#'   "purple" will be replaced with three columns (the new dummy variables),
#'   named colour_red,  colour_green and colour_purple.
#' @export
#'
#' @seealso \code{\link{pred_input_info}}
#'
#' @examples
#' dummy_vars(data.frame("Colour" = factor(sample(c("red",
#'                                                  "azure",
#'                                                  "green",
#'                                                  "white"),
#'                                               500,
#'                                               replace = TRUE))))
dummy_vars <- function(df) {

  #Check that df contains at least one factor variable
  if (all(sapply(df, is.factor)==FALSE)) {
    stop("data.frame contains no factor variables to convert to dummy variables")
  } else{
    for (j in names(df)[which(sapply(df, is.factor))]) {
      dummy_mat <- stats::model.matrix.lm(~-1 + df[[j]],
                                          na.action = "na.pass")
      #Note: above we keep NA's in the dummy variables; gives users flexibility
      #to decide how to handle in other functions

      #create sensible names
      colnames(dummy_mat) <- paste(j,
                                   sub(".*j\\]\\]", "", colnames(dummy_mat)),
                                   sep="_")
      df <- cbind(df, dummy_mat)
    }
    #remove factor variables (replaced by dummay vars)
    df <- df[,-which(sapply(df, is.factor))]
    df
  }
}
