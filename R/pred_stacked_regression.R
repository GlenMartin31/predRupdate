#' Perform Stacked Regression on Existing Prediction Models
#'
#' This function takes a set of existing prediction models, and uses the new
#' dataset to combine/aggregate them into a single 'meta-model', as described in
#' Debray et al. 2014.
#'
#' @param x an object of class "\code{predinfo}" produced by calling
#'   \code{\link{pred_input_info}} containing information on at least two
#'   existing prediction models.
#'
#' @param positivity_constraint TRUE/FALSE denoting if the weights within the
#'   stacked regression model should be constrained to be non-negative (TRUE) or
#'   should be allowed to take any value (FALSE). See details
#'
#' @details The aim of this function is to take a set of (previously estimated)
#'   prediction models that were each originally developed for the same
#'   prediction task, and pool/aggregate these into a single prediction model
#'   (meta-model) using stacked regression based on new data (data not used to
#'   develop any of the existing models). The methodological details can be
#'   found in Debray et al. 2014.
#'
#'   Given that the existing models are likely to be highly co-linear (given
#'   that they were each developed for the same prediction task), it has been
#'   proposed to impose a positivity constraint on the weights of the stacked
#'   regression model (Debray et al. 2014.). This is not strictly needed, so the
#'   use of this constraint can be controlled using
#'   \code{positivity_constraint}. If \code{positivity_constraint} is set to
#'   TRUE, then the stacked regression model will be estimated by optimising the
#'   (log-)likelihood of a logistic regression model using bound constrained
#'   optimization ("L-BFGS-B" in \code{stats::optim()}).
#'
#' @return A object of class "\code{predSR}". This is a list containing the
#'   information/estimates about the meta-model obtained by stacked regression.
#'   Specifically, \itemize{ \item{Stacked_Regression_Model = the results of the
#'   stacked regression equation} \item{Aggregated_Coefs = a data.frame of the
#'   pooled/aggregated coefficients of each covariate across the existing
#'   prediction models} \item{pred_info = the original "\code{predinfo}" object,
#'   as passed to \code{x}}}
#'
#' @examples
#' model_input <- pred_input_info(model_type = "logistic",
#'                                model_info = SYNPM$Existing_models,
#'                                newdata = SYNPM$ValidationData,
#'                                binary_outcome = "Y")
#' SR <- pred_stacked_regression(model_input)
#' print(SR)
#' summary(SR)
#' #one could then validate this as follows (but note this should be adjusted for in-sample optimism)
#' pred_validate(pred_input_info(model_type = "logistic",
#'                               model_info = SR$Aggregated_Coefs,
#'                               newdata = SYNPM$ValidationData,
#'                               binary_outcome = "Y"))
#'
#' @references Debray, T.P., Koffijberg, H., Nieboer, D., Vergouwe, Y.,
#'   Steyerberg, E.W. and Moons, K.G. (2014), Meta-analysis and aggregation of
#'   multiple published prediction models. \emph{Statistics in Medicine}, 33:
#'   2341-2362
#'
#' @export
pred_stacked_regression <- function(x, positivity_constraint = FALSE, ...) {
  UseMethod("pred_stacked_regression")
}


#' @export
pred_stacked_regression.default <- function(x, positivity_constraint = FALSE, ...) {
  stop("'x' is not of class 'predinfo'; please see pred_input_info()",
       call. = FALSE)
}


#' @export
pred_stacked_regression.predinfo_logistic <- function(x, positivity_constraint = FALSE, ...) {

  #Check outcomes were inputted into predinfo object
  if (is.null(x$Outcomes)) {
    stop("Observed outcomes must be supplied in newdata to agregate the existing models. Recall pred_input_info() with outcome specified.",
         call. = FALSE)
  }
  #Check that multiple models are included in predinfo object
  if (x$M == 1) {
    stop("Multiple existing models should be included in predinfo object. Recall pred_input_info() with multiple models inputted.",
         call. = FALSE)
  }

  predictions <- predRupdate::pred_predict(x)

  SR_dat <- data.frame(x$Outcomes,
                       do.call(cbind.data.frame,
                               lapply(predictions,
                                      function(X) X[names(X)=="LinearPredictor"])))
  names(SR_dat) <- c("Y", paste("LP", 1:x$M, sep = ""))

  if (positivity_constraint == FALSE) {

    SR <- stats::glm(Y ~ .,
                     data = SR_dat,
                     family = stats::binomial(link = "logit"))

    alpha <- coef(SR)

  } else {

    likelihood.fn <- function(alpha, y, LP){
      #input: alpha = the vector of weights for the stacked regression
      #       LP = the matrix of linear predictors from each simulated model
      #       y = the vector of outcomes

      SR <- LP%*%alpha

      joint <- -sum((y%*%log(1+exp(-SR)))+((1-y)%*%log(1+exp(SR)))) #log-Likelihood of logistic regression
      joint
    }

    bl <- c(-Inf, rep(0.00000001,x$M)) #lower bound on parameters - intercept is un-restricted, other terms are non-negative
    bu <- c(rep(Inf,x$M+1)) #upper bound on parameters
    start <- c(0, rep(1/x$M, x$M)) #initial guess for weights- each model assigned equal weighting

   LinPreds <- data.matrix(SR_dat[,-which(names(SR_dat) == "Y")])
   LinPreds <- cbind(rep(1, dim(LinPreds)[1]), LinPreds) #Add intercept into the design matrix

    MLE <- stats::optim(start, likelihood.fn,
                        y = SR_dat$Y,
                        LP = LinPreds,
                        method = "L-BFGS-B",
                        lower=bl,
                        upper=bu,
                        control = list("fnscale" = -1)) #Optimise the likelihood function under parameter restrictions
    if (MLE$convergence != 0) {
      warning("Stacked regression under positivity contraint failed to converge")
    }

    alpha <- MLE$par #parameter estimates
    alpha <- ifelse(abs(alpha)<0.000001, 0, alpha)

  }

  #Convert the results of stacked regression into the pooled model coefficients:
  coef_long <- do.call(rbind, lapply(x$coefs, stack))
  coef_long$model <- rep(1:length(x$coefs), times = sapply(x$coefs, length))
  coef_table <- stats::reshape(coef_long,
                               direction = "wide",
                               idvar = "model",
                               timevar = "ind",
                               v.names = "values",
                               sep = "-")
  names(coef_table) <- sub('values-', '', names(coef_table))
  coef_table <- coef_table[,-which(names(coef_table) == "model")]
  coef_table[is.na(coef_table)] <- 0
  for (m in 1:x$M) {
    coef_table[m,] <- coef_table[m,] * as.numeric(alpha[m+1]) #m+1 due to intercept
  }
  coef_table <- colSums(coef_table)
  coef_table["Intercept"] <- coef_table["Intercept"] + as.numeric(alpha[1])

  #Return results and set S3 class
  SR_results <- list("Stacked_Regression_Weights" = alpha,
                     "Aggregated_Coefs" = data.frame(as.list(coef_table)),
                     "pred_info" = x)
  class(SR_results) <- c("predSR")
  SR_results
}


#' @export
pred_stacked_regression.predinfo_survival <- function(x, positivity_constraint = FALSE, ...){
  stop("Stacked regression for models of type='survival' are not currently supported",
       call. = FALSE)
}


#' @export
summary.predSR <- function(x, ...) {
  cat(paste("Stacked regression applied to", x$pred_info$M, "existing models",
            paste("of type '", x$pred_info$model_type, "' \n", sep = ""), sep = " "))
  cat("The pooled coefficients are: \n")
  print(x$Aggregated_Coefs)
}

#' @export
print.predSR <- function(x, ...) {
  print(list(x$Stacked_Regression_Weights,
             x$Aggregated_Coefs))
}
