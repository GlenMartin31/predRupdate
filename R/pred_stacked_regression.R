#' Perform Stacked Regression on Existing Prediction Models
#'
#' This function takes a set of existing prediction models, and uses the new
#' dataset to combine/aggregate them into a single 'meta-model', as described in
#' Debray et al. 2014.
#'
#' @param x an object of class "\code{predinfo}" produced by calling
#'   \code{\link{pred_input_info}} containing information on at least two
#'   existing prediction models.
#' @param positivity_constraint TRUE/FALSE denoting if the weights within the
#'   stacked regression model should be constrained to be non-negative (TRUE) or
#'   should be allowed to take any value (FALSE). See details.
#' @param new_data data.frame upon which the prediction models should be
#'   aggregated.
#' @param binary_outcome Character variable giving the name of the column in
#'   \code{new_data} that represents the observed binary outcomes (should be
#'   coded 0 and 1 for non-event and event, respectively). Only relevant for
#'   \code{model_type}="logistic"; leave as \code{NULL} otherwise. Leave as
#'   \code{NULL} if \code{new_data} does not contain any outcomes.
#' @param survival_time Character variable giving the name of the column in
#'   \code{new_data} that represents the observed survival times. Only relevant
#'   for \code{x$model_type}="survival"; leave as \code{NULL} otherwise.
#' @param event_indicator Character variable giving the name of the column in
#'   \code{new_data} that represents the observed survival indicator (1 for
#'   event, 0 for censoring). Only relevant for \code{x$model_type}="survival";
#'   leave as \code{NULL} otherwise.
#'
#' @details This function takes a set of (previously estimated) prediction
#'   models that were each originally developed for the same prediction task,
#'   and pool/aggregate these into a single prediction model (meta-model) using
#'   stacked regression based on new data (data not used to develop any of the
#'   existing models). The methodological details can be found in Debray et al.
#'   2014.
#'
#'   Given that the existing models are likely to be highly co-linear (since
#'   they were each developed for the same prediction task), it has been
#'   suggested to impose a positivity constraint on the weights of the stacked
#'   regression model (Debray et al. 2014.). If \code{positivity_constraint} is
#'   set to TRUE, then the stacked regression model will be estimated by
#'   optimising the (log-)likelihood using bound constrained optimization
#'   ("L-BFGS-B"). This is currently only implemented for logistic regression
#'   models (i.e., if \code{x$model_type}="logistic"). For survival models,
#'   positivity_constraint = FALSE.
#'
#'   \code{new_data} should be a data.frame, where each row should be an
#'   observation (e.g. patient) and each variable/column should be a predictor
#'   variable. The predictor variables need to include (as a minimum) all of the
#'   predictor variables that are included in the existing prediction models
#'   (i.e., each of the variable names supplied to
#'   \code{\link{pred_input_info}}, through the \code{model_info} parameter,
#'   must match the name of a variables in \code{new_data}).
#'
#'   Any factor variables within \code{new_data} must be converted to dummy
#'   (0/1) variables before calling this function. \code{\link{dummy_vars}} can
#'   help with this. See \code{\link{pred_predict}} for examples.
#'
#'   \code{binary_outcome}, \code{survival_time} and \code{event_indicator} are
#'   used to specify the outcome variable(s) within \code{new_data} (use
#'   \code{binary_outcome} if \code{x$model_type} = "logistic", or use
#'   \code{survival_time} and \code{event_indicator} if \code{x$model_type} =
#'   "survival").
#'
#' @return A object of class "\code{predSR}". This is the same as that detailed
#'   in \code{\link{pred_input_info}}, with the added element containing the
#'   estimates of the meta-model obtained by stacked regression.
#'
#' @examples
#' LogisticModels <- pred_input_info(model_type = "logistic",
#'                                   model_info = SYNPM$Existing_logistic_models)
#' SR <- pred_stacked_regression(x = LogisticModels,
#'                               new_data = SYNPM$ValidationData,
#'                               binary_outcome = "Y")
#' summary(SR)
#'
#' #Survival model example:
#' TTModels <- pred_input_info(model_type = "survival",
#'                             model_info = SYNPM$Existing_TTE_models,
#'                             cum_hazard = list(SYNPM$TTE_mod1_baseline,
#'                                                   SYNPM$TTE_mod2_baseline,
#'                                                   SYNPM$TTE_mod3_baseline))
#' SR <- pred_stacked_regression(x = TTModels,
#'                               new_data = SYNPM$ValidationData,
#'                               survival_time = "ETime",
#'                               event_indicator = "Status")
#' summary(SR)
#'
#' @references Debray, T.P., Koffijberg, H., Nieboer, D., Vergouwe, Y.,
#'   Steyerberg, E.W. and Moons, K.G. (2014), Meta-analysis and aggregation of
#'   multiple published prediction models. \emph{Statistics in Medicine}, 33:
#'   2341-2362
#'
#' @seealso \code{\link{pred_input_info}}
#'
#' @export
pred_stacked_regression <- function(x,
                                    positivity_constraint = FALSE,
                                    new_data,
                                    binary_outcome = NULL,
                                    survival_time = NULL,
                                    event_indicator = NULL) {
  UseMethod("pred_stacked_regression")
}


#' @export
pred_stacked_regression.default <- function(x,
                                            positivity_constraint = FALSE,
                                            new_data,
                                            binary_outcome = NULL,
                                            survival_time = NULL,
                                            event_indicator = NULL) {
  stop("'x' is not of class 'predinfo'; please see pred_input_info()",
       call. = FALSE)
}


#' @export
pred_stacked_regression.predinfo_logistic <- function(x,
                                                      positivity_constraint = FALSE,
                                                      new_data,
                                                      binary_outcome = NULL,
                                                      survival_time = NULL,
                                                      event_indicator = NULL) {

  #Check outcomes were inputted (needed to validate the model)
  if (is.null(binary_outcome)) {
    stop("binary_outcome must be supplied to aggregate the existing model(s)",
         call. = FALSE)
  }
  #Check that multiple models are included in predinfo object
  if (x$M == 1) {
    stop("Multiple existing models should be included in predinfo object. Recall pred_input_info() with multiple models inputted.",
         call. = FALSE)
  }

  #Make predictions within new_data using the existing prediction model(s)
  predictions <- predRupdate::pred_predict(x = x,
                                           new_data = new_data,
                                           binary_outcome = binary_outcome,
                                           survival_time = survival_time,
                                           event_indicator = event_indicator)

  #double-check all outcome columns identical across the M models (should be by definition)
  if((length(unique(lapply(predictions, function(X) X$Outcomes))) == 1) == FALSE){
    stop("Outcomes differ across models", call. = FALSE)
  }
  SR_dat <- data.frame(predictions[[1]]$Outcomes,
                       do.call(cbind.data.frame,
                               lapply(predictions,
                                      function(X) X[names(X)=="LinearPredictor"])))
  names(SR_dat) <- c("Y", paste("LP", 1:x$M, sep = ""))

  if (positivity_constraint == FALSE) {

    SR <- stats::glm(Y ~ .,
                     data = SR_dat,
                     family = stats::binomial(link = "logit"))

    alpha <- stats::coef(SR)

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
                        control = list("fnscale" = -1))
    if (MLE$convergence != 0) {
      warning("Stacked regression under positivity contraint failed to converge")
    }

    alpha <- MLE$par #parameter estimates

  }

  #Convert the results of stacked regression into the pooled model coefficients:
  coef_table <- x$model_info
  coef_table[is.na(coef_table)] <- 0
  for (m in 1:x$M) {
    coef_table[m,] <- coef_table[m,] * as.numeric(alpha[m+1]) #m+1 due to intercept
  }
  coef_table <- colSums(coef_table)
  coef_table["Intercept"] <- coef_table["Intercept"] + as.numeric(alpha[1])

  #Return results and set S3 class
  SR_results <- list("M" = 1,
                     "model_type" = x$model_type,
                     "coefs" = data.frame(as.list(coef_table)),
                     "coef_names" = names(coef_table),
                     "formula" = stats::as.formula(paste("~",
                                                         paste(names(coef_table)[
                                                           -which(names(coef_table)=="Intercept")
                                                         ],
                                                         collapse = "+"),
                                                         sep="")),
                     "model_info" = data.frame(as.list(coef_table)),
                     "Stacked_Regression_Weights" = alpha)

  class(SR_results) <- c("predSR", "predinfo_logistic", "predinfo")
  SR_results
}


#' @export
pred_stacked_regression.predinfo_survival <- function(x,
                                                      positivity_constraint = FALSE,
                                                      new_data,
                                                      binary_outcome = NULL,
                                                      survival_time = NULL,
                                                      event_indicator = NULL){
  #Check outcomes were inputted (needed to validate the model)
  if (is.null(survival_time) | is.null(event_indicator)) {
    stop("survival_time and event_indicator must be supplied to aggregate the existing model(s)",
         call. = FALSE)
  }
  #Check that multiple models are included in predinfo object
  if (x$M == 1) {
    stop("Multiple existing models should be included in predinfo object. Recall pred_input_info() with multiple models inputted.",
         call. = FALSE)
  }

  #Make predictions within new_data using the existing prediction model(s)
  predictions <- predRupdate::pred_predict(x = x,
                                           new_data = new_data,
                                           binary_outcome = binary_outcome,
                                           survival_time = survival_time,
                                           event_indicator = event_indicator)

  #double-check all outcome columns identical across the M models (should be by definition)
  if((length(unique(lapply(predictions, function(X) X$Outcomes))) == 1) == FALSE){
    stop("Outcomes differ across models", call. = FALSE)
  }
  SR_dat <- data.frame(predictions[[1]]$Outcomes,
                       do.call(cbind.data.frame,
                               lapply(predictions,
                                      function(X) X[names(X)=="LinearPredictor"])))
  names(SR_dat) <- c("Outcomes", paste("LP", 1:x$M, sep = ""))


  #Fit the SR cox model:
  SR <- survival::coxph(Outcomes ~ .,
                        data = SR_dat)
  beta <- SR$coefficients

  BH <- survival::basehaz(SR, centered = FALSE)
  cum_hazard <- data.frame("time" = BH$time,
                           "hazard" = BH$hazard)

  #Convert the results of stacked regression into the pooled model coefficients:
  coef_table <- x$model_info
  coef_table[is.na(coef_table)] <- 0
  for (m in 1:x$M) {
    coef_table[m,] <- coef_table[m,] * as.numeric(beta[m])
  }
  coef_table <- colSums(coef_table)

  #Return results and set S3 class
  SR_results <- list("M" = 1,
                     "model_type" = x$model_type,
                     "coefs" = data.frame(as.list(coef_table)),
                     "coef_names" = names(coef_table),
                     "formula" = stats::as.formula(paste("~",
                                                         paste(names(coef_table),
                                                         collapse = "+"),
                                                         sep="")),
                     "cum_hazard" = cum_hazard,
                     "model_info" = data.frame(as.list(coef_table)),
                     "Stacked_Regression_Weights" = beta)

  class(SR_results) <- c("predSR", "predinfo_survival", "predinfo")
  SR_results
}


#' @export
summary.predSR <- function(object, ...) {

  cat("Existing models aggregated using stacked regression")
  cat("\nThe model stacked regression weights are as follows: \n")
  print(object$Stacked_Regression_Weights)

  if(object$model_type == "survival"){

    cat("\nThe new model baseline cumulative hazard is: \n")
    if(nrow(object$cum_hazard) > 6){
      print(utils::head(object$cum_hazard, 6))
      cat("...\n")
    }else{
      print((object$cum_hazard))
    }

  }

  cat("\nUpdated Model Coefficients \n",
      "================================= \n", sep = "")
  print(object$coefs)

  cat("\nModel Functional Form \n",
      "================================= \n", sep = "")
  cat(as.character(object$formula)[2])

}

