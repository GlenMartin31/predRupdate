pm_predict_logistic <- function(existingcoefs,
                                DM) {
  #Input checking
  if (ncol(DM) != length(existingcoefs)) {
    stop("Existing coefficients of the model and new data to make predictions on are non-conformable",
         call. = FALSE)
  }
  #Calculate the linear predictor
  LP <- as.numeric(DM %*% existingcoefs)
  #Map to predicted risks
  PR <- pmupdate::inv_logit(LP)

  predictions <- list("LP" = LP,
                      "PR" = PR)
}
