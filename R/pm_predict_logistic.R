pm_calculate_logistic <- function(existingcoefs,
                                  DM) {
  #Calculate the linear predictor
  LP <- as.numeric(DM %*% existingcoefs)
  #Map to predicted risks
  PR <- pmupdate::inv_logit(LP)

  predictions <- list("LP" = LP,
                      "PR" = PR)
}
