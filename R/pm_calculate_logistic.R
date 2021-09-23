pm_calculate_logistic <- function(formula,
                                  existingcoefs,
                                  newdata) {
  #Define a design matrix given the provided functional form of the existing
  #prediction model
  DM <- stats::model.matrix(formula, newdata)

  #Test if the names given in DM match
  if (all(names(existingcoefs) %in% colnames(DM)) == FALSE) {
    stop(paste(paste("Variable",
                     names(existingcoefs)[which(names(existingcoefs) %in%
                                                  colnames(DM) == FALSE)],
                     "in existingcoefs is not found in functional form of newdata. \n",
                     collapse = " "),
               "Check that elements in formula are also included in existingcoefs"))
  }

  #Ensure that order of existingcoefs matches the design matrix
  existingcoefs <- existingcoefs[colnames(DM)]

  #Calculate the linear predictor
  LP <- as.numeric(DM %*% existingcoefs)
  #Map to predicted risks
  PR <- pmupdate::inv_logit(LP)

  predictions <- list("LP" = LP,
                      "PR" = PR)
}
