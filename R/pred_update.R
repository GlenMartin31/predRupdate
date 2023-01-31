# test
pred_update <- function(x, # needs to be pred_info() object
                        model_type = c("logistic", "survival"),
                        update_type = c("recalibration"),
                        recal_type){

  if (class(x)[2] != "predinfo") {
    stop("'x' is not of class 'predinfo'; please see pred_input_info()",
         call. = FALSE)
  }

  model_type <- match.arg(model_type, several.ok = FALSE)
  if (model_type  == "survival") {
    stop("model_type = 'survival' not yet supported for 'pred_update'",
         call. = FALSE)
  }


  if (update_type  == "recalibration") {
    if (x$M == 1) {
      #Gather information from the predinfo blueprint:
      existingcoefs <- as.numeric(x$coefs)
      DM <- stats::model.matrix(x$formula, x$PredictionData) # removed x$formula from this as not sure its needed
      UD <- x$PredictionData
      #Double-check dimensions:
      if (ncol(DM) != length(existingcoefs)) {
        stop("Existing coefficients of the model and new data to make predictions on are non-conformable",
             call. = FALSE)
      }
      #Calculate the linear predictor
      LP <- as.numeric(DM %*% existingcoefs)
      #return results
      updateinfo <- list("LinearPredictor" = LP,
                         "UpdateData" = UD,
                         "Outcomes" = x$Outcomes)

      #Run model update
      fit <- glm(updateinfo$Outcomes ~ updateinfo$LinearPredictor, family = binomial())
      param <- data.frame(cbind(fit$coefficients, confint(fit)))
      rownames(param)[2] <- c("Slope")
      #Update old coefficients
      updatedcoefs <- c(existingcoefs[1] + param[1,1], existingcoefs[-1] * param[2,1])
      updatedcoefs <- setNames(updatedcoefs, x$coef_names)

      ##glm(updateinfo$Outcomes ~ offset(updateinfo$LinearPredictor), family = binomial())

      #Return results and set class
      update_results <- list("Update_parameters" = param,
                             "Updated_Coefs" = data.frame(as.list(updatedcoefs)),
                             "pred_original" = x)

      class(update_results) <- c("predUpdate") # Do we also want to make it predinfo object or have function to convert to pred info
      update_results

    } else{
      stop("M > 1,'pred_update' currently only supports updating a single model",
           call. = FALSE)
    }

  }

}

summary.predUpdate <- function(x, ...) {
  cat(paste("Stacked regression applied to", x$pred_info$M, "existing models",
            paste("of type '", x$pred_info$model_type, "' \n", sep = ""), sep = " "))
  cat("The updated coefficients are: \n")
  print(x$Updated_Coefs)
}

print.predUpdate <- function(x, ...) {
  print(list("Update_params" = x$Update_parameters,
             "Updated_Coefs" = x$Updated_Coefs))
}
