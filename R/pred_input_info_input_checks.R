# Internal function for pred_input_info() ---------------------------------------
pred_input_info_input_checks <- function(model_type,
                                         model_info,
                                         cum_hazard) {

  #check that model_info is provided as a data.frame
  if (inherits(model_info, "data.frame") == FALSE) {
    stop("'model_info' should be a data.frame", call. = FALSE)
  }
  #Check that each 'cell' of model_info data.frame is a number
  if (all(sapply(model_info, is.numeric)) == FALSE) {
    stop("All columns of 'model_info' should be numeric", call. = FALSE)
  }

  if (model_type == "logistic") {
    #check that 'model_info' contains an intercept column for logistic models
    if ("Intercept" %in% names(model_info) == FALSE) {
      stop("When model_type=logistic, then 'model_info' should contain a column named 'Intercept'",
           call. = FALSE)
    }

    #check that the intercept column is the first one
    if (names(model_info)[[1]] != "Intercept"){
      stop("When model_type=logistic, then first column of 'model_info' should be 'Intercept'",
           call. = FALSE)
    }

    if (!is.null(cum_hazard)) {
      stop("'cum_hazard' should be set to NULL if model_type=logistic",
           call. = FALSE)
    }

  } else if (model_type == "survival") {
    #check baseline hazard specifications
    if (nrow(model_info) > 1) {
      if ((inherits(cum_hazard, "list") == FALSE) |
          (length(cum_hazard) != nrow(model_info) )) {
        stop("If multiple existing models entered, and model_type = survival, then 'cum_hazard' should be supplied as list of length equal to number of models",
             call. = FALSE)
      }

      for(m in 1:nrow(model_info)) {
        if (!is.null(cum_hazard[[m]])){
          if (inherits(cum_hazard[[m]], "data.frame") == FALSE |
              ncol(cum_hazard[[m]]) !=2) {
            stop("all supplied baseline hazards should be a data.frame of two columns",
                 call. = FALSE)
          }
          if (names(cum_hazard[[m]])[1] != "time" |
              names(cum_hazard[[m]])[2] != "hazard") {
            stop("all supplied baseline hazards should be a data.frame with columns being 'time' and 'hazard'",
                 call. = FALSE)
          }
          if(sum(duplicated(cum_hazard[[m]][,1])) > 0){
            stop("all supplied baseline hazard times must be unique",
                 call. = FALSE)
          }
          if(min(cum_hazard[[m]][,1]) <= 0){
            stop("all supplied baseline hazard times must be positive",
                 call. = FALSE)
          }
          if(min(cum_hazard[[m]][,2]) < 0){
            stop("all supplied baseline hazards must be nonnegative",
                 call. = FALSE)
          }
        }
      }

    } else {
      if (!is.null(cum_hazard)){
        if (inherits(cum_hazard, "data.frame") == FALSE |
            ncol(cum_hazard) !=2) {
          stop("baseline hazard should be a data.frame of two columns",
               call. = FALSE)
        }
        if (names(cum_hazard)[[1]] != "time" |
            names(cum_hazard)[[2]] != "hazard") {
          stop("baseline hazard should be a data.frame with columns being 'time' and 'hazard'",
               call. = FALSE)
        }
        if(sum(duplicated(cum_hazard[,1])) > 0){
          stop("all baseline hazard times must be unique",
               call. = FALSE)
        }
        if(min(cum_hazard[,1]) <= 0){
          stop("all baseline hazard times must be positive",
               call. = FALSE)
        }
        if(min(cum_hazard[,2]) < 0){
          stop("all baseline hazards must be nonnegative",
               call. = FALSE)
        }
      }
    }
  }
}
