#' Apply specified pre-processing steps/functions to a dataset
#'
#' @param newdata  data.frame which will be used to make predictions on using
#'   the existing prediction model. Variable names must match those in
#'   \code{existingcoefs} and \code{formula}, after applying any pre-processing
#'   steps (using \code{pre_processing}) where needed.
#' @param pre_processing a list where each element is a function that describes
#'   transformations to apply to columns of \code{newdata}.
#' @noRd
apply_pre_processing <- function(newdata,
                                 pre_processing) {
  #Run some input checks on structure of user-supplied 'pre_processing'
  if (!is.list(pre_processing)) {
    stop("'pre_processing' should be a list",
         call. = FALSE)
  }
  if (all(sapply(pre_processing,
                 is.function)) == FALSE){
    stop("'pre_processing' should be a list where each element of the list is a function",
         call. = FALSE)
  }

  #Apply the list of transformation/pre-processing steps to the dataset. Will
  #check that the user-supplied functions in 'pre_processing' can be evaluated
  #safely. Will also check that results from functions in 'pre_processing' are
  #of the correct length/dim to merge with newdata:
  transformed_vars <- tryCatch(
    {
      lapply(pre_processing, function(f) f(newdata))
    },
    error = function(cond){
      stop(paste("Some elements of pre_processing causes the following error: \n", cond),
           call. = FALSE)
    },
    warning = function(cond){
      stop(paste("Some elements of pre_processing causes the following warning: \n", cond),
           call. = FALSE)
    }
  )
  #Check if all of the functions within pre_processing return empty/no values,
  #then proceed without any pre-processing steps
  if (all(lapply(transformed_vars, length) == 0) == TRUE) {
    warning("All functions supplied in pre_processing return empty results - proceeding without pre-processing data.",
            call. = FALSE)
  } else{
    #check whether any of the functions within pre_processing return empty/no
    #values, returning a warning if so
    if (any(lapply(transformed_vars, length) == 0) == TRUE) {
      warning("Some functions supplied in pre_processing return empty result - removing these steps from pre-processing.",
              call. = FALSE)
    }

    #convert the results of the pre_processing functions to be a list where
    #each element is a transformed/pre-processed variable:
    transformed_vars <- lapply(rapply(transformed_vars, enquote, how="unlist"),
                               eval)
    #Test to make sure that each transformed/pre-processed variable is the
    #correct length to column-bind with newdata
    if (any(sapply(transformed_vars, length) != nrow(newdata))) {
      stop("Length of output returned by some elements of 'pre_processing' does not match nrow(newdata)",
           call. = FALSE)
    }
    #Merge the transformation/pre-processing variables into the dataset
    newdata <- cbind(newdata, transformed_vars)
  }
  newdata
}
