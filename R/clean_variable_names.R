# Internal function for cleaning variable names of user inputed data.frames ----
clean_variable_names <- function(string_vector,
                                 pattern) {

  #remove any white space in leading or trailing spaces of the string
  string_vector <- base::trimws(x = string_vector)

  #replace the invalid characters with an underscore
  string_vector <- base::gsub(pattern = pattern,
                              replacement = "_",
                              x = string_vector,
                              perl = TRUE)
  string_vector
}
