logit <- function(x) {
  log(x / (1 - x))
}

expit <- function(x) {
  exp(x) / (1 + exp(x))
}
