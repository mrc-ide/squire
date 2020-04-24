##' Create an explicit model
##'
##' @title Explicit SEEIR model creation.
##'
##' We will use this structure to ensure that model fitting is flexible in the
##' future as more models are added
##'
##' @export
explicit_model <- function() {

  model_class <- "explicit_SEEIR_model"
  compare_model <- function(model, pars_obs, data) {
    compare_output(model, pars_obs, data, type=model_class)
  }

  explicit_model <- list(odin_model = explicit_SEIR,
                         generate_beta_func = beta_est_explicit,
                         parameter_func = parameters_explicit_SEEIR,
                         run_func = run_explicit_SEEIR_model,
                         compare_model = compare_model)
  class(explicit_model) <- c(model_class, "squire_model")
  explicit_model

}

##' Create a simple  model
##'
##' @title Simple SEEIR model creation.
##'
##' We will use this structure to ensure that model fitting is flexible in the
##' future as more models are added
##'
##' @export
simple_model <- function() {

  model_class <- "simple_SEEIR_model"
  compare_model <- function(model, pars_obs, data) {
    compare_output(model, pars_obs, data, type=model_class)
  }

  explicit_model <- list(odin_model = simple_SEIR,
                         generate_beta_func = beta_est_simple,
                         parameter_func = parameters_simple_SEEIR,
                         run_func = run_simple_SEEIR_model,
                         compare_model = compare_model)
  class(explicit_model) <- c(model_class, "squire_model")
  explicit_model

}



# Loads a model by its name
# Must be in inst/odin
load_odin_model <- function(x) {
  if (inherits(x, "squire_model")) {
    return(x)
  }
  path <- system.file("odin", package = "squire", mustWork = TRUE)
  possible <- sub("\\.json$", "", dir(path, pattern = "\\.json$"))
  if (x %in% possible) {
    env <- asNamespace("explicit_SEIR")
    model <- get(x, envir = env, mode = "function", inherits = FALSE)
  } else {
    stop("Unknown model: ", x)
  }

  model
}
