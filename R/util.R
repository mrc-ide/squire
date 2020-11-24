#' @noRd
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

#' @noRd
is_ptr_null <- function(pointer){
  a <- attributes(pointer)
  attributes(pointer) <- NULL
  out <- identical(pointer, methods::new("externalptr"))
  attributes(pointer) <- a
  return(out)
}

#' @noRd
squire_file <- function(path) {
  system.file(path, package = "squire", mustWork = TRUE)
}

## Index locations of outputs in odin model
#' @noRd
odin_index <- function(model) {
  len <- length(model$.__enclos_env__$private$ynames)
  model$transform_variables(seq_len(len))
}


## Indices for cumulative cases total
#' @noRd
cases_total_index <- function(model) {

  index <- odin_index(model)
  indices <- c("IMild", "ICase1", "ICase2", "IOxGetLive1", "IOxGetLive2",
               "IOxGetDie1", "IOxGetDie2", "IOxNotGetLive1", "IOxNotGetLive2",
               "IOxNotGetDie1", "IOxNotGetDie2", "IMVGetLive1", "IMVGetLive2",
               "IMVGetDie1", "IMVGetDie2", "IMVNotGetLive1", "IMVNotGetLive2",
               "IMVNotGetDie1", "IMVNotGetDie2", "IRec1", "IRec2", "R", "D")
  return(unlist(index[indices]))
}

## Take odin state and calculate sum across ages in a replicate and vectorise
#' @noRd
odin_sv <- function(state, replicates, nt, reduce_age = TRUE) {
  if (reduce_age) {
    as.numeric(vapply(seq_len(replicates), function(x) {
      rowSums(state[,,x])
    }, FUN.VALUE = double(nt)))
  } else { # note: whole age-group results for single replicate produced, then next age-group etc
    as.numeric(vapply(seq_len(replicates), function(x) {
      state[, , x]
    }, FUN.VALUE = rep(double(nt), dim(state)[2])))
  }
}

odin_is_discrete <- function(x) {
  ## This could be determined by parsing the IR
  ##   odin::odin_ir(x, TRUE)$features$discrete
  ## but faster would be to do this:
  "step" %in% names(formals(x$run))
}
