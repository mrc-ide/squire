#' Check dimensions of inputs
#'
#' @inheritParams run_SEEIR_model
#'
#' @return Null if checks pass
matrix_check <- function(population,
                         baseline_contact_matrix,
                         contact_matrix_set){

  dims <- c(length(population), dim(baseline_contact_matrix),
            sapply(contact_matrix_set, dim))
  if(length(unique(dims)) != 1){
    stop("Lengh of popualtion vector, dimensions of baseline_contact_matrix
         and dimensions of matrices in contact_matrix_set must all be equal")
  }
  return(NULL)
}


#' Check and set up initial values
#'
#' @inheritParams run_SEEIR_model
#'
#' @return Checked initial values data.frame
init_check <- function(init, population){
  if(is.null(init)){
    init = data.frame(
      S = population - 1,
      E = 0,
      E2 = 0,
      I = 1,
      R = 0
    )
  } else{
    if(!is.data.frame(init)){
      stop("init should be a data.frame with columns:, S, E, E2, I, R
           and rows 1:age_groups")
    }
    if(!identical(names(init) == c("S", "E", "E2", "I", "R"))){
      stop("Names of init must be identical to S, E, E2, I, R if sepecified manually")
    }
  }
  if(!identical(rowSums(init), population)){
    stop("Row sums of init should be identical to population")
  }
  return(init)
}

#' Check time change inputs are correct
#'
#' @param tt Time change points
#' @inheritParams run_SEEIR_model
#'
#' @return Nothing if check pass
check_time_change <- function(tt, time_period){
  if(any(tt > time_period) | any(tt < 0)){
    stop("Time change points must all be < time period
         and > 0")
  }
  return(NULL)
}

#' Check argument is a single positive numeric
#'
#' @param x argument
#' @param name Name of argument
#'
#' @return Nothing if check pass
pos_num <- function(x, name){
  if(length(x) != 1 | !is.numeric(x) | x < 0){
    stop(name, " must be a single positive number")
  }
  return(NULL)
}
