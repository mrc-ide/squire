#' Check dimensions of inputs
#'
#' @inheritParams run_simple_SEEIR_model
#'
#' @return Null if checks pass
matrix_check <- function(population, contact_matrix_set){

  dims <- c(length(population),
            sapply(contact_matrix_set, dim))
  if(length(unique(dims)) != 1){
    stop("Length of population vector and dimensions of matrices
         in contact_matrix_set must all be equal")
  }
  return(NULL)
}

#' Check and set up initial values
#'
#' @inheritParams run_simple_SEEIR_model
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
    if(!all(names(init) == c("S", "E", "E2", "I", "R"))){
      stop("Names of init must be identical to S, E, E2, I, R if sepecified manually")
    }
  }
  if(!all(rowSums(init) == population)){
    stop("Row sums of init should be identical to population")
  }
  return(init)
}


#' Check and set up initial values for explicit model
#'
#' @inheritParams run_explicit_SEEIR_model
#'
#' @return Checked initial values data.frame
init_check_explicit <- function(init, population, seeding_cases = 20){

  if (length(population) != 17) {
    stop("population must be divided up into 17x 5-year age bands spanning 0 to 80+")
  }
  assert_int(seeding_cases)
  age_group_indices <- c(8, 9, 10, 11) # age_group indices corresponding to middle-aged travellers

  if(is.null(init)){
    raw_seeding_cases <- rep(0, length(population))
    raw_seeding_cases[age_group_indices] <- as.vector(stats::rmultinom(1, size = seeding_cases, prob = rep(0.25, 4)))
    init = data.frame(
      S = population - raw_seeding_cases,
      E1 = raw_seeding_cases,
      E2 = 0,
      IMild = 0,
      ICase1 = 0,
      ICase2 = 0,
      IOxGetLive1 = 0,
      IOxGetLive2 = 0,
      IOxGetDie1 = 0,
      IOxGetDie2 = 0,
      IOxNotGetLive1 = 0,
      IOxNotGetLive2 = 0,
      IOxNotGetDie1 = 0,
      IOxNotGetDie2 = 0,
      IMVGetLive1 = 0,
      IMVGetLive2 = 0,
      IMVGetDie1 = 0,
      IMVGetDie2 = 0,
      IMVNotGetLive1 = 0,
      IMVNotGetLive2 = 0,
      IMVNotGetDie1 = 0,
      IMVNotGetDie2 = 0,
      IRec1 = 0,
      IRec2 = 0,
      R = 0,
      D = 0
    )
  } else {
    if(!is.data.frame(init)){
      stop("init should be a data.frame with columns:
      S, E1, E2, ICase1, ICase2, IOxGetLive1, IOxGetLive2,
      IOxGetDie1, IOxGetDie2, IOxNotGetLive1, IOxNotGetLive2,
      IOxNotGetDie1, IOxNotGetDie2, IMVGetLive1, IMVGetLive2,
      IMVGetDie1, IMVGetDie2, IMVNotGetLive1, IMVNotGetLive2,
      IMVNotGetDie1, IMVNotGetDie2, IRec1, IRec2, R, D and rows 1:age_groups")
    }
    if(!all(names(init) == c("S","E1","E2","IMild","ICase1","ICase2","IOxGetLive1",
                             "IOxGetLive2","IOxGetDie1","IOxGetDie2",
                             "IOxNotGetLive1","IOxNotGetLive2","IOxNotGetDie1",
                             "IOxNotGetDie2","IMVGetLive1","IMVGetLive2",
                             "IMVGetDie1","IMVGetDie2","IMVNotGetLive1",
                             "IMVNotGetLive2","IMVNotGetDie1","IMVNotGetDie2",
                             "IRec1","IRec2","R","D"))){
      stop("If specified, names of init must be identical to:
      S, E1, E2, ICase1, ICase2, IOxGetLive1, IOxGetLive2,
      IOxGetDie1, IOxGetDie2, IOxNotGetLive1, IOxNotGetLive2,
      IOxNotGetDie1, IOxNotGetDie2, IMVGetLive1, IMVGetLive2,
      IMVGetDie1, IMVGetDie2, IMVNotGetLive1, IMVNotGetLive2,
      IMVNotGetDie1, IMVNotGetDie2, IRec1, IRec2, R, D")
    }
  }
  # cases randomly distributed across 4 age groups so can't check
  # whole population is equal by row. Instead do it for first 7 age
  # groups
  if(!all(rowSums(init[1:7, ]) == population[1:7])){
    stop("Row sums of init should be identical to population")
  }
  if(!all(init >= 0)) {
    stop("population size is not large enough in each age bracket")
  }

  return(init)
}

#' Check and set up initial values for vaccine model
#'
#' @inheritParams run_explicit_SEEIR_model
#'
#' @return Checked initial values data.frame
init_check_vaccine <- function(init, population, seeding_cases = 20){

  if (length(population) != 17) {
    stop("population must be divided up into 17x 5-year age bands spanning 0 to 80+")
  }
  assert_int(seeding_cases)
  age_group_indices <- c(8, 9, 10, 11) # age_group indices corresponding to middle-aged travellers

  if(is.null(init)){
    raw_seeding_cases <- rep(0, length(population))
    raw_seeding_cases[age_group_indices] <- as.vector(stats::rmultinom(1, size = seeding_cases, prob = rep(0.25, 4)))
    init = data.frame(
      S = population - raw_seeding_cases,
      E1 = raw_seeding_cases,
      E2 = 0,
      IMild = 0,
      ICase1 = 0,
      ICase2 = 0,
      IOxGetLive1 = 0,
      IOxGetLive2 = 0,
      IOxGetDie1 = 0,
      IOxGetDie2 = 0,
      IOxNotGetLive1 = 0,
      IOxNotGetLive2 = 0,
      IOxNotGetDie1 = 0,
      IOxNotGetDie2 = 0,
      IMVGetLive1 = 0,
      IMVGetLive2 = 0,
      IMVGetDie1 = 0,
      IMVGetDie2 = 0,
      IMVNotGetLive1 = 0,
      IMVNotGetLive2 = 0,
      IMVNotGetDie1 = 0,
      IMVNotGetDie2 = 0,
      IRec1 = 0,
      IRec2 = 0,
      R1 = 0,
      R2 = 0,
      D = 0,
      V1 = 0,
      V2 = 0,
      E1_vac = 0,
      E2_vac = 0
    )
  } else {
    if(!is.data.frame(init)){
      stop("init should be a data.frame with columns:
      S, E1, E2, ICase1, ICase2, IOxGetLive1, IOxGetLive2,
      IOxGetDie1, IOxGetDie2, IOxNotGetLive1, IOxNotGetLive2,
      IOxNotGetDie1, IOxNotGetDie2, IMVGetLive1, IMVGetLive2,
      IMVGetDie1, IMVGetDie2, IMVNotGetLive1, IMVNotGetLive2,
      IMVNotGetDie1, IMVNotGetDie2, IRec1, IRec2, R1, R2, D, V1, V2, E1_vac, E2_vac, and rows 1:age_groups")
    }
    if(!all(names(init) == c("S","E1","E2","IMild","ICase1","ICase2","IOxGetLive1",
                             "IOxGetLive2","IOxGetDie1","IOxGetDie2",
                             "IOxNotGetLive1","IOxNotGetLive2","IOxNotGetDie1",
                             "IOxNotGetDie2","IMVGetLive1","IMVGetLive2",
                             "IMVGetDie1","IMVGetDie2","IMVNotGetLive1",
                             "IMVNotGetLive2","IMVNotGetDie1","IMVNotGetDie2",
                             "IRec1","IRec2","R","D"))){
      stop("If specified, names of init must be identical to:
      S, E1, E2, ICase1, ICase2, IOxGetLive1, IOxGetLive2,
      IOxGetDie1, IOxGetDie2, IOxNotGetLive1, IOxNotGetLive2,
      IOxNotGetDie1, IOxNotGetDie2, IMVGetLive1, IMVGetLive2,
      IMVGetDie1, IMVGetDie2, IMVNotGetLive1, IMVNotGetLive2,
      IMVNotGetDie1, IMVNotGetDie2, IRec1, IRec2, R1, R2, V1, V2, E1_vac, E2_vac, D")
    }
  }
  # cases randomly distributed across 4 age groups so can't check
  # whole population is equal by row. Instead do it for first 7 age
  # groups
  if(!all(rowSums(init[1:7, ]) == population[1:7])){
    stop("Row sums of init should be identical to population")
  }
  if(!all(init >= 0)) {
    stop("population size is not large enough in each age bracket")
  }

  return(init)
}

#' Check time change inputs are correct
#'
#' @param tt Time change points
#' @inheritParams run_simple_SEEIR_model
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
pos_num <- function(x, name = deparse(substitute(x))){
  if(length(x) > 1){
    stop(name, " must have length = 1")
  }
  if(length(x) != 1 | !is.numeric(x) | x < 0){
    stop(name, " must be a positive number")
  }
  return(NULL)
}
