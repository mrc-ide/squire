
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
      EVac1 = 0,
      EVac2 = 0,
      SVac1 = 0,
      SVac2 = 0,
      RVac1 = 0,
      RVac2 = 0
    )
  } else {
    if(!is.data.frame(init)){
      stop("init should be a data.frame with columns:
      S, E1, E2, ICase1, ICase2, IOxGetLive1, IOxGetLive2,
      IOxGetDie1, IOxGetDie2, IOxNotGetLive1, IOxNotGetLive2,
      IOxNotGetDie1, IOxNotGetDie2, IMVGetLive1, IMVGetLive2,
      IMVGetDie1, IMVGetDie2, IMVNotGetLive1, IMVNotGetLive2,
      IMVNotGetDie1, IMVNotGetDie2, IRec1, IRec2, R1, R2, D, V1, V2,
           EVac1, EVac2, SVac1, SVac2
           RVac1, RVac2 and rows 1:age_groups")
    }
    if(!all(names(init) == c("S","E1","E2","IMild","ICase1","ICase2","IOxGetLive1",
                             "IOxGetLive2","IOxGetDie1","IOxGetDie2",
                             "IOxNotGetLive1","IOxNotGetLive2","IOxNotGetDie1",
                             "IOxNotGetDie2","IMVGetLive1","IMVGetLive2",
                             "IMVGetDie1","IMVGetDie2","IMVNotGetLive1",
                             "IMVNotGetLive2","IMVNotGetDie1","IMVNotGetDie2",
                             "IRec1","IRec2","R","D", "EVac1", "EVac2",
                             "SVac1", "SVac2", "RVac1", "RVac2"))){
      stop("If specified, names of init must be identical to:
      S, E1, E2, ICase1, ICase2, IOxGetLive1, IOxGetLive2,
      IOxGetDie1, IOxGetDie2, IOxNotGetLive1, IOxNotGetLive2,
      IOxNotGetDie1, IOxNotGetDie2, IMVGetLive1, IMVGetLive2,
      IMVGetDie1, IMVGetDie2, IMVNotGetLive1, IMVNotGetLive2,
      IMVNotGetDie1, IMVNotGetDie2, IRec1, IRec2, R1, R2, V1, V2,
           EVac1, E2_vac, D, SVac1, SVac2", "RVac1", "RVac2")
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
