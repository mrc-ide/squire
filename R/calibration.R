#' Calibrate deaths
#'
#' @param country Country name
#' @param deaths Number of observed deaths
#' @param reporting_fraction REporting fraction
#' @param seeding_age_groups Age groups for seeding
#' @param min_seeding_cases Minimum seeding cases
#' @param max_seeding_cases Maximum seeding cases
#' @param replicates Replicates
#' @param dt dt
#' @param ...
#'
#' @return List of time adjusted squire_simulations
calibrate <- function(country, deaths, reporting_fraction = 1,
                      seeding_age_groups = c("35-40", "40-45", "45-50", "50-55"),
                      min_seeding_cases = 5, max_seeding_cases = 50,
                      replicates = 100, dt = 0.5, ...) {

  # getting indices for relevant age groups where seeding cases occurred
  age_groups <- c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-35",
                  "35-40", "40-45", "45-50", "50-55", "55-60", "60-65", "65-70",
                  "70-75", "75-80", "80+")
  if (sum(!(seeding_age_groups %in% age_groups)) > 0) {
    stop("inputted age groups not valid")
  }
  age_group_indices <- which(age_groups %in% seeding_age_groups)
  num_age_groups <- length(age_group_indices)

  # assertions
  assert_string(country)

  # adjust for reporting fraction
  true_deaths <- deaths / reporting_fraction

  # get population and mixing matrix for specific country
  pop <- get_population(country)
  contact_matrix <- get_mixing_matrix(country)

  # generating the seeding cases for each of the replicates
  E1_0 <- lapply(seq_len(replicates), function(x) {
    seeding_cases <- rep(0, length = length(pop$n))
    raw_seeding_cases <- round(runif(n = 1, min = min_seeding_cases, max = max_seeding_cases))
    seeding_cases[age_group_indices] <- as.vector(rmultinom(1,
                                                            size = raw_seeding_cases,
                                                            prob = rep(1/num_age_groups,
                                                                       num_age_groups)))
    seeding_cases
  })

  # run model with fixed day step (to match up with daily deaths)
  r <- run_explicit_SEEIR_model(population = pop$n,
                                contact_matrix_set = contact_matrix,
                                replicates = 1,
                                dt = dt, ...)
  t <- seq(from = 1, to = r$parameters$time_period / dt)
  out <- list()
  out[[1]] <- r
  # running and storing the model output for each of the different initial seeding cases
  for(i in 2:replicates) {
    print(i)
    r$mod$set_user(E1_0 = E1_0[[i]])
    r$output <- r$mod$run(t, replicate = 1)
    out[[i]] = r
  }

  # Get deaths timepoint
  deaths_sim <- lapply(out, format_output, var_select = "D")
  times <- sapply(deaths_sim, function(x){
    x$t[x$y > true_deaths][1]
  })

  # Adjust time
  for(i in 1:length(out)){
    out[[i]]$output[,"time",] <- out[[i]]$output[,"time",] - times[i]
  }

  outarray <- array(NA, dim = c(nrow(out[[1]]$output), ncol(out[[1]]$output), replicates))
  for(i in 1:length(out)){
    outarray[,,i] <- out[[i]]$output
  }
  colnames(outarray) <- names(r$output[1,,1])
  r$output <- outarray
  r$parameters$replicates <- replicates

  return(r)
}

#' Format output of calibration for plotting
#'
#' @details Calibration output is taken to give time series of infections, cases,
#' cases requiring hospitilisation, case requiring critical care facilities. Used
#' in plotting for nowcasting reports.
#'
#' @param r Output of \code{\link{calibrate}}
#'
#' @return \code{list} with:
#' \itemize{
#'       \item{df:}{ Data frame of case numbers, hospital beds, icu beds and deaths }
#'       \item{data:}{ Raw data used in calibration}
#'       \item{parameters:}{ Parameters used in simulation}
#'       }
#'
calibrate_output_parsing <- function(r) {

  ## Assertions
  assert_custom_class(r, "squire_simulation")
  if(!"date" %in% names(r)) {
    stop("r needs date element")
  }

  # get the index for looking up D
  index <- odin_index(r$model)
  nt <- nrow(r$output)

  mv <- unlist(index[c("IMVGetLive1","IMVGetLive2","IMVGetDie1","IMVGetDie2",
                       "IMVNotGetLive1","IMVNotGetLive2","IMVNotGetDie1","IMVNotGetDie2")])

  ox <- unlist(index[c("IOxGetLive1","IOxGetLive2","IOxGetDie1","IOxGetDie2",
                       "IOxNotGetLive1","IOxNotGetLive2","IOxNotGetDie1","IOxNotGetDie2")])


  # collet outputs as vectors
  mild_cases <- odin_sv(r$output[,index$n_E2_I,] - r$output[,index$n_E2_ICase1,],
                        replicates = r$parameters$replicates, nt = nt)
  hospital_cases <- odin_sv(r$output[,index$n_E2_ICase1,],
                            replicates = r$parameters$replicates, nt = nt)
  icu <- odin_sv(r$output[,mv,],
                 replicates = r$parameters$replicates, nt = nt)
  hospital_bed <- odin_sv(r$output[,ox,],
                          replicates = r$parameters$replicates, nt = nt)
  deaths <- odin_sv(r$output[,index$delta_D,],
                    replicates = r$parameters$replicates, nt = nt)

  # collect into a long data frame
  vars <- c("mild_cases", "hospital_cases", "deaths", "icu", "hospital_bed")
  df <- data.frame("date" = as.numeric(r$date),
                   "replicate" = as.numeric(mapply(rep, seq_len(r$parameters$replicates), nt)),
                   "variable" = as.character(mapply(rep, vars, nt*r$parameters$replicates)),
                   "value" = c(mild_cases, hospital_cases, deaths, icu, hospital_bed))

  ret <- list(df = df, data = r$data, parameters = r$parameters)
  class(ret) <- "squire_calibration"

  return(ret)
}

## Index locations of outputs in odin model
#' @noRd
odin_index <- function(model) {
  n_out <- environment(model$initialize)$private$n_out %||% 0
  n_state <- length(model$initial())
  model$transform_variables(seq_len(1L + n_state + n_out))
}


## Take odin state and calculate sum across ages in a replicate and vectorise
#' @noRd
odin_sv <- function(state, replicates, nt) {
  as.numeric(vapply(seq_len(replicates), function(x) {
    rowSums(state[,,x])
  }, FUN.VALUE = double(nt)))
}
