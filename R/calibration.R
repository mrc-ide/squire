#' Calibrate deaths
#'
#' @param country Country name
#' @param deaths Number of observed deaths
#' @param reporting_fraction Reporting fraction. Numeric for what proportion of
#'   the total deaths the reported deaths represent. E.g. 0.5 results in
#'   the model calibrating to twice the deaths provided by \code{deaths}
#' @param seeding_age_groups Age groups for seeding
#' @param R0 Vector or R0 values to sample from to introduce uncertainty
#'   in predictions. Default = c(2.7, 3.0, 3.5)
#' @param min_seeding_cases Minimum seeding cases
#' @param max_seeding_cases Maximum seeding cases
#' @param replicates Replicates
#' @param dt dt
#' @param ... Further aguments for \code{run_explicit_SEEIR_model()}
#'
#' @export
#' @return List of time adjusted squire_simulations
calibrate <- function(country, deaths, reporting_fraction = 1,
                      seeding_age_groups = c("35-40", "40-45", "45-50", "50-55"),
                      min_seeding_cases = 5,
                      max_seeding_cases = 50,
                      R0 = c(2.7, 3.0, 3.5),
                      replicates = 100,
                      dt = 0.1, ...) {

  assert_numeric(deaths)
  assert_numeric(reporting_fraction)
  assert_bounded(reporting_fraction, 0, 1)
  assert_greq(deaths, 1)
  # getting indices for relevant age groups where seeding cases occurred
  age_groups <- c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-35",
                  "35-40", "40-45", "45-50", "50-55", "55-60", "60-65", "65-70",
                  "70-75", "75-80", "80+")
  if (!all(seeding_age_groups %in% age_groups)) {
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
    raw_seeding_cases <- round(stats::runif(n = 1, min = min_seeding_cases, max = max_seeding_cases))
    seeding_cases[age_group_indices] <- as.vector(stats::rmultinom(1,
                                                            size = raw_seeding_cases,
                                                            prob = rep(1/num_age_groups,
                                                                       num_age_groups)))
    seeding_cases
  })

  # sample our R0
  if (length(R0) == 1) {
    R0 <- rep(R0, replicates)
  } else {
    R0 <- sample(R0, replicates, TRUE)
  }

  # run model with fixed day step (to match up with daily deaths)
  r <- run_explicit_SEEIR_model(population = pop$n,
                                contact_matrix_set = contact_matrix,
                                replicates = 1,
                                R0 = R0[1],
                                dt = dt, ...)

  # get the index for looking up D and R
  index <- odin_index(r$model)

  # run our replicates
  t <- seq(from = 1, to = r$parameters$time_period / dt)
  nt <- length(t)
  out <- list()
  out[[1]] <- r
  # running and storing the model output for each of the different initial seeding cases
  for(i in 2:replicates) {
    r$mod$set_user(E1_0 = E1_0[[i]])
    beta <- beta_est_explicit(dur_IMild = r$parameters$dur_IMild,
                      dur_ICase = r$parameters$dur_ICase,
                      prob_hosp = r$parameters$prob_hosp,
                      mixing_matrix =  process_contact_matrix_scaled_age(r$parameters$contact_matrix_set[[1]], r$parameters$population),
                      R0 = R0[i])
    r$mod$set_user(beta_set = beta)
    r$output <- r$mod$run(t, replicate = 1)
    while (sum(r$output[nt, index$R, 1]) < (sum(pop$n)/10)) {
      r$output <- r$mod$run(t, replicate = 1)
    }
    out[[i]] <- r
  }

  # Get deaths timepoint
  deaths_sim <- lapply(out, format_output, var_select = "D")
  times <- sapply(deaths_sim, function(x){
    x$t[x$y >= true_deaths][1]
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
  r$parameters$R0 <- R0

  return(r)
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
