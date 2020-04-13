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
#' @param ... Further aguments for \code{run_explicit_SEEIR_model()}
#'
#' @return List of time adjusted squire_simulations
calibrate <- function(country, deaths, reporting_fraction = 1,
                      seeding_age_groups = c("35-40", "40-45", "45-50", "50-55"),
                      min_seeding_cases = 5, max_seeding_cases = 50,
                      replicates = 100, dt = 0.5, ...) {

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

  # run model with fixed day step (to match up with daily deaths)
  r <- run_explicit_SEEIR_model(population = pop$n,
                                contact_matrix_set = contact_matrix,
                                replicates = 1,
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
    r$output <- r$mod$run(t, replicate = 1)
    while (sum(r$output[nt, index$R, 1]) < (sum(pop$n)/10)) {
      r$output <- r$mod$run(t, replicate = 1)
    }
    out[[i]] <- r
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


## Index locations of outputs in odin model
#' @noRd
odin_index <- function(model) {
  n_out <- environment(model$initialize)$private$n_out %||% 0
  n_state <- length(model$initial())
  model$transform_variables(seq_len(1L + n_state + n_out))
}

