#' #' Trigger run
#' #'
#' #' @param trigger_type Type of trigger (deaths, ICU capacity)
#' #' @param trigger_threshold Number of observed deaths suppression to be triggered at
#' #' @param ... Further aguments for \code{run_explicit_SEEIR_model()}
#' #' @inheritParams run_explicit_SEEIR_model
#' #'
#' #' @export
#' #' @return List of time adjusted squire_simulations
#' trigger_run <- function(trigger_type = NULL,
#'                         trigger_threshold = NULL,
#'                         suppression_reduction = 0.3,
#'                         suppression_duration = 30,
#'                         replicates = 25,
#'                         country = NULL,
#'                         population = NULL,
#'                         contact_matrix_set = NULL,
#'                         dt = 0.1,
#'                         R0 = 3,
#'                         tt_R0 = 0,
#'                         ...) {
#'
#'   # converting R0 and tt_R0 to format required
#'   if (length(R0) != length(tt_R0)) {
#'     stop("length of R0 does not equal length tt_R0")
#'   }
#'   if (length(R0) == 1) {
#'     R0 <- c(R0, R0)
#'   }
#'   if (length(tt_R0) == 1) {
#'     tt_R0 <- c(0, 50)
#'   }
#'
#'   # argument checks
#'   assert_character(trigger_type)
#'   assert_numeric(trigger_threshold)
#'   assert_numeric(replicates)
#'
#'   # check trigger_type
#'   valid_trigger_types <- c("ICU_capacity", "deaths")
#'   if (!(trigger_type %in% valid_trigger_types)) {
#'     stop("trigger type not valid: must be one of ICU_capacity or deaths")
#'   }
#'
#'   # return errors if trigger parameters not specified
#'   if (is.null(trigger_type)) {
#'     stop("Argument triger_type not specified: choose one of deaths or ICU_capacity")
#'   }
#'   if(is.null(trigger_threshold)) {
#'     stop("Argument trigger_threshold not specified.")
#'   }
#'
#'   # Handle country population args
#'   cpm <- parse_country_population_mixing_matrix(country = country,
#'                                                 population = population,
#'                                                 contact_matrix_set = contact_matrix_set)
#'   country <- cpm$country
#'   population <- cpm$population
#'   contact_matrix_set <- cpm$contact_matrix_set
#'
#'   # run model with fixed day step (to match up with daily deaths)
#'   r <- run_explicit_SEEIR_model(population = population,
#'                                 contact_matrix_set = contact_matrix_set,
#'                                 tt_R0 = tt_R0, R0 =R0, replicates = replicates, time_period =800, ICU_bed_capacity = 100000, hosp_bed_capacity = 100000)
#'
#'   # get the index for looking up ICU requirements and deaths
#'   baseline_beta <- r$model$contents()$beta_set[1]
#'   index <- odin_index(r$model)
#'   initials <- seq_along(r$model$initial()) + 1L
#'   out <- r$output
#'   length_output <- dim(r$output)[1]
#'   trigger_times <- rep(1, replicates)
#'
#'   for (i in 1:6) {
#'     if (i == 1) {
#'       if (trigger_type == "ICU_capacity") {
#'         req <- out[, index$total_ICU_req, ]
#'         trigger_times <- lapply(seq_along(trigger_times), function(x){
#'           trigger_times <- min(which(req[, x] > trigger_threshold))
#'         })
#'         trigger_times <- unlist(trigger_times)
#'       } else if (trigger_type == "deaths") {
#'         req <- out[, index$total_deaths, ]
#'         trigger_times <- lapply(seq_along(trigger_times), function(x){
#'           trigger_times <- min(which(req[, x] > trigger_threshold))
#'         })
#'         trigger_times <- unlist(trigger_times)
#'       }
#'     } else {
#'       if (trigger_type == "ICU_capacity") {
#'         req <- out[, index$total_ICU_req, ]
#'         trigger_times <- lapply(seq_along(trigger_times), function(x){
#'           starting_point <- trigger_times[x] + suppression_duration/dt
#'           timing_index <- min(length_output, starting_point)
#'           if (timing_index >= length_output) {
#'             trigger_times <- length_output
#'           } else {
#'             timing_index + min(which(req[timing_index:length_output, x] > trigger_threshold))
#'           }
#'         })
#'         trigger_times <- unlist(trigger_times)
#'       } else if (trigger_type == "deaths") {
#'         req <- out[, index$total_deaths, ]
#'         starting_point <- trigger_times[x] + suppression_duration/dt
#'         timing_index <- min(length_output, starting_point)
#'         if (timing_index >= length_output) {
#'           trigger_times <- length_output
#'         } else {
#'           trigger_times <- lapply(seq_along(trigger_times), function(x){
#'             trigger_times <- min(which(req[timing_index:length_output, x] > trigger_threshold))
#'           })
#'           trigger_times <- unlist(trigger_times)
#'         }
#'       }
#'     }
#'     print(trigger_times)
#'     if (sum(trigger_times == length_output) == length(trigger_times)) {
#'       return(out)
#'     }
#'
#'     for(j in 1:replicates) {
#'       if (trigger_times[j] == length_output) {
#'
#'       } else {
#'         r$model$set_user(beta_set = c(baseline_beta * suppression_reduction, baseline_beta))
#'         r$model$set_user(tt_beta = c(trigger_times[j], trigger_times[j] + suppression_duration/dt))
#'         out[trigger_times[j]:length_output, , j] <- r$model$run(step = trigger_times[j]:length_output,
#'                                                                 replicate = 1,
#'                                                                 y = as.numeric(out[trigger_times[j], initials, j]))
#'       }
#'     }
#'     print(i)
#'   }
#'
#' }
#'
#' plot(out[, index$total_ICU_req, 1], type = "l")# , ylim = c(0, 100), xlim = c(1200, 1300))
#'
#'
#'
#'
#'
#'
#'
#' ## Index locations of outputs in odin model
#' #' @noRd
#' odin_index <- function(model) {
#'   n_out <- environment(model$initialize)$private$n_out %||% 0
#'   n_state <- length(model$initial())
#'   model$transform_variables(seq_len(1L + n_state + n_out))
#' }
#'
#' ## Take odin state and calculate sum across ages in a replicate and vectorise
#' #' @noRd
#' odin_sv <- function(state, replicates, nt, reduce_age = TRUE) {
#'   if (reduce_age) {
#'     as.numeric(vapply(seq_len(replicates), function(x) {
#'       rowSums(state[,,x])
#'     }, FUN.VALUE = double(nt)))
#'   } else { # note: whole age-group results for single replicate produced, then next age-group etc
#'     as.numeric(vapply(seq_len(replicates), function(x) {
#'       state[, , x]
#'     }, FUN.VALUE = rep(double(nt), dim(state)[2])))
#'   }
#' }
#'
#' ## Final time varying variables at t = 0 in calibrate
#' #' @noRd
#' t0_variables <- function(r) {
#'
#'   dims <- dim(r$output)
#'
#'   # what state time point do we want
#'   state_pos <- vapply(seq_len(dims[3]), function(x) {
#'     which(r$output[,"time",x] == 0)
#'   }, FUN.VALUE = numeric(1))
#'
#'   lapply(seq_len(dims[3]), function(i) {
#'
#'     last <- tail(which(r$parameters$tt_R0 < state_pos[i]), 1)
#'     R0 <- r$parameters$R0[last]
#'
#'     last <- tail(which(r$parameters$tt_contact_matrix < state_pos[i]), 1)
#'     contact_matrix_set <- r$parameters$contact_matrix_set[last]
#'
#'     last <- tail(which(r$parameters$tt_hosp_beds < state_pos[i]), 1)
#'     hosp_bed_capacity <- r$parameters$hosp_bed_capacity[last]
#'
#'     last <- tail(which(r$parameters$tt_ICU_beds < state_pos[i]), 1)
#'     ICU_bed_capacity <- r$parameters$ICU_bed_capacity[last]
#'
#'     return(list(
#'       R0 = R0,
#'       contact_matrix_set = contact_matrix_set,
#'       hosp_bed_capacity = hosp_bed_capacity,
#'       ICU_bed_capacity = ICU_bed_capacity
#'     ))
#'
#'   })
#'
#' }
