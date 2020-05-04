#' Provide projections from calibrated simulations by changing RO, contact
#' matrices or bed availability
#'
#' @details The user can specify changes to R0, contact matrices and bed
#' provision, which will come into effect from the current day in the calibration.
#' These changes can either set these to be specific values or change them
#' relative to their values in the original simulation. If no change is
#' requested, the simulation will use parameters chosen for the calibration run.
#'
#' @param r Calibrated \code{{squire_simulation}} object.
#' @param time_period How many days is the projection. Deafult = NULL, which will
#'   carry the projection forward from t = 0 in the calibration (i.e. the number
#'   of days set in calibrate using forecast)
#' @param R0 Numeric vector for R0 from t = 0 in the calibration.
#'   E.g. \code{R0 = c(2, 1)}. Default = NULL, which will use \code{R0_change}
#'   to alter R0 if provided.
#' @param R0_change Numeric vector for relative changes in R0 relative to the
#'   final R0 used in the calibration (i.e. at t = 0 in the calibration)
#'   E.g. \code{R0 = c(0.8, 0.5)}. Default = NULL, which will use \code{R0} to
#'   parameterise changes in R0 if provided.
#' @param tt_R0 Change time points for R0
#'
#' @param contact_matrix_set Contact matrices used in simulation. Default =
#'   NULL, which will use \code{contact_matrix_set_change} to alter the contact
#'   matrix if provided.
#' @param contact_matrix_set_change Numeric vector for relative changes in the
#'   contact matrix realtive to the final contact matrix used in the calibration
#'   (i.e. at t = 0 in the calibration).
#'   E.g. \code{contact_matrix_set_change = c(0.8, 0.5)}. Default = NULL, which
#'   will use \code{contact_matrix_set} to parameterise changes in contact
#'   matrices if if provided.
#' @param tt_contact_matrix Time change points for matrix change. Default = 0
#'
#' @param hosp_bed_capacity Numeric vector for hospital bed capacity
#'   from t = 0 in the calibration. Default = NULL, which will use
#'   \code{hosp_bed_capacity_change} to alter hosp_bed_capacity if provided.
#' @param hosp_bed_capacity_change Numeric vector for relative changes in
#'   hospital bed capacity relative to the final hospital bed capacity used in the
#'   calibration (i.e. at t = 0 in the calibration).
#'   E.g. \code{hosp_bed_capacity_change = c(0.8, 0.5)}. Default = NULL, which
#'   will use \code{hosp_bed_capacity} to parameterise changes in hospital bed capacity
#'   if provided.
#' @param tt_hosp_beds Change time points for hosp_bed_capacity
#'
#' @param ICU_bed_capacity Numeric vector for ICU bed capacity
#'   from t = 0 in the calibration. Default = NULL, which will use
#'   \code{ICU_bed_capacity_change} to alter ICU_bed_capacity if provided.
#' @param ICU_bed_capacity_change Numeric vector for relative changes in
#'   ICU bed capacity relative to the final ICU bed capacity used in the
#'   calibration (i.e. at t = 0 in the calibration).
#'   E.g. \code{ICU_bed_capacity_change = c(0.8, 0.5)}. Default = NULL, which
#'   will use \code{ICU_bed_capacity} to parameterise changes in ICU bed capacity
#'   if provided.
#' @param tt_ICU_beds Change time points for ICU_bed_capacity
#'
#'
#' @export
projections <- function(r,
                        time_period = NULL,
                        R0 = NULL,
                        R0_change = NULL,
                        tt_R0 = 0,
                        contact_matrix_set = NULL,
                        contact_matrix_set_change = NULL,
                        tt_contact_matrix = 0,
                        hosp_bed_capacity = NULL,
                        hosp_bed_capacity_change = NULL,
                        tt_hosp_beds = 0,
                        ICU_bed_capacity = NULL,
                        ICU_bed_capacity_change = NULL,
                        tt_ICU_beds = 0
) {

  # Grab function arguments
  args <- as.list(environment())

  # ----------------------------------------------------------------------------
  ## assertion checks on parameters
  # ----------------------------------------------------------------------------
  assert_custom_class(r, "squire_simulation")
  if (inherits(r$scan_results$inputs$model, "deterministic")) {
    stop("projections unlikely to work with deterministic squire model currently")
  }
  assert_pos_int(tt_R0)
  if(!0 %in% tt_R0) {
    stop("tt_R0 must start with 0")
  }
  assert_pos_int(tt_contact_matrix)
  if(!0 %in% tt_contact_matrix) {
    stop("tt_contact_matrix must start with 0")
  }
  assert_pos_int(tt_hosp_beds)
  if(!0 %in% tt_hosp_beds) {
    stop("tt_hosp_beds must start with 0")
  }
  assert_pos_int(tt_ICU_beds)
  if(!0 %in% tt_ICU_beds) {
    stop("tt_ICU_beds must start with 0")
  }

  # ----------------------------------------------------------------------------
  # remove the change arguments if the absolute is provided
  # ----------------------------------------------------------------------------

  if(!is.null(R0) && !is.null(R0_change)) {
    message("Both R0 or R0_change were specified. R0 is being used.")
    R0_change <- NULL
  }
  if(!is.null(contact_matrix_set) && !is.null(contact_matrix_set_change)) {
    message("Both contact_matrix_set or contact_matrix_set_change were specified. contact_matrix_set is being used.")
    contact_matrix_set_change <- NULL
  }
  if(!is.null(hosp_bed_capacity) && !is.null(hosp_bed_capacity_change)) {
    message("Both hosp_bed_capacity or hosp_bed_capacity_change were specified. hosp_bed_capacity is being used.")
    hosp_bed_capacity_change <- NULL
  }
  if(!is.null(ICU_bed_capacity) && !is.null(ICU_bed_capacity_change)) {
    message("Both ICU_bed_capacity or ICU_bed_capacity_change were specified. ICU_bed_capacity is being used.")
    ICU_bed_capacity_change <- NULL
  }

  # ----------------------------------------------------------------------------
  # check are variables are correctly formatted
  # ----------------------------------------------------------------------------
  if(!is.null(R0)){
    assert_numeric(R0)
    assert_pos(R0)
    assert_same_length(R0, tt_R0)
  }

  if(!is.null(R0_change)){
    assert_numeric(R0_change)
    assert_pos(R0_change)
    assert_same_length(R0_change, tt_R0)
  }

  if(!is.null(contact_matrix_set)) {
    # Standardise contact matrix set
    if(is.matrix(contact_matrix_set)){
      contact_matrix_set <- list(contact_matrix_set)
    }
    mc <- matrix_check(r$parameters$population[-1], contact_matrix_set)
    matrices_set <- matrix_set_explicit(contact_matrix_set, r$parameters$population)
    assert_same_length(contact_matrix_set, tt_contact_matrix)
  }

  if(!is.null(contact_matrix_set_change)){
    assert_numeric(contact_matrix_set_change)
    assert_pos(contact_matrix_set_change)
    assert_same_length(contact_matrix_set_change, tt_contact_matrix)
  }

  if(!is.null(hosp_bed_capacity)){
    assert_numeric(hosp_bed_capacity)
    assert_pos(hosp_bed_capacity)
    assert_same_length(hosp_bed_capacity, tt_hosp_beds)
  }

  if(!is.null(hosp_bed_capacity_change)){
    assert_numeric(hosp_bed_capacity_change)
    assert_pos(hosp_bed_capacity_change)
    assert_same_length(hosp_bed_capacity_change, tt_hosp_beds)
  }

  if(!is.null(ICU_bed_capacity)){
    assert_numeric(ICU_bed_capacity)
    assert_pos(ICU_bed_capacity)
    assert_same_length(ICU_bed_capacity, tt_ICU_beds)
  }

  if(!is.null(ICU_bed_capacity_change)){
    assert_numeric(ICU_bed_capacity_change)
    assert_pos(ICU_bed_capacity_change)
    assert_same_length(ICU_bed_capacity_change, tt_ICU_beds)
  }

  # ----------------------------------------------------------------------------
  # generating pre simulation variables
  # ----------------------------------------------------------------------------

  # odin model keys
  index <- odin_index(r$model)
  initials <- seq_along(r$model$initial()) + 1L
  ds <- dim(r$output)

  # what state time point do we want
  state_pos <- vapply(seq_len(ds[3]), function(x) {
    which(r$output[,"time",x] == 0)
  }, FUN.VALUE = numeric(1))

  # what are the remaining time points
  t_steps <- lapply(state_pos, function(x) {
    r$output[which(r$output[,1,1] > r$output[x,1,1]),1 ,1]
  })

  # do we need to do more than just the remaining time from calibrate
  if (!is.null(time_period)) {

    t_diff <- diff(tail(r$output[,1,1],2))
    t_start <- r$output[(r$output[,"time",1]==0),1,1]+t_diff
    t_initial <- unique(stats::na.omit(r$output[1,1,]))

    t_steps <- lapply(t_steps, function(x) {
      seq(t_start, t_start - t_diff + time_period/r$parameters$dt, t_diff)
    })
    steps <- seq(t_initial, max(t_steps[[1]]), t_diff)

    arr_new <- array(NA, dim = c((max(t_steps[[1]])/(t_diff)) + (1-t_initial),
                                 ncol(r$output), dim(r$output)[3]))
    arr_new[seq_len(nrow(r$output)),,] <- r$output
    rownms <- rownames(r$output)
    colnms <- colnames(r$output)
    if(!is.null(rownms)) {
      rownames(arr_new) <- as.character(as.Date(rownms[1]) + seq_len(nrow(arr_new)) - 1L)
    }
    r$output <- arr_new
    r$output[,1,] <- matrix(rep(steps,r$parameters$replicates), ncol = r$parameters$replicates)
    colnames(r$output) <- colnms
  }

  # final values of R0, contacts, and beds
  finals <- t0_variables(r)

  # ----------------------------------------------------------------------------
  # conduct simulations
  # ----------------------------------------------------------------------------
  out <- lapply(seq_len(ds[3]), function(x) {

    # ----------------------------------------------------------------------------
    # adapt our time changing variables as needed
    # ----------------------------------------------------------------------------

    # first if R0 is not provided we use the last R0
    if (is.null(R0)) {
      R0 <- finals[[x]]$R0
    }

    # are we modifying the R0
    if (!is.null(R0_change)) {
      R0 <- R0*R0_change
    }

    # second if contact_matrix_set is not provided we use the last contact_matrix_set
    if (is.null(contact_matrix_set)) {
      contact_matrix_set <- finals[[x]]$contact_matrix_set
      baseline_contact_matrix_set <- contact_matrix_set[1]
    } else {
      baseline_contact_matrix_set <- contact_matrix_set[1]
    }

    # are we modifying it
    if (!is.null(contact_matrix_set_change)) {
      if (length(contact_matrix_set) == 1) {
        contact_matrix_set <- lapply(seq_along(tt_contact_matrix),function(x){
          contact_matrix_set[[1]]
        })
      }
      baseline_contact_matrix_set <- contact_matrix_set[1]
      contact_matrix_set <- lapply(
        seq_len(length(contact_matrix_set_change)),
        function(x){
          contact_matrix_set[[x]]*contact_matrix_set_change[x]
        })
    }


    # third if hosp_bed_capacity is not provided we use the last hosp_bed_capacity
    if (is.null(hosp_bed_capacity)) {
      hosp_bed_capacity <- finals[[x]]$hosp_bed_capacity
    }

    # are we modifying it
    if (!is.null(hosp_bed_capacity_change)) {
      hosp_bed_capacity <- hosp_bed_capacity*hosp_bed_capacity_change
    }

    # last if ICU_bed_capacity is not provided we use the last contact_matrix_set
    if (is.null(ICU_bed_capacity)) {
      ICU_bed_capacity <- finals[[x]]$ICU_bed_capacity
    }

    # are we modifying it
    if (!is.null(ICU_bed_capacity_change)) {
      ICU_bed_capacity <- ICU_bed_capacity*ICU_bed_capacity_change
    }

    # ----------------------------------------------------------------------------
    # Generate new variables to pass to model
    # ----------------------------------------------------------------------------

    # Convert contact matrices to input matrices
    matrices_set <- matrix_set_explicit(contact_matrix_set, r$parameters$population)

    # create new betas going forwards
    beta <- beta_est_explicit(dur_IMild = r$parameters$dur_IMild,
                              dur_ICase = r$parameters$dur_ICase,
                              prob_hosp = r$parameters$prob_hosp,
                              mixing_matrix = process_contact_matrix_scaled_age(
                                baseline_contact_matrix_set[[1]],
                                r$parameters$population),
                              R0 = R0)

    # Is the model still valid
    if(is_ptr_null(r$model$.__enclos_env__$private$ptr)) {
      r$model <- r$scan_results$inputs$model$odin_model(
        user = r$scan_results$inputs$model_params,
        unused_user_action = "ignore")
    }

    # change these user params
    r$model$set_user(tt_beta = round(tt_R0/r$parameters$dt))
    r$model$set_user(beta_set = beta)
    r$model$set_user(tt_matrix = round(tt_contact_matrix/r$parameters$dt))
    r$model$set_user(mix_mat_set = matrices_set)
    r$model$set_user(tt_hosp_beds = round(tt_hosp_beds/r$parameters$dt))
    r$model$set_user(hosp_beds = hosp_bed_capacity)
    r$model$set_user(tt_ICU_beds = round(tt_ICU_beds/r$parameters$dt))
    r$model$set_user(ICU_beds = ICU_bed_capacity)

    # run the model
    if(diff(tail(r$output[,1,1],2)) != 1) {
      step <- c(0,round(seq_len(length(t_steps[[x]]))/r$parameters$dt))
    } else {
      step <- seq_len(length(t_steps[[x]]))
    }

    r$model$run(step = step,
                y = as.numeric(r$output[state_pos[x], initials, x, drop=TRUE]),
                use_names = TRUE,
                replicate = 1)

  })

  ## collect results
  for(i in seq_len(ds[3])) {
    if(diff(tail(r$output[,"step",1],2)) != 1) {
      r$output[which(r$output[,1,1] %in% t_steps[[i]]), -1, i] <- out[[i]][-1, -1, 1]
    } else {
      r$output[which(r$output[,1,1] %in% t_steps[[i]]), -1, i] <- out[[i]][, -1, 1]
    }
  }

  ## append projections
  r$projection_args <- args

  return(r)

}



#' Plot projections against each other
#'
#' @param r_list List of different projection runs from \code{\link{projections}}
#' @param scenarios Character vector describing the different scenarios.
#' @param add_parms_to_scenarios Logical. Should the parameters used for the
#'   projection runs be added to scenarios. Default = TRUE
#' @param date_0 Date of time 0, if specified a date column will be added
#' @inheritParams plot.squire_simulation
#' @param ... additional arguments passed to \code{\link{format_output}}
#'
#' @export
projection_plotting <- function(r_list,
                                scenarios,
                                add_parms_to_scenarios = TRUE,
                                var_select = NULL,
                                replicates = FALSE,
                                summarise = TRUE,
                                ci = TRUE,
                                q = c(0.025, 0.975),
                                summary_f = mean,
                                date_0 = Sys.Date(),
                                x_var = "t", ...) {


  # assertion checks
  assert_list(r_list)
  assert_string(scenarios)
  assert_same_length(r_list, scenarios)
  if(!all(unlist(lapply(r_list, class)) == "squire_simulation")) {
    stop("One of r_list is not a squire_simulation")
  }

  pd_list <- lapply(r_list, FUN = squire_simulation_plot_prep,
                    var_select = var_select,
                    x_var = x_var, q = q,
                    summary_f = summary_f,
                    date_0 = date_0)

  if (add_parms_to_scenarios) {
    parms <- lapply(r_list,projection_inputs)
    scenarios <- mapply(paste, scenarios, parms)
  }

  # append scenarios
  for(i in seq_along(scenarios)) {
    pd_list[[i]]$pd$Scenario <- scenarios[i]
    pd_list[[i]]$pds$Scenario <- scenarios[i]
  }

  pds <- do.call(rbind, lapply(pd_list, "[[", "pds"))
  pd <- do.call(rbind, lapply(pd_list, "[[", "pd"))

  # Plot
  p <- ggplot2::ggplot()

  # Add lines for individual draws
  if(replicates){
    p <- p + ggplot2::geom_line(data = pd,
                                ggplot2::aes(x = .data$x,
                                             y = .data$y,
                                             col = .data$Scenario,
                                             linetype = .data$compartment,
                                             group = interaction(.data$compartment,
                                                                 .data$replicate,
                                                                 .data$Scenario)),
                                alpha = max(0.2, 1 / r_list[[1]]$parameters$replicates))
  }

  if(summarise){
    if(r_list[[1]]$parameters$replicates < 10){
      warning("Summary statistic estimated from <10 replicates")
    }
    p <- p + ggplot2::geom_line(data = pds,
                                ggplot2::aes(x = .data$x, y = .data$y,
                                             col = .data$Scenario,
                                             linetype = .data$compartment))
  }

  if(ci){
    if(r_list[[1]]$parameters$replicates < 10){
      warning("Confidence bounds estimated from <10 replicates")
    }
    p <- p + ggplot2::geom_ribbon(data = pds,
                                  ggplot2::aes(x = .data$x,
                                               ymin = .data$ymin,
                                               ymax = .data$ymax,
                                               fill = .data$Scenario,
                                               linetype = .data$compartment),
                                  alpha = 0.25, col = "black")
  }

  # Add remaining formatting
  p <- p +
    ggplot2::scale_color_discrete(name = "") +
    ggplot2::scale_fill_discrete(guide = FALSE) +
    ggplot2::xlab("Time") +
    ggplot2::ylab("N") +
    ggplot2::theme_bw()

  return(p)


}

## Final time varying variables at t = 0 in calibrate
#' @noRd
t0_variables <- function(r) {

  dims <- dim(r$output)

  # is this the outputs of a grid scan
  if("scan_results" %in% names(r)) {

    # grab the final R0, contact matrix and bed capacity.
    ret <- lapply(seq_len(dims[3]), function(x) {

      if(!is.null(r$interventions$R0_change)) {
        R0 <- tail(r$replicate_parameters$R0[x] * r$interventions$R0_change, 1)
      } else {
        R0 <- r$replicate_parameters$R0[x]
      }
      contact_matrix_set <- tail(r$parameters$contact_matrix_set,1)

      hosp_bed_capacity <- tail(r$parameters$hosp_bed_capacity,1)
      ICU_bed_capacity <- tail(r$parameters$ICU_bed_capacity,1)

      return(list(
        R0 = R0,
        contact_matrix_set = contact_matrix_set,
        hosp_bed_capacity = hosp_bed_capacity,
        ICU_bed_capacity = ICU_bed_capacity))
    })

  } else {

    # what state time point do we want
    state_pos <- vapply(seq_len(dims[3]), function(x) {
      which(r$output[,"time",x] == 0)
    }, FUN.VALUE = numeric(1))

    # build list of the final variables that change
    ret <- lapply(seq_len(dims[3]), function(i) {

      last <- tail(which(r$parameters$tt_R0 < state_pos[i]), 1)
      R0 <- r$parameters$R0[last]

      last <- tail(which(r$parameters$tt_contact_matrix < state_pos[i]), 1)
      contact_matrix_set <- r$parameters$contact_matrix_set[last]

      last <- tail(which(r$parameters$tt_hosp_beds < state_pos[i]), 1)
      hosp_bed_capacity <- r$parameters$hosp_bed_capacity[last]

      last <- tail(which(r$parameters$tt_ICU_beds < state_pos[i]), 1)
      ICU_bed_capacity <- r$parameters$ICU_bed_capacity[last]

      return(list(
        R0 = R0,
        contact_matrix_set = contact_matrix_set,
        hosp_bed_capacity = hosp_bed_capacity,
        ICU_bed_capacity = ICU_bed_capacity
      ))

    })

  }

  return(ret)

}


#' @noRd
projection_inputs <- function(p3){

  if(!"projection_args" %in% names(p3)) {
    return("(No interventions)")
  } else {

    pos <- seq_along(p3$projection_args)[-(1:2)]
    nms <- p3$projection_args[pos]

    cat_f <- function(x, c = ""){
      if(!is.null(x[[1]]) && (is.null(x[[3]]) || is.null(x[[2]]))) {
        paste0(c, names(x[1]), ": ", paste0(x[[1]], collapse = ", "), " @ t = ", paste0(x[[3]], collapse = ", "))
      } else if(!is.null(x[[2]])) {
        paste0(c, names(x[2]), ": ", paste0(x[[2]]*100,"%",collapse=", "), " @ t = ", paste0(x[[3]], collapse = ", "))
      } else {
        ""
      }
    }

    paste0("\n",cat_f(nms[1:3], "("),
           cat_f(nms[4:6],", "),
           cat_f(nms[7:9],", "),
           cat_f(nms[10:12],")"))

  }
}
