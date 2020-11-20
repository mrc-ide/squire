#' Provide projections from calibrated simulations by changing RO, contact
#' matrices or bed availability.
#'
#' This extends previous \code{projections} as you can pass in lists of each argument
#' that then get passed to each simulation replicate.
#'
#' @details The user can specify changes to R0, contact matrices and bed
#' provision, which will come into effect from the current day in the calibration.
#' These changes can either set these to be specific values or change them
#' relative to their values in the original simulation. If no change is
#' requested, the simulation will use parameters chosen for the calibration run.
#' This extends previous versions of \code{projections} as you can now pass in
#' lists of each argument that then get passed to each simulation replicate.
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
#' @param to_be_run List of logicals for whether each replicate should be run.
#'   Default = TRUE, which causes all replictes to be run.
#' @param model_user_args List of other parameters to be passed to the model to
#'   be run. Default = NULL. An example would be:
#'
#'   \code{list(
#'   list(
#'   "prob_severe" = runif(17),
#'   "tt_dur_get_ox_survive" = c(0, 10),
#'   "gamma_get_ox_survive" = 0.2),
#'   list(
#'   "prob_severe" = runif(17),
#'   "tt_dur_get_mv_survive" = c(0, 5),
#'   "gamma_get_mv_survive" = 0.1)
#'   )}
#'
#'   The list should be the same length as the number of replicates in the
#'   simulations. Each list element should then be a list with elements
#'   named to match the arguments expected by the odin model with \code{r}.
#'   Above would be suitable to set the model parameters for a simulation with
#'   2 replicates. You do not have to have the same arguments in each list.
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
                        tt_ICU_beds = 0,
                        to_be_run = TRUE,
                        model_user_args = NULL) {

  # Grab function arguments
  args <- as.list(environment())

  # Grab replicates
  reps <- dim(r$output)[3]

  # ----------------------------------------------------------------------------
  ## assertion checks on parameters
  # ----------------------------------------------------------------------------
  assert_custom_class(r, "squire_simulation")
  # TODO future asserts if these are our "end" classes
  if (is.null(r$output) & is.null(r$scan_results) & is.null(r$pmcmc_results)) {
    stop("Model must have been produced either with Squire Default, Scan Grid (calibrate), or pMCMC (pmcmc) Approach")
  }

  # check arg length
  if(!is.null(model_user_args)) {
    assert_list(model_user_args)
    assert_list(model_user_args[[1]])
    assert_same_length(model_user_args, rep(0, dim(r$output)[3]),
                       name_x = "model_user_args",
                       name_y = "number of simulation replicates, i.e. dim(r$output)[3],")
  }

  # ----------------------------------------------------------------------------
  ## tt_ checks
  # ----------------------------------------------------------------------------

  tt_creation <- function(tt, reps, name = deparse(substitute(tt))) {

    if (!is.list(tt)) {
      assert_pos_int(tt, name = name)
      if(tt[1] != 0) {
        stop(paste0(name, " must start with 0"))
      }
      tt <- rep(list(tt), reps)
    } else {
      lapply(seq_along(tt), function(x) {
        assert_pos_int(tt[[x]], name = paste0(name, " replicate ", x))
        if(tt[[x]][1] != 0) {
          stop(paste0(name, " replicate ", x, " must start with 0"))
        }
      })
    }
    assert_length(tt, reps)
    return(tt)

  }



  # ----------------------------------------------------------------------------
  # check are variables are correctly formatted
  # ----------------------------------------------------------------------------

  # tt checks
  tt_R0 <- tt_creation(tt_R0, reps = reps)
  tt_contact_matrix <- tt_creation(tt_contact_matrix, reps = reps)
  tt_hosp_beds <- tt_creation(tt_hosp_beds, reps = reps)
  tt_ICU_beds <- tt_creation(tt_ICU_beds, reps = reps)

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
  ## num checks
  # ----------------------------------------------------------------------------

  num_creation <- function(num, tt, reps,
                           name = deparse(substitute(num)),
                           name2 = deparse(substitute(tt))) {

    if(!is.null(num)) {

      if (!is.list(num)) {
        assert_numeric(num, name = name)
        assert_pos(num, name = name)
        num <- rep(list(num), reps)
      }

      assert_length(num, reps)
      lapply(seq_along(num), function(x) {
        assert_same_length(num[[x]], tt[[x]],
                           name_x = paste0(name, " replicate ", x),
                           name_y = paste0(name2, " replicate ", x))
        assert_numeric(num[[x]], name = paste0(name, " replicate ", x))
        assert_pos(num[[x]], name = paste0(name, " replicate ", x))
      })

    } else {
      num <- rep(list(num), reps)
    }

    return(num)

  }

  R0 <- num_creation(R0, tt_R0, reps = reps)
  R0_change <- num_creation(R0_change, tt_R0, reps = reps)
  hosp_bed_capacity <- num_creation(hosp_bed_capacity, tt_hosp_beds, reps = reps)
  hosp_bed_capacity_change <- num_creation(hosp_bed_capacity_change, tt_hosp_beds, reps = reps)
  ICU_bed_capacity <- num_creation(ICU_bed_capacity, tt_ICU_beds, reps = reps)
  ICU_bed_capacity_change <- num_creation(ICU_bed_capacity_change, tt_ICU_beds, reps = reps)
  contact_matrix_set_change <- num_creation(contact_matrix_set_change, tt_contact_matrix, reps = reps)



  ## Contact matrix set is the only different one

  if (!is.null(contact_matrix_set)) {

    # Standardise contact matrix set if its a matrix
    if(is.matrix(contact_matrix_set)){
      contact_matrix_set <- list(contact_matrix_set)
      mc <- matrix_check(r$parameters$population[-1], contact_matrix_set)
      contact_matrix_set <- rep(list(contact_matrix_set), reps)
    } else if(is.list(contact_matrix_set)) {
      if(is.matrix(contact_matrix_set[[1]])) {

        # if the list length is less than the tt_contact_matrix then this is to be
        # repeated as a list
        if(length(contact_matrix_set) < sum(lengths(tt_contact_matrix))) {
          contact_matrix_set <- rep(list(contact_matrix_set), reps)
        }
      }
    }
    # check the lengths
    lapply(seq_along(contact_matrix_set), function(x) {
      assert_same_length(contact_matrix_set[[x]], tt_contact_matrix[[x]],
                         name_x = paste0("contact_matrix_set", " replicate ", x),
                         name_y = paste0("tt_contact_matrix", " replicate ", x))
    })

  } else {
    contact_matrix_set <- rep(list(contact_matrix_set), reps)
  }


  # Lastly our list for whether to do each replicate
  if (!is.list(to_be_run)) {
    assert_logical(to_be_run)
    to_be_run <- rep(list(to_be_run), reps)
  } else {
    lapply(seq_along(to_be_run), function(x) {
      assert_logical(to_be_run[[x]], name = paste0("to_be_run replicate ", x))
    })
  }
  assert_length(to_be_run, reps)

  # ----------------------------------------------------------------------------
  # generating pre simulation variables
  # ----------------------------------------------------------------------------

  # odin model keys
  index <- odin_index(r$model)
  initials <- seq_along(r$model$initial()) + 1L
  ds <- dim(r$output)

  # what state time point do we want
  state_pos <- vapply(seq_len(ds[3]), function(x) {
    pos <- which(r$output[,"time",x] == 0)
    if(length(pos) == 0) {
      stop("projections needs time value to be equal to 0 to know how to project forwards")
    }
    return(pos)
  }, FUN.VALUE = numeric(1))

  # what are the remaining time points
  t_steps <- lapply(state_pos, function(x) {
    r$output[which(r$output[,1,1] > r$output[x,1,1]),1 ,1]
  })

  # if there are no remaining steps
  if(any(!unlist(lapply(t_steps, length))) && is.null(time_period)) {
    stop("projections needs either time_period set or the calibrate/pmcmc object ",
         "to have been run with forecast > 0")
  }

  # do we need to do more than just the remaining time from calibrate
  if (!is.null(time_period)) {

    t_diff <- diff(tail(r$output[,1,1],2))
    t_start <- r$output[which((r$output[,"time",1]==0)),1,1]+t_diff
    t_initial <- unique(stats::na.omit(r$output[1,1,]))

    if (r$model$.__enclos_env__$private$discrete) {
      t_steps <- lapply(t_steps, function(x) {
        seq(t_start, t_start - t_diff + time_period/r$parameters$dt, t_diff)
      })
    } else {
      t_steps <- lapply(t_steps, function(x) {
        seq(t_start, t_start - t_diff + time_period, t_diff)
      })
    }
    steps <- seq(t_initial, max(t_steps[[1]]), t_diff)

    arr_new <- array(NA, dim = c(which(r$output[,"time",1]==0) + length(t_steps[[1]]),
                                 ncol(r$output), dim(r$output)[3]))
    arr_new[seq_len(nrow(r$output)),,] <- r$output
    rownms <- rownames(r$output)
    colnms <- colnames(r$output)
    if(!is.null(rownms)) {
      rownames(arr_new) <- as.character(as.Date(rownms[1]) + seq_len(nrow(arr_new)) - 1L)
    }
    r$output <- arr_new
    colnames(r$output) <- colnms
    r$output[(which(r$output[,1,1]==(t_start-t_diff))+1):nrow(r$output),1,] <- matrix(unlist(t_steps), ncol = r$parameters$replicates)
  }

  # ----------------------------------------------------------------------------
  # conduct simulations
  # ----------------------------------------------------------------------------
  out <- lapply(seq_len(ds[3]), function(x) {

    # are we doing this replicate
    if (to_be_run[[x]]) {
      conduct_replicate(x = x,
                        r = r,
                        t_steps = t_steps,
                        R0 = R0[[x]],
                        R0_change = R0_change[[x]],
                        tt_R0 = tt_R0[[x]],
                        contact_matrix_set = contact_matrix_set[[x]],
                        contact_matrix_set_change = contact_matrix_set_change[[x]],
                        tt_contact_matrix = tt_contact_matrix[[x]],
                        hosp_bed_capacity = hosp_bed_capacity[[x]],
                        hosp_bed_capacity_change = hosp_bed_capacity_change[[x]],
                        tt_hosp_beds = tt_hosp_beds[[x]],
                        ICU_bed_capacity = ICU_bed_capacity[[x]],
                        ICU_bed_capacity_change = ICU_bed_capacity_change[[x]],
                        tt_ICU_beds = tt_ICU_beds[[x]],
                        model_user_args = model_user_args)
    } else {
      NULL
    }
  })

  ## get output columns that match
  cn <- colnames(r$output[which(r$output[,1,1] %in% t_steps[[1]]), , 1])
  out <- lapply(out, function(x) {
    if(!is.null(x)) {
      x[, which(colnames(x) %in% cn), , drop=FALSE]
    }
  })


  ## collect results
  # step handling for stochastic
  for(i in seq_len(ds[3])) {
    if (to_be_run[[i]]) {
      if(r$model$.__enclos_env__$private$discrete) {
        if(diff(tail(r$output[,1,1],2)) != 1) {
          r$output[which(r$output[,1,1] %in% t_steps[[i]]), -1, i] <- out[[i]][-1, -1, 1]
        } else {
          r$output[which(r$output[,1,1] %in% t_steps[[i]]), -1, i] <- out[[i]][, -1, 1]
        }
      }
      else {
        r$output[which(r$output[,1,1] %in% t_steps[[i]]), -1, i] <- out[[i]][-1, -1, 1]
      }
    }
  }

  ## append projections
  r$projection_args <- args
  r$projection_args$args$r <- NULL

  return(r)

}


#' Handles simulating replicates within \code{projections}
#'
#' @param x Simulation replicate number
#' @param r Output of \code{pmcmc} or \code{calibrate}
#' @param t_steps List of time steps to be run for each replicate
#' @inheritParams projections
#'
conduct_replicate <- function(x,
                              r = NULL,
                              t_steps = NULL,
                              R0 = NULL,
                              R0_change = NULL,
                              tt_R0 = NULL,
                              contact_matrix_set = NULL,
                              contact_matrix_set_change = NULL,
                              tt_contact_matrix = NULL,
                              hosp_bed_capacity = NULL,
                              hosp_bed_capacity_change = NULL,
                              tt_hosp_beds = NULL,
                              ICU_bed_capacity = NULL,
                              ICU_bed_capacity_change = NULL,
                              tt_ICU_beds = NULL,
                              model_user_args = NULL) {

  # what type of object isout squire_simulation
  if ("scan_results" %in% names(r)) {
    wh <- "scan_results"
  } else if ("pmcmc_results" %in% names(r)) {
    wh <- "pmcmc_results"
  }

  # final values of R0, contacts, and beds
  finals <- t0_variables(r)

  # odin model keys
  index <- odin_index(r$model)
  initials <- seq_along(r$model$initial()) + 1L
  ds <- dim(r$output)

  # what state time point do we want
  state_pos <- vapply(seq_len(ds[3]), function(x) {
    pos <- which(r$output[,"time",x] == 0)
    if(length(pos) == 0) {
      stop("projections needs time value to be equal to 0 to know how to project forwards")
    }
    return(pos)
  }, FUN.VALUE = numeric(1))

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
    r$model <- r[[wh]]$inputs$squire_model$odin_model(
      user = r[[wh]]$inputs$model_params,
      unused_user_action = "ignore")
  }

  # get the dt for the simulation
  dt_step <- r$parameters$dt

  # step handling for stochastic
  if(r$model$.__enclos_env__$private$discrete) {
    if(diff(tail(r$output[,1,1],2)) != 1) {
      step <- c(0,round(seq_len(length(t_steps[[x]]))/r$parameters$dt))
    } else {
      step <- seq_len(length(t_steps[[x]]))
      dt_step <- 1
    }
  } else {
    if(diff(tail(r$output[,1,1],2)) != 1) {
      step <- c(0,round(seq_len(length(t_steps[[x]]))*r$parameters$dt))
    } else {
      step <- c(0, seq_len(length(t_steps[[x]])))
      dt_step <- 1
    }
  }

  # change these user params
  r$model$set_user(tt_beta = round(tt_R0/dt_step))
  r$model$set_user(beta_set = beta)
  r$model$set_user(tt_matrix = round(tt_contact_matrix/dt_step))
  r$model$set_user(mix_mat_set = matrices_set)
  r$model$set_user(tt_hosp_beds = round(tt_hosp_beds/dt_step))
  r$model$set_user(hosp_beds = hosp_bed_capacity)
  r$model$set_user(tt_ICU_beds = round(tt_ICU_beds/dt_step))
  r$model$set_user(ICU_beds = ICU_bed_capacity)

  # make sure these time varying parameters are also updated
  r$model$set_user(tt_dur_get_mv_die = 0)
  r$model$set_user(tt_dur_get_ox_die = 0)
  r$model$set_user(tt_dur_get_mv_survive = 0)
  r$model$set_user(tt_dur_get_ox_survive = 0)
  r$model$set_user(gamma_get_mv_die = finals[[x]]$gamma_get_mv_die)
  r$model$set_user(gamma_get_ox_die = finals[[x]]$gamma_get_ox_die)
  r$model$set_user(gamma_get_mv_survive = finals[[x]]$gamma_get_mv_survive)
  r$model$set_user(gamma_get_ox_survive = finals[[x]]$gamma_get_ox_survive)

  # and update any custom model user args
  if (!is.null(model_user_args)) {
    r$model$set_user(user = model_user_args[[x]])
  }

  # run the model
  get <- r$model$run(step,
                     y = as.numeric(r$output[state_pos[x], initials, x, drop=TRUE]),
                     use_names = TRUE,
                     replicate = 1)

  # coerce to array if deterministic
  if(length(dim(get)) == 2) {
    # coerce to array
    get <- array(get, dim = c(dim(get),1), dimnames = dimnames(get))
  }

  return(get)

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

  # specifics of r object
  dims <- dim(r$output)
  if("pmcmc_results" %in% names(r)) {
    wh <- "pmcmc_results"
  } else {
    wh <- "scan_results"
  }

  # quick check to make sure has t == 0
  has_t0 <- vapply(seq_len(dims[3]), function(x) {any(r$output[,"time",x] == 0)}, logical(1))
  if(!all(has_t0)) {
    stop("r$output does not have a time value = 0")
  }

  # is this the outputs of a grid scan
  if("scan_results" %in% names(r) || "pmcmc_results" %in% names(r)) {

    # grab the final R0, contact matrix and bed capacity.
    ret <- lapply(seq_len(dims[3]), function(x) {

      if("scan_results" %in% names(r)) {

        if(!is.null(r$interventions$R0_change)) {
          if (is.null(r$replicate_parameters$Meff)) {
            R0 <- tail(r$replicate_parameters$R0[x] * r$interventions$R0_change, 1)
          } else {
            R0 <- r[[wh]]$inputs$Rt_func(R0 = r$replicate_parameters$R0[x],
                                         R0_change = tail(r$interventions$R0_change, 1),
                                         Meff = r$replicate_parameters$Meff[x])
          }
        } else {
          R0 <- r$replicate_parameters$R0[x]
        }

      } else if (("pmcmc_results" %in% names(r))) {

        if(!is.null(r$interventions$R0_change)) {

          pars <- as.list(r$replicate_parameters[x,-which(names(r$replicate_parameters) %in% c("start_date", "R0"))])
          names(pars) <- names(r$replicate_parameters)[-which(names(r$replicate_parameters) %in% c("start_date", "R0"))]
          R0 <- tail(evaluate_Rt_pmcmc(R0_change = r$interventions$R0_change,
                                       R0 = r$replicate_parameters$R0[x],
                                       date_R0_change = r$interventions$date_R0_change,
                                       pars = pars,
                                       Rt_args = r[[wh]]$inputs$Rt_args), 1)

        } else {
          R0 <- r$replicate_parameters$R0[x]
        }

      }


      # and return with time varying durations and other time varying components
      return(
        list(
          R0 = R0,
          contact_matrix_set = tail(r$parameters$contact_matrix_set,1),
          hosp_bed_capacity = tail(r$parameters$hosp_bed_capacity,1),
          ICU_bed_capacity = tail(r$parameters$ICU_bed_capacity,1),
          gamma_get_ox_survive = tail(r$parameters$gamma_get_ox_survive,1),
          gamma_get_ox_die = tail(r$parameters$gamma_get_ox_die,1),
          gamma_get_mv_die = tail(r$parameters$gamma_get_mv_die,1),
          gamma_get_mv_survive = tail(r$parameters$gamma_get_mv_survive,1)
        )
      )
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

      last <- tail(which(r$parameters$tt_dur_get_ox_survive < state_pos[i]), 1)
      gamma_get_ox_survive <- r$parameters$gamma_get_ox_survive[last]

      last <- tail(which(r$parameters$tt_dur_get_ox_die < state_pos[i]), 1)
      gamma_get_ox_die <- r$parameters$gamma_get_ox_die[last]

      last <- tail(which(r$parameters$tt_dur_get_mv_die < state_pos[i]), 1)
      gamma_get_mv_die <- r$parameters$gamma_get_mv_die[last]

      last <- tail(which(r$parameters$tt_dur_get_mv_survive < state_pos[i]), 1)
      gamma_get_mv_survive <- r$parameters$gamma_get_mv_survive[last]

      return(
        list(
          R0 = R0,
          contact_matrix_set = contact_matrix_set,
          hosp_bed_capacity = hosp_bed_capacity,
          ICU_bed_capacity = ICU_bed_capacity,
          gamma_get_ox_survive = gamma_get_ox_survive,
          gamma_get_ox_die = gamma_get_ox_die,
          gamma_get_mv_die = gamma_get_mv_die,
          gamma_get_mv_survive = gamma_get_mv_survive
        )
      )

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
