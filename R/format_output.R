#' Format deterministic model output as data.frame
#'
#' @param x squire_simulation object
#'
#' @return Formatted long data.frame
#' @export
format_deterministic_output <- function(x) {
  x$output <- x$output[,,1,drop=TRUE]
  index <- odin_index(x$model)

  hospital_demand = c(
    "IOxGetLive1","IOxGetLive2","IOxGetDie1","IOxGetDie2",
    "IRec1", "IRec2","IOxNotGetLive1","IOxNotGetLive2","IOxNotGetDie1",
    "IOxNotGetDie2")
  ICU_demand = c(
    "IMVGetLive1","IMVGetLive2","IMVGetDie1","IMVGetDie2",
    "IMVNotGetLive1","IMVNotGetLive2","IMVNotGetDie1","IMVNotGetDie2")

  diff_variable_compartments <- list(
    deaths = "D"
  )

  summary_variable_compartments <- list(
    infections = c("IMild", "ICase1", "ICase2"),
    hospital_demand = hospital_demand,
    ICU_demand = ICU_demand,
    hospital_incidence = "number_requiring_Ox",
    ICU_incidence = "number_requiring_IMV"
  )

  diff_data <- vapply(
    diff_variable_compartments,
    function(compartments) {
      c(
        sum(x$output[1,unlist(index[compartments])]),
        diff(rowSums(x$output[,unlist(index[compartments])]))
      )
    },
    numeric(dim(x$output)[[1]])
  )

  sum_data <- vapply(
    summary_variable_compartments,
    function(compartments) {
      rowSums(x$output[,unlist(index[compartments])])
    },
    numeric(dim(x$output)[[1]])
  )

  aggregated <- cbind(x$output[, 't'], diff_data, sum_data)
  colnames(aggregated)[[1]] <- 't'
  wide_df <- as.data.frame(aggregated)
  cols <- names(wide_df)[names(wide_df) != 't']
  out <- stats::reshape(
    wide_df,
    cols,
    idvar = 't',
    timevar = 'compartment',
    direction = 'long',
    v.names = 'value'
  )
  out$compartment <- vapply(
    out$compartment,
    function(i) cols[[i]], character(1)
  )

  out
}

#' Format model output as data.frame
#'
#' @param x squire_simulation object
#' @param var_select Vector of compartment names, e.g. \code{c("S", "R")}. In
#'   addition a number of summary compartment can be requested. These include:
#' \itemize{
#'       \item{"deaths"}{ Daily Deaths }
#'       \item{"infections"}{ Daily Infections }
#'       \item{"hospital_occupancy"}{ Occupied Hospital Beds }
#'       \item{"ICU_occupancy"}{ Occupied ICU Beds }
#'       \item{"hospital_demand}{ Required Hospital Beds }
#'       \item{"ICU_demand}{ Required ICU Beds }
#'       }
#' @param reduce_age Collapse age-dimension, calculating the total in the
#'   compartment.
#' @param combine_compartments Collapse compartments of same type together
#'   (e.g. E1 and E2 -> E)
#' @param date_0 Date of time 0, if specified a date column will be added
#'
#' @return Formatted long data.frame
#' @export
format_output <- function(x, var_select = NULL, reduce_age = TRUE,
                          combine_compartments = TRUE, date_0 = NULL){

  # Get relevant model details
  nt <- nrow(x$output)
  index <- odin_index(x$model)
  all_names <- names(x$output[1,,1])
  all_names_simp <- gsub("\\[.*?]", "", all_names)

  # Multi/Single Compartment Variables
  single_compartments <- c("S", "IMild", "R", "D", "n_E2_I",
                           "n_E2_ICase1", "n_E2_IMild", "delta_D")
  multi_compartments <- c("E", "ICase", "IOxGetLive", "IOxGetDie", "IOxNotGetLive", "IOxNotGetDie",
                          "IMVGetLive", "IMVGetDie", "IMVNotGetLive", "IMVNotGetDie", "IRec")
  all_case_compartments <- unlist(
    index[c("IMild", "ICase1", "ICase2", "IOxGetLive1", "IOxGetLive2",
            "IOxGetDie1", "IOxGetDie2", "IOxNotGetLive1", "IOxNotGetLive2",
            "IOxNotGetDie1", "IOxNotGetDie2", "IMVGetLive1", "IMVGetLive2",
            "IMVGetDie1", "IMVGetDie2", "IMVNotGetLive1", "IMVNotGetLive2",
            "IMVNotGetDie1", "IMVNotGetDie2", "IRec1", "IRec2", "R", "D")])

  ## Check they are available if simple model
  if(grepl("simple", x$model$ir[2])) {
    if(sum(!(var_select %in% c("S", "E", "I", "R", "n_EI")) > 0)) {
      stop("Selected variable are not all present in output. Variables must be one of:\n\n",
           "S, E, I, R, n_EI")
    }
  }

  ## Fix for handling deterministic outputs
  # ----------------------------------------------------------------------------

  # is this output without n_E2_I
  if(!any(grepl("n_E2_I",colnames(x$output))) && !grepl("simple", x$model$ir[2])) {

    index$n_E2_I <- seq(tail(unlist(index),1)+1, tail(unlist(index),1)+length(index$S),1)
    dimnms <- dimnames(x$output)
    dimnms[[2]] <- c(dimnms[[2]], paste0("n_E2_I[", seq_len(length(index$S)),"]"))
    new_data <- array(data = NA,
                      dim = c(dim(x$output) + c(0, length(index$n_E2_I), 0)),
                      dimnames = dimnms)

    new_data[, seq_len(dim(x$output)[2]), ] <- x$output
    x$output <- new_data

  }

  # is this output without delta_D
  if(!any(grepl("delta_D",colnames(x$output))) && !grepl("simple", x$model$ir[2])) {

    index$delta_D <- seq(tail(unlist(index),1)+1, tail(unlist(index),1)+length(index$S),1)
    dimnms <- dimnames(x$output)
    dimnms[[2]] <- c(dimnms[[2]], paste0("delta_D[", seq_len(length(index$S)),"]"))
    new_data <- array(data = NA,
                      dim = c(dim(x$output) + c(0, length(index$delta_D), 0)),
                      dimnames = dimnms)

    new_data[, seq_len(dim(x$output)[2]), ] <- x$output
    x$output <- new_data

  }


  ## Fix for handling day_return outputs
  # ----------------------------------------------------------------------------

  # Generate hospital incidence at each timestep from the cumulative hospital incidence
  # backwards compatible
  if ("cum_hosp_inc" %in% names(index)) {
  if(!grepl("simple", x$model$ir[2])) {
  for(i in seq_along(x$parameters$population)) {
    collect <- vapply(1:x$parameters$replicates, function(j) {
      pos <- seq(i, length(index$cum_hosp_inc), by = length(x$parameters$population))
      pos <- index$cum_hosp_inc[pos]
      diff(x$output[,pos,j])
    }, FUN.VALUE = numeric(nt-1))
    x$output[1+seq_len(nt-1),index$cum_hosp_inc[i],] <- collect
  }

  # Generate ICU incidence at each timestep from the cumulative ICU incidence
  for(i in seq_along(x$parameters$population)) {
    collect <- vapply(1:x$parameters$replicates, function(j) {
      pos <- seq(i, length(index$cum_ICU_inc), by = length(x$parameters$population))
      pos <- index$cum_ICU_inc[pos]
      diff(x$output[,pos,j])
    }, FUN.VALUE = numeric(nt-1))
    x$output[1+seq_len(nt-1),index$cum_ICU_inc[i],] <- collect
  }
  }
  }

  # are the steps not 1 apart? if so we need to sum the incident variables (infecions/deaths)
  if(!grepl("simple", x$model$ir[2])) {
  if (x$parameters$day_return || !x$model$.__enclos_env__$private$discrete) {

    # assign the infections
    for(i in seq_along(x$parameters$population)) {
      collect <- vapply(1:x$parameters$replicates, function(j) {
        pos <- seq(i,length(all_case_compartments), by = length(x$parameters$population))
        pos <- all_case_compartments[pos]
        diff(rowSums(x$output[,pos,j]))
      }, FUN.VALUE = numeric(nt-1))
      x$output[1+seq_len(nt-1),index$n_E2_I[i],] <- collect
    }

    # assign the deaths
    for(i in seq_along(x$parameters$population)) {
      collect <- vapply(1:x$parameters$replicates, function(j) {
        pos <- seq(i, length(index$D), by = length(x$parameters$population))
        pos <- index$D[pos]
        diff(x$output[,pos,j])
      }, FUN.VALUE = numeric(nt-1))
      x$output[1+seq_len(nt-1),index$delta_D[i],] <- collect
    }

  }
  }

  # Summary Values and Relevant Compartments
  summary_variables <- c("deaths", "infections", "hospital_occupancy",
                         "ICU_occupancy", "hospital_demand", "ICU_demand", "hospital_incidence", "ICU_incidence")
  summary_variable_compartments <- list(
    deaths = "delta_D",
    infections = "n_E2_I",
    hospital_occupancy = c("IOxGetLive1","IOxGetLive2","IOxGetDie1","IOxGetDie2", "IRec1", "IRec2"),
    ICU_occupancy = c("IMVGetLive1","IMVGetLive2","IMVGetDie1","IMVGetDie2"),
    hospital_demand = c("IOxGetLive1","IOxGetLive2","IOxGetDie1","IOxGetDie2", "IRec1", "IRec2",
                        "IOxNotGetLive1","IOxNotGetLive2","IOxNotGetDie1","IOxNotGetDie2"),
    ICU_demand = c("IMVGetLive1","IMVGetLive2","IMVGetDie1","IMVGetDie2",
                   "IMVNotGetLive1","IMVNotGetLive2","IMVNotGetDie1","IMVNotGetDie2"),
    hospital_incidence = "cum_hosp_inc",
    ICU_incidence = "cum_ICU_inc"
  )

  # Check var_select contains only variables described above
  if(sum(!(var_select %in% c(single_compartments, multi_compartments, summary_variables))) > 0) {
    stop("Selected variable are not all present in output. Either specify a compartment:\n\n",
         paste0(c(paste0(single_compartments, collapse = ","), "\n\n",
                  paste0(multi_compartments, collapse = ","))),
         "\n\nor a summary compartment:\n\n",
         paste0(summary_variables, collapse = ", "))
  }

  # Disaggregating var_select into compartments and summary variables
  compartments <- var_select[!(var_select %in% summary_variables)]
  compartments <- if (identical(compartments, character(0))) NULL else compartments
  summaries <- var_select[var_select %in% summary_variables]
  summaries <- if (identical(summaries, character(0))) NULL else summaries

  # Extracting relevant columns for compartment variables
  # -> if var_select = NULL extract all compartments
  # -> if var_select = names specific compartments, extract those
  # -> if var_select = summary variables but no specific compartments, return empty list
  if(is.null(var_select)) {
    compartments <- unique(all_names_simp[!all_names_simp %in% c("step", "time")])
    compartment_output_list <- lapply(compartments, function(j) {
      temp <- x$output[,unlist(index[j]),]
      temp_array <- array(temp, dim = c(dim(temp)[1], dim(temp)[2], x$parameters$replicates))
      odin_sv(temp_array, replicates = x$parameters$replicates, nt = nt, reduce_age)
    })
    names(compartment_output_list) <- compartments
  } else if (!is.null(var_select) & !is.null(compartments)) {
    number_variables <- length(compartments)
    new_compartments <- c()
    for (i in 1:number_variables) {
      if (compartments[i] %in% single_compartments) {
        new_compartments <- c(new_compartments, compartments[i])
      } else {
        temp <- unique(all_names_simp[grepl(paste0("^", compartments[i], "[1-2]"), all_names_simp)])
        new_compartments <- c(new_compartments, temp)
      }
    }
    compartment_output_list <- lapply(new_compartments, function(j) {
      temp <- x$output[,unlist(index[j]),]
      temp_array <- array(temp, dim = c(dim(temp)[1], dim(temp)[2], x$parameters$replicates))
      odin_sv(temp_array, replicates = x$parameters$replicates, nt = nt, reduce_age)
    })
    names(compartment_output_list) <- new_compartments
  } else {
    compartment_output_list <- list()
  }

  # summaries
  if (!is.null(var_select) & !is.null(summaries)) {
    summaries_output_list <- vector(mode = "list", length = length(summaries))
    for (i in 1:length(summaries)) {
      indices <- which(summary_variables %in% summaries[i])
      temp_compartments <- summary_variable_compartments[[indices]]
      temp <- x$output[,unlist(index[temp_compartments]),]
      temp_array <- array(temp, dim = c(dim(temp)[1], dim(temp)[2], x$parameters$replicates))
      summaries_output_list[[i]] <- odin_sv(temp_array, replicates = x$parameters$replicates, nt = nt, reduce_age)
    }
    names(summaries_output_list) <- summaries
  } else {
    summaries_output_list <- list()
  }

  # combining outputs for compartments and overall summaries into 1 list
  output_list <- c(compartment_output_list, summaries_output_list)
  vars <- names(output_list)
  n_age_groups <- 17

  # generating df of extracted compartment/summary outputs, disaggregated by age or not
  if (reduce_age == TRUE) {
    out <- data.frame("t" = as.numeric(x$output[,index$time,]),
                      "replicate" = as.numeric(mapply(rep, seq_len(x$parameters$replicates), nt)),
                      "compartment" = as.character(mapply(rep, vars, nt*x$parameters$replicates)),
                      "y" = unlist(output_list))
  } else {
    out <- data.frame("t" = rep(as.numeric(x$output[,index$time, ]), n_age_groups), # ASK OJ TO CHECK THIS
                      "age_group" = rep(1:n_age_groups, each = nt), ##### NEED TO CHANGE ####
                      "replicate" = as.numeric(mapply(rep, seq_len(x$parameters$replicates), n_age_groups * nt)),
                      "compartment" = as.character(mapply(rep, vars, n_age_groups*nt*x$parameters$replicates)),
                      "y" = unlist(output_list))
  }

  # If combine_compartments is TRUE, sum compartments of same type e.g.
  # E1 and E2 together
  if (combine_compartments == TRUE & reduce_age == FALSE) {
    out <- out %>%
      dplyr::mutate(compartment = gsub("[1-2]$", "", .data$compartment)) %>%
      dplyr::group_by(.data$replicate, .data$age_group, .data$compartment, .data$t) %>%
      dplyr::summarise(y = sum(.data$y)) %>%
      dplyr::ungroup()
  } else if (combine_compartments == TRUE & reduce_age == TRUE) {
    out <- out %>%
      dplyr::mutate(compartment = gsub("[1-2]$", "", .data$compartment)) %>%
      dplyr::group_by(.data$replicate, .data$compartment, .data$t) %>%
      dplyr::summarise(y = sum(.data$y)) %>%
      dplyr::ungroup()
  }

  # replacting time with date if date_0 is provided
  if(!is.null(date_0)){
    assert_date(date_0)
    out$date <- as.Date(out$t + as.Date(date_0),
                        format = "%Y-%m-%d")
  }

  return(out)
}



#' Format model output from simple as data.frame
#'
#' @param x squire_simulation object
#' @param var_select Vector of compartment names, e.g. \code{c("S", "R")}
#' @param reduce_age Collapse age-dimension
#' @param combine_compartments Collapse compartments of same type together (e.g. E1 and E2 -> E)
#' @param date_0 Date of time 0, if specified a date column will be added
#'
#' @return Formatted long data.frame
#' @export
format_output_simple_model <- function(x, var_select = NULL, reduce_age = TRUE,
                                       combine_compartments = TRUE, date_0 = NULL){

  # Get relevant model details
  nt <- nrow(x$output)
  index <- odin_index(x$model)
  all_names <- names(x$output[1,,1])
  all_names_simp <- gsub("\\[.*?]", "", all_names)

  # Multi/Single Compartment Variables
  single_compartments <- c("S", "I", "R", "n_EI")
  multi_compartments <- c("E")

  # Check var_select contains only variables described above
  if(sum(!(var_select %in% c(single_compartments, multi_compartments))) > 0) {
    stop("Selected variable are not all present in output")
  }

  # Extracting relevant columns for compartment variables
  # -> if var_select = NULL extract all compartments
  # -> if var_select = names specific compartments, extract those
  # -> if var_select = summary variables but no specific compartments, return empty list
  compartments <- var_select
  if(is.null(var_select)) {
    compartments <- unique(all_names_simp[!all_names_simp %in% c("step", "time")])
    compartment_output_list <- lapply(compartments, function(j) {
      temp <- x$output[,unlist(index[j]),]
      temp_array <- array(temp, dim = c(dim(temp)[1], dim(temp)[2], x$parameters$replicates))
      odin_sv(temp_array, replicates = x$parameters$replicates, nt = nt, reduce_age)
    })
    names(compartment_output_list) <- compartments
  } else if (!is.null(var_select) & !is.null(compartments)) {
    number_variables <- length(compartments)
    new_compartments <- c()
    for (i in 1:number_variables) {
      if (compartments[i] %in% single_compartments) {
        new_compartments <- c(new_compartments, compartments[i])
      } else {
        temp <- unique(all_names_simp[grepl(paste0("^", compartments[i], "[1-2]"), all_names_simp)])
        new_compartments <- c(new_compartments, temp)
      }
    }
    compartment_output_list <- lapply(new_compartments, function(j) {
      temp <- x$output[,unlist(index[j]),]
      temp_array <- array(temp, dim = c(dim(temp)[1], dim(temp)[2], x$parameters$replicates))
      odin_sv(temp_array, replicates = x$parameters$replicates, nt = nt, reduce_age)
    })
    names(compartment_output_list) <- new_compartments
  }

  # combining outputs for compartments and overall summaries into 1 list
  output_list <- compartment_output_list
  vars <- names(output_list)
  n_age_groups <- 16

  # generating df of extracted compartment/summary outputs, disaggregated by age or not
  if (reduce_age == TRUE) {
    out <- data.frame("t" = as.numeric(x$output[,index$time,]),
                      "replicate" = as.numeric(mapply(rep, seq_len(x$parameters$replicates), nt)),
                      "compartment" = as.character(mapply(rep, vars, nt*x$parameters$replicates)),
                      "y" = unlist(output_list))
  } else {
    out <- data.frame("t" = rep(as.numeric(x$output[,index$time, ]), n_age_groups), # ASK OJ TO CHECK THIS
                      "age_group" = rep(1:n_age_groups, each = nt), ##### NEED TO CHANGE ####
                      "replicate" = as.numeric(mapply(rep, seq_len(x$parameters$replicates), n_age_groups * nt)),
                      "compartment" = as.character(mapply(rep, vars, n_age_groups*nt*x$parameters$replicates)),
                      "y" = unlist(output_list))
  }

  # If combine_compartments is TRUE, sum compartments of same type e.g.
  # E1 and E2 together
  if (combine_compartments == TRUE & reduce_age == FALSE) {
    out <- out %>%
      dplyr::mutate(compartment = gsub("[1-2]$", "", .data$compartment)) %>%
      dplyr::group_by(.data$replicate, .data$age_group, .data$compartment, .data$t) %>%
      dplyr::summarise(y = sum(.data$y)) %>%
      dplyr::ungroup()
  } else if (combine_compartments == TRUE & reduce_age == TRUE) {
    out <- out %>%
      dplyr::mutate(compartment = gsub("[1-2]$", "", .data$compartment)) %>%
      dplyr::group_by(.data$replicate, .data$compartment, .data$t) %>%
      dplyr::summarise(y = sum(.data$y)) %>%
      dplyr::ungroup()
  }

  # replacting time with date if date_0 is provided
  if(!is.null(date_0)){
    stopifnot(inherits(date_0, "Date"))
    out$date <- as.Date(out$t + date_0,
                        format = "%d/%m/%y")
  }

  return(out)
}


#' Extract deaths from model output
#'
#' @param x squire_simulation object
#' @param reduce_age Collapse age-dimension
#' @param date_0 Date of time 0, if specified a date column will be added
#'
#' @return Formatted long data.frame
#' @export
extract_deaths <- function(x, reduce_age = TRUE, date_0 = NULL){
  output <- format_output(x, var_select = "delta_D", reduce_age = reduce_age,
                          date_0 = date_0)
  output$replicate <- factor(output$replicate)
  return(output)
}

#' Extract infection incidence from model output
#'
#' @param x squire_simulation object
#' @param reduce_age Collapse age-dimension
#' @param date_0 Date of time 0, if specified a date column will be added
#'
#' @return Formatted long data.frame
#' @export
extract_infection_incidence <- function(x, reduce_age = TRUE, date_0 = NULL){
  output <- format_output(x, var_select = "n_E2_I", reduce_age = reduce_age,
                          date_0 = date_0)
  output$replicate <- factor(output$replicate)
  return(output)
}

#' Extract hospital bed occupancy from model output
#'
#' @param x squire_simulation object
#' @param reduce_age Collapse age-dimension
#' @param date_0 Date of time 0, if specified a date column will be added
#'
#' @return Formatted long data.frame
#' @export
extract_hospital_occ <- function(x, reduce_age = TRUE, date_0 = NULL){
  output <- format_output(x, var_select = c("IOxGetLive", "IOxGetDie", "IRec"),
                          date_0 = date_0)
  output <- output %>%
    dplyr::group_by(.data$t, .data$replicate) %>%
    dplyr::summarise(y = sum(.data$y))
  output$replicate <- factor(output$replicate)

  return(output)
}

#' Extract ICU bed occupancy from model output
#'
#' @param x squire_simulation object
#' @param reduce_age Collapse age-dimension
#' @param date_0 Date of time 0, if specified a date column will be added
#'
#' @return Formatted long data.frame
#' @export
extract_ICU_occ <- function(x, reduce_age = TRUE, date_0 = NULL){
  output <- format_output(x, var_select = c("IMVGetLive", "IMVGetDie"),
                          date_0 = date_0)
  output <- output %>%
    dplyr::group_by(.data$t, .data$replicate) %>%
    dplyr::summarise(y = sum(.data$y))
  output$replicate <- factor(output$replicate)

  return(output)
}
