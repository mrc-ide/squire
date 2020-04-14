

#' Collapse age groups in output
#'
#' Sums over age groups for each compartment
#'
#' @param d output data.frame
#'
#' @return Output data.frame
collapse_age <- function(d){
  d %>%
    dplyr::group_by(.data$compartment, .data$t, .data$replicate) %>%
    dplyr::summarise(y =  sum(.data$y)) %>%
    dplyr::ungroup()
}

#' Collapse compartments for reporting major reporters of epidemic
#'
#' Sums over simplified groups (ICU demand, ICU occupance, hospital demand,
#' hospital occupancy, infections, deaths)
#'
#' @param d output data.frame
#'
#' @return Output data.frame
collapse_for_report <- function(d){

  if ("date" %in% names(d)) {
  d %>%
    dplyr::mutate(group = dplyr::case_when(
      grepl("IMV", .data$compartment) ~ "ICU",
      grepl("IOx", .data$compartment) ~ "hospital",
      .data$compartment == "n_E2_I" ~ "infections",
      .data$compartment == "delta_D" ~ "deaths",
      TRUE ~ .data$compartment)) %>%
    dplyr::group_by(.data$group, .data$t, .data$date, .data$replicate) %>%
    dplyr::summarise(y = sum(.data$y)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(compartment = .data$group)
  } else {
    d %>%
      dplyr::mutate(group = dplyr::case_when(
        grepl("IMV", .data$compartment) ~ "ICU",
        grepl("IOx", .data$compartment) ~ "hospital",
        .data$compartment == "n_E2_I" ~ "infections",
        .data$compartment == "delta_D" ~ "deaths",
        TRUE ~ .data$compartment)) %>%
      dplyr::group_by(.data$group, .data$t, .data$replicate) %>%
      dplyr::summarise(y = sum(.data$y)) %>%
      dplyr::ungroup() %>%
      dplyr::rename(compartment = .data$group)
}

}
#' Format model output as data.frame
#'
#' @param x squire_simulation object
#' @param var_select Vector of compartment names, e.g. \code{c("S", "R")}
#' @param reduce_age Collapse age-dimension
#' @param combine_compartments Collapse compartments of same type together (e.g. E1 and E2 -> E)
#' @param date_0 Date of time 0, if specified a date column will be added
#'
#' @return Formatted long data.frame
#' @export
format_output <- function(x, var_select = NULL, reduce_age = TRUE,
                          combine_compartments = TRUE, date_0 = NULL){
  # Single Compartment Variables
  single_variables <- c("S", "IMild", "R", "D", "n_E2_I",
                        "n_E2_ICase1", "n_E2_IMild", "delta_D")

  # Select variables
  vars <- x$output

  # Variable names
  all_names <- names(vars[1,,1])
  all_names_simp <- gsub("\\[.*?]", "", all_names)

  if(!all(var_select %in% c(all_names_simp, unique(gsub("[1-2]$", "", all_names_simp))))){
    stop("Selected variable are not all present in output")
  }

  # Select relevant column names for subsetting
  #   if - if no variables are selected, use all for subsetting
  #   else if - if some variables are selected and none are single-compartment variables,
  #             extract all compartments with compartment_name1 or compartment_name2
  #   else - if some of the variables are single-compartment variables, work through
  #          each individually, treating single and double compartment variables differently
  number_variables <- length(var_select)
  if(is.null(var_select)){
    var_select <- unique(all_names_simp[!all_names_simp %in% c("step", "time")])
  } else if (!is.null(var_select) & sum(var_select %in% single_variables) == 0)  {
    new_var_select <- c()
    for (i in 1:number_variables) {
      temp <- unique(all_names_simp[grepl(paste0("^", var_select[i], "[1-2]"), all_names_simp)])
      new_var_select <- c(new_var_select, temp)
    }
    var_select <- new_var_select
  } else {
    new_var_select <- c()
    for (i in 1:number_variables) {
      if (var_select[i] %in% single_variables) {
        new_var_select <- c(new_var_select, var_select[i])
      } else {
        temp <- unique(all_names_simp[grepl(paste0("^", var_select[i], "[1-2]"), all_names_simp)])
        new_var_select <- c(new_var_select, temp)
      }
    }
    var_select <- new_var_select
  }
  if(!all(var_select %in% all_names_simp)){
    stop("Selected variable are not all present in output")
  }

  # Subsetting relevant columns
  vars <- vars[,all_names_simp %in% var_select , ,drop = FALSE]

  # Select components of data.frame
  raw_names <- gsub("\\[.*?]", "", names(vars[1,,1]))
  age_groups <- 1:table(raw_names)[1]
  compartments <- unique(raw_names)
  time <- vapply(seq_len(dim(x$output)[3]), function(y) {
    rep(x$output[,"time",y],length(compartments) * length(age_groups))
  }, FUN.VALUE = numeric(length(compartments) * length(age_groups) * nrow(x$output[,,1])))

  # time <- as.vector(apply(x$output[,"time",, drop = FALSE], 2, function(x){
  #   rep(x, length(compartments) * length(age_groups))
  # }))

  # Generating and filling output
  out <- tidyr::expand_grid(replicate = 1:x$parameters$replicates,
                            compartment = compartments,
                            age_group = age_groups,
                            t = x$output[,"time", 1])
  out$t <- as.vector(time)
  out$y <- as.vector(vars)

  # If combine_compartments is TRUE, sum compartments of same type e.g.
  # E1 and E2 together
  if (combine_compartments == TRUE) {
    out <- out %>%
      dplyr::mutate(compartment = gsub("[1-2]$", "", .data$compartment)) %>%
      dplyr::group_by(.data$replicate, .data$age_group, .data$compartment, .data$t) %>%
      dplyr::summarise(y = sum(.data$y))
  }

  # Collapse ages
  if(reduce_age){
    out <- collapse_age(out)
  }

  # Add date
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

#' Extract report summaries
#'
#' @param x squire_simulation object
#' @param date_0 Date of time 0, if specified a date column will be added
#'
#' @return Formatted long data.frame
#' @export
extract_report_summaries <- function(x, date_0 = NULL){
  output <- format_output(x, reduce_age = TRUE, combine_compartments = FALSE,
                          date_0 = date_0)
  output <- collapse_for_report(output)

  return(output)
}
