

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

#' Collapse compartments in output
#'
#' Sums over simplified groups across compartments
#'
#' @param d output data.frame
#'
#' @return Output data.frame
collapse_compartment <- function(d){
  d %>%
    dplyr::mutate(group = dplyr::case_when(
      grepl("IMVGet", .data$compartment) ~ "ICU",
      grepl("IOxGet", .data$compartment) ~ "hospital",
      .data$compartment == "n_E2_I" ~ "infections",
      .data$compartment == "delta_D" ~ "deaths",
      TRUE ~ .data$compartment)) %>%
    dplyr::group_by(.data$group, .data$t, .data$replicate) %>%
    dplyr::summarise(y = sum(.data$y)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(compartment = .data$group)
}

#' Format model output as data.frame
#'
#' @param x squire_simulation object
#' @param var_select Vector of variable names
#' @param reduce_age Collapse age-dimension
#' @param reduce_compartment Collapse compartments
#' @param date_0 Date of time 0, if specified a date column will be added
#'
#' @return Formatted long data.frame
#' @export
format_output <- function(x, var_select = NULL, reduce_age = TRUE,
                          reduce_compartment = TRUE, date_0 = NULL){
  # Select variables
  vars <- x$output
  # Variable names
  all_names <- names(vars[1,,1])
  all_names_simp <- gsub("\\[.*?]", "", all_names)
  if(is.null(var_select)){
    var_select <- unique(all_names_simp[!all_names_simp %in% c("step", "time")])
  }
  if(!all(var_select %in% all_names_simp)){
    stop("Selected variable are not all present in output")
  }
  vars <- vars[,all_names_simp %in% var_select , ,drop = FALSE]
  # If reducing compartments, we must collapse ages
  if(reduce_compartment){
    reduce_age = TRUE
  }
  # Select components of data.frame
  raw_names <- gsub("\\[.*?]", "", names(vars[1,,1]))
  age_groups <- 1:table(raw_names)[1]
  compartments <- unique(raw_names)
  time <- x$output[,"time",1]
  # Output
  out <- tidyr::expand_grid(replicate = 1:x$parameters$replicates,
                            compartment = compartments,
                            age_group = age_groups,
                            t = time)
  out$y <- as.vector(vars)
  # Collapse ages
  if(reduce_age){
    out <- collapse_age(out)
  }
  # Combine compartments
  if(reduce_compartment){
    out <- collapse_compartment(out)
  }
  # Add date
  if(!is.null(date_0)){
    stopifnot(inherits(date_0, "Date"))
    out$date <- as.Date(out$t + date_0,
                        format = "%d/%m/%y")
  }
  return(out)
}
