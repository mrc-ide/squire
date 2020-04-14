

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

  # Get relevant model details
  nt <- nrow(x$output)
  index <- odin_index(x$model)
  all_names <- names(x$output[1,,1])
  all_names_simp <- gsub("\\[.*?]", "", all_names)

  # Multi/Single Compartment Variables
  single_compartments <- c("S", "IMild", "R", "D", "n_E2_I", "n_E2_ICase1", "n_E2_IMild", "delta_D")
  multi_compartments <- c("E", "ICase", "IOxGetLive", "IOxGetDie", "IOxNotGetLive", "IOxNotGetDie",
                          "IMVGetLive", "IMVGetDie", "IMVNotGetLive", "IMVNotGetDie", "IRec")

  # Summary Values and Relevant Compartments
  summary_variables <- c("deaths", "infections", "hospital_occupancy", "ICU_occupancy")
  summary_variable_compartments <- list(deaths = "delta_D",
                                        infections = "n_E2_I",
                                        hospital_occupancy = c("IOxGetLive1","IOxGetLive2","IOxGetDie1","IOxGetDie2", "IRec1", "IRec2"),
                                        ICU_occupancy = c("IMVGetLive1","IMVGetLive2","IMVGetDie1","IMVGetDie2"))

  # Check var_select contains only variables described above
  if(sum(!(var_select %in% c(single_compartments, multi_compartments, summary_variables))) > 0) {
    stop("Selected variable are not all present in output")
  }

  # Disaggregating var_select into compartments vs summary variables
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

  # generating df of extracted compartment/summary outputs, disaggregated by age or not
  if (reduce_age == TRUE) {
    out <- data.frame("t" = as.numeric(x$output[,index$time,]),
                      "replicate" = as.numeric(mapply(rep, seq_len(x$parameters$replicates), nt)),
                      "compartment" = as.character(mapply(rep, vars, nt*x$parameters$replicates)),
                      "y" = unlist(output_list))
  } else {
    out <- data.frame("t" = rep(as.numeric(x$output[,index$time, ]), 17), # ASK OJ TO CHECK THIS
                      "age_group" = rep(1:17, each = nt), ##### NEED TO CHANGE ####
                      "replicate" = as.numeric(mapply(rep, seq_len(x$parameters$replicates), 17 * nt)),
                      "compartment" = as.character(mapply(rep, vars, 17*nt*x$parameters$replicates)),
                      "y" = unlist(output_list))
  }

  # replacting time with date if date_0 is provided
  if(!is.null(date_0)){
    stopifnot(inherits(date_0, "Date"))
    out$date <- as.Date(out$t + date_0,
                        format = "%d/%m/%y")
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
