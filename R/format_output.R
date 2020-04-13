

#' Collapse age groups in output
#'
#' Sums over age groups for each compartment
#'
#' @param d output data.frame
#'
#' @return Output data.frame
collapse_age <- function(d){
  d %>%
    group_by(compartment, t, replicate) %>%
    summarise(y =  sum(y)) %>%
    ungroup()
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
    mutate(group = case_when(grepl("IMV", compartment) ~ "ICU",
                             grepl("IOx", compartment) ~ "Beds",
                             TRUE ~ compartment)) %>%
    group_by(group, t, replicate) %>%
    summarise(y = sum(y)) %>%
    ungroup() %>%
    rename(compartment = group)
}

#' Format model output as data.frame
#'
#' @param x squire_simulation object
#' @param var_select Vector of variable names
#' @param reduce_age Collapse age-dimension
#' @param reduce_compartment Collapse compartments
#'
#' @return Formatted long data.frame
#' @export
format_output <- function(x, var_select = NULL, reduce_age = TRUE,
                          reduce_compartment = TRUE){
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
  vars <- vars[,all_names_simp %in% var_select ,]
  # If reducing compartments, we must collapse ages
  if(reduce_compartment){
    reduce_age = TRUE
  }
  # Select components of data.frame
  raw_names <- gsub("\\[.*?]", "", names(vars[1,,1]))
  age_groups <- 1:sum(raw_names == ("S"))
  compartments <- unique(raw_names)
  time <- x$output[,"time",1]
  # Output
  out <- tidyr::expand_grid(replicate = 1:r1$parameters$replicates,
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
  return(out)
}
