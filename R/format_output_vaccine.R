#' Format vaccine model output as data.frame
#'
#' @param x squire_simulation object
#' @param compartments Vector of compartment names, e.g. \code{c("S", "R")}.
#' @param summaries Vecto of summary names, which may be:
#' \itemize{
#'       \item{"deaths"}{ Daily Deaths (stochatic model) }
#'       \item{"infections"}{ Daily Infections. New infections (note this is currently a slightly different definitionto the main Squire mode)}
#'       \item{"hospital_occupancy"}{ Occupied Hospital Beds }
#'       \item{"ICU_occupancy"}{ Occupied ICU Beds }
#'       \item{"hospital_demand}{ Required Hospital Beds }
#'       \item{"ICU_demand}{ Required ICU Beds }
#'       \item{"vaccinated"}{ Daily vaccines administered (Stochastic model)}
#'       }
#' @param reduce_age Collapse age-dimension, calculating the total in the
#'   compartment.
#' @param date_0 Date of time 0, if specified a date column will be added
#'
#' @return Formatted long data.frame
#' @export
format_vaccine <- function(x,
                           compartments = c("S", "E", "V", "E_vac",
                                            "IMild", "ICase", "IICU", "IHospital",
                                            "IRec", "R", "D", "N"),
                           summaries = c("deaths", "infections", "hospital_occupancy",
                                         "ICU_occupancy", "hospital_demand",
                                         "ICU_demand", "vaccines"),
                           reduce_age = TRUE,
                           date_0 = NULL){

  # Arg checks
  assert_custom_class(x, "squire_simulation")
  assert_logical(reduce_age)

  # Standardise output dimensions
  if(length(dim(x$output)) == 4){
    x$output <- abind::adrop(x$output, drop = c(FALSE, FALSE, FALSE, TRUE))
  }

  # Get columns indices of variables
  index <- odin_index(x$model)
  if(!all(compartments %in% names(index))){
    stop("Some compartments specified not output by model")
  }

  # Extract time
  time <- x$output[,index$t,1]
  # N replicates
  replicates = dim(x$output)[3]
  # Format over each replicate
  output <- list()
  for(i in 1:replicates){
    output[[i]] <- format_vaccine_internal(x = x, compartments = compartments, summaries = summaries,
                                           reduce_age = reduce_age, index = index,
                                           time = time, replicate = i)
  }
  output <- dplyr::bind_rows(output)
  # Add date
  if(!is.null(date_0)){
    assert_date(date_0)
    output$date <- as.Date(output$time + as.Date(date_0),
                           format = "%Y-%m-%d")
  }
  return(output)
}

#' Internals of Format vaccine model output as data.frame
#' @inheritParams format_vaccine
#' @param index odin ouput index
#' @param time time vector
#' @param replicate outpu replicate number
format_vaccine_internal <- function(x, compartments, summaries, reduce_age, index, time,
                                    replicate){


  # Select variables and summary outputs
  get <- c(compartments, summaries)
  get <- get[get %in% names(index)]
  i_select <- index[get]
  o <- lapply(i_select, function(x, y){
    y[,x,replicate]
  }, y = x$output)

  # Collapse age
  if(reduce_age){
    o <- lapply(o, collapse_age)
  } else {
    o <- lapply(o, add_age)
  }

  # Add names of variables
  for(i in 1:length(o)){
    o[[i]] <- data.frame(o[[i]]) %>%
      dplyr::mutate(compartment = names(o)[i])
  }

  # Add time and replicate columns
  o <- dplyr::bind_rows(o) %>%
    dplyr::mutate(t = rep(time, dplyr::n() / length(time)),
           replicate = replicate)

  return(o)
}

#' Keep age groups
#'
#' @param x age-disaggregated odin output matrix
#'
#' @return age-disaggregated output matrix
add_age <- function(x){
  m <- matrix(c(rep(1:ncol(x), each = (nrow(x))), as.vector(x)), ncol = 2)
  colnames(m) <- c("age_index", "value")
  return(m)
}
#' Collapse age groups
#'
#' @param x age-disaggregated odin output matrix
#'
#' @return age-aggregated output matrix
collapse_age <- function(x){
  m <- matrix(rowSums(x), ncol = 1)
  colnames(m) <- "value"
  return(m)
}

