
#' Summarise over time and replicates
#'
#' @param x output element
#'
#' @return Summary output element
vec_o <- function(x){
  as.vector(apply(x, 3, rowSums))
}


#' Convert squire_simulation object output to lond data.frame
#'
#' @param sim squire_simulation object
#'
#' @return Long data.frame of output
quick_long <- function(sim){
  # Transform if needed
  if(!is.null(sim$parameters$output_transform)){
    if(!sim$parameters$output_transform){
      sim$output <- sim$model$transform_variables(sim$output)
    }
  }
  o <- sim$output
  vars <- o[!names(o) %in% c("t", "time")]
  t <- o$time[,1]
  for(i in 1:length(vars)){
    vars[[i]] <- data.frame(compartment = names(vars)[i], t = t,
                            replicate = rep(1:sim$parameters$replicates, each = length(t)),
                            y = vec_o(vars[[i]]), stringsAsFactors = FALSE)
  }
  vars <- dplyr::bind_rows(vars)
  return(vars)
}
