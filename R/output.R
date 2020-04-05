#' Create data.frame from odin output element
#'
#' @param m Model
#' @param compartment Output name (from odin output)
#'
#' @return Data.frame for variable
df_output <- function(m, compartment){
  as.data.frame.table(m[[compartment]], responseName = "y") %>%
    dplyr::mutate(t = as.numeric(.data$Var1),
                  age_group = as.numeric(.data$Var2),
                  replicate = as.numeric(.data$Var3),
                  compartment = compartment) %>%
    dplyr::select(.data$t, .data$age_group, .data$replicate,
                  .data$compartment, .data$y)
}


#' Convert model output to long format
#'
#' @param m Model output
#'
#' @return Long format model output data.frame
#' @export
long_output <- function(m){
  vars <- names(m)[grepl("^[[:upper:]]+$", substr(names(m), 1, 1))]
  o1 <- dplyr::bind_rows(lapply(vars, df_output, m = m))
  o1$t <- m$time[o1$t,1]
  o1$compartment <- factor(o1$compartment, levels = vars)
  return(o1)
}
