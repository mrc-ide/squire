#' Create data.frame from odin output element
#'
#' @param m Model
#' @param compartment Output name (from odin output)
#'
#' @return Data.frame for variable
df_output <- function(m, compartment){
  as.data.frame.table(m[[compartment]], responseName = "y") %>%
    dplyr::mutate(t = as.numeric(Var1),
                  age_group = as.numeric(Var2),
                  replicate = as.numeric(Var3),
                  compartment = compartment) %>%
    dplyr::select(t, age_group, replicate, compartment, y)
}


#' Convert model output to long format
#'
#' @param m Model output
#'
#' @return Long format model output data.frame
#' @export
long_output <- function(m){
  o1 <- dplyr::bind_rows(lapply(c("S", "E1", "E2", "I", "R"), df_output, m = m))
  o1$t <- m$time[o1$t,1]
  return(o1)
}
