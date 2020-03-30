#' Estimate beta parameter
#'
#' @param duration_infectiousness Duration of infectiousness (days)
#' @param mixing_matrix Mixing matrix
#' @param R0 Basic reproduction number
#'
#' @return Beta parameter
#' @export
#'
# #' @examples
beta_est <- function(duration_infectiousness, mixing_matrix, R0) {
  if(length(duration_infectiousness) > 1){
    stop("duration_infectiousness must be of length = 1")
  }
  if(!is.numeric(duration_infectiousness) | length(duration_infectiousness) > 1 |
     duration_infectiousness < 0){
    stop("duration_infectiousness must be a positive numeric value")
  }
  if(length(R0) > 1){
    stop("R0 must be of length = 1")
  }
  if(!is.numeric(R0) | length(R0) > 1 | R0 < 0){
    stop("R0 must be a positive numeric value")
  }
  if(!is.matrix(mixing_matrix)){
    stop("mixing_matrix must be a matrix")
  }

  ng_eigen <- Re(eigen(mixing_matrix)$values[1])
  beta <- R0/(ng_eigen * duration_infectiousness)
  return(beta)
}
