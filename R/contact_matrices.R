#' Process set of contact matrices ->  mixing matrices
#'
#' @param contact_matrix_set Set of contact matrices
#' @param population Vector of populaion by age
#'
#' @return Processed set of mixing matrices
matrix_set <- function(contact_matrix_set, population){
  contact <- lapply(contact_matrix_set, process_contact_matrix,
                    population = population)
  mixing <- lapply(contact, div_pop, population = population)

  aperm(array(unlist(mixing), dim = c(dim(mixing[[1]]), length(mixing))), c(3, 1, 2))
}

matrix_set_explicit <- function(contact_matrix_set, population){
  contact <- lapply(contact_matrix_set, process_contact_matrix_scaled_age,
                    population = population)
  mixing <- lapply(contact, div_pop, population = population)

  aperm(array(unlist(mixing), dim = c(dim(mixing[[1]]), length(mixing))), c(3, 1, 2))
}

#' Divide matrix by population
#'
#' @param contact Matrix
#' @param population Population vector
#'
#' @return Matrix
div_pop <- function(contact, population){
  t(t(contact) / population)
}

#' Process a contact matrix with an extra
#'
#' @param contact_matrix A contact matrix
#' @param population Vector of population by age
#'
#' @return Processed matrix
#' @export
process_contact_matrix_scaled_age <- function(contact_matrix, population) {
  # Convert Unbalanced Matrix of Per-Capita Rates to Total Number of Contacts
  # Between Diff Age Groups and Balance By Taking the Mean of i->j and j->i

  contact_matrix <- rbind(contact_matrix, contact_matrix[16,])
  contact_matrix <- cbind(contact_matrix, contact_matrix[,16]*population[17] / sum(population[16:17]))
  contact_matrix[,16] <- contact_matrix[,16]*population[16] / sum(population[16:17])


  MIJ <- t(vapply(seq(population),function(x){
    contact_matrix[x,] * population[x]
  }, FUN.VALUE = numeric(length(population))))

  adjust_mat <- (MIJ + t(MIJ))/2 # symmetric and balanced


  # Convert to New Per-Capita Rates By Dividing By Population
  # Resulting Matrix Is Asymmetric But Balanced
  # Asymmetric in that c_ij != c_ji BUT Total Number of Contacts i->j and j->i
  # Is Balanced (so when we divide by pop at end, will be balanced)
  processed_matrix <- t(vapply(seq(population), function(x) {
    adjust_mat[x, ] / population[x]
  }, FUN.VALUE = numeric(length(population))))

  # Adjusting to create input for model i.e. per capita rates divided by
  # population to give the number of contacts made on each individual
  return(processed_matrix)
}


#' Process a contact matrix
#'
#' @param contact_matrix A contact matrix
#' @param population Vector of population by age
#'
#' @return Processed matrix
process_contact_matrix <- function(contact_matrix, population) {
  # Convert Unbalanced Matrix of Per-Capita Rates to Total Number of Contacts
  # Between Diff Age Groups and Balance By Taking the Mean of i->j and j->i
  MIJ <- t(sapply(seq(population),function(x){
    contact_matrix[x,] * population[x]
  }))
  adjust_mat <- (MIJ + t(MIJ))/2 # symmetric and balanced

  # Convert to New Per-Capita Rates By Dividing By Population
  # Resulting Matrix Is Asymmetric But Balanced
  # Asymmetric in that c_ij != c_ji BUT Total Number of Contacts i->j and j->i
  # Is Balanced (so when we divide by pop at end, will be balanced)
  processed_matrix <- t(sapply(seq(population), function(x) {
    adjust_mat[x, ] / population[x]
  }))

  # Adjusting to create input for model i.e. per capita rates divided by
  # population to give the number of contacts made on each individual
  return(processed_matrix)
}
