matrix_set <- function(contact_matrix_set, population){
  if(is.matrix(contact_matrix_set)){
    contact_matrix_set <- list(contact_matrix_set)
  }
  contact <- lapply(contact_matrix_set, generate_contact_matrix,
                    population = population)
  mixing <- lapply(contact, div_pop, population = population)

  aperm(array(unlist(mixing), dim = c(dim(mixing[[1]]), length(mixing))), c(3, 1, 2))
}

div_pop <- function(contact, population){
  t(t(contact) / population)
}

generate_contact_matrix <- function(mixing_matrix, population) {
  if (length(population) != dim(mixing_matrix)[1]) {
    return("Dimensions of demography and matrix must be equal")
  }

  # Convert Unbalanced Matrix of Per-Capita Rates to Total Number of Contacts
  # Between Diff Age Groups and Balance By Taking the Mean of i->j and j->i
  MIJ <- t(sapply(seq(population),function(x){
    mixing_matrix[x,] * population[x]
  }))
  adjust_mat <- (MIJ + t(MIJ))/2 # symmetric and balanced

  # Convert to New Per-Capita Rates By Dividing By Population
  # Resulting Matrix Is Asymmetric But Balanced
  # Asymmetric in that c_ij != c_ji BUT Total Number of Contacts i->j and j->i
  # Is Balanced (so when we divide by pop at end, will be balanced)
  contact_matrix <- t(sapply(seq(population), function(x) {
    adjust_mat[x, ] / population[x]
  }))

  # Adjusting to create input for model i.e. per capita rates divided by
  # population to give the number of contacts made on each individual
  return(contact_matrix)
}
