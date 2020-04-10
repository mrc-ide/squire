#' Get population data
#'
#' @param country Country name
#' @param simple_SEIR Logical. Is the population for the \code{simple_SEIR}.
#'   Default = FALSE
#'
#' @return Population data.frame
#' @importFrom utils head tail
#' @export
get_population <-  function(country, simple_SEIR = FALSE){
  if(!country %in% unique(squire::population$country)){
    stop("Country not found")
  }
  pc <- squire::population[squire::population$country == country, ] %>%
    dplyr::arrange(.data$age_group)

  if (simple_SEIR) {
    pc$n <- c(head(pc$n, -2), sum(tail(pc$n, 2)), 0)
    pc$age_group <- as.character(pc$age_group)
    pc$age_group[length(pc$n)-1] <- "75+"
    pc <- head(pc, -1)
  }

  return(pc)
}


#' Get mixing matrix
#'
#' @param country Country name
#'
#' @return Age mixing matrix
#' @export
get_mixing_matrix <-  function(country){
  if(!country %in% unique(squire::population$country)){
    stop("Country not found")
  }

  pop <- get_population(country)

  mm <- squire::population$matrix[match(country, squire::population$country)]
  mm <- squire::contact_matrices[[mm]]

  return(mm)
}
