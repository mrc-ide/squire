#' Get population data
#'
#' @param country Country name
#'
#' @return Population data.frame
#' @export
get_population <-  function(country){
  if(country %in% unique(population$country)){
    stop("Country not found")
  }
  pc <- population[population$country == country, ] %>%
    dplyr::arrange(.data$age_group)
  return(pc)
}
