#' Get population data
#'
#' @param country Country name
#' @param iso3c ISO 3C Country Code
#' @param simple_SEIR Logical. Is the population for the \code{simple_SEIR}.
#'   Default = FALSE
#'
#' @return Population data.frame
#' @importFrom utils head tail
#' @export
get_population <-  function(country = NULL, iso3c = NULL, simple_SEIR = FALSE){

  ## country route
  if(!is.null(country)) {
    assert_string(country)
    if(!country %in% unique(squire::population$country)){
      stop("Country not found")
    }
    pc <- squire::population[squire::population$country == country, ] %>%
      dplyr::arrange(.data$age_group)
  }

  # iso3c route
  if(!is.null(iso3c)) {
    assert_string(iso3c)
    if(!iso3c %in% unique(squire::population$iso3c)){
      stop("iso3c not found")
    }
    pc <- squire::population[squire::population$iso3c == iso3c, ] %>%
      dplyr::arrange(.data$age_group)
  }

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

#'
#' Get healthcare capacity data
#'
#' @param country Country name
#' @param simple_SEIR Logical. Is the population for the \code{simple_SEIR}.
#'   Default = FALSE
#'
#' @return Healthcare capacity data
#' @importFrom utils head tail
#' @export
get_healthcare_capacity <-  function(country, simple_SEIR = FALSE){
  if(!country %in% unique(squire::population$country)){
    stop("Country not found")
  }

  if(country %in% unique(squire::country_specific_healthcare_capacity$country)) {
    beds <- squire::country_specific_healthcare_capacity[match(country, squire::country_specific_healthcare_capacity$country), ]
    hosp_beds <- beds$hosp_beds
    ICU_beds <- beds$ICU_beds
    hc <- list(hosp_beds = hosp_beds, ICU_beds = ICU_beds)
  } else {
    income_group <- squire::income_group$income_group[match(country, squire::income_group$country)]
    if (is.na(income_group)) {
      stop("healthcare capacity data not available for this country - specify hospital and ICU beds in the run_explicit_SEEIR call manually")
    }
    beds <- squire::income_strata_healthcare_capacity[squire::income_strata_healthcare_capacity$income_group == income_group, ]
    hosp_beds <- as.vector(beds$hosp_beds)
    ICU_beds <- as.vector(beds$ICU_beds)
    hc <- list(hosp_beds = hosp_beds, ICU_beds = ICU_beds)
  }

  return(hc)
}

#' @noRd
parse_country_population_mixing_matrix <- function(country = NULL,
                                                   population = NULL,
                                                   contact_matrix_set = NULL) {

  # Handle country population args
  if (is.null(country) &&
      (is.null(population) || is.null(contact_matrix_set))) {
    stop("User must provide either the country being simulated or
         both the population size and contact_matrix_set")
  }

  # If a country was provided then grab the population and matrices if needed
  if (is.null(population)) {
    population <- get_population(country)

    if (is.null(contact_matrix_set)) {
      contact_matrix_set <- get_mixing_matrix(country)
    }
    population <- population$n
  }

  ret <- list(population = population,
              country = country,
              contact_matrix_set = contact_matrix_set)

  return(ret)

}
