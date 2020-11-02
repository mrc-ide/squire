#' Get supported LMIC countries
#'
#' @return vector of support LMIC
#' @export
get_lmic_countries <-  function() {
  lmic <- c(
    squire::income_group$country[(squire::income_group$income_group != 'High income')],
    squire::income_group$country[
      is.na(squire::income_group$income_group) & squire::income_group$country != 'China, Taiwan Province of China'
    ]
  )
  lmic[!is.na(lmic)]
}

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

#' Get elderly population data (5 year age-breakdown for 80-84, 85-89 and 90+)
#'
#' @param country Country name
#' @param iso3c ISO 3C Country Code
#' @param simple_SEIR Logical. Is the population for the \code{simple_SEIR}.
#'   Default = FALSE
#'
#' @return Population data.frame
#' @importFrom utils head tail
#' @export
get_elderly_population <-  function(country = NULL, iso3c = NULL, simple_SEIR = FALSE){

  ## country route
  if(!is.null(country)) {
    assert_string(country)
    if(!country %in% unique(squire::elderly_pop$country)){
      stop("Country not found")
    }
    pc <- squire::elderly_pop[squire::elderly_pop$country == country, ] %>%
      dplyr::arrange(.data$age_group)
  }

  # iso3c route
  if(!is.null(iso3c)) {
    assert_string(iso3c)
    if(!iso3c %in% unique(squire::elderly_pop$iso3c)){
      stop("iso3c not found")
    }
    pc <- squire::elderly_pop[squire::elderly_pop$iso3c == iso3c, ] %>%
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
#' @param iso3c ISO 3C Country Code
#'
#' @return Age mixing matrix
#' @export
get_mixing_matrix <-  function(country = NULL, iso3c = NULL){

  if(!is.null(country) && !is.null(iso3c)) {
    message("Both iso3c and country were provided. Country will be used")
    iso3c <- NULL
  }

  pop <- get_population(country, iso3c)

  mm <- pop$matrix[1]
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

#' @noRd
parse_country_IFR <- function(country = NULL,
                              population = NULL,
                              elderly_pop = NULL) {

  # Handle country population args
  if (is.null(country) ||
      (is.null(population) && is.null(elderly_population_disaggregation))) {
    stop("User must provide either the country being simulated or both the
          population and a more detailed breakdown of their elderly population")
  }

  # If a country was provided then grab the population and matrices if needed
  if (is.null(population)) {
    population <- get_population(country)
    population <- population$n
  }
  if (is.null(elderly_pop)) {
    elderly_pop <- get_elderly_population(country)
    elderly_pop <- elderly_pop$n
  }

  # Loading in squire's prob_hosp and prob_severe
  prob_hosp <- probs()$prob_hosp
  prob_severe <- probs()$prob_severe

  # Adjusting death probability for country-specific 80+ demographic compositions
  prob_non_severe_death_treatment <- probs()$prob_non_severe_death_treatment
  prob_severe_death_treatment <- probs()$prob_severe_death_treatment
  index <- length(prob_non_severe_death_treatment)

  prop_deaths_ICU_80plus <- 0.15 # assumed, based off CHESS data
  elderly_IFR <- c(0.05659,	0.08862, 0.17370) # from Brazeau et al, for 80-84, 85-89 and 90+
  IFR_80plus <- elderly_pop/sum(elderly_pop) * elderly_IFR
  CFR_hosp_80plus <- IFR_80plus/prob_hosp[index]

  prob_severe_death_treatment[index] <- CFR_hosp_80plus * prop_deaths_ICU_80plus/prob_severe[index]
  prob_non_severe_death_treatment[index] <- (CFR_hosp_80plus - prob_severe_death_treatment[index] * prob_severe[index])/(1 - prob_severe[index])

  ret <- list(country = country,
              prob_non_severe_death_treatment = prob_non_severe_death_treatment,
              prob_severe_death_treatment = prob_severe_death_treatment)

  return(ret)

}
