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
parse_country_severity <- function(country = NULL,
                                   prob_hosp = NULL,
                                   prob_severe = NULL,
                                   prob_non_severe_death_treatment = NULL,
                                   prob_severe_death_treatment = NULL,
                                   prob_non_severe_death_no_treatment = NULL,
                                   prob_severe_death_no_treatment = NULL,
                                   walker = FALSE) {

  # If walker == TRUE, use the original squire parameters described in Walker et al.
  assert_logical(walker)
  if (walker) {
    prob_hosp <- c(
      0.000744192, 0.000634166, 0.001171109, 0.002394593, 0.005346437 ,
      0.010289885, 0.016234604, 0.023349169, 0.028944623, 0.038607042 ,
      0.057734879, 0.072422135, 0.101602458, 0.116979814, 0.146099064,
      0.176634654 ,0.180000000)
    prob_severe <- c(
      0.05022296,	0.05022296,	0.05022296,	0.05022296,	0.05022296,
      0.05022296,	0.05022296,	0.053214942, 0.05974426,	0.074602879,
      0.103612417, 0.149427991, 0.223777304,	0.306985918,
      0.385779555, 0.461217861, 0.709444444)
    prob_non_severe_death_treatment <- c(
      0.0125702, 0.0125702,	0.0125702, 0.0125702,
      0.0125702, 0.0125702,	0.0125702, 0.013361147,
      0.015104687, 0.019164124,	0.027477519, 0.041762108,
      0.068531658, 0.105302319,	0.149305732, 0.20349534, 0.5804312)
    prob_severe_death_treatment <- rep(0.5, length(prob_hosp))
    prob_non_severe_death_no_treatment <- rep(0.6, length(prob_hosp))
    prob_severe_death_no_treatment <- rep(0.95, length(prob_hosp))
    ret <- list(country = country,
                prob_hosp = prob_hosp,
                prob_severe = prob_severe,
                prob_non_severe_death_treatment = prob_non_severe_death_treatment,
                prob_severe_death_treatment = prob_severe_death_treatment,
                prob_non_severe_death_no_treatment = prob_non_severe_death_no_treatment,
                prob_severe_death_no_treatment = prob_severe_death_no_treatment)
  }

  # Filling in any missing parameters
  if (is.null(prob_hosp)) {
    prob_hosp <- probs$prob_hosp
  }
  if (is.null(prob_severe)) {
    prob_severe <- probs$prob_severe
  }
  if (is.null(prob_non_severe_death_no_treatment)) {
    prob_non_severe_death_no_treatment <- rep(0.6, length(prob_hosp))
  }
  if (is.null(prob_severe_death_no_treatment)) {
    prob_severe_death_no_treatment <- rep(0.95, length(prob_hosp))
  }

  # If no country specified, fill in remaining missing probs with defaults, make no adjustment
  if (is.null(country)) {
    if (is.null(prob_non_severe_death_treatment)) {
      prob_non_severe_death_treatment <- probs$prob_non_severe_death_treatment
    }
    if (is.null(prob_severe_death_treatment)) {
      prob_severe_death_treatment <- probs$prob_severe_death_treatment
    }
    ret <- list(country = country,
                  prob_hosp = prob_hosp,
                  prob_severe = prob_severe,
                  prob_non_severe_death_treatment = prob_non_severe_death_treatment,
                  prob_severe_death_treatment = prob_severe_death_treatment,
                  prob_non_severe_death_no_treatment = prob_non_severe_death_no_treatment,
                  prob_severe_death_no_treatment = prob_severe_death_no_treatment)
  }

  # If country is specified, check valid and then adjust default probs based on demography
  if (!is.null(country)) {

    # Check country valid and then grab relevant elderly population
    if(!country %in% unique(squire::population$country)){
      stop("Country not found")
    }
    population <- get_population(country)
    population <- population$n
    elderly_pop <- get_elderly_population(country)
    elderly_pop <- elderly_pop$n

    # Adjusting death probability for country-specific 80+ demographic compositions
    index <- length(prob_severe)
    prop_deaths_ICU_80plus <- 0.15 # assumed, based off CHESS data
    elderly_IFR <- c(0.05659,	0.08862, 0.17370) # from Brazeau et al, for 80-84, 85-89 and 90+
    IFR_80plus <- sum(elderly_pop/sum(elderly_pop) * elderly_IFR)
    CFR_hosp_80plus <- IFR_80plus/prob_hosp[index]

    if (is.null(prob_severe_death_treatment)) {
      prob_severe_death_treatment <- probs$prob_severe_death_treatment
      prob_severe_death_treatment[index] <- min(1, CFR_hosp_80plus * prop_deaths_ICU_80plus/prob_severe[index])
    }
    if (is.null(prob_non_severe_death_treatment)) {
      prob_non_severe_death_treatment <- probs$prob_non_severe_death_treatment
      prob_non_severe_death_treatment[index] <- min(1, (CFR_hosp_80plus - prob_severe_death_treatment[index] * prob_severe[index])/(1 - prob_severe[index]))
    }

    ret <- list(country = country,
                prob_hosp = prob_hosp,
                prob_severe = prob_severe,
                prob_non_severe_death_treatment = prob_non_severe_death_treatment,
                prob_severe_death_treatment = prob_severe_death_treatment,
                prob_non_severe_death_no_treatment = prob_non_severe_death_no_treatment,
                prob_severe_death_no_treatment = prob_severe_death_no_treatment)
  }

  return(ret)

}
