% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/population.R
\name{get_elderly_population}
\alias{get_elderly_population}
\title{Get elderly population data (5 year age-breakdown for 80-84, 85-89 and 90+)}
\usage{
get_elderly_population(country = NULL, iso3c = NULL, simple_SEIR = FALSE)
}
\arguments{
\item{country}{Country name}

\item{iso3c}{ISO 3C Country Code}

\item{simple_SEIR}{Logical. Is the population for the \code{simple_SEIR}.
Default = FALSE}
}
\value{
Population data.frame
}
\description{
Get elderly population data (5 year age-breakdown for 80-84, 85-89 and 90+)
}
