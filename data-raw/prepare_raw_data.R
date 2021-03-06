# Process raw data
library(tidyverse)

# Contact Matrices sourced from literature used in the global report
# ------------------------------------------------------------------------------
contact_matrices <- readRDS("data-raw/contact_matrices.rds")
contact_matrices[[12]] <- as.matrix(contact_matrices[[12]])
usethis::use_data(contact_matrices, overwrite = TRUE)

# Demography
# ------------------------------------------------------------------------------
demog <- read.csv("data-raw/WPP_demog_matrix.csv", stringsAsFactors = FALSE)

population <- demog %>%
  dplyr::rename(country = "Region..subregion..country.or.area..") %>%
  dplyr::mutate(`X80+` = X80.84 + X85.89 + X90.94 + X95.99 + X100.) %>%
  dplyr::select(country, X0.4,   X5.9,   X10.14, X15.19, X20.24, X25.29, X30.34, X35.39,
                  X40.44, X45.49, X50.54, X55.59, X60.64, X65.69, X70.74, X75.79,`X80+`) %>%
  tidyr::pivot_longer(cols = -country, names_to = "age_group", values_to = "n",
                      names_prefix = "X") %>%
  dplyr::mutate(age_group = stringr::str_replace(age_group, "[.]", "-"),
                n = as.integer(round(n * 1000)))

population$age_group <- factor(population$age_group, levels = c("0-4",
                                                                "5-9",
                                                                "10-14",
                                                                "15-19",
                                                                "20-24",
                                                                "25-29",
                                                                "30-34",
                                                                "35-39",
                                                                "40-44",
                                                                "45-49",
                                                                "50-54",
                                                                "55-59",
                                                                "60-64",
                                                                "65-69",
                                                                "70-74",
                                                                "75-79",
                                                                "80+"))
population$matrix <- demog$Matrix[match(population$country, demog$Region..subregion..country.or.area..)]

population$iso3c <- countrycode::countrycode(population$country, "country.name","iso3c")
population$iso3c[population$country == "Channel Islands"] <- "CHI"
population$iso3c[population$country == "Eswatini"] <- "SWZ"
population$iso3c[population$country == "Micronesia"] <- "FSM"

# Fix ASCII encoding package error
Encoding(population$country) <- "latin1"
population$country <- iconv(
  population$country,
  "latin1",
  "UTF-8"
)

population <- as.data.frame(population)
usethis::use_data(population, overwrite = TRUE)

# Demography for 80+ disaggregation - required for country-specific IFR estimation
# --------------------------------------------------------------------------------
elderly_pop <- demog %>%
  dplyr::rename(country = "Region..subregion..country.or.area..") %>%
  dplyr::mutate(`X90+` = X90.94 + X95.99 + X100.) %>%
  dplyr::select(country, X80.84, X85.89, `X90+`) %>%
  tidyr::pivot_longer(cols = -country, names_to = "age_group", values_to = "n",
                      names_prefix = "X") %>%
  dplyr::mutate(age_group = stringr::str_replace(age_group, "[.]", "-"),
                n = n * 1000)

elderly_pop$age_group <- factor(elderly_pop$age_group, levels = c("80-84",
                                                          "85-89",
                                                          "90+"))

elderly_pop$matrix <- demog$Matrix[match(elderly_pop$country, demog$Region..subregion..country.or.area..)]

elderly_pop$iso3c <- countrycode::countrycode(elderly_pop$country, "country.name","iso3c")
elderly_pop$iso3c[elderly_pop$country == "Channel Islands"] <- "CHI"
elderly_pop$iso3c[elderly_pop$country == "Eswatini"] <- "SWZ"
elderly_pop$iso3c[elderly_pop$country == "Micronesia"] <- "FSM"

elderly_pop <- as.data.frame(elderly_pop)
usethis::use_data(elderly_pop, overwrite = TRUE)

# Income Group
# ------------------------------------------------------------------------------
income_group <- demog %>%
  select(Region..subregion..country.or.area.., income_group) %>%
  rename(country = Region..subregion..country.or.area..)

# few extra additions
income_group$income_group[income_group$country == "China, Taiwan Province of China"] <- "High income"
usethis::use_data(income_group, overwrite = TRUE)

# Bed Capacity
# ------------------------------------------------------------------------------

bed <- read.csv("data-raw/hospital_bed_capacity.csv")
hosp_beds_by_country <- bed %>%
  dplyr::select(country_name, income_group, beds_to_use) %>%
  dplyr::rename(country = country_name, hosp_beds = beds_to_use)

income <- readRDS("data-raw/Income_Strata_Predicted_Hospital_and_ICU_Beds.Rds")
income_for_join <- income %>%
  dplyr::select(income_group, ICU_median)

country_specific_healthcare_capacity <- hosp_beds_by_country %>%
  left_join(income_for_join, by = "income_group") %>%
  mutate(ICU_beds = hosp_beds * (ICU_median / 100)) %>%
  select(-ICU_median) %>%
  filter(hosp_beds > 0) # remove Mali result which is -ve - need to sort this

# https://www.paho.org/hq/dmdocuments/2012/2012-hia-frenchguiana.pdf
# http://www.dortek.com/project/chu-hospital-martinique/
# https://www.wikiwand.com/en/COVID-19_pandemic_in_French_Guiana#/Preventive_measures
# https://www.thedailyherald.sx/islands/ars-ramps-up-supply-of-beds-ventilators-for-guadeloupe-st-martin-and-st-barths
country_specific_healthcare_capacity <- rbind(country_specific_healthcare_capacity,
                                              data.frame("country"=c("French Guiana","Guadeloupe","Martinique"),
                                                         "income_group"=c(NA,NA,NA),
                                                         "hosp_beds"=c(2.7,5.5,4.1),
                                                         "ICU_beds"=c(0.1272256,0.1249603,0.1812053)))


usethis::use_data(country_specific_healthcare_capacity, overwrite = TRUE)

income_strata_healthcare_capacity <- income %>%
  mutate(hosp_beds = hosp_median_pred, ICU_beds = hosp_beds * (ICU_median / 100)) %>%
  select(income_group, hosp_beds, ICU_beds)
usethis::use_data(income_strata_healthcare_capacity, overwrite = TRUE)
