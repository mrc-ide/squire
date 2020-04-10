# Process raw data

# Contact Matrices sourced from literature used in the global report
# ------------------------------------------------------------------------------
contact_matrices <- readRDS("data-raw/contact_matrices.rds")
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
                n = n * 1000)

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

# Fix ASCII encoding package error
Encoding(population$country) <- "latin1"
population$country <- iconv(
  population$country,
  "latin1",
  "UTF-8"
)

population <- as.data.frame(population)
usethis::use_data(population, overwrite = TRUE)

# Bed Capacity
# ------------------------------------------------------------------------------

bed <- readRDS("data-raw/Hospital_Bed_Capacity_Predictions.Rds")
hosp_beds_by_country <- bed %>%
  dplyr::select(country_name, income_group, beds_to_use) %>%
  dplyr::rename(country = country_name, hosp_beds = beds_to_use)

income <- readRDS("data-raw/Income_Strata_Predicted_Hospital_and_ICU_Beds.Rds")
income <- income %>%
  dplyr::select()



