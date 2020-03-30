# Process raw data

# Contact Matrices
# TODO: have the full processing of raw data here
contact_matrices <- readRDS("data-raw/contact_matrices.rds")
usethis::use_data(contact_matrices, overwrite = TRUE)

# Demography
demog <- read.csv("data-raw/WPP_demog_matrix.csv", stringsAsFactors = FALSE)

population <- demog %>%
  dplyr::rename(country = "Region..subregion..country.or.area..") %>%
  dplyr::mutate(`X75+` = X75.79 + X80.84 + X85.89 + X90.94 + X95.99 + X100.) %>%
  dplyr::select(country, X0.4,   X5.9,   X10.14, X15.19, X20.24, X25.29, X30.34, X35.39,
                  X40.44, X45.49, X50.54, X55.59, X60.64, X65.69, X70.74, `X75+`) %>%
  tidyr::pivot_longer(cols = -country, names_to = "age_group", values_to = "n",
                      names_prefix = "X") %>%
  dplyr::mutate(age_group = stringr::str_replace(age_group, "[.]", "-"),
                n = n * 1000)
usethis::use_data(population, overwrite = TRUE)
