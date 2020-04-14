
<!-- README.md is generated from README.Rmd. Please edit that file -->

# squire

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/mrc-ide/squire.svg?branch=master)](https://travis-ci.org/mrc-ide/squire)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/mrc-ide/squire?branch=master&svg=true)](https://ci.appveyor.com/project/pwinskill/squire)
[![Codecov test
coverage](https://codecov.io/gh/mrc-ide/squire/branch/master/graph/badge.svg)](https://codecov.io/gh/mrc-ide/squire?branch=master)
<!-- badges: end -->

squire enables users to simulate models of SARS-CoV-2 epidemics. This is
done using an age-structured SEIR model that also explicitly considers
healthcare capacity and disease severity.

## Overview

squire is a package enabling users to quickly and easily generate
calibrated estimates of SARS-CoV-2 epidemic trajectories under different
control scenarios. It consists of the following:

  - An age-structured SEIR model incorporating explicit passage through
    healthcare settings and explicit progression through disease
    severity stages.
  - The ability to calibrate the model to different epidemic start-dates
    based on available death data.
  - Simulate the impacts of different control interventions (including
    general social distancing, specific shielding of elderly
    populations, and more stringent suppression strategies).

If you are new to squire, the best place to start is below, where we
detail how to install the package, how to set up the model, and how to
run it with and without control
interventions.

## Model Structure

### Overall Structure

<img src="https://raw.githubusercontent.com/mrc-ide/squire/master/images/Explicit_Healthcare_Model_Structure.JPG" align="center" style = "border: none; float: center;" width = "600px">

squire uses an age-structured SEIR model, with the infectious class
divided into different stages reflecting progression through different
disese severity pathways. These compartments are:  
\* S = Susceptibles  
\* E = Exposed (Latent Infection)  
\* I<sub>Mild</sub> = Mild Infections (Not Requiring Hospitalisation)  
\* I<sub>Case</sub> = Infections Requiring Hospitalisation  
\* I<sub>Hospital</sub> = Hospitalised (Requires Hospital Bed)  
\* I<sub>ICU</sub> = ICU (Requires ICU Bed)  
\* I<sub>Rec</sub> = Recovering from ICU Stay (Requires Hospital Bed)  
\* R = Recovered  
\* D =
Dead

### Decision Trees for Healthcare Capacity

<img src="https://raw.githubusercontent.com/mrc-ide/squire/master/images/Explicit_Healthcare_Oxygen_Decision_Tree.JPG" align="center" style = "border: none; float: center;" width = "400px">

Given initial inputs of hospital/ICU bed capacity and the average time
cases spend in hospital, the model dynamically tracks available hospital
and ICU beds over time.

Individuals newly requiring hospitalisation (either a hospital or ICU
bed) are then assigned to either receive care (if the relevant bed is
available) or not (if maximum capacity would be exceeded otherwise).
Whether or not an individual receives the required care modifies their
probability of dying.

## Installation

<i>squire</i> utilises the package
[ODIN](https://github.com/mrc-ide/odin) to generate the model.
[ODIN](https://github.com/mrc-ide/odin) implements a high-level language
for implementing mathematical models and can be installed by running the
following command:

``` r
install.packages("odin")
```

The model generated using ODIN is written in C and so you will require a
compiler to install dependencies for the package and to build any models
with ODIN. Windows users should install
[Rtools](https://cran.r-project.org/bin/windows/Rtools/). See the
relevant section in
[R-admin](https://cran.r-project.org/doc/manuals/r-release/R-admin.html#The-Windows-toolset)
for advice. Be sure to select the “edit PATH” checkbox during
installation or the tools will not be found.

The function `odin::can_compile()` will check if it is able to compile
things, but by the time you install the package that will probably have
been satisfied.

After installation of ODIN, ensure you have the devtools package
installed by running the following:

``` r
install.packages("devtools")
```

Then install the <i>squire</i> package directly from GitHub by running:

``` r
devtools::install_github("mrc-ide/squire")
```

If you have any problems installing then please raise an issue on the
<i>squire</i> [`GitHub`](https://github.com/mrc-ide/squire/issues).

If everything has installed correctly, we then need to load the
package:

``` r
library(squire)
```

## Getting Started

### Running the Model (Unmitigated)

### 1\. Running the model using baseline parameters and no control interventions

The full model is referred to as the **explicit\_SEEIR** model, with
hospital pathways explicltly exploring whether individuals will require
a general hospital bed providing oxygen or an ICU bed that provides
ventilation.

To run the model we need to provide at least one of the following
arguments:

  - `country`
  - `population` and `contact_matrix_set`

If the `country` is provided, the `population` and `contact_matrix_set`
will be generated (if not also specified) using the demographics and
matrices specified in the [global
report](https://www.imperial.ac.uk/mrc-global-infectious-disease-analysis/covid-19/report-12-global-impact-covid-19/).

To run the model by providing the `country` we use
`run_explicit_SEEIR_model()`:

``` r
r <- run_explicit_SEEIR_model(country = "Afghanistan")
```

The returned object is a `squire_simulation` object, which is a list of
two ojects:

  - `output` - model output
  - `parameters` - model parameters

`squire_simulation` objects can be plotted as follows:

``` r
plot(r)
```

<img src="man/figures/README-base plot-1.png" width="100%" />

This plot will plot each of the compartments of the model output. We can
also plot specific compartments using the `var_select` argument that can
be passed to `plot()`. Arguments passed to `var_select` must be one of
the variables in the plot
above.

``` r
plot(r, var_select = c("E", "IMild"))
```

<img src="man/figures/README-subset variables plot-1.png" width="100%" />

All of the plotting above makes uses of the `squire` function
`format_output` which provides you with a means of manipulating and
managing the output from a `run_explicit_SEEIR_model` call. Using it you
can specify the model outputs (e.g. compartments) you want, as well as
whether you want that output aggregated over age or not. Here we extract
the latent compartment (E). The data columns correspond to the
compartment name (`compartment`), timestep (`t`), model run number
(`replicate`) and the model output (`y`).

``` r

output <- format_output(r, var_select = "E")
head(output)
#> # A tibble: 6 x 4
#>   compartment     t replicate     y
#>   <chr>       <dbl>     <int> <dbl>
#> 1 E             0.1         1    20
#> 2 E             0.1         2    20
#> 3 E             0.1         3    20
#> 4 E             0.1         4    20
#> 5 E             0.1         5    20
#> 6 E             0.1         6    20
```

If we wanted age-disaggregated data, we could set `reduce_age` to
`FALSE` which will generate the same dataframe as before, but with an
additional column indicating the age-group.

``` r

output <- format_output(r, var_select = "E", reduce_age = FALSE)
head(output)
#> # A tibble: 6 x 5
#> # Groups:   replicate, age_group, compartment [1]
#>   replicate age_group compartment     t     y
#>       <int>     <int> <chr>       <dbl> <dbl>
#> 1         1         1 E             0.1     0
#> 2         1         1 E             0.2     0
#> 3         1         1 E             0.3     0
#> 4         1         1 E             0.4     0
#> 5         1         1 E             0.5     0
#> 6         1         1 E             0.6     0
```

### 2\. Changing parameters in the model.

The model has a number of parameters for setting the R0, demography,
contact matrices, the durations of each compartment and the health care
outcomes and healthcare availability. In addition, the initial state of
the population can be changed as well as simulation parameters, such as
the number of replicates, length of simulation and the timestep. For a
full list of model inputs, please see the function
[documentation](https://mrc-ide.github.io/squire/reference/run_explicit_SEEIR_model.html)

For example, changing the initial R0 (default = 3), number of replicates
( default = 10), simualtion length (default = 365 days) and time step
(default = 0.5 days), as well as setting the population and contact
matrix manually:

``` r

# Get the population
pop <- get_population("United Kingdom")
population <- pop$n

# Get the mixing matrix
contact_matrix <- get_mixing_matrix("United Kingdom")

# run the model
r <- run_explicit_SEEIR_model(population = population, 
                              contact_matrix_set = contact_matrix,
                              R0 = 2.5, 
                              time_period = 200,
                              dt = 0.1,
                              replicates = 5)
plot(r)
#> Warning in plot.squire_simulation(r): Summary statistic estimated from <10
#> replicates
#> Warning in plot.squire_simulation(r): Confidence bounds estimated from <10
#> replicates
```

<img src="man/figures/README-set params-1.png" width="100%" />

We can also change the R0 and contact matrix at set time points, to
reflect changing behaviour resulting from interventions. For example to
set a 80% reduction in the contact matrix after 100 days :

``` r

# run the model
r <- run_explicit_SEEIR_model(population = population, 
                              tt_contact_matrix = c(0, 100),
                              contact_matrix_set = list(contact_matrix,
                                                        contact_matrix*0.2),
                              R0 = 2.5, 
                              time_period = 200,
                              dt = 0.1,
                              replicates = 5)
plot(r, var_select = "n_E2_I")
#> Warning in plot.squire_simulation(r, var_select = "n_E2_I"): Summary statistic
#> estimated from <10 replicates
#> Warning in plot.squire_simulation(r, var_select = "n_E2_I"): Confidence bounds
#> estimated from <10 replicates
```

<img src="man/figures/README-set contact matrix decrease-1.png" width="100%" />
where `n_E2_I` is the daily number of new infections.

To show an 80% reduction after 50 days but only maintained for 30 days :

``` r

# run the model
r <- run_explicit_SEEIR_model(population = population, 
                              tt_contact_matrix = c(0, 80, 120),
                              contact_matrix_set = list(contact_matrix,
                                                        contact_matrix*0.2,
                                                        contact_matrix),
                              R0 = 2.5, 
                              time_period = 220,
                              dt = 0.1,
                              replicates = 5)
plot(r, var_select = "n_E2_I")
#> Warning in plot.squire_simulation(r, var_select = "n_E2_I"): Summary statistic
#> estimated from <10 replicates
#> Warning in plot.squire_simulation(r, var_select = "n_E2_I"): Confidence bounds
#> estimated from <10 replicates
```

<img src="man/figures/README-set contact matrix decrease and relax-1.png" width="100%" />

Alternatively, we could set a changing R0, which falls below 1 after 50
days:

``` r

# run the model
r <- run_explicit_SEEIR_model(population = population, 
                              contact_matrix_set = contact_matrix,
                              tt_R0 = c(0, 80),
                              R0 = c(2.5, 0.9),
                              time_period = 200,
                              dt = 0.1,
                              replicates = 5)
plot(r, var_select = "n_E2_I")
#> Warning in plot.squire_simulation(r, var_select = "n_E2_I"): Summary statistic
#> estimated from <10 replicates
#> Warning in plot.squire_simulation(r, var_select = "n_E2_I"): Confidence bounds
#> estimated from <10 replicates
```

<img src="man/figures/README-set R0 decrease-1.png" width="100%" />

### 3\. Extracting and Plotting Relevant Outputs

Alternative summaries of the models can be created, which give commonly
reported measures, such as deaths, number of ICU beds and general
hospital beds required.

These could be created by using the `format_output` on the output and
selecting relevant compartments but this is quite slow to do, so we
provide some helper functions. These include:

``` r
library(ggplot2)
library(patchwork)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

x <- run_explicit_SEEIR_model(country = "Afghanistan", hosp_bed_capacity = 500, ICU_bed_capacity = 1000)

deaths <- extract_deaths(r)
a <- ggplot(deaths, aes(x = t, y = y, col = replicate)) +
  geom_line() + ylab("Daily Deaths")

infection_incidence <- extract_infection_incidence(r)
b <- ggplot(infection_incidence, aes(x = t, y = y, col = replicate)) +
  geom_line() + ylab("Case Incidence")

hosp_occ <- extract_hospital_occ(r)
c <- ggplot(hosp_occ, aes(x = t, y = y, col = replicate)) +
  geom_line() + ylab("Hospital Bed Occupancy")

ICU_occ <- extract_ICU_occ(r)
d <- ggplot(ICU_occ, aes(x = t, y = y, col = replicate)) +
  geom_line() + ylab("ICU Occupancy")

z <- a + b + c + d +
  plot_layout(guides = 'collect')
z
```

<img src="man/figures/README-untransformed-1.png" width="100%" />

### 4\. Calibrating the Model to Observed Deaths Data

The model can be simply calibrated to the cumulative time series of
deaths reported in settings. This can be done using the `calibrate`
function. For example, let’s generate some dummy time series data and
calibrate to it:

``` r
df <- squire:::death_data_format(reporting_quality = 1)
df
#>         date deaths cases
#> 1 2020-04-14      4     4
#> 2 2020-04-13      3     3
#> 3 2020-04-12      3     3
```

Then to calibrate to
this:

``` r
calibration <- calibrate(country = "Afghanistan", deaths = max(df$deaths), 
                         reporting_fraction = 1,
                         seeding_age_groups = c("35-40", "40-45", "45-50", "50-55"),
                         min_seeding_cases = 5, max_seeding_cases = 50,
                         replicates = 10, dt = 0.25)
```

Simulation replicates are aligned to the current death total and the
outputs are returned as a `squire_simulation` object.

These can be plotted by either extracting the desired variables again
using `format_output` and providing the date that the deaths relates to.
E.g. for the following 4 weeks:

``` r

x <- format_output(calibration, var_select = c("n_E2_I"), date_0 = Sys.Date()) %>%
  mutate(replicate = factor(replicate)) %>%
  filter(t < 14)
ggplot(x, aes(x = date, y = y, col = replicate)) +
  geom_line() + ylab("Case Incidence") + xlab("Date") +
  theme(legend.position = "none")
```

<img src="man/figures/README-deaths over time-1.png" width="100%" />

Alternatively, there are a few unexposed functions for plotting these
outputs:

``` r

o1 <- squire:::calibrate_output_parsing(calibration, date_0 = Sys.Date())
a <- squire:::plot_calibration_healthcare(df = o1, data = df)
b <- squire:::plot_calibration_healthcare_barplot(df = o1, data = df)
c <- squire:::plot_calibration_cases(df = o1, data = df)
d <- squire:::plot_calibration_cases_barplot(df = o1, data = df)

z <- a + b + c + d +
  plot_layout(guides = 'collect')
z
#> Warning: Removed 4347 row(s) containing missing values (geom_path).
#> Warning: Removed 489 row(s) containing missing values (geom_path).
```

<img src="man/figures/README-orderly style-1.png" width="100%" />