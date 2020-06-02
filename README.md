
<!-- README.md is generated from README.Rmd. Please edit that file -->

# squire

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
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

## IMPORTANT NOTES

:warning: This code is released with no support. Please submit any
questions or bugs as [issues](https://github.com/mrc-ide/squire/issues)
and we will try to address them as quickly as possible.

:warning: This model is in active development and so parameter name and
behaviours, and output file formats will change without notice.

:warning: The model is stochastic. Multiple runs with different seeds
should be undertaken to see average behaviour.

:warning: As with any mathematical model, it is easy to misconfigure
inputs and therefore get meaningless outputs. Please contact the authors
if you intend to publish results using `squire`.

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
run it with and without control interventions.

## Model Structure

### Overall Structure

<img src="https://raw.githubusercontent.com/mrc-ide/squire/master/images/Explicit_Healthcare_Model_Structure.JPG" align="center" style = "border: none; float: center;" width = "600px">

squire uses an age-structured SEIR model, with the infectious class
divided into different stages reflecting progression through different
disease severity pathways. These compartments are:  
\* S = Susceptibles  
\* E = Exposed (Latent Infection)  
\* I<sub>Mild</sub> = Mild Infections (Not Requiring Hospitalisation)  
\* I<sub>Case</sub> = Infections Requiring Hospitalisation  
\* I<sub>Hospital</sub> = Hospitalised (Requires Hospital Bed)  
\* I<sub>ICU</sub> = ICU (Requires ICU Bed)  
\* I<sub>Rec</sub> = Recovering from ICU Stay (Requires Hospital Bed)  
\* R = Recovered  
\* D = Dead

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
[odin](https://github.com/mrc-ide/odin) to generate the model.
[odin](https://github.com/mrc-ide/odin) implements a high-level language
for implementing mathematical models and can be installed by running the
following command:

``` r
install.packages("odin")
```

The model generated using odin is written in C and so you will require a
compiler to install dependencies for the package and to build any models
with odin. Windows users should install
[Rtools](https://cran.r-project.org/bin/windows/Rtools/). See the
relevant section in
[R-admin](https://cran.r-project.org/doc/manuals/r-release/R-admin.html#The-Windows-toolset)
for advice. Be sure to select the “edit PATH” checkbox during
installation or the tools will not be found.

The function `odin::can_compile()` will check if it is able to compile
things, but by the time you install the package that will probably have
been satisfied.

After installation of odin, ensure you have the devtools package
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

If everything has installed correctly, we then need to load the package:

``` r
library(squire)
```

## Getting Started

### Running the Model (Unmitigated)

### 1\. Running the model using baseline parameters and no control interventions

The full model is referred to as the **explicit\_SEEIR** model, with
hospital pathways explicitly exploring whether individuals will require
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
the variables in the plot above.

``` r
plot(r, var_select = c("E", "IMild"))
```

<img src="man/figures/README-subset variables plot-1.png" width="100%" />
Or, you can specify one of `deaths`, `infections`, `hospital_occupancy`,
`ICU_occupancy`, `hospital_demand` or `ICU_demand`, and plot these
summary metrics that represent the combintion of a number of different
compartment e.g:

``` r
plot(r, var_select = "deaths")
```

<img src="man/figures/README-subset variables plot2-1.png" width="100%" />

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
#>   replicate compartment     t     y
#>       <dbl> <chr>       <dbl> <dbl>
#> 1         1 E             0.1    20
#> 2         1 E             0.2    20
#> 3         1 E             0.3    20
#> 4         1 E             0.4    20
#> 5         1 E             0.5    19
#> 6         1 E             0.6    19
```

If we wanted age-disaggregated data, we could set `reduce_age` to
`FALSE` which will generate the same dataframe as before, but with an
additional column indicating the age-group.

``` r

output <- format_output(r, var_select = "E", reduce_age = FALSE)
head(output)
#> # A tibble: 6 x 5
#>   replicate age_group compartment     t     y
#>       <dbl>     <int> <chr>       <dbl> <dbl>
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
plot(r, var_select = "infections")
#> Warning in plot.squire_simulation(r, var_select = "infections"): Summary
#> statistic estimated from <10 replicates
#> Warning in plot.squire_simulation(r, var_select = "infections"): Confidence
#> bounds estimated from <10 replicates
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
plot(r, var_select = "infections")
#> Warning in plot.squire_simulation(r, var_select = "infections"): Summary
#> statistic estimated from <10 replicates
#> Warning in plot.squire_simulation(r, var_select = "infections"): Confidence
#> bounds estimated from <10 replicates
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
plot(r, var_select = "infections")
#> Warning in plot.squire_simulation(r, var_select = "infections"): Summary
#> statistic estimated from <10 replicates
#> Warning in plot.squire_simulation(r, var_select = "infections"): Confidence
#> bounds estimated from <10 replicates
```

<img src="man/figures/README-set R0 decrease-1.png" width="100%" />

The model in squire also allows you to alter healthcare capacity.
Default values for the arguments `hosp_bed_capacity` and `ICU_capacity`
are taken from the World Bank and a systematic review of the literature.
However, you can also specify your own:

``` r

library(patchwork)
#> Warning: package 'patchwork' was built under R version 3.5.3

r <- run_explicit_SEEIR_model(population = population, 
                              contact_matrix_set = contact_matrix,
                              R0 = 2.5, 
                              time_period = 200,
                              dt = 0.1,
                              replicates = 15,
                              hosp_bed_capacity = 1000,
                              ICU_bed_capacity = 100)

c <- plot(r, var_select = "hospital_occupancy")
d <- plot(r, var_select = "ICU_occupancy")
c / d 
```

<img src="man/figures/README-change healthcare capacity-1.png" width="100%" />

### 3\. Extracting and Plotting Relevant Outputs

Whilst the above provides quick and easy ways to plot relevant outputs,
we recognise users might want to play around with the data themselves.
This can be done with the `format_output` function.

``` r
library(ggplot2)
#> Warning: package 'ggplot2' was built under R version 3.5.3
library(patchwork)
library(dplyr)
#> Warning: package 'dplyr' was built under R version 3.5.3
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

x <- run_explicit_SEEIR_model(country = "Afghanistan", hosp_bed_capacity = 500, ICU_bed_capacity = 200)

deaths <- format_output(x = x, var_select = "deaths") %>%
  mutate(replicate = factor(replicate))
a <- ggplot(deaths, aes(x = t, y = y, col = replicate)) +
  geom_line() + ylab("Daily Deaths")

infections <- format_output(x = x, var_select = "infections") %>%
  mutate(replicate = factor(replicate))
b <- ggplot(infections, aes(x = t, y = y, col = replicate)) +
  geom_line() + ylab("Daily Infections")

hosp_bed <- format_output(x = x, var_select = "hospital_occupancy") %>%
  mutate(replicate = factor(replicate))
c <- ggplot(hosp_bed, aes(x = t, y = y, col = replicate)) +
  geom_line() + ylab("Hospital Bed Occupancy")

ICU_bed <- format_output(x = x, var_select = "ICU_occupancy") %>%
  mutate(replicate = factor(replicate))
d <- ggplot(ICU_bed, aes(x = t, y = y, col = replicate)) +
  geom_line() + ylab("ICU Bed Occupancy")

z <- a + b + c + d +
  plot_layout(guides = 'collect')
z
```

<img src="man/figures/README-untransformed-1.png" width="100%" />

Hospital bed occupancy fluctuates very slightly above the user-specified
number of hospital beds available (+1 in the above example). This is due
to individuals being discharged from ICU beds into general hospital beds
when general hospital bed occupancy is already at maximum capacity.

We assume that these individuals newly discharged from the ICU would in
reality be prioritised to receive general hospital beds and so give them
a general hospital bed. Whilst this is now shown in the above panel of
hospital bed occupancy, this means that less-sick individuals in general
hospital beds would be discharged slightly early to make space. We
assume that for these small number of the healthiest individuals in
hospital, there is no excess mortality associated with this early
discharge.

Future work will explore a range of bed-prioritisation structures to
consider how results might be shaped by who gets a bed and when.

### 4\. Calibrating the Model to Observed Deaths Data

The model can be simply calibrated to time series of deaths reported in
settings. This can be done using the `calibrate` function. For example,
let’s use the time series of deaths in Algeria up to the 22nd April
2020:

``` r
df <- read.csv(squire:::squire_file("extdata/example_DZA.csv"), stringsAsFactors = FALSE)
head(df)
#>   X                 date deaths cases
#> 1 1 2020-02-26T00:00:00Z      0     1
#> 2 2 2020-03-01T00:00:00Z      0     2
#> 3 3 2020-03-04T00:00:00Z      0     2
#> 4 4 2020-03-05T00:00:00Z      0     7
#> 5 5 2020-03-06T00:00:00Z      0     5
#> 6 6 2020-03-09T00:00:00Z      0     3
```

We can calibrate the model to this data. This calibration will infer the
most likely start date of the epidemic and the R0 at the start of the
epidemic. Calibration occurs by scanning across a range of possible R0
values and start dates. Fitting works using a particle filter, which can
be parallelised using `future::plan(future::multiprocess())` before
running `calibrate`.

``` r
# set up for parallelisation
future::plan(future::multiprocess())

# Fit model
out <- calibrate(
      data = df,
      R0_min = 2,
      R0_max = 4,
      R0_step = 0.5,
      first_start_date = "2020-02-10",
      last_start_date = "2020-02-22",
      day_step = 4,
      replicates = 10,
      n_particles = 20,
      country = "Algeria"
    )
#> 
 Progress: ------------------------------------------------------------------------------------ 100%
#> 
#> 
 Progress: ------------------------------------------------------------------------------------ 100%
```

`calibrate` returns the same output as `run_explicit_SEEIR_model`, with
the first three elements in `out` being the simulation outputs, the
model and model parameters. Note that simulation replicates are aligned
to the maximum date in the data provided, and as a result we can use the
same plotting functions as before:

``` r
plot(out, "deaths", date_0 = max(df$date), x_var = "date")
```

<img src="man/figures/README-plot particle deaths-1.png" width="100%" />

With default parameters, `calibrate` will simulate up the maximum date
in the data provided. The fit to this data can be shown using the
plotting function and specifying `particle_fit` to be `TRUE`

``` r
plot(out, particle_fit = TRUE)
```

<img src="man/figures/README-particle fit no ints-1.png" width="100%" />

We can see that this is not the best fit, capturing only the beginning
of the data points. We can also diagnose the fitting further by looking
at the `scan_results` of the grid search.

``` r
plot(out$scan_results)
```

<img src="man/figures/README-show grid fits-1.png" width="100%" />

This grid shows the negative log likeihood for each parameter pair. We
can also plot the related probability for each pair as follows:

``` r
plot(out$scan_results, what = "probability")
```

<img src="man/figures/README-show grid fits probs-1.png" width="100%" />

The reason for the poor fits to the data shown earlier is because
Algeria has implemented interventions prior to today. These can also be
incorporated into `calibrate`. For example, we can grab the assumed
changes to transmission fased on government intervention for Algeria.

``` r
interventions <- read.csv(squire:::squire_file("extdata/example_DZA_intervention.csv"))
int_unique <- squire:::interventions_unique(interventions)
int_unique
#> $dates_change
#> [1] "2020-03-12" "2020-03-18" "2020-03-22" "2020-03-23"
#> 
#> $change
#> [1] 0.900 0.850 0.550 0.225
```

We can then provide these to `calibrate` as the dates (`date_R0_change`)
and relative reductions to R0 (`R0_change`). We will also specify for
model fits to be continued for 14 days into the future with `forceast
= 14`:

``` r
out <- calibrate(
      data = df,
      R0_min = 2,
      R0_max = 4,
      R0_step = 0.5,
      first_start_date = "2020-02-10",
      last_start_date = "2020-02-22",
      day_step = 4,
      replicates = 10,
      n_particles = 20,
      forecast = 14,
      R0_change = int_unique$change,
      date_R0_change = int_unique$dates_change,
      country = "Algeria"
    )
#> 
 Progress: ------------------------------------------------------------------------------------ 100%
#> 
#> 
 Progress: ---------------------------------------------------------------------------          100%
 Progress: ------------------------------------------------------------------------------------ 100%
```

Let’s see if that is any better.

``` r
plot(out, particle_fit = TRUE)
```

<img src="man/figures/README-particle fit ints-1.png" width="100%" />

That is a much better fit.

Any parameter that you could provide to `run_explicit_SEEIR_model` can
be passed to `calibrate`. This includes time varying arguments such as
`contact_matrix_set`, `ICU_bed_capacity` and `hosp_bed_capacity`. To
incorporate these into model fitting correctly, the date at which these
change must be provided (similarly to how `date_R0_change` was provided
above) using `date_ICU_bed_capacity_change`,
`date_ICU_bed_capacity_change` and `date_hosp_bed_capacity_change`
respecitvely. In addition, the user must provide a baseline value for
these, i.e. the contact matrix and bed capacity at the beginning of the
epidemic:

``` r
out <- calibrate(
      data = df,
      R0_min = 2,
      R0_max = 4,
      R0_step = 0.5,
      first_start_date = "2020-02-10",
      last_start_date = "2020-02-22",
      day_step = 4,
      replicates = 10,
      n_particles = 20,
      forecast = 14,
      R0_change = int_unique$change,
      date_R0_change = int_unique$dates_change,
      baseline_contact_matrix = get_mixing_matrix("Algeria"),
      contact_matrix_set = list(get_mixing_matrix("Algeria")*0.9),
      date_contact_matrix_set_change = "2020-03-16",
      baseline_hosp_bed_capacity = squire:::get_hosp_bed_capacity("Algeria"),
      hosp_bed_capacity = squire:::get_hosp_bed_capacity("Algeria")*c(1.1,1.2),
      date_hosp_bed_capacity_change = c("2020-04-02", "2020-04-08"),
      baseline_ICU_bed_capacity = squire:::get_ICU_bed_capacity("Algeria"),
      ICU_bed_capacity = squire:::get_ICU_bed_capacity("Algeria")*c(1.05),
      date_ICU_bed_capacity_change = c("2020-04-10"),
      country = "Algeria"
    )
#> 
 Progress: ------------------------------------------------------------------------------------ 100%
#> 
#> 
 Progress: ------------------------------------------------------------------------------------ 100%
```

(N.B. Given the potentially long running time for the grid search, the
model state is returned at the end of every day rather than every time
step (provided by `dt`). As a result model outputs such as `n_E2_I`
(number of infections in a time step) and `delta_D` (number of deaths in
a time step) reflect the outputs in the last time step rather than in
all the time steps in the last day. The plotting functions and
`format_output` provided in `squire` handle this correctly and work out
the correct number for these in the last day.)

### 5\. Conducting scenario projections to fitted data

Once you have calibrated the model to death data, it is possible to then
implement mitigation strategies forwards in time from the point of
calibration. This is achieved using the `projections` function. For
example, let’s consider a country that as of 28th April 2020 is yet to
experience an epidemic take off but may be about to. Guinea reported its
first COVID-19 death on 16th April 2020 and has reported 7 deaths by the
28th April 2020 according to data from the [European
CDC](https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide).

First let’s calibrate to this data with no interventions in place (there
are likely some interventions in place but nothing major, such as a
lockdown, has been implemented by 2020-04-28) and simulate forward for
180 days:

``` r
df <- read.csv(squire:::squire_file("extdata/example_GIN.csv"), stringsAsFactors = FALSE)

out <- calibrate(
      data = df,
      R0_min = 2.5,
      R0_max = 4,
      R0_step = 0.5,
      first_start_date = "2020-03-10",
      last_start_date = "2020-03-25",
      day_step = 5,
      replicates = 10,
      n_particles = 20,
      forecast = 180,
      country = "Guinea"
    )
#> 
 Progress: -------------------------------------------------------------------                  100%
 Progress: -------------------------------------------------------------------                  100%
 Progress: ------------------------------------------------------------------------------------ 100%
```

Firstly, let’s plot the fit up to the current day

``` r
plot(out, particle_fit = TRUE) + 
    ggplot2::xlim(as.Date(c("2020-04-10","2020-04-28"))) + 
    ggplot2::ylim(c(0,10))
```

<img src="man/figures/README-particle gin plots-1.png" width="100%" />

The fit is good and captures the stuttering chains at the beginning of
the epidemic. However, if we plot the forecasted deaths we can see the
epidemic is likely to take off:

``` r
plot(out, "deaths")
```

<img src="man/figures/README-particle gin forecast-1.png" width="100%" />

We can now use the output of `calibrate` to explore different scenario
projections using `projections`. For example, to contast this
unmitigated epidemic against a mitigation scenario with a 50% reduction
in R0 today and a further 30% in 2 weeks time:

``` r
# create our projections
p <- projections(r = out, R0_change = c(0.5, 0.2), tt_R0 = c(0, 14))
```

The output generated from `projections` is the same class and structure
as from `calibrate` and can be plotted against the unmitigated scenario
using `projection_plotting`:

``` r
ggproj <- projection_plotting(r_list = list(out,p),
                    scenarios = c("Unmitigated","Mitigation"),
                    var_select = c("ICU_occupancy", "ICU_demand"),
                    add_parms_to_scenarios = TRUE,ci = FALSE,summarise = TRUE)

# and lets add in the ICU capacity
ggproj + ggplot2::geom_hline(yintercept = out$parameters$ICU_bed_capacity)
```

<img src="man/figures/README-projection plotting-1.png" width="100%" />

We can see above that the intervention introduced is nearly sufficient
to prevent ICU demand (solid line red) from exceeding the supply,
whereas in the unmitigated strategy this did occur.

We can also model changing interventions by changing the contact matrix
over time as well as the availability of ICU and hospital beds. E.g.
decreasing contacts by 75% in a week before relaxing it to 80% in 20
days time, while increasing hospital and ICU beds by 20% in 30 days
time. (N.B. We can turn off the automatic scenario parameter labelling
with `add_parms_to_scenarios = FALSE`):

``` r
# create our projections
p <- projections(r = out, 
                  contact_matrix_set_change = c(1, 0.25, 0.8), 
                  tt_contact_matrix = c(0, 7, 30), 
                  hosp_bed_capacity_change = c(1, 3),
                  tt_hosp_beds = c(0, 30),
                  ICU_bed_capacity_change = c(1, 3),
                  tt_ICU_beds = c(0,30))

projection_plotting(r_list = list(out, p),
                    scenarios = c("Unmitigated","Mitigation"),
                    var_select = c("ICU_occupancy", "ICU_demand","deaths"),
                    add_parms_to_scenarios = FALSE,
                    ci = FALSE,summarise = TRUE) + 
ggplot2::geom_hline(yintercept = out$parameters$ICU_bed_capacity)
```

<img src="man/figures/README-projection relative beds-1.png" width="100%" />

Above we can see that the increase in bed causes the mitigation strategy
to plateau at a higher bed occupancy, however, not high enough to meet
the demand. However, the impacts to the contact matrix and beds have
caused both a shift and a decrease in the daily deaths, which in turn
causes a noticable decrease in total deaths:

``` r
projection_plotting(r_list = list(out,p),
                    scenarios = c("Unmitigated","Mitigation"),
                    var_select = c("D"),
                    add_parms_to_scenarios = FALSE,
                    ci = FALSE,summarise = TRUE)
```

<img src="man/figures/README-total deaths projection-1.png" width="100%" />

Rather than using relative changes to the interventions, we can provide
absolute measures. For example, to change the R0 to 2 today and add 500
ICU beds in 40 days time:

``` r
# what is the current capacity
icu <- tail(out$parameters$ICU_bed_capacity,1)

# create our projections
p <- projections(r = out, 
                  R0 = 2,
                  tt_R0 = 0,
                  ICU_bed_capacity = c(icu, icu + 500),
                  tt_ICU_beds = c(0,40))

projection_plotting(r_list = list(out,p),
                    scenarios = c("Unmitigated","Mitigation"),
                    var_select = c("ICU_occupancy", "ICU_demand","deaths"),
                    add_parms_to_scenarios = FALSE,
                    ci = FALSE,summarise = TRUE)
```

<img src="man/figures/README-projection plotting absolute-1.png" width="100%" />

Lastly, in order to run simulations for longer than the number set in
`calibrate(forecast = x)`, we can use the argument `time_period` to set
the numbers of days that `projections` should simulate for. For example,
we could redo our calibration and have it only run up the current day:

``` r
df <- read.csv(squire:::squire_file("extdata/example_GIN.csv"), stringsAsFactors = FALSE)

out <- calibrate(
      data = df,
      R0_min = 2.5,
      R0_max = 4,
      R0_step = 0.5,
      first_start_date = "2020-03-10",
      last_start_date = "2020-03-25",
      day_step = 5,
      replicates = 10,
      n_particles = 20,
      forecast = 0,
      country = "Guinea"
    )

plot(out, particle_fit = TRUE)
```

<img src="man/figures/README-particle gin alt-1.png" width="100%" />

We can now use this to project 90 days forwards with the current level
of interventions, as well as a projection of 90 days with a 50%
reduction in R0:

``` r
p_no_change <- projections(out, time_period = 90)
p_change <- projections(out, R0_change = 0.5, tt_R0 = 0, time_period = 90)

projection_plotting(list(p_no_change, p_change), 
                    scenarios = c("No Change", "50% R0"), 
                    add_parms_to_scenarios = FALSE,
                    var_select = "deaths",
                    date_0 = max(df$date), 
                    x_var = "date")
```

<img src="man/figures/README-particle gin alt proj-1.png" width="100%" />
