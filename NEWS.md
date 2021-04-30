# squire 0.6.6

* `projections` can be used now for `nimue` models

# squire 0.6.5

* `pmcmc` now accepts dates of change for vaccine efficacy for `nimue` 

# squire 0.6.4

* `pmcmc` changes to adapt for `nimue` class models and vaccines

# squire 0.6.3

* `beta_est` accepts `nimue` models

# squire 0.6.2

* Typo in `dur_not_get_mv_survive`

# squire 0.6.1

* `pmcmc(..., scaling_factor)` allows scaling factors to be passed to pmcmc
* Correction to `deterministic_model` model class

# squire 0.6.0

* Waning added to squire

# squire 0.5.8

* Updated to use odin 1.1.0
* `odin_is_discrete` new unexported function

# squire 0.5.7

* Github actions included for automatic release tagging

# squire 0.5.6

* `pmcmc(..., gibbs_sampling = TRUE)` implements Gibbs sampler

# squire 0.5.5

* `population$n` are all integer class

# squire 0.5.4

* `beta_est` accepts apothecary models
* Updated json odin files that weren't compiled correctly

# squire 0.5.3

* `default_durations` contains all durations
* `default_durations` and `default_probs` now exported for ease
* `run_deterministic_SEIR_model` has odin pars exported now

# squire 0.5.2

* `Parameters` vignette updated to include new parameters
* `run_deterministic_comparison` bug fix for when transmission is eliminated 

# squire 0.5.1

* `projections` bug fixed for setting time varying durations
* Taiwan healthcare capacity added 
* `default_durations` updated for incorrect hospital durations

# squire 0.5.0

## 0.5.0 Major

* Parameter Updates. See parameters vignette for update.
* `triggering` function for doing `projections` with model state triggered R0 changes 
* Time-varying durations of ICU and hospital stay

## 0.5.0 Minor

* `projections` allows for different args to be passed to each replicate
* `projections` allows for additional user model args to be passed


# squire 0.4.34

* `pmcmc` argument `treated_deaths_only` will use deaths from treatment in likelihood

# squire 0.4.33

* `format_output` accepts `deaths_treatment` and `deaths_no_treatment`

# squire 0.4.32

* `intervention_dates_for_odin` fills dates earlier than `date_R0_change`

# squire 0.4.31

* Correction to random walk parameters to more accurately continue the walk

# squire 0.4.30

* `evaluate_Rt_pmcmc` takes `Rt_rw_n` for random walk esque shifts

# squire 0.4.29

* `format_output` backwards compatible for incidence

# squire 0.4.28

* `calibrate` patched as `dt` argument was not being passed through
* `format_output` corrected for hospital/ICU incidence

# squire 0.4.27 

* `evaluate_Rt_pmcmc` patch for non specified `Rt_shift` in 3 parameter version

# squire 0.4.26 

* `pmcmc` more flexible for varying which parameters are fitted.
* `evaluate_Rt_pmcmc` altered for increased flexibility

# squire 0.4.25

* `format_output` now returns `hospital_incidence` and `ICU_incidence`
* `evaluate_Rt_pmcmc` patch for `Meff_switch_date` earlier than `date_R0_change[1]`

# squire 0.4.24

* `Meff_pl` can be scaled or not in relation to Meff

# squire 0.4.23

* `pmcmc` no longer takes Rt_func as hard coded given Meff_pl complexity

# squire 0.4.22

* Johnstone Change approach implemented for pmcmc RM

# squire 0.4.21

* `evaluate_Rt` patch for `roll` of Meff

# squire 0.4.20

* `evaluate_Rt` allows for `date_Meff_change` to be later/earlier than `date_R0_change` ends

# squire 0.4.19

* Large overhaul to make `projections` and plotting work with deterministic models

# squire 0.4.18

* `calc_loglikelihood` fix for reduced date vectors

# squire 0.4.17

* `pars_init` for `pmcmc` can be provided as a list for where each chain starts

# squire 0.4.16

* `pmcmc` method for 4 parameter estimation.

# squire 0.4.15

* `projections` now matches by column names when including projection outputs
* Patch to `t0_variables` for new Rt_func approach

# squire 0.4.14

* replace chained binomials with mutivariate hypergeometric sampling in stochastic model
* stochastic model to output number newly requiring ICU beds at each timepoint

# squire 0.4.13

* `calibrate` now takes an `Rt_func` argument for specifying Rt calculation

# squire 0.4.12

* Explicit Meff inclusion in `R0_date_particle_filter` and `projections` fix

# squire 0.4.11

* patching `projections` when projecting further than original calibrate run

# squire 0.4.10

* `calibrate` new argument for `R0_prior`

# squire 0.4.9

* `calibrate` now allows intervetnion dates to be before epidemic start date

# squire 0.4.8

* `calibrate` argument `reporting_fraction` implemented now in likelihood 

# squire 0.4.7

* `calibrate` can do 3D grid scans investigating Meff
* `plot.squire_scan` will collapse n-dimensional scans using `show` argument

# squire 0.4.6

* Patch for `particle_filter_data` when only 1 row of data is provided

# squire 0.4.5

* Add functionality for running the deterministic model in javascript

# squire 0.4.4

* Patch for `calibrate` to ensure correct dt values provided

# squire 0.4.3

* `calibrate` can now take `squire:::deterministic_model()` as model type.
* Output of above does not work with plotting or projections yet. 

# squire 0.4.2

* Patch for `plot.scan_results` where log likelihood was being plotted for both

# squire 0.4.1

* Patch for passing in user defined population and matrices

# squire 0.4.0

* `calibrate` now uses particle filter. See README for examples (#12)
  * Dates are to be provided for timings of interventions before today (#44, #17)
  * Custom populations, contact matrices and bed capacity allowed (#73, #80, #28, #16)
  * Increased modularity for new models (#62, #61, #49)
  * Returns lieklihood of different baseline R0s and start dates (#14)
* `get_mixing_matrix` takes iso3c (#84)
* Number of plotting enhancement (#72, #69)
* Parameter references vignette and website update (#13, #1, #5, #78)

# squire 0.3.0

* `projection` function for doing scenario projections

# squire 0.2.3

* `format_output` and calibrate orderly plotting functioanlity in

# squire 0.2.2

* `calibrate` now allows different seeding events (#50) and undereporting (#11)
* New plotting functions for `plot.squire_calibration`
* iso3c country codes in (#52)

# squire 0.2.1

* `extract_all_outputs` and `extract_specific_output` (#37)

# squire 0.2.0

* New minor version for iterative PR development cycle from now

# squire 0.1.0

* initial version of the model
