devtools::install_github("mrc-ide/odin.js@mrc-1494", force = TRUE)

# Probs
prob_hosp <- c(0.000744192, 0.000634166,0.001171109, 0.002394593, 0.005346437 ,
              0.010289885, 0.016234604, 0.023349169, 0.028944623, 0.038607042 ,
              0.057734879, 0.072422135, 0.101602458, 0.116979814, 0.146099064,
              0.176634654 ,0.180000000)
prob_severe <- c(0.05022296,	0.05022296,	0.05022296,	0.05022296,	0.05022296,
                0.05022296,	0.05022296,	0.053214942, 0.05974426,	0.074602879,
                0.103612417, 0.149427991, 0.223777304,	0.306985918,
                0.385779555, 0.461217861, 0.709444444)
prob_non_severe_death_treatment <- c(0.0125702,	0.0125702,	0.0125702,	0.0125702,
                                    0.0125702,	0.0125702,	0.0125702,	0.013361147,
                                    0.015104687,	0.019164124,	0.027477519,	0.041762108,
                                    0.068531658,	0.105302319,	0.149305732,	0.20349534,	0.5804312)
prob_non_severe_death_no_treatment <- vapply(prob_non_severe_death_treatment * 2, min, numeric(1), 1)
prob_severe_death_treatment <- rep(0.5, length(prob_hosp))
prob_severe_death_no_treatment <- rep(0.95, length(prob_hosp))

# Setting Up Model Parameters
population <- squire::get_population("United Kingdom")
population <- population$n
m <- squire::get_mixing_matrix("United Kingdom")
m <- squire:::process_contact_matrix_scaled_age(m, population)
dur_R <- 2.09
dur_hosp <- 5
beta <- squire::beta_est_explicit(dur_R, dur_hosp, prob_hosp, m, 2)

pars <- list(N_age = length(population),
             S_0 = population,
             E1_0 = rep(0, length(population)),
             E2_0 = rep(0, length(population)),
             IMild_0 = rep(1, length(population)),
             ICase1_0 = rep(1, length(population)),
             ICase2_0 = rep(1, length(population)),
             IOxGetLive1_0 = rep(0, length(population)),
             IOxGetLive2_0 = rep(0, length(population)),
             IOxGetDie1_0 = rep(0, length(population)),
             IOxGetDie2_0 = rep(0, length(population)),
             IOxNotGetLive1_0 = rep(0, length(population)),
             IOxNotGetLive2_0 = rep(0, length(population)),
             IOxNotGetDie1_0 = rep(0, length(population)),
             IOxNotGetDie2_0 = rep(0, length(population)),
             IMVGetLive1_0 = rep(0, length(population)),
             IMVGetLive2_0 = rep(0, length(population)),
             IMVGetDie1_0 = rep(0, length(population)),
             IMVGetDie2_0 = rep(0, length(population)),
             IMVNotGetLive1_0 = rep(0, length(population)),
             IMVNotGetLive2_0 = rep(0, length(population)),
             IMVNotGetDie1_0 = rep(0, length(population)),
             IMVNotGetDie2_0 = rep(0, length(population)),
             IRec1_0 = rep(0, length(population)),
             IRec2_0 = rep(0, length(population)),
             R_0 = rep(0, length(population)),
             D_0 = rep(0, length(population)),
             gamma_E = (2 * 1/4.58),
             gamma_R = (1/dur_R),
             gamma_hosp = (2 * 1/dur_hosp),
             gamma_get_ox_survive = (2 * 1/6),
             gamma_get_ox_die = (2 * 1/3.5),
             gamma_not_get_ox_survive = (2 * 1/9),
             gamma_not_get_ox_die = (0.5 * 2 * 1/9),
             gamma_get_mv_survive = (2 * 1/5.5),
             gamma_get_mv_die = (2 * 1/4),
             gamma_not_get_mv_survive = (2 * 1/12),
             gamma_not_get_mv_die = (2 * 1/1),
             gamma_rec = (2 * 1/6),
             prob_hosp = prob_hosp,
             prob_severe = prob_severe,
             prob_non_severe_death_treatment = prob_non_severe_death_treatment,
             prob_non_severe_death_no_treatment = prob_non_severe_death_no_treatment,
             prob_severe_death_treatment = prob_severe_death_treatment,
             prob_severe_death_no_treatment = prob_severe_death_no_treatment,
             p_dist = rep(1, length(population)),
             hosp_bed_capacity = 10,
             ICU_bed_capacity = 10,
             tt_matrix = c(0, 50, 100),
             tt_beta = c(0, 50, 200),
             beta_set = c(beta, beta/2, beta))

mm <- t(t(m) / population)
pars$mix_mat_set <- aperm(array(c(mm, mm, mm), dim = c(dim(mm), 3)), c(3, 1, 2))

y <- odin.js::odin_js("inst/odin/explicit_SEIR_deterministic.R")
pars$hosp_bed_capacity <- 10000000000
pars$ICU_bed_capacity <- 10000000000
mod <- y(user = pars)
t <- seq(from = 1, to = 250)
output <- mod$run(t)
S_js <- apply(output[, grepl("S", colnames(output))], 1, sum)
D_js <- apply(output[, 427:443], 1, sum)
R_js <- apply(output[, 410:426], 1, sum)

x <- odin::odin("inst/odin/explicit_SEIR_deterministic.R")
pars$hosp_bed_capacity <- 10000000000
pars$ICU_bed_capacity <- 10000000000
mod <- x(user = pars)
t <- seq(from = 1, to = 250)
output <- mod$run(t)
output <- mod$transform_variables(output)
t <- output$t
S <- apply(output$S, 1, sum)
R <- apply(output$R, 1, sum)
D <- apply(output$D, 1, sum)

plot(t, S, ylim = c(0, max(S)), type = "l")
lines(t, S_js, col = "red")

plot(t, D, type = "l")
lines(t, D_js, col = "red")

plot(t, R, type = "l")
lines(t, R_js, col = "green")

lines(t, R, col = "green")
max(R)/max(S)
plot(t, D, type = "l")
max(D)


output$ICU_occ[100]
output$current_free_ICUs[100]
sum(output$number_requiring_IMV[100, ])
output$total_number_requiring_IMV[100]
output$total_number_get_IMV[100]
sum(output$IMV_dist_weighting[100, ])
sum(output$number_get_IMV[100, ])


capacity <- c(1, 10, 100, 1000, 10000, 100000, 1000000, 100000000)
for (i in 1:8) {
  pars$hosp_bed_capacity <- capacity[i]
  pars$ICU_bed_capacity <- capacity[i]
  mod <- x(user = pars)
  t <- seq(from = 1, to = 500)
  output <- mod$run(t)
  output <- mod$transform_variables(output)
  D <- apply(output$D, 1, sum)
  t <- output$t
  if (i == 1) {
    plot(t, D, ylim = c(0, max(D)), type = "l")
  } else {
    lines(t, D)
  }
}

N <- output$S + output$E1 + output$E2 + output$IMild + output$ICase1 + output$ICase2 +
  output$IOxGetLive1 + output$IOxGetLive2 + output$IOxGetDie1 + output$IOxGetDie2 +
  output$IOxNotGetLive1 + output$IOxNotGetLive2 + output$IOxNotGetDie1 + output$IOxNotGetDie2 +
  output$IMVGetLive1 + output$IMVGetLive2 + output$IMVGetDie1 + output$IMVGetDie2 +
  output$IMVNotGetLive1 + output$IMVNotGetLive2 + output$IMVNotGetDie1 + output$IMVNotGetDie2 +
  output$IRec1 + output$IRec2 + output$R + output$D
N <- apply(N, 1, sum)
plot(t, N, ylim = c(0, max(N)), type = "l")



