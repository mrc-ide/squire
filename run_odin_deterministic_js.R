# Setting Up Model Parameters
population <- get_population("United Kingdom")
population <- population$n
m <- squire::get_mixing_matrix("United Kingdom")
MIJ <- t(vapply(seq(population),function(x){
  m[x,] * population[x]
}, FUN.VALUE = numeric(length(population))))
adjust_mat <- (MIJ + t(MIJ))/2 # symmetric and balanced
processed_matrix <- t(vapply(seq(population), function(x) {
  adjust_mat[x, ] / population[x]
}, FUN.VALUE = numeric(length(population))))


squire::process_contact_matrix_scaled_age(m, population)

# Probs
prob_hosp <- c(0.000744192, 0.000634166,0.001171109, 0.002394593, 0.005346437 ,
              0.010289885, 0.016234604, 0.023349169, 0.028944623, 0.038607042 ,
              0.057734879, 0.072422135, 0.101602458, 0.116979814, 0.146099064,
              0.176634654 ,0.180000000),
prob_severe <- c(0.05022296,	0.05022296,	0.05022296,	0.05022296,	0.05022296,
                0.05022296,	0.05022296,	0.053214942, 0.05974426,	0.074602879,
                0.103612417, 0.149427991, 0.223777304,	0.306985918,
                0.385779555, 0.461217861, 0.709444444),
prob_non_severe_death_treatment <- c(0.0125702,	0.0125702,	0.0125702,	0.0125702,
                                    0.0125702,	0.0125702,	0.0125702,	0.013361147,
                                    0.015104687,	0.019164124,	0.027477519,	0.041762108,
                                    0.068531658,	0.105302319,	0.149305732,	0.20349534,	0.5804312)
prob_non_severe_death_no_treatment <- vapply(prob_non_severe_death_treatment * 2, min, numeric(1), 1)
prob_severe_death_treatment <- rep(0.5, length(prob_hosp))
prob_severe_death_no_treatment <- rep(0.95, length(prob_hosp))

dur_R <- 2.09
dur_hosp <- 5
prob_hosp <- c(0.5, 0.5)
relative_R0_by_age <- prob_hosp * dur_hosp + (1 - prob_hosp) * dur_R
adjusted_eigen <- Re(eigen(m * relative_R0_by_age)$values[1])
R0 <- 3
beta <- R0/(adjusted_eigen)
pars <- list(N_age = length(population),
             S_0 = population,
             E1_0 = rep(0, 16),
             E2_0 = rep(0, 16),
             IMild_0 = rep(1, 16),
             ICase1_0 = rep(1, 16),
             ICase2_0 = rep(1, 16),
             IOxGetLive1_0 = rep(0, 16),
             IOxGetLive2_0 = rep(0, 16),
             IOxGetDie1_0 = rep(0, 16),
             IOxGetDie2_0 = rep(0, 16),
             IOxNotGetLive1_0 = rep(0, 16),
             IOxNotGetLive2_0 = rep(0, 16),
             IOxNotGetDie1_0 = rep(0, 16),
             IOxNotGetDie2_0 = rep(0, 16),
             IMVGetLive1_0 = rep(0, 16),
             IMVGetLive2_0 = rep(0, 16),
             IMVGetDie1_0 = rep(0, 16),
             IMVGetDie2_0 = rep(0, 16),
             IMVNotGetLive1_0 = rep(0, 16),
             IMVNotGetLive2_0 = rep(0, 16),
             IMVNotGetDie1_0 = rep(0, 16),
             IMVNotGetDie2_0 = rep(0, 16),
             IRec1_0 = rep(0, 16),
             IRec2_0 = rep(0, 16),
             R_0 = rep(0, 16),
             D_0 = rep(0, 16),
             gamma_E = (2 * 1/4.58),
             gamma_R = (1/2.09),
             gamma_hosp = (2 * 1/5),
             gamma_get_ox_survive = (2 * 1/6),
             gamma_get_ox_die = (2 * 1/3.5),
             gamma_not_get_ox_survive = (2 * 1/9),
             gamma_not_get_ox_die = (0.5 * 2 * 1/9),
             gamma_get_mv_survive = (2 * 1/5.5),
             gamma_get_mv_die = (2 * 1/4),
             gamma_not_get_mv_survive = (2 * 1/12),
             gamma_not_get_mv_die = (2 * 1/1),
             gamma_rec = (2 * 1/6),
             prob_hosp = c(0.5, 0.5),
             prob_severe = c(0.6, 0.6),
             prob_non_severe_death_treatment = c(0.1, 0.1),
             prob_non_severe_death_no_treatment = c(0.9, 0.9),
             prob_severe_death_treatment = c(0.2, 0.2),
             prob_severe_death_no_treatment = c(0.9, 0.9),
             p_dist = c(0.5, 0.5),
             hosp_bed_capacity = 100000,
             ICU_bed_capacity = 100000,
             beta = 0.33,
             m = m)
pars$beta <- beta
pars$m <- t(t(m) / population)

x <- odin::odin("inst/odin/explicit_SEIR_deterministic.R")
mod <- x(user = pars)
t <- seq(from = 1, to = 500)
output <- mod$run(t)
output <- mod$transform_variables(output)
t <- output$t
S <- apply(output$S, 1, sum)
R <- apply(output$R, 1, sum)
D <- apply(output$D, 1, sum)
plot(t, S, ylim = c(0, max(S)), type = "l")
lines(t, R, col = "green")

N <- output$S + output$E1 + output$E2 + output$IMild + output$ICase1 + output$ICase2 +
  output$IOxGetLive1 + output$IOxGetLive2 + output$IOxGetDie1 + output$IOxGetDie2 +
  output$IOxNotGetLive1 + output$IOxNotGetLive2 + output$IOxNotGetDie1 + output$IOxNotGetDie2 +
  output$IMVGetLive1 + output$IMVGetLive2 + output$IMVGetDie1 + output$IMVGetDie2 +
  output$IMVNotGetLive1 + output$IMVNotGetLive2 + output$IMVNotGetDie1 + output$IMVNotGetDie2 +
  output$IRec1 + output$IRec2 + output$R + output$D

plot(t, N[, 1], ylim = c(0, max(N[, 1])), type = "l")
