## Model Parameters
##------------------------------------------------------------------------------

# Number of Age groups
N_age <- user()
time <- t
output(time) <- TRUE

# Generating Force of Infection
m[, ] <- interpolate(tt_matrix, mix_mat_set, "constant")
dim(m) <- c(N_age, N_age)
tt_matrix[] <- user()
mix_mat_set[, ,] <- user()
dim(tt_matrix) <- user()
dim(mix_mat_set) <- c(length(tt_matrix), N_age, N_age)

# Interpolation for beta
beta <- interpolate(tt_beta, beta_set, "constant")
tt_beta[] <- user()
beta_set[] <- user()
dim(tt_beta) <- user()
dim(beta_set) <- length(tt_beta)

# Generating Force of Infection
temp[] <- IMild[i] + ICase1[i] + ICase2[i]
s_ij[,] <- m[i, j] * temp[j]
lambda[] <- beta * sum(s_ij[i, ])

## Interpolation for Hospital and ICU Capacity
hosp_bed_capacity <- interpolate(tt_hosp_beds, hosp_beds, "constant")
tt_hosp_beds[] <- user()
hosp_beds[] <- user()
dim(tt_hosp_beds) <- user()
dim(hosp_beds) <- length(tt_hosp_beds)

ICU_bed_capacity <- interpolate(tt_ICU_beds, ICU_beds, "constant")
tt_ICU_beds[] <- user()
ICU_beds[] <- user()
dim(tt_ICU_beds) <- user()
dim(ICU_beds) <- length(tt_ICU_beds)


dim(lambda) <- N_age
dim(s_ij) <- c(N_age,N_age)
dim(temp) <- N_age

# Rates of Progression

# rate of progression through latent infection
gamma_E <- user()

# rate of progression from mild infection to recovery
gamma_IMild <- user()

# rate of progression from symptom onset to requiring hospitalisation
gamma_ICase <- user()

# rate of progression through requiring oxygen compartment conditional on getting oxygen and surviving
gamma_get_ox_survive[] <- user()
tt_dur_get_ox_survive[] <- user()
dim(tt_dur_get_ox_survive) <- user()
dim(gamma_get_ox_survive) <- length(tt_dur_get_ox_survive)
gamma_get_ox_survive_i <- interpolate(tt_dur_get_ox_survive, gamma_get_ox_survive, "constant")

# rate of progression through requiring oxygen compartment conditional on getting oxygen and dying
gamma_get_ox_die[] <- user()
tt_dur_get_ox_die[] <- user()
dim(tt_dur_get_ox_die) <- user()
dim(gamma_get_ox_die) <- length(tt_dur_get_ox_die)
gamma_get_ox_die_i <- interpolate(tt_dur_get_ox_die, gamma_get_ox_die, "constant")

# rate of progression through requiring oxygen compartment conditional on not getting oxygen and surviving
gamma_not_get_ox_survive <- user()

# rate of progression through requiring oxygen compartment conditional on not getting oxygen and dying
gamma_not_get_ox_die <- user()

# rate of progression through requiring mechanical ventilation compartment conditional on getting ventilation and surviving
gamma_get_mv_survive[] <- user()
tt_dur_get_mv_survive[] <- user()
dim(tt_dur_get_mv_survive) <- user()
dim(gamma_get_mv_survive) <- length(tt_dur_get_mv_survive)
gamma_get_mv_survive_i <- interpolate(tt_dur_get_mv_survive, gamma_get_mv_survive, "constant")

# rate of progression through requiring mechanical ventilation compartment conditional on getting ventilation and dying
gamma_get_mv_die[] <- user()
tt_dur_get_mv_die[] <- user()
dim(tt_dur_get_mv_die) <- user()
dim(gamma_get_mv_die) <- length(tt_dur_get_mv_die)
gamma_get_mv_die_i <- interpolate(tt_dur_get_mv_die, gamma_get_mv_die, "constant")

# rate of progression through requiring mechanical ventilation compartment conditional on not getting ventilation and surviving
gamma_not_get_mv_survive <- user()

# rate of progression through requiring mechanical ventilation compartment conditional on not getting ventilation and dying
gamma_not_get_mv_die <- user()

# rate of progression through post-ICU recovery compartment
gamma_rec <- user()

# Probabilities
prob_hosp[] <- user() # probability of requiring hospitalisation by age
prob_severe[] <- user() # probability of severe disease (requiring mechanical ventilation) by age
prob_non_severe_death_treatment[] <- user() # probability of dying from non-severe disease (i.e. requiring oxygen but not mechanical ventilation) by age given you receive appropriate treatment (proxy here is whether a general hospital bed is available)
prob_non_severe_death_no_treatment[] <- user() # probability of dying from non-severe disease (i.e. requiring oxygen but not mechanical ventilation) by age given you do NOT receive appropriate treatment (proxy here is whether a general hospital bed is available)
prob_severe_death_treatment[] <- user() # probability of dying from severe disease (i.e. requiring mechanical ventilation) by age given you receive appropriate treatment (proxy here is whether an ICU bed is available)
prob_severe_death_no_treatment[] <- user() # probability of dying from severe disease (i.e. requiring mechanical ventilation) by age given you do NOT receive appropriate treatment (proxy here is whether an ICU bed is available)
p_dist[] <- user() # distributing infections in given age class to available hosp/ICU beds (make all equal to make it random and not related to age)



## Derivatives for Flows Between Compartments
##------------------------------------------------------------------------------

# Susceptibles, Latent and Infections Prior to Hospitalisation
deriv(S[]) <- -S[i] * lambda[i]
deriv(E1[]) <- lambda[i] * S[i] - gamma_E * E1[i]
deriv(E2[]) <- gamma_E * E1[i] - gamma_E * E2[i]
deriv(IMild[]) <- gamma_E * E2[i] * (1 - prob_hosp[i]) - gamma_IMild * IMild[i]
deriv(ICase1[]) <- gamma_E * E2[i] * prob_hosp[i] - gamma_ICase * ICase1[i]
deriv(ICase2[]) <- gamma_ICase * ICase1[i] - gamma_ICase * ICase2[i]

# Infections Requiring Mechanical Ventilation (an ICU Bed)
ICU_occ <- sum(IMVGetLive1) + sum(IMVGetLive2) + sum(IMVGetDie1) + sum(IMVGetDie2) # Summing number of infections in compartments that use ICU beds
current_free_ICUs <- ICU_bed_capacity - ICU_occ + gamma_get_mv_survive_i *sum(IMVGetLive2) + gamma_get_mv_die_i *sum(IMVGetDie2)
number_requiring_IMV[] <- gamma_ICase * ICase2[i] * prob_severe[i]
total_number_requiring_IMV <- sum(number_requiring_IMV)
total_number_get_IMV <- if(current_free_ICUs <= 0) 0 else(if(current_free_ICUs - total_number_requiring_IMV >= 0) total_number_requiring_IMV else(current_free_ICUs)) # Working out the number of new ICU requiring infections that get a bed
IMV_dist_weighting[] <- number_requiring_IMV[i] * p_dist[i]
number_get_IMV[] <- if (total_number_requiring_IMV == 0) 0 else IMV_dist_weighting[i]/sum(IMV_dist_weighting) * total_number_get_IMV

# Tracking Cumulative Hosp/ICU Incidence for Analysis Purposes
deriv(cum_hosp_inc[]) <- number_requiring_Ox[i]
deriv(cum_ICU_inc[]) <- number_requiring_IMV[i]

# Updating MV and Ox Related Compartments
deriv(IMVGetLive1[]) <- (1 - prob_severe_death_treatment[i]) * number_get_IMV[i] - gamma_get_mv_survive_i * IMVGetLive1[i]
deriv(IMVGetLive2[]) <- gamma_get_mv_survive_i * IMVGetLive1[i] -  gamma_get_mv_survive_i * IMVGetLive2[i]
deriv(IMVGetDie1[]) <- (prob_severe_death_treatment[i] * number_get_IMV[i]) - gamma_get_mv_die_i * IMVGetDie1[i]
deriv(IMVGetDie2[]) <- gamma_get_mv_die_i * IMVGetDie1[i] -  gamma_get_mv_die_i * IMVGetDie2[i]
deriv(IMVNotGetLive1[]) <- (number_requiring_IMV[i] - number_get_IMV[i]) * (1 - prob_severe_death_no_treatment[i]) - gamma_not_get_mv_survive * IMVNotGetLive1[i]
deriv(IMVNotGetLive2[]) <- gamma_not_get_mv_survive * IMVNotGetLive1[i] -  gamma_not_get_mv_survive * IMVNotGetLive2[i]
deriv(IMVNotGetDie1[]) <- (number_requiring_IMV[i] - number_get_IMV[i]) * prob_severe_death_no_treatment[i] - gamma_not_get_mv_die * IMVNotGetDie1[i]
deriv(IMVNotGetDie2[]) <- gamma_not_get_mv_die * IMVNotGetDie1[i] -  gamma_not_get_mv_die * IMVNotGetDie2[i]

deriv(IRec1[]) <- gamma_get_mv_survive_i * IMVGetLive2[i] - gamma_rec * IRec1[i]
deriv(IRec2[]) <- gamma_rec * IRec1[i] - gamma_rec * IRec2[i]

# Infections Requiring Oxygen (a general Hosptial Bed)
hosp_occ <- sum(IOxGetLive1) + sum(IOxGetLive2) + sum(IOxGetDie1) + sum(IOxGetDie2) + sum(IRec1) + sum(IRec2) # Summing number of infections in compartments that use general hospital beds
current_free_hosp <- hosp_bed_capacity - hosp_occ + gamma_get_ox_die_i*sum(IOxGetDie2) + gamma_get_ox_survive_i * sum(IOxGetLive2) + gamma_rec * sum(IRec2) - gamma_get_mv_survive_i * sum(IMVGetLive2)
number_requiring_Ox[] <- gamma_ICase * ICase2[i] * (1 - prob_severe[i])
total_number_requiring_ox <- sum(number_requiring_Ox)

# Working out the number of new hospital bed requiring infections that get a bed
total_number_get_hosp <- if (current_free_hosp <= 0) 0 else (if(current_free_hosp - total_number_requiring_ox >= 0) total_number_requiring_ox else(current_free_hosp))
Ox_dist_weighting[] <- number_requiring_Ox[i] * p_dist[i]
number_get_Ox[] <- if (total_number_requiring_ox == 0) 0 else Ox_dist_weighting[i]/sum(Ox_dist_weighting) * total_number_get_hosp

deriv(IOxGetLive1[]) <- (1 - prob_non_severe_death_treatment[i]) * number_get_Ox[i] - gamma_get_ox_survive_i * IOxGetLive1[i]
deriv(IOxGetLive2[]) <- gamma_get_ox_survive_i * IOxGetLive1[i] -  gamma_get_ox_survive_i * IOxGetLive2[i]
deriv(IOxGetDie1[]) <- (prob_non_severe_death_treatment[i] * number_get_Ox[i]) - gamma_get_ox_die_i * IOxGetDie1[i]
deriv(IOxGetDie2[]) <- gamma_get_ox_die_i * IOxGetDie1[i] -  gamma_get_ox_die_i * IOxGetDie2[i]
deriv(IOxNotGetLive1[]) <- (number_requiring_Ox[i] - number_get_Ox[i]) * (1 - prob_non_severe_death_no_treatment[i]) - gamma_not_get_ox_survive * IOxNotGetLive1[i]
deriv(IOxNotGetLive2[]) <- gamma_not_get_ox_survive * IOxNotGetLive1[i] -  gamma_not_get_ox_survive * IOxNotGetLive2[i]
deriv(IOxNotGetDie1[]) <- (number_requiring_Ox[i] - number_get_Ox[i]) * prob_non_severe_death_no_treatment[i] - gamma_not_get_ox_die * IOxNotGetDie1[i]
deriv(IOxNotGetDie2[]) <- gamma_not_get_ox_die * IOxNotGetDie1[i] -  gamma_not_get_ox_die * IOxNotGetDie2[i]

# Recoveries and Deaths
deriv(R[]) <- (gamma_rec * IRec2[i]) + (gamma_IMild * IMild[i]) + (gamma_get_ox_survive_i * IOxGetLive2[i]) + (gamma_not_get_ox_survive * IOxNotGetLive2[i]) + (gamma_not_get_mv_survive * IMVNotGetLive2[i])
deriv(D[]) <- (gamma_get_ox_die_i * IOxGetDie2[i]) + (gamma_not_get_ox_die * IOxNotGetDie2[i]) + (gamma_get_mv_die_i * IMVGetDie2[i]) + (gamma_not_get_mv_die * IMVNotGetDie2[i])

deriv(D_get[]) <- (gamma_get_ox_die_i * IOxGetDie2[i]) + (gamma_get_mv_die_i * IMVGetDie2[i])
deriv(D_not_get[]) <- (gamma_not_get_ox_die * IOxNotGetDie2[i]) + (gamma_not_get_mv_die * IMVNotGetDie2[i])

# Outputting Hospitalisation and ICU Incidence
output(number_requiring_Ox[]) <- TRUE
output(number_requiring_IMV[]) <- TRUE

## Initial states:
initial(S[]) <- S_0[i]
initial(E1[]) <- E1_0[i]
initial(E2[]) <- E2_0[i]
initial(IMild[]) <- IMild_0[i]
initial(ICase1[]) <- ICase1_0[i]
initial(ICase2[]) <- ICase2_0[i]
initial(cum_hosp_inc[]) <- 0
initial(cum_ICU_inc[]) <- 0
initial(IOxGetLive1[]) <- IOxGetLive1_0[i]
initial(IOxGetLive2[]) <- IOxGetLive2_0[i]
initial(IOxGetDie1[]) <- IOxGetDie1_0[i]
initial(IOxGetDie2[]) <- IOxGetDie2_0[i]
initial(IOxNotGetLive1[]) <- IOxNotGetLive1_0[i]
initial(IOxNotGetLive2[]) <- IOxNotGetLive2_0[i]
initial(IOxNotGetDie1[]) <- IOxNotGetDie1_0[i]
initial(IOxNotGetDie2[]) <- IOxNotGetDie2_0[i]
initial(IMVGetLive1[]) <- IMVGetLive1_0[i]
initial(IMVGetLive2[]) <- IMVGetLive2_0[i]
initial(IMVGetDie1[]) <- IMVGetDie1_0[i]
initial(IMVGetDie2[]) <- IMVGetDie2_0[i]
initial(IMVNotGetLive1[]) <- IMVNotGetLive1_0[i]
initial(IMVNotGetLive2[]) <- IMVNotGetLive2_0[i]
initial(IMVNotGetDie1[]) <- IMVNotGetDie1_0[i]
initial(IMVNotGetDie2[]) <- IMVNotGetDie2_0[i]
initial(IRec1[]) <- IRec1_0[i]
initial(IRec2[]) <- IRec2_0[i]
initial(R[]) <- R_0[i]
initial(D[]) <- D_0[i]
initial(D_get[]) <- 0
initial(D_not_get[]) <- 0

##Initial vectors
S_0[] <- user()
E1_0[] <- user()
E2_0[] <- user()
IMild_0[] <- user()
ICase1_0[] <- user()
ICase2_0[] <- user()
IOxGetLive1_0[] <- user()
IOxGetLive2_0[] <- user()
IOxGetDie1_0[] <- user()
IOxGetDie2_0[] <- user()
IOxNotGetLive1_0[] <- user()
IOxNotGetLive2_0[] <- user()
IOxNotGetDie1_0[] <- user()
IOxNotGetDie2_0[] <- user()
IMVGetLive1_0[] <- user()
IMVGetLive2_0[] <- user()
IMVGetDie1_0[] <- user()
IMVGetDie2_0[] <- user()
IMVNotGetLive1_0[] <- user()
IMVNotGetLive2_0[] <- user()
IMVNotGetDie1_0[] <- user()
IMVNotGetDie2_0[] <- user()
IRec1_0[] <- user()
IRec2_0[] <- user()
R_0[] <- user()
D_0[] <- user()

##Dimensions of the different "vectors" used
# For the State Variables
dim(S) <- N_age
dim(E1) <- N_age
dim(E2) <- N_age
dim(IMild) <- N_age
dim(ICase1) <- N_age
dim(ICase2) <- N_age
dim(cum_hosp_inc) <- N_age
dim(cum_ICU_inc) <- N_age
dim(IOxGetLive1) <- N_age
dim(IOxGetLive2) <- N_age
dim(IOxGetDie1) <- N_age
dim(IOxGetDie2) <- N_age
dim(IOxNotGetLive1) <- N_age
dim(IOxNotGetLive2) <- N_age
dim(IOxNotGetDie1) <- N_age
dim(IOxNotGetDie2) <- N_age
dim(IMVGetLive1) <- N_age
dim(IMVGetLive2) <- N_age
dim(IMVGetDie1) <- N_age
dim(IMVGetDie2) <- N_age
dim(IMVNotGetLive1) <- N_age
dim(IMVNotGetLive2) <- N_age
dim(IMVNotGetDie1) <- N_age
dim(IMVNotGetDie2) <- N_age
dim(IRec1) <- N_age
dim(IRec2) <- N_age
dim(R) <- N_age
dim(D) <- N_age
dim(D_get) <- N_age
dim(D_not_get) <- N_age

# For the Initial Values
dim(S_0) <- N_age
dim(E1_0) <- N_age
dim(E2_0) <- N_age
dim(IMild_0) <- N_age
dim(ICase1_0) <- N_age
dim(ICase2_0) <- N_age
dim(IOxGetLive1_0) <- N_age
dim(IOxGetLive2_0) <- N_age
dim(IOxGetDie1_0) <- N_age
dim(IOxGetDie2_0) <- N_age
dim(IOxNotGetLive1_0) <- N_age
dim(IOxNotGetLive2_0) <- N_age
dim(IOxNotGetDie1_0) <- N_age
dim(IOxNotGetDie2_0) <- N_age
dim(IMVGetLive1_0) <- N_age
dim(IMVGetLive2_0) <- N_age
dim(IMVGetDie1_0) <- N_age
dim(IMVGetDie2_0) <- N_age
dim(IMVNotGetLive1_0) <- N_age
dim(IMVNotGetLive2_0) <- N_age
dim(IMVNotGetDie1_0) <- N_age
dim(IMVNotGetDie2_0) <- N_age
dim(IRec1_0) <- N_age
dim(IRec2_0) <- N_age
dim(R_0) <- N_age
dim(D_0) <- N_age

# Severity Parameters
dim(prob_hosp) <- N_age
dim(prob_severe) <- N_age
dim(prob_non_severe_death_treatment) <- N_age
dim(prob_non_severe_death_no_treatment) <- N_age
dim(prob_severe_death_treatment) <- N_age
dim(prob_severe_death_no_treatment) <- N_age
dim(p_dist) <- N_age

dim(number_requiring_IMV) <- N_age
dim(number_get_IMV) <- N_age
dim(number_requiring_Ox) <- N_age
dim(number_get_Ox) <- N_age
dim(IMV_dist_weighting) <- N_age
dim(Ox_dist_weighting) <- N_age
