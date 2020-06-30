################################################################################
### Vaccine model: deterministic ###############################################
################################################################################

### S: susceptible #############################################################
deriv(S[]) <- -S[i] * lambda[i] + (gamma_R * R2[i]) - (vr * vaccination_target[i] * S[i]) + (gamma_V * V2[i])
################################################################################

### SVac (SVac1 & SVac2): Vaccinated but still susceptible #####################
gamma_SVac <- user() # Rate of progression through vaccinnated but susceptible compartment
deriv(SVac1[]) <- (vr * vaccination_target[i] * S[i]) - gamma_SVac * SVac1[i] - SVac1[i] * lambda[i]
deriv(SVac2[]) <- gamma_SVac * SVac1[i] - SVac2[i] * lambda[i] - gamma_SVac * SVac2[i]
output(SVac[]) <- SVac1[i] + SVac2[i]
################################################################################

### V (V1 & v2): Vaccinated ####################################################
gamma_V <- user() # Rate of progression through vaccine compartment (loss of vaccine acquired immunity)
deriv(V1[]) <- (gamma_SVac * SVac2[i]) + (gamma_RVac * RVac2[i]) - (gamma_V * V1[i]) - (lambda[i] * V1[i] * vaccine_efficacy_infection[i])
deriv(V2[]) <- (gamma_V * V1[i]) - (gamma_V * V2[i]) - (lambda[i] * V2[i] * vaccine_efficacy_infection[i])
output(V[]) <- V1[i] + V2[i]
deriv(vaccinated[]) <- (vr * vaccination_target[i] * S[i]) + (vr * vaccination_target[i] * R2[i]) + (vr * vaccination_target[i] * R1[i])
vaccinatedlag[] <- delay(vaccinated[i], dt)
output(vaccines[]) <- vaccinated[i] - vaccinatedlag[i]
################################################################################

### E (E1 & E2): Latent ########################################################
gamma_E <- user() # rate of progression through latent infection
deriv(E1[]) <- lambda[i] * S[i] - gamma_E * E1[i]
deriv(E2[]) <- gamma_E * E1[i] - gamma_E * E2[i]
output(E[]) <- E1[i] + E2[i]
################################################################################

### Evac (E1vac & E2vac): Latent vaccinated ####################################
prob_hosp_vaccine[] <- user() # probability of requiring hospitalisation by age, given vaccinated
deriv(EVac1[]) <- (SVac1[i] * lambda[i]) + (SVac2[i] * lambda[i]) + (lambda[i] * V1[i] * vaccine_efficacy_infection[i]) + (lambda[i] * V2[i] * vaccine_efficacy_infection[i]) - (gamma_E * EVac1[i])
deriv(EVac2[]) <- (gamma_E * EVac1[i]) - (gamma_E * EVac2[i])
output(EVac[]) <- EVac1[i] + EVac2[i]
################################################################################

### IMild: Unhospitalised infection ############################################
prob_hosp[] <- user() # probability of requiring hospitalisation by age
gamma_IMild <- user() # rate of progression from mild infection to recovery
deriv(IMild[]) <- (gamma_E * E2[i] * (1 - prob_hosp[i])) + (gamma_E * EVac2[i] * (1 - prob_hosp_vaccine[i])) - gamma_IMild * IMild[i]
################################################################################

### ICase (ICase1 & ICase2): To-be hospitalised infection ######################
gamma_ICase <- user() # rate of progression from symptom onset to requiring hospitalisation
deriv(ICase1[]) <- (gamma_E * E2[i] * prob_hosp[i]) +  (gamma_E * EVac2[i] * prob_hosp_vaccine[i]) - gamma_ICase * ICase1[i]
deriv(ICase2[]) <- gamma_ICase * ICase1[i] - gamma_ICase * ICase2[i]
output(ICase[]) <- ICase1[i] + ICase2[i]
################################################################################

### IOxGetLive (IOxGetLive1 & IOxGetLive2): Get oxygen, go on to survive #######
gamma_get_ox_survive <- user() # rate of progression through requiring oxygen compartment conditional on getting oxygen and surviving
deriv(IOxGetLive1[]) <- (1 - prob_non_severe_death_treatment[i]) * number_get_Ox[i] - gamma_get_ox_survive * IOxGetLive1[i]
deriv(IOxGetLive2[]) <- gamma_get_ox_survive * IOxGetLive1[i] -  gamma_get_ox_survive * IOxGetLive2[i]
################################################################################

### IOxGetDie (IOxGetDie1 & IOxGetDie2): Get oxygen go on to die ###############
gamma_get_ox_die <- user() # rate of progression through requiring oxygen compartment conditional on getting oxygen and dying
deriv(IOxGetDie1[]) <- (prob_non_severe_death_treatment[i] * number_get_Ox[i]) - gamma_get_ox_die * IOxGetDie1[i]
deriv(IOxGetDie2[]) <- gamma_get_ox_die * IOxGetDie1[i] -  gamma_get_ox_die * IOxGetDie2[i]
################################################################################

### IOxNotGetLive (IOxNotGetLive1 & IOxNotGetLive2): Do not get oxygen, go on to survive #######
gamma_not_get_ox_survive <- user() # rate of progression through requiring oxygen compartment conditional on not getting oxygen and surviving
deriv(IOxNotGetLive1[]) <- (number_requiring_Ox[i] - number_get_Ox[i]) * (1 - prob_non_severe_death_no_treatment[i]) - gamma_not_get_ox_survive * IOxNotGetLive1[i]
deriv(IOxNotGetLive2[]) <- gamma_not_get_ox_survive * IOxNotGetLive1[i] -  gamma_not_get_ox_survive * IOxNotGetLive2[i]
################################################################################

### IOxNotGetDie (IOxNotGetDie1 & IOxNotGetDie2): Do not get oxygen, go on to die #######
gamma_not_get_ox_die <- user() # rate of progression through requiring oxygen compartment conditional on not getting oxygen and dying
deriv(IOxNotGetDie1[]) <- (number_requiring_Ox[i] - number_get_Ox[i]) * prob_non_severe_death_no_treatment[i] - gamma_not_get_ox_die * IOxNotGetDie1[i]
deriv(IOxNotGetDie2[]) <- gamma_not_get_ox_die * IOxNotGetDie1[i] -  gamma_not_get_ox_die * IOxNotGetDie2[i]
################################################################################

### IMVGetLive (IMVGetLive1 & IMVGetLive2): Get mechanical ventilation, go on to live ########
gamma_get_mv_survive <- user() # rate of progression through requiring mechanical ventilation compartment conditional on getting ventilation and surviving
deriv(IMVGetLive1[]) <- (1 - prob_severe_death_treatment[i]) * number_get_IMV[i] - gamma_get_mv_survive * IMVGetLive1[i]
deriv(IMVGetLive2[]) <- gamma_get_mv_survive * IMVGetLive1[i] -  gamma_get_mv_survive * IMVGetLive2[i]
################################################################################

### IMVGetDie (IMVGetDie1 & IMVGetDie2): Get mechanical ventilation, go on to die ########
prob_severe[] <- user() # probability of severe disease (requiring mechanical ventilation) by age
gamma_get_mv_die <- user() # rate of progression through requiring mechanical ventilation compartment conditional on getting ventilation and dying
deriv(IMVGetDie1[]) <- (prob_severe_death_treatment[i] * number_get_IMV[i]) - gamma_get_mv_die * IMVGetDie1[i]
deriv(IMVGetDie2[]) <- gamma_get_mv_die * IMVGetDie1[i] -  gamma_get_mv_die * IMVGetDie2[i]
################################################################################

### IMVNotGetLive (IMVNotGetLive1 & IMVNotGetLive2): Do no get mechanical ventilation, go on to live ########
gamma_not_get_mv_survive <- user() # rate of progression through requiring mechanical ventilation compartment conditional on not getting ventilation and surviving
deriv(IMVNotGetLive1[]) <- (number_requiring_IMV[i] - number_get_IMV[i]) * (1 - prob_severe_death_no_treatment[i]) - gamma_not_get_mv_survive * IMVNotGetLive1[i]
deriv(IMVNotGetLive2[]) <- gamma_not_get_mv_survive * IMVNotGetLive1[i] -  gamma_not_get_mv_survive * IMVNotGetLive2[i]
################################################################################

### IMVNotGetDie (IMVNotGetDie1 & IMVNotGetDie2): Do no get mechanical ventilation, go on to die ########
gamma_not_get_mv_die <- user() # rate of progression through requiring mechanical ventilation compartment conditional on not getting ventilation and dying
deriv(IMVNotGetDie1[]) <- (number_requiring_IMV[i] - number_get_IMV[i]) * prob_severe_death_no_treatment[i] - gamma_not_get_mv_die * IMVNotGetDie1[i]
deriv(IMVNotGetDie2[]) <- gamma_not_get_mv_die * IMVNotGetDie1[i] -  gamma_not_get_mv_die * IMVNotGetDie2[i]
################################################################################

### IRec (IRec1 & IRec2): Recovering from ICU ##################################
gamma_rec <- user() # rate of progression through post-ICU recovery compartment
deriv(IRec1[]) <- gamma_get_mv_survive * IMVGetLive2[i] - gamma_rec * IRec1[i]
deriv(IRec2[]) <- gamma_rec * IRec1[i] - gamma_rec * IRec2[i]
output(IRec[]) <- IRec1[i] + IRec2[i]
################################################################################

### D: Dead ####################################################################
deriv(D[]) <- (gamma_get_ox_die * IOxGetDie2[i]) + (gamma_not_get_ox_die * IOxNotGetDie2[i]) + (gamma_get_mv_die * IMVGetDie2[i]) + (gamma_not_get_mv_die * IMVNotGetDie2[i])
################################################################################

### R: (R1 & R2): Recovered ####################################################
gamma_R <- user() # rate of progression through recovered compartment (loss of naturally acquired immunity)
deriv(R1[]) <- (gamma_rec * IRec2[i]) + (gamma_IMild * IMild[i]) + (gamma_get_ox_survive * IOxGetLive2[i]) + (gamma_not_get_ox_survive * IOxNotGetLive2[i]) + (gamma_not_get_mv_survive * IMVNotGetLive2[i]) - (gamma_R * R1[i]) - (vr * vaccination_target[i] * R1[i])
deriv(R2[]) <- (gamma_R * R1[i]) - (gamma_R * R2[i]) - (vr * vaccination_target[i] * R2[i])
output(R[]) <- R1[i] + R2[i]
################################################################################

### RVac (RVac1 & RVac2): Recovered, vaccinated but nor vaccine-protected ######
gamma_RVac <- user() # Rate of progression through vaccinnated but susceptible compartment
deriv(RVac1[]) <- (vr * vaccination_target[i] * R1[i]) + (vr * vaccination_target[i] * R2[i]) - (gamma_RVac * RVac1[i])
deriv(RVac2[]) <- gamma_RVac * RVac1[i] - gamma_RVac * RVac2[i]
output(RVac[]) <- RVac1[i] + RVac2[i]
################################################################################

################################################################################
### Summary outputs ############################################################
################################################################################

################################################################################
################################################################################

################################################################################
### Hospital and ICU capacity ##################################################
################################################################################
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

prob_non_severe_death_treatment[] <- user() # probability of dying from non-severe disease (i.e. requiring oxygen but not mechanical ventilation) by age given you receive appropriate treatment (proxy here is whether a general hospital bed is available)
prob_non_severe_death_no_treatment[] <- user() # probability of dying from non-severe disease (i.e. requiring oxygen but not mechanical ventilation) by age given you do NOT receive appropriate treatment (proxy here is whether a general hospital bed is available)
prob_severe_death_treatment[] <- user() # probability of dying from severe disease (i.e. requiring mechanical ventilation) by age given you receive appropriate treatment (proxy here is whether an ICU bed is available)
prob_severe_death_no_treatment[] <- user() # probability of dying from severe disease (i.e. requiring mechanical ventilation) by age given you do NOT receive appropriate treatment (proxy here is whether an ICU bed is available)
p_dist[] <- user() # distributing infections in given age class to available hosp/ICU beds (make all equal to make it random and not related to age)

# Infections Requiring Oxygen (a general Hosptial Bed)
hosp_occ <- sum(IOxGetLive1) + sum(IOxGetLive2) + sum(IOxGetDie1) + sum(IOxGetDie2) + sum(IRec1) + sum(IRec2) # Summing number of infections in compartments that use general hospital beds
current_free_hosp <- hosp_bed_capacity - hosp_occ + gamma_get_ox_die*sum(IOxGetDie2) + gamma_get_ox_survive * sum(IOxGetLive2) + gamma_rec * sum(IRec2) - gamma_get_mv_survive * sum(IMVGetLive2)
number_requiring_Ox[] <- gamma_ICase * ICase2[i] * (1 - prob_severe[i]) # NOTE THIS IS DIFF IN SYNTAX FROM STOCHSTIC VERSION WHERE WE SUBTRACT THE NUMBER GETTING IMV - MIGHT BE BETTER FROM A ROUNDING ERROR PERSPECITVE
total_number_requiring_ox <- sum(number_requiring_Ox)
total_number_get_hosp <- if (current_free_hosp <= 0) 0 else (if(current_free_hosp - total_number_requiring_ox >= 0) total_number_requiring_ox else(current_free_hosp)) # Working out the number of new hospital bed requiring infections that get a bed
Ox_dist_weighting[] <- number_requiring_Ox[i] * p_dist[i]
number_get_Ox[] <- if (total_number_requiring_ox == 0) 0 else Ox_dist_weighting[i]/sum(Ox_dist_weighting) * total_number_get_hosp

# Infections Requiring Mechanical Ventilation (an ICU Bed)
ICU_occ <- sum(IMVGetLive1) + sum(IMVGetLive2) + sum(IMVGetDie1) + sum(IMVGetDie2) # Summing number of infections in compartments that use ICU beds
current_free_ICUs <- ICU_bed_capacity - ICU_occ + gamma_get_mv_survive *sum(IMVGetLive2) + gamma_get_mv_die *sum(IMVGetDie2)
number_requiring_IMV[] <- gamma_ICase * ICase2[i] * prob_severe[i]
total_number_requiring_IMV <- sum(number_requiring_IMV)
total_number_get_IMV <- if(current_free_ICUs <= 0) 0 else(if(current_free_ICUs - total_number_requiring_IMV >= 0) total_number_requiring_IMV else(current_free_ICUs)) # Working out the number of new ICU requiring infections that get a bed
IMV_dist_weighting[] <- number_requiring_IMV[i] * p_dist[i]
number_get_IMV[] <- if (total_number_requiring_IMV == 0) 0 else IMV_dist_weighting[i]/sum(IMV_dist_weighting) * total_number_get_IMV

################################################################################
################################################################################

################################################################################
### Vaccination capacity #######################################################
################################################################################
# Vaccination
vaccination_target[] <- user() # 0/1 index of targeted age groups
vaccine_efficacy_infection[] <- user() # Reduction in lambda for vaccinated individuals by age

# Interpolation of vaccination rate over time
mv <- interpolate(tt_vaccine, max_vaccine, "constant")
tt_vaccine[] <- user()
max_vaccine[] <- user()
dim(tt_vaccine) <- user()
dim(max_vaccine) <- length(tt_vaccine)

vr_temp[] <- S[i] * vaccination_target[i] + R1[i] * vaccination_target[i] + R2[i] * vaccination_target[i]
dim(vr_temp) <- N_age
vr <- mv / sum(vr_temp) # Vaccination rate to achieve capacity given number in S or R
################################################################################
################################################################################

################################################################################
### FOI and contact matrix #####################################################
################################################################################
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
################################################################################
################################################################################

################################################################################
### Output #####################################################################
################################################################################
# Hospital occupancy and demand
output(hospital_occupancy[]) <- IOxGetLive1[i] + IOxGetLive2[i] + IOxGetDie1[i] + IOxGetDie2[i] +  IRec1[i] + IRec2[i]
output(ICU_occupancy[]) <- IMVGetLive1[i] +  IMVGetLive2[i] + IMVGetDie1[i] + IMVGetDie2[i]
output(hospital_demand[]) <- IOxGetLive1[i] + IOxGetLive2[i] + IOxGetDie1[i] + IOxGetDie2[i] +  IRec1[i] + IRec2[i] + IOxNotGetLive1[i] + IOxNotGetLive2[i] + IOxNotGetDie1[i] + IOxNotGetDie2[i]
output(ICU_demand[]) <- IMVGetLive1[i] +  IMVGetLive2[i] + IMVGetDie1[i] + IMVGetDie2[i] + IMVNotGetLive1[i] +  IMVNotGetLive2[i] + IMVNotGetDie1[i] + IMVNotGetDie2[i]

# Number in hospital or ICU compartments
output(IICU[]) <- IMVGetLive1[i] + IMVGetLive2[i] + IMVGetDie1[i] + IMVGetDie2[i] + IMVNotGetLive1[i] + IMVNotGetLive2[i] + IMVNotGetDie1[i] + IMVNotGetDie2[i]
output(IHospital[]) <- IOxGetLive1[i] + IOxGetLive2[i] + IOxGetDie1[i] + IOxGetDie2[i] + IOxNotGetLive1[i] + IOxNotGetLive2[i] + IOxNotGetDie1[i] + IOxNotGetDie2[i]

# Deaths and infections
dt <- user() # Specified timestep
Dlag[] <- delay(D[i], dt)
output(deaths[]) <- D[i] - Dlag[i]
deriv(I[]) <- lambda[i] * S[i] + (lambda[i] * V1[i] * vaccine_efficacy_infection[i]) + (lambda[i] * V2[i] * vaccine_efficacy_infection[i])
Ilag[] <- delay(I[i], dt)
output(infections[]) <- I[i] - Ilag[i]

# Population size
output(N[]) <- S[i] + E1[i] + E2[i] + EVac1[i] + EVac2[i] + IMild[i] + ICase1[i] + ICase2[i] +
  SVac1[i] + SVac2[i] + RVac1[i] + RVac2[i] +
  IMVGetLive1[i] + IMVGetLive2[i] +
  IMVGetDie1[i] + IMVGetDie2[i] + IMVNotGetLive1[i] + IMVNotGetLive2[i] + IMVNotGetDie1[i] + IMVNotGetDie2[i] +
  IOxGetLive1[i] + IOxGetLive2[i] + IOxGetDie1[i] + IOxGetDie2[i] + IOxNotGetLive1[i] + IOxNotGetLive2[i] +
  IOxNotGetDie1[i] + IOxNotGetDie2[i] +
  IRec1[i] + IRec2[i] +
  R1[i] + R2[i] + D[i] + V1[i] + V2[i]
#########################################

################################################################################
################################################################################

################################################################################
### Input ######################################################################
################################################################################
N_age <- user()
time <- t
output(time) <- TRUE

## Initial states:
initial(S[]) <- S_0[i]
initial(E1[]) <- E1_0[i]
initial(E2[]) <- E2_0[i]
initial(EVac1[]) <- EVac1_0[i]
initial(EVac2[]) <- EVac2_0[i]
initial(IMild[]) <- IMild_0[i]
initial(ICase1[]) <- ICase1_0[i]
initial(ICase2[]) <- ICase2_0[i]
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
initial(R1[]) <- R1_0[i]
initial(R2[]) <- R2_0[i]
initial(D[]) <- D_0[i]
initial(V1[]) <- V1_0[i]
initial(V2[]) <- V2_0[i]
initial(vaccinated[]) <- 0
initial(I[]) <- 0
initial(RVac1[]) <- RVac1_0[i]
initial(RVac2[]) <- RVac2_0[i]
initial(SVac1[]) <- SVac1_0[i]
initial(SVac2[]) <- SVac2_0[i]

##Initial vectors
S_0[] <- user()
E1_0[] <- user()
E2_0[] <- user()
EVac1_0[] <- user()
EVac2_0[] <- user()
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
R1_0[] <- user()
R2_0[] <- user()
D_0[] <- user()
V1_0[] <- user()
V2_0[] <- user()
RVac1_0[] <- user()
RVac2_0[] <- user()
SVac1_0[] <- user()
SVac2_0[] <- user()

##Dimensions of the different "vectors" used
# For the State Variables
dim(S) <- N_age
dim(E1) <- N_age
dim(E2) <- N_age
dim(E) <- N_age
dim(EVac1) <- N_age
dim(EVac2) <- N_age
dim(EVac) <- N_age
dim(IMild) <- N_age
dim(ICase1) <- N_age
dim(ICase2) <- N_age
dim(ICase) <- N_age
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
dim(IRec) <- N_age
dim(R1) <- N_age
dim(R2) <- N_age
dim(R) <- N_age
dim(D) <- N_age
dim(V1) <- N_age
dim(V2) <- N_age
dim(V) <- N_age
dim(IICU) <- N_age
dim(IHospital) <- N_age
dim(hospital_occupancy) <- N_age
dim(hospital_demand) <- N_age
dim(ICU_occupancy) <- N_age
dim(ICU_demand) <- N_age
dim(N) <- N_age
dim(vaccinated) <- N_age
dim(vaccinatedlag) <- N_age
dim(vaccines) <- N_age
dim(I) <- N_age
dim(Ilag) <- N_age
dim(infections) <- N_age
dim(Dlag) <- N_age
dim(deaths) <- N_age
dim(SVac1) <- N_age
dim(SVac2) <- N_age
dim(SVac) <- N_age
dim(RVac1) <- N_age
dim(RVac2) <- N_age
dim(RVac) <- N_age
dim(temp) <- N_age
dim(s_ij) <- c(N_age, N_age)
dim(lambda) <- N_age


# For the Initial Values
dim(S_0) <- N_age
dim(E1_0) <- N_age
dim(E2_0) <- N_age
dim(EVac1_0) <- N_age
dim(EVac2_0) <- N_age
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
dim(R1_0) <- N_age
dim(R2_0) <- N_age
dim(D_0) <- N_age
dim(V1_0) <- N_age
dim(V2_0) <- N_age
dim(RVac1_0) <- N_age
dim(RVac2_0) <- N_age
dim(SVac1_0) <- N_age
dim(SVac2_0) <- N_age


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

# Vaccination parameters
dim(vaccination_target) <- N_age
dim(vaccine_efficacy_infection) <- N_age
dim(prob_hosp_vaccine) <- N_age


