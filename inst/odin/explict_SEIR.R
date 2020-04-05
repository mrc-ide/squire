# Setting Number of Age Classes and the Time Step
dt <- user() # Specified timestep
time <- step * dt # Tracking actual time
output(time) <- TRUE
N_age <- user() # Number of age groups

## Core equations for transitions between compartments:
update(S[]) <- S[i] - n_S_E1[i]  # Susceptibles (1 comp)
update(E1[]) <- E1[i] + delta_E1[i]  # First of the latent infection compartments (2 comps)
update(E2[]) <- E2[i] + delta_E2[i]  # Second of the latent infection compartments (2 comps)
update(IMild[]) <- IMild[i] + delta_IMild[i]  # Mild infections (1 comp)
update(ICase1[]) <- ICase1[i] + delta_ICase1[i] # First of the compartments for infections that will require hospitalisation (2 comps)
update(ICase2[]) <- ICase2[i] + delta_ICase2[i] # Second of the compartments for infections that will require hospitalisation (2 comps)
update(IOx1[]) <- IOx1[i] + delta_IOx1[i] # First of the compartments for infections that will require oxygen (2 comps)
update(IOx2[]) <- IOx2[i] + delta_IOx2[i] # Second of the compartments for infections that will require oxygen (2 comps)
update(IMV1[]) <- IMV1[i] + delta_IMV1[i] # First of the compartments for infections that will require mechanical ventilation (2 comps)
update(IMV2[]) <- IMV2[i] + delta_IMV2[i] # Second of the compartments for infections that will require mechanical ventilation (2 comps)
update(IRec1[]) <- IRec1[i] + delta_IRec1[i] # First of the compartments for those recovering from ICU (2 comps)
update(IRec2[]) <- IRec2[i] + delta_IRec2[i] # Second of the compartments for those recovering from ICU (2 comps)
update(R[]) <- R[i] + delta_R[i] # Recovered
update(D[]) <- D[i] + delta_D[i] # Deaths

## Individual probabilities of transition:
p_S_E1[] <- 1 - exp(-lambda[i] * dt) # Infection - age dependent FOI based on mixing patterns
p_E1_E2 <- 1 - exp(-gamma_E * dt) # Progression through latent infection
p_E2_I <- 1 - exp(-gamma_E * dt) # Progression to onset of infectiousness. Number split between I_Mild and I_Case
p_IMild_R <- 1 - exp(-gamma_R * dt) # Recovery from mild disease
p_ICase1_ICase2 <- 1 - exp(-gamma_hosp * dt) # Delay between symptom onset and requiring hospitalisation
p_ICase2_Hosp <- 1 - exp(-gamma_hosp * dt) # Progression to requiring hospitalisation. Number split between I_Oxygen and I_MV
p_IOx1_IOx2 <- 1 - exp(-gamma_ox * dt) # Progression through requiring oxygen to either Recovery of Death
p_IOx2_RD <- 1 - exp(-gamma_ox * dt) # Number moving is split between R and D in age-specific manner
p_IMV1_IMV2 <- 1 - exp(-gamma_mv * dt) # Progression through requiring mechanical ventilation to either Recovery or Death
p_IMV2_RecD <- 1 - exp(-gamma_mv * dt) # Number moving is split between I_Rec and Death in age-specific manner
p_Rec1_Rec2 <- 1 - exp(-gamma_rec * dt) # Progression through recovery from ICU in hospital bed to eventual discharge (R)
p_Rec2_R <- 1 - exp(-gamma_rec * dt) # As above

## Draws from binomial distributions for numbers changing between compartments:
n_S_E1[] <- rbinom(S[i], p_S_E1[i]) # Number of newly infected individuals
n_E1_E2[] <- rbinom(E1[i], p_E1_E2) # Number progressing through latent compartments
n_E2_I[] <- rbinom(E2[i], p_E2_I) # Number of new symptom onsets
output(n_E2_I[]) <- TRUE

n_E2_ICase1[] <- round(n_E2_I[i] * prob_hosp[i]) # Proportion of the new symptom onsets that will require hospitalisation (note: haven't entered hospital yet, delay between onset and hospitalisation)
n_E2_IMild[] <- n_E2_I[i] - n_E2_ICase1[i] # 1 - Above, the rest of the infections, which we consider to be mild and not require hospitalisation

n_IMild_R[] <- rbinom(IMild[i], p_IMild_R) # Number of mild infections recovering

n_ICase1_ICase2[] <- rbinom(ICase1[i], p_ICase1_ICase2) # Number progressing through the onset but not hospitalised compartment
n_ICase2_Hosp[] <- rbinom(ICase2[i], p_ICase2_Hosp) # Number progressing to requiring hospitalisation
n_ICase2_IMV1[] <- round(n_ICase2_Hosp[i] * prob_severe[i]) # Number of hospitalisations that are going to require mechanical ventilation
n_ICase2_IOx1[] <- n_ICase2_Hosp[i] - n_ICase2_IMV1[i] # Number of hospitalisations that are going to require oxygen

n_IOx1_IOx2[] <- rbinom(IOx1[i], p_IOx1_IOx2) # Number progressing through the "require oxygen" compartment
n_IOx2_RD[] <- rbinom(IOx2[i], p_IOx2_RD) # Number leaving the "require oxygen" compartment
n_IOx2_D[] <- round(n_IOx2_RD[i] * prob_non_severe_death[i]) # Number leaving "require oxygen" compartment who die
n_IOx2_R[] <- n_IOx2_RD[i] - n_IOx2_D[i] # Number leaving "require oxygen" compartment who recover

n_IMV1_IMV2[] <- rbinom(IMV1[i], p_IMV1_IMV2) # Number progressing through the "require mechanical ventilation" compartment
n_IMV2_RecD[] <- rbinom(IMV2[i], p_IMV2_RecD) # Number leaving the "require mechanical ventilation" compartment

#Note: how to handle that duration of time in ICU differs dep on recov/death???
n_IMV2_D[] <- round(n_IMV2_RecD[i] * prob_severe_death[i]) # Number leaving "require mechanical ventilation" who die
n_IMV2_IRec1[] <- n_IMV2_RecD[i] - n_IMV2_D[i] # Number leaving "require mechanical ventilation" who progress to ICU recovery compartment
n_IRec1_IRec2[] <- rbinom(IRec1[i], p_Rec1_Rec2) # Number progressing through ICU recovery compartment
n_IRec2_R[] <- rbinom(IRec2[i], p_Rec2_R) # Number recovering completely

# Totalling up the flows in and out of each compartment
delta_E1[] <- n_S_E1[i] - n_E1_E2[i]
delta_E2[] <- n_E1_E2[i] - n_E2_I[i]
delta_IMild[] <- n_E2_IMild[i] - n_IMild_R[i]
delta_ICase1[] <- n_E2_ICase1[i] - n_ICase1_ICase2[i]
delta_ICase2[] <- n_ICase1_ICase2[i] - n_ICase2_Hosp[i]
delta_IOx1[] <- n_ICase2_IOx1[i] - n_IOx1_IOx2[i]
delta_IOx2[] <- n_IOx1_IOx2[i] - n_IOx2_RD[i]
delta_IMV1[] <- n_ICase2_IMV1[i] - n_IMV1_IMV2[i]
delta_IMV2[] <- n_IMV1_IMV2[i] - n_IMV2_RecD[i]
delta_IRec1[] <- n_IMV2_IRec1[i] - n_IRec1_IRec2[i]
delta_IRec2[] <- n_IRec1_IRec2[i] - n_IRec2_R[i]
delta_R[] <- n_IRec2_R[i] + n_IOx2_R[i] + n_IMild_R[i]
delta_D[] <- n_IOx2_D[i] + n_IMV2_D[i]

##Compute the Force of infection

# Interpolation for Mixing Matrix
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

## Model Parameters
gamma_E <- user() # rate of progression through latent infection
gamma_R <- user() # rate of progression from mild infection to recovery
gamma_hosp <- user() # rate of progression from symptom onset to requiring hospitalisation
gamma_ox <- user() # rate of progression through requiring oxygen compartment
gamma_mv <- user() # rate of progression through requiring mechanical ventilation compartment
gamma_rec <- user() # rate of progression through post-ICU recovery compartment

prob_hosp[] <- user() # probability of requiring hospitalisation by age
prob_severe[] <- user() # probability of severe disease (requiring mechanical ventilation) by age
prob_non_severe_death[] <- user() # probability of dying from non-severe disease (i.e. requiring oxygen but not mechanical ventilation) by age
prob_severe_death[] <- user() # probability of dying from severe disease (i.e. requiring mechanical ventilation) by age

dim(prob_hosp) <- N_age
dim(prob_severe) <- N_age
dim(prob_non_severe_death) <- N_age
dim(prob_severe_death) <- N_age

## Initial states:
initial(S[]) <- S_0[i]
initial(E1[]) <- E1_0[i]
initial(E2[]) <- E2_0[i]
initial(IMild[]) <- IMild_0[i]
initial(ICase1[]) <- ICase1_0[i]
initial(ICase2[]) <- ICase2_0[i]
initial(IOx1[]) <- IOx1_0[i]
initial(IOx2[]) <- IOx2_0[i]
initial(IMV1[]) <- IMV1_0[i]
initial(IMV2[]) <- IMV2_0[i]
initial(IRec1[]) <- IRec1_0[i]
initial(IRec2[]) <- IRec2_0[i]
initial(R[]) <- R_0[i]
initial(D[]) <- D_0[i]

##Initial vectors
S_0[] <- user()
E1_0[] <- user()
E2_0[] <- user()
IMild_0[] <- user()
ICase1_0[] <- user()
ICase2_0[] <- user()
IOx1_0[] <- user()
IOx2_0[] <- user()
IMV1_0[] <- user()
IMV2_0[] <- user()
IRec1_0[] <- user()
IRec2_0[] <- user()
R_0[] <- user()
D_0[] <- user()

##Dimensions of the different "vectors" used
# For the State Variables
dim(S) <- N_age
dim(E1) <- c(N_age)
dim(E2) <- c(N_age)
dim(IMild) <- c(N_age)
dim(ICase1) <- c(N_age)
dim(ICase2) <- c(N_age)
dim(IOx1) <- c(N_age)
dim(IOx2) <- c(N_age)
dim(IMV1) <- c(N_age)
dim(IMV2) <- c(N_age)
dim(IRec1) <- c(N_age)
dim(IRec2) <- c(N_age)
dim(R) <- c(N_age)
dim(D) <- c(N_age)

# For the Initial Values
dim(S_0) <- N_age
dim(E1_0) <- c(N_age)
dim(E2_0) <- c(N_age)
dim(IMild_0) <- c(N_age)
dim(ICase1_0) <- c(N_age)
dim(ICase2_0) <- c(N_age)
dim(IOx1_0) <- c(N_age)
dim(IOx2_0) <- c(N_age)
dim(IMV1_0) <- c(N_age)
dim(IMV2_0) <- c(N_age)
dim(IRec1_0) <- c(N_age)
dim(IRec2_0) <- c(N_age)
dim(R_0) <- c(N_age)
dim(D_0) <- c(N_age)

# For the Flows Between State Variables
dim(delta_E1) <- N_age
dim(delta_E2) <- N_age
dim(delta_IMild) <- N_age
dim(delta_ICase1) <- N_age
dim(delta_ICase2) <- N_age
dim(delta_IOx1) <- N_age
dim(delta_IOx2) <- N_age
dim(delta_IMV1) <- N_age
dim(delta_IMV2) <- N_age
dim(delta_IRec1) <- N_age
dim(delta_IRec2) <- N_age
dim(delta_R) <- N_age
dim(delta_D) <- N_age

# For the Number of People Moving In and Out of Compartments
dim(n_E1_E2) <- N_age
dim(n_E2_I) <- N_age
dim(n_E2_ICase1) <- N_age
dim(n_E2_IMild) <- N_age
dim(n_IMild_R) <- N_age
dim(n_ICase1_ICase2) <- N_age
dim(n_ICase2_Hosp) <- N_age
dim(n_ICase2_IMV1) <- N_age
dim(n_ICase2_IOx1) <- N_age
dim(n_IOx1_IOx2) <- N_age
dim(n_IOx2_RD) <- N_age
dim(n_IOx2_D) <- N_age
dim(n_IOx2_R) <- N_age
dim(n_IMV1_IMV2) <- N_age
dim(n_IMV2_RecD) <- N_age
dim(n_IMV2_D) <- N_age
dim(n_IMV2_IRec1) <- N_age
dim(n_IRec1_IRec2) <- N_age
dim(n_IRec2_R) <- N_age

# Related to Calculating Age-Structured Force of Infection
dim(p_S_E1) <- N_age
dim(n_S_E1) <- N_age
dim(lambda) <- N_age
dim(s_ij) <- c(N_age,N_age)
dim(temp) <- N_age
