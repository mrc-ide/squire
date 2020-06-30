################################################################################
### Vaccine model: stochatsic ##################################################
################################################################################


### S: susceptible #############################################################
delta_S[] <- n_R2_S[i] - n_S_E1_SVac1[i] + n_V2_S[i]

update(S[]) <- S[i] + delta_S[i]  # Susceptibles (1 compartment)

p_leave_S[] <- 1 - exp(-(lambda[i] + vr * vaccination_target[i]) * dt) # Infection - age dependent FOI based on mixing patterns
p_E[] <- if(lambda[i] > 0) lambda[i] / (lambda[i] + vr * vaccination_target[i]) else 0 # Probability infection

n_S_E1_SVac1[] <- rbinom(S[i], p_leave_S[i]) # Leaving S
n_S_E1[] <- if(n_S_E1_SVac1[i] > 0) rbinom(n_S_E1_SVac1[i], p_E[i]) else 0 # S->E1
n_S_SVac1[] <- n_S_E1_SVac1[i] - n_S_E1[i] # S->V1
################################################################################

### SVac (SVac1 & SVac2): Vaccinated but still susceptible #####################
delta_SVac1[] <- n_S_SVac1[i] - n_SVac1_EVac1_SVac2[i]
delta_SVac2[] <- n_SVac1_SVac2[i] - n_SVac2_EVac1_V1[i]

update(SVac1[]) <- SVac1[i] + delta_SVac1[i]
update(SVac2[]) <- SVac2[i] + delta_SVac2[i]
SVac[] <- SVac1[i] + SVac2[i]

gamma_SVac <- user() # Rate of progression through vaccinnated but susceptible compartment
p_leave_SVac[] <- 1 - exp(-(lambda[i] + gamma_SVac) * dt)
p_SVac1_SVac2[] <- gamma_SVac / (lambda[i] + gamma_SVac)
p_SVac2_V1[] <- gamma_SVac / (lambda[i] + gamma_SVac)

n_SVac1_EVac1_SVac2[] <- rbinom(SVac1[i], p_leave_SVac[i]) # Leaving SVac1
n_SVac1_SVac2[] <- if(n_SVac1_EVac1_SVac2[i] > 0) rbinom(n_SVac1_EVac1_SVac2[i], p_SVac1_SVac2[i]) else 0 # SVac1->SVac2
n_SVac1_EVac1[] <- n_SVac1_EVac1_SVac2[i] - n_SVac1_SVac2[i] # SVac1->E1

n_SVac2_EVac1_V1[] <- rbinom(SVac2[i], p_leave_SVac[i]) # Leaving SVac2
n_SVac2_V1[] <- if(n_SVac2_EVac1_V1[i] > 0) rbinom(n_SVac2_EVac1_V1[i], p_SVac2_V1[i]) else 0 # SVac2->V1
n_SVac2_EVac1[] <- n_SVac2_EVac1_V1[i] - n_SVac2_V1[i] # SVac2->E1
################################################################################

### V (V1 & v2): Vaccinated ####################################################
delta_V1[] <- n_RVac2_V1[i] + n_SVac2_V1[i] - n_V1_V2_Evac[i]
delta_V2[] <- n_V1_V2[i] - n_V2_S_Evac[i]

update(V1[]) <- V1[i] + delta_V1[i] # First of the vaccinated compartments (2 comps)
update(V2[]) <- V2[i] + delta_V2[i] # Second of the vaccinated compartments (2 comps)
V[] <- V1[i] + V2[i] # Summary V
vaccines[] <- n_R1_RVac1[i] + n_R2_RVac1[i] + n_S_SVac1[i] # Summary number of people vaccinated

gamma_V <- user() # Rate of progression through vaccine compartment (loss of vaccine acquired immunity)

p_leave_V[] <- 1 - exp(-(gamma_V + lambda[i] * vaccine_efficacy_infection[i]) * dt) # Probability leaving V
p_V[] <- gamma_V / (gamma_V + lambda[i] * vaccine_efficacy_infection[i]) # Probability of progression through V

n_V1_V2_Evac[] <- rbinom(V1[i], p_leave_V[i]) # Number leaving V1
n_V1_V2[] <- if (n_V1_V2_Evac[i] > 0) rbinom(n_V1_V2_Evac[i], p_V[i]) else 0 # V1->V2
n_V1_Evac[] <- n_V1_V2_Evac[i] - n_V1_V2[i] # V1->Evac

n_V2_S_Evac[] <- rbinom(V2[i], p_leave_V[i]) # Number leaving V2
n_V2_S[] <- if (n_V2_S_Evac[i] > 0) rbinom(n_V2_S_Evac[i], p_V[i]) else 0 # V2->S
n_V2_Evac[] <- n_V2_S_Evac[i] - n_V2_S[i] # V2->Evac
################################################################################

### E (E1 & E2): Latent ########################################################
delta_E1[] <- n_S_E1[i] - n_E1_E2[i]
delta_E2[] <- n_E1_E2[i] - n_E2_I[i]

update(E1[]) <- E1[i] + delta_E1[i]  # First of the latent infection compartments (2 comps)
update(E2[]) <- E2[i] + delta_E2[i]  # Second of the latent infection compartments (2 comps)
E[] <- E1[i] + E2[i] # Summary of E

gamma_E <- user() # rate of progression through latent infection
p_E1_E2 <- 1 - exp(-gamma_E * dt) # Progression through latent infection
p_E2_I <- 1 - exp(-gamma_E * dt) # Progression to onset of infectiousness. Number split between I_Mild and I_Case
prob_hosp[] <- user() # probability of requiring hospitalisation by age

n_E1_E2[] <- rbinom(E1[i], p_E1_E2) # E1->E2
n_E2_I[] <- rbinom(E2[i], p_E2_I) # E2->I
n_E2_ICase1[] <- rbinom(n_E2_I[i], prob_hosp[i]) # E2->ICase
n_E2_IMild[] <- n_E2_I[i] - n_E2_ICase1[i] # E2->IMild
################################################################################

### Evac (E1vac & E2vac): Latent vaccinated ####################################
delta_EVac1[] <- n_V1_Evac[i] + n_V2_Evac[i] + n_SVac1_EVac1[i] + n_SVac2_EVac1[i] - n_EVac1_EVac2[i]
delta_EVac2[] <- n_EVac1_EVac2[i] - n_EVac2_I[i]

update(EVac1[]) <- EVac1[i] + delta_EVac1[i]  # First of the latent vaccinated infection compartments (2 comps)
update(EVac2[]) <- EVac2[i] + delta_EVac2[i]  # Second of the latent vaccinated infection compartments (2 comps)
EVac[] <- EVac1[i] + EVac2[i] # Summary of Evac

p_EVac1_EVac2 <- 1 - exp(-gamma_E * dt) # Progression through latent infection
p_EVac2_I <- 1 - exp(-gamma_E * dt) # Progression to onset of infectiousness. Number split between I_Mild and I_Case
prob_hosp_vaccine[] <- user() # probability of requiring hospitalisation by age, given vaccinated

n_EVac1_EVac2[] <- rbinom(EVac1[i], p_EVac1_EVac2) # E1vac->E2vac
n_EVac2_I[] <- rbinom(EVac2[i], p_EVac2_I) # E2vac->I
n_EVac2_ICase[] <- if(n_EVac2_I[i] > 0) rbinom(n_EVac2_I[i], prob_hosp_vaccine[i]) else 0 # E2vac->ICase
n_EVac2_IMild[] <- n_EVac2_I[i] - n_EVac2_ICase[i] # E2->IMild
################################################################################

### IMild: Unhospitalised infection ############################################
delta_IMild[] <- n_E2_IMild[i] - n_IMild_R[i] + n_EVac2_IMild[i]

update(IMild[]) <- IMild[i] + delta_IMild[i]  # Mild infections (1 comp)

gamma_IMild <- user() # rate of progression from mild infection to recovery
p_IMild_R <- 1 - exp(-gamma_IMild * dt) # Recovery from mild disease

n_IMild_R[] <- rbinom(IMild[i], p_IMild_R) # IMild->R
################################################################################

### ICase (ICase1 & ICase2): To-be hospitalised infection ######################
delta_ICase1[] <- n_E2_ICase1[i] - n_ICase1_ICase2[i] + n_EVac2_ICase[i]
delta_ICase2[] <- n_ICase1_ICase2[i] - n_ICase2_Hosp[i]

update(ICase1[]) <- ICase1[i] + delta_ICase1[i] # First of the compartments for infections that will require hospitalisation (2 comps)
update(ICase2[]) <- ICase2[i] + delta_ICase2[i] # Second of the compartments for infections that will require hospitalisation (2 comps)
ICase[] <- ICase1[i] + ICase2[i] # Summary of ICase

gamma_ICase <- user() # rate of progression from symptom onset to requiring hospitalisation
p_ICase1_ICase2 <- 1 - exp(-gamma_ICase * dt) # Delay between symptom onset and requiring hospitalisation
p_ICase2_Hosp <- 1 - exp(-gamma_ICase * dt) # Progression to requiring hospitalisation. Number split between I_Oxygen and I_MV

n_ICase1_ICase2[] <- rbinom(ICase1[i], p_ICase1_ICase2) # ICase1->ICase2
n_ICase2_Hosp[] <- rbinom(ICase2[i], p_ICase2_Hosp) # ICase2->Hosp
################################################################################

### IOxGetLive (IOxGetLive1 & IOxGetLive2): Get oxygen, go on to survive #######
delta_IOxGetLive1[] <- n_IOxGetLive1[i] - n_IOxGetLive1_IOxGetLive2[i]
delta_IOxGetLive2[] <- n_IOxGetLive1_IOxGetLive2[i] - n_IOxGetLive2_R[i]

update(IOxGetLive1[]) <- IOxGetLive1[i] + delta_IOxGetLive1[i] # First of the compartments for infections that will require oxygen, get it, and who survive (2 comps)
update(IOxGetLive2[]) <- IOxGetLive2[i] + delta_IOxGetLive2[i] # Second of the compartments for infections that will require oxygen, get it and who survive (2 comps)

gamma_get_ox_survive <- user() # rate of progression through requiring oxygen compartment conditional on getting oxygen and surviving
p_IOxGetLive1_IOxGetLive2 <- 1 - exp(-gamma_get_ox_survive * dt) # Progression through requiring oxygen and receiving it -> Recovery
p_IOxGetLive2_R <- 1 - exp(-gamma_get_ox_survive * dt) # Progression through requiring oxygen and recieving it -> Recovery

n_IOxGetLive1_IOxGetLive2[] <- rbinom(IOxGetLive1[i], p_IOxGetLive1_IOxGetLive2) # IOxGetLive1->IOxGetLive2
n_IOxGetLive2_R[] <- rbinom(IOxGetLive2[i], p_IOxGetLive2_R) # IOxGetLive2->R
################################################################################

### IOxGetDie (IOxGetDie1 & IOxGetDie2): Get oxygen go on to die ###############
delta_IOxGetDie1[] <- n_IOxGetDie1[i] - n_IOxGetDie1_IOxGetDie2[i]
delta_IOxGetDie2[] <- n_IOxGetDie1_IOxGetDie2[i] - n_IOxGetDie2_D[i]

update(IOxGetDie1[]) <- IOxGetDie1[i] + delta_IOxGetDie1[i] # First of the compartments for infections that will require oxygen, get it, and die (2 comps)
update(IOxGetDie2[]) <- IOxGetDie2[i] + delta_IOxGetDie2[i] # Second of the compartments for infections that will require oxygen, get it, and die (2 comps)

gamma_get_ox_die <- user() # rate of progression through requiring oxygen compartment conditional on getting oxygen and dying

p_IOxGetDie1_IOxGetDie2 <- 1 - exp(-gamma_get_ox_die * dt) # Progression through requiring oxygen and receiving it -> Death
p_IOxGetDie2_D <- 1 - exp(-gamma_get_ox_die * dt) # Progression through requiring oxygen and receiving it -> Death

n_IOxGetDie1_IOxGetDie2[] <- rbinom(IOxGetDie1[i], p_IOxGetDie1_IOxGetDie2) # IOxGetDie1->IOxGetDie2
n_IOxGetDie2_D[] <- rbinom(IOxGetDie2[i], p_IOxGetDie2_D) # IOxGetDie2->D
################################################################################

### IOxNotGetLive (IOxNotGetLive1 & IOxNotGetLive2): Do not get oxygen, go on to survive #######
delta_IOxNotGetLive1[] <- n_IOxNotGetLive1[i] - n_IOxNotGetLive1_IOxNotGetLive2[i]
delta_IOxNotGetLive2[] <- n_IOxNotGetLive1_IOxNotGetLive2[i] - n_IOxNotGetLive2_R[i]

update(IOxNotGetLive1[]) <- IOxNotGetLive1[i] + delta_IOxNotGetLive1[i] # First of the compartments for infections that will require oxygen, do NOT get it, and live (2 comps)
update(IOxNotGetLive2[]) <- IOxNotGetLive2[i] + delta_IOxNotGetLive2[i] # Second of the compartments for infections that will require oxygen, do NOT get it, and live (2 comps)

gamma_not_get_ox_survive <- user() # rate of progression through requiring oxygen compartment conditional on not getting oxygen and surviving

p_IOxNotGetLive1_IOxNotGetLive2 <- 1 - exp(-gamma_not_get_ox_survive * dt) # Progression through requiring oxygen and not receiving it -> Recovery
p_IOxNotGetLive2_R <- 1 - exp(-gamma_not_get_ox_survive * dt) # Progression through requiring oxygen and not receiving it -> Recovery

n_IOxNotGetLive1_IOxNotGetLive2[] <- rbinom(IOxNotGetLive1[i], p_IOxNotGetLive1_IOxNotGetLive2) # IOxNotGetLive1->IOxNotGetLive2
n_IOxNotGetLive2_R[] <- rbinom(IOxNotGetLive2[i], p_IOxNotGetLive2_R) # IOxNotGetLive2->R
################################################################################

### IOxNotGetDie (IOxNotGetDie1 & IOxNotGetDie2): Do not get oxygen, go on to die #######
delta_IOxNotGetDie1[] <- n_IOxNotGetDie1[i] - n_IOxNotGetDie1_IOxNotGetDie2[i]
delta_IOxNotGetDie2[] <- n_IOxNotGetDie1_IOxNotGetDie2[i] - n_IOxNotGetDie2_D[i]

update(IOxNotGetDie1[]) <- IOxNotGetDie1[i] + delta_IOxNotGetDie1[i] # First of the compartments for infections that will require oxygen, do NOT get it, and die (2 comps)
update(IOxNotGetDie2[]) <- IOxNotGetDie2[i] + delta_IOxNotGetDie2[i] # Second of the compartments for infections that will require oxygen, do NOT get it, and die (2 comps)

gamma_not_get_ox_die <- user() # rate of progression through requiring oxygen compartment conditional on not getting oxygen and dying

p_IOxNotGetDie1_IOxNotGetDie2 <- 1 - exp(-gamma_not_get_ox_die * dt) # Progression through requiring oxygen and not receiving it -> Death
p_IOxNotGetDie2_D <- 1 - exp(-gamma_not_get_ox_die * dt) # Progression through requiring oxygen and not receiving it -> Death

n_IOxNotGetDie1_IOxNotGetDie2[] <- rbinom(IOxNotGetDie1[i], p_IOxNotGetDie1_IOxNotGetDie2) # IOxNotGetDie1->IOxNotGetDie2
n_IOxNotGetDie2_D[] <- rbinom(IOxNotGetDie2[i], p_IOxNotGetDie2_D) # IOxNotGetDie2->D
################################################################################

### IMVGetLive (IMVGetLive1 & IMVGetLive2): Get mechanical ventilation, go on to live ########
delta_IMVGetLive1[] <- n_IMVGetLive1[i] - n_IMVGetLive1_IMVGetLive2[i]
delta_IMVGetLive2[] <- n_IMVGetLive1_IMVGetLive2[i] - n_IMVGetLive2_Rec[i]

update(IMVGetLive1[]) <- IMVGetLive1[i] + delta_IMVGetLive1[i] # First of the compartments for infections that will require mechanical ventilation, get it, and who survive (2 comps)
update(IMVGetLive2[]) <- IMVGetLive2[i] + delta_IMVGetLive2[i] # Second of the compartments for infections that will require mechanical ventilation, get it, and who survive (2 comps)

gamma_get_mv_survive <- user() # rate of progression through requiring mechanical ventilation compartment conditional on getting ventilation and surviving

p_IMVGetLive1_IMVGetLive2 <- 1 - exp(-gamma_get_mv_survive * dt) # Progression through requiring mechanical ventilation and recieving it -> ICU recovery
p_IMVGetLive2_Rec <- 1 - exp(-gamma_get_mv_survive * dt) # Progression through requiring mechanical ventilation and recieving it -> ICU recovery

n_IMVGetLive1_IMVGetLive2[] <- rbinom(IMVGetLive1[i], p_IMVGetLive1_IMVGetLive2) # IMVGetLive1->IMVGetLive2
n_IMVGetLive2_Rec[] <- rbinom(IMVGetLive2[i], p_IMVGetLive2_Rec) # IMVGetLive2->Rec
################################################################################

### IMVGetDie (IMVGetDie1 & IMVGetDie2): Get mechanical ventilation, go on to die ########
delta_IMVGetDie1[] <- n_IMVGetDie1[i] - n_IMVGetDie1_IMVGetDie2[i]
delta_IMVGetDie2[] <- n_IMVGetDie1_IMVGetDie2[i] - n_IMVGetDie2_D[i]

update(IMVGetDie1[]) <- IMVGetDie1[i] + delta_IMVGetDie1[i] # First of the compartments for infections that will require mechanical ventilation, get it, and die (2 comps)
update(IMVGetDie2[]) <- IMVGetDie2[i] + delta_IMVGetDie2[i] # Second of the compartments for infections that will require mechanical ventilation, get it, and die (2 comps)

gamma_get_mv_die <- user() # rate of progression through requiring mechanical ventilation compartment conditional on getting ventilation and dying

prob_severe_death_treatment[] <- user() # probability of dying from severe disease (i.e. requiring mechanical ventilation) by age given you receive appropriate treatment (proxy here is whether an ICU bed is available)
p_IMVGetDie1_IMVGetDie2 <- 1 - exp(-gamma_get_mv_die * dt) # Progression through requiring mechanical ventilation and receving it -> Death
p_IMVGetDie2_D <- 1 - exp(-gamma_get_mv_die * dt) # Progression through requiring mechanical ventilation and receving it -> Death

n_IMVGetDie1[] <- rbinom(number_get_IMV[i], prob_severe_death_treatment[i]) # Number of individuals requiring mechanical ventilation and who recieve it who die
n_IMVGetDie1_IMVGetDie2[] <- rbinom(IMVGetDie1[i], p_IMVGetDie1_IMVGetDie2) # IMVGetDie1->IMVGetDie2
n_IMVGetDie2_D[] <- rbinom(IMVGetDie2[i], p_IMVGetDie2_D) # IMVGetDie2->D
################################################################################

### IMVNotGetLive (IMVNotGetLive1 & IMVNotGetLive2): Do no get mechanical ventilation, go on to live ########
delta_IMVNotGetLive1[] <- n_IMVNotGetLive1[i] - n_IMVNotGetLive1_IMVNotGetLive2[i]
delta_IMVNotGetLive2[] <- n_IMVNotGetLive1_IMVNotGetLive2[i] - n_IMVNotGetLive2_R[i]

update(IMVNotGetLive1[]) <- IMVNotGetLive1[i] + delta_IMVNotGetLive1[i] # First of the compartments for infections that will require mechanical ventilation, do NOT get it, and survive (2 comps)
update(IMVNotGetLive2[]) <- IMVNotGetLive2[i] + delta_IMVNotGetLive2[i] # Second of the compartments for infections that will require mechanical ventilation, do NOT get it, and survive (2 comps)

gamma_not_get_mv_survive <- user() # rate of progression through requiring mechanical ventilation compartment conditional on not getting ventilation and surviving

p_IMVNotGetLive1_IMVNotGetLive2 <- 1 - exp(-gamma_not_get_mv_survive * dt) # Progression through requiring mechanical ventilation and not recieving it -> Recovery
p_IMVNotGetLive2_R <- 1 - exp(-gamma_not_get_mv_survive * dt) # Progression through requiring mechanical ventilation and not recieving it -> Recovery

n_IMVNotGetLive1[] <- number_notget_IMV[i] - n_IMVNotGetDie1[i] # Number of individuals requiring mechanical ventilation but who do not receive it and who survive
n_IMVNotGetLive1_IMVNotGetLive2[] <- rbinom(IMVNotGetLive1[i], p_IMVNotGetLive1_IMVNotGetLive2) # IMVNotGetLive1->IMVNotGetLive2
n_IMVNotGetLive2_R[] <- rbinom(IMVNotGetLive2[i], p_IMVNotGetLive2_R) # IMVNotGetLive1->R
################################################################################

### IMVNotGetDie (IMVNotGetDie1 & IMVNotGetDie2): Do no get mechanical ventilation, go on to die ########
delta_IMVNotGetDie1[] <- n_IMVNotGetDie1[i] - n_IMVNotGetDie1_IMVNotGetDie2[i]
delta_IMVNotGetDie2[] <-  n_IMVNotGetDie1_IMVNotGetDie2[i] - n_IMVNotGetDie2_D[i]

update(IMVNotGetDie1[]) <- IMVNotGetDie1[i] + delta_IMVNotGetDie1[i] # First of the compartments for infections that will require mechanical ventilation, do NOT get it, and die (2 comps)
update(IMVNotGetDie2[]) <- IMVNotGetDie2[i] + delta_IMVNotGetDie2[i] # Second of the compartments for infections that will require mechanical ventilation, do NOT get it, and die (2 comps)

gamma_not_get_mv_die <- user() # rate of progression through requiring mechanical ventilation compartment conditional on not getting ventilation and dying

p_IMVNotGetDie1_IMVNotGetDie2 <- 1 - exp(-gamma_not_get_mv_die * dt) # Progression through requiring mechanical ventilation and not receving it -> Death
p_IMVNotGetDie2_D <- 1 - exp(-gamma_not_get_mv_die * dt) # Progression through requiring mechanical ventilation and not receving it -> Death

n_IMVNotGetDie1[] <- rbinom(number_notget_IMV[i], prob_severe_death_no_treatment[i]) # Number of individuals requiring mechanical ventilation, who do not receive it, and who die
n_IMVNotGetDie1_IMVNotGetDie2[] <- rbinom(IMVNotGetDie1[i], p_IMVNotGetDie1_IMVNotGetDie2)  # IMVNotGetDie1->IMVNotGetDie2
n_IMVNotGetDie2_D[] <- rbinom(IMVNotGetDie2[i], p_IMVNotGetDie2_D) # IMVNotGetDie2->R
################################################################################

### IRec (IRec1 & IRec2): Recovering from ICU ##################################
delta_IRec1[] <- n_IMVGetLive2_Rec[i] - n_IRec1_IRec2[i]
delta_IRec2[] <- n_IRec1_IRec2[i] - n_IRec2_R[i]

update(IRec1[]) <- IRec1[i] + delta_IRec1[i] # First of the compartments for those recovering from ICU (2 comps)
update(IRec2[]) <- IRec2[i] + delta_IRec2[i] # Second of the compartments for those recovering from ICU (2 comps)
IRec[] <- IRec1[i] + IRec2[i] # Summary

gamma_rec <- user() # rate of progression through post-ICU recovery compartment

p_Rec1_Rec2 <- 1 - exp(-gamma_rec * dt) # Progression through recovery from ICU in hospital bed to eventual discharge (R)
p_Rec2_R <- 1 - exp(-gamma_rec * dt) # Progression through recovery from ICU in hospital bed to eventual discharge (R)

n_IRec1_IRec2[] <- rbinom(IRec1[i], p_Rec1_Rec2) # IRec1->IRec2
n_IRec2_R[] <- rbinom(IRec2[i], p_Rec2_R) # IRec2->R
################################################################################

### D: Dead ####################################################################
delta_D[] <- n_IOxGetDie2_D[i] + n_IOxNotGetDie2_D[i] + n_IMVGetDie2_D[i] + n_IMVNotGetDie2_D[i]

update(D[]) <- D[i] + delta_D[i] # Deaths
################################################################################

### R: (R1 & R2): Recovered ####################################################
delta_R1[] <- n_IOxGetLive2_R[i] + n_IOxNotGetLive2_R[i] + n_IRec2_R[i] + n_IMVNotGetLive2_R[i] + n_IMild_R[i] - n_R1_R2_RVac1[i]
delta_R2[] <- n_R1_R2[i] - n_R2_S_RVac1[i]

update(R1[]) <- R1[i] + delta_R1[i] # Recovered 1
update(R2[]) <- R2[i] + delta_R2[i] # Recovered 2
R[] <- R1[i] + R2[i] # Summary R

gamma_R <- user() # rate of progression through recovered compartment (loss of naturally acquired immunity)

p_leave_R[] <- 1 - exp(-(gamma_R + vr * vaccination_target[i]) * dt) # Probability leaving R
p_R[] <- gamma_R / (gamma_R + vr * vaccination_target[i]) # Probability of progression through R

n_R1_R2_RVac1[] <- rbinom(R1[i], p_leave_R[i]) # Number leaving R1
n_R1_R2[] <- if (n_R1_R2_RVac1[i] > 0) rbinom(n_R1_R2_RVac1[i], p_R[i]) else 0 # R1->R2
n_R1_RVac1[] <- n_R1_R2_RVac1[i] - n_R1_R2[i] # R1->V

n_R2_S_RVac1[] <- rbinom(R2[i], p_leave_R[i]) # Number leaving R2
n_R2_S[] <- if (n_R2_S_RVac1[i] > 0) rbinom(n_R2_S_RVac1[i], p_R[i]) else 0 # R2->S
n_R2_RVac1[] <- n_R2_S_RVac1[i] - n_R2_S[i] # R2->V
################################################################################

### RVac (RVac1 & RVac2): Recovered, vaccinated but nor vaccine-protected ######
delta_RVac1[] <- n_R1_RVac1[i] + n_R2_RVac1[i] - n_RVac1_RVac2[i]
delta_RVac2[] <- n_RVac1_RVac2[i] - n_RVac2_V1[i]

update(RVac1[]) <- RVac1[i] + delta_RVac1[i]
update(RVac2[]) <- RVac2[i] + delta_RVac2[i]
RVac[] <- RVac1[i] + RVac2[i]

gamma_RVac <- user() # Rate of progression through vaccinated but recovered compartment
p_leave_RVac <- 1 - exp(-gamma_RVac * dt)

n_RVac1_RVac2[] <- rbinom(RVac1[i], p_leave_RVac)
n_RVac2_V1[] <- rbinom(RVac2[i], p_leave_RVac)
################################################################################


################################################################################
### Summary outputs ############################################################
################################################################################
# Hospital occupancy and demand
hospital_occupancy[] <- IOxGetLive1[i] + IOxGetLive2[i] + IOxGetDie1[i] + IOxGetDie2[i] +  IRec1[i] + IRec2[i]
ICU_occupancy[] <- IMVGetLive1[i] +  IMVGetLive2[i] + IMVGetDie1[i] + IMVGetDie2[i]
hospital_demand[] <- IOxGetLive1[i] + IOxGetLive2[i] + IOxGetDie1[i] + IOxGetDie2[i] +  IRec1[i] + IRec2[i] + IOxNotGetLive1[i] + IOxNotGetLive2[i] + IOxNotGetDie1[i] + IOxNotGetDie2[i]
ICU_demand[] <- IMVGetLive1[i] +  IMVGetLive2[i] + IMVGetDie1[i] + IMVGetDie2[i] + IMVNotGetLive1[i] +  IMVNotGetLive2[i] + IMVNotGetDie1[i] + IMVNotGetDie2[i]

# Number in hospital or ICU compartments
IHospital[] <- IOxGetLive1[i] + IOxGetLive2[i] + IOxGetDie1[i] + IOxGetDie2[i] + IOxNotGetLive1[i] + IOxNotGetLive2[i] + IOxNotGetDie1[i] + IOxNotGetDie2[i]
IICU[] <- IMVGetLive1[i] + IMVGetLive2[i] + IMVGetDie1[i] + IMVGetDie2[i] + IMVNotGetLive1[i] + IMVNotGetLive2[i] + IMVNotGetDie1[i] + IMVNotGetDie2[i]

# Deaths and infections
deaths[] <- n_IOxGetDie2_D[i] + n_IOxNotGetDie2_D[i] + n_IMVGetDie2_D[i] + n_IMVNotGetDie2_D[i]
infections[] <- n_S_E1[i] + n_V1_Evac[i] + n_V2_Evac[i]

# Population size
N[] <- S[i] + E1[i] + E2[i] + EVac1[i] + EVac2[i] + IMild[i] + ICase1[i] + ICase2[i] +
  SVac1[i] + SVac2[i] + RVac1[i] + RVac2[i] +
  IMVGetLive1[i] + IMVGetLive2[i] +
  IMVGetDie1[i] + IMVGetDie2[i] + IMVNotGetLive1[i] + IMVNotGetLive2[i] + IMVNotGetDie1[i] + IMVNotGetDie2[i] +
  IOxGetLive1[i] + IOxGetLive2[i] + IOxGetDie1[i] + IOxGetDie2[i] + IOxNotGetLive1[i] + IOxNotGetLive2[i] +
  IOxNotGetDie1[i] + IOxNotGetDie2[i] +
  IRec1[i] + IRec2[i] +
  R1[i] + R2[i] + D[i] + V1[i] + V2[i]
################################################################################
################################################################################

################################################################################
### Hospital and ICU capacity ##################################################
################################################################################
# Interpolation for Hospital Capacity
hosp_bed_capacity <- interpolate(tt_hosp_beds, hosp_beds, "constant")
tt_hosp_beds[] <- user()
hosp_beds[] <- user()
dim(tt_hosp_beds) <- user()
dim(hosp_beds) <- length(tt_hosp_beds)
# Interpolation for ICU Capacity
ICU_bed_capacity <- interpolate(tt_ICU_beds, ICU_beds, "constant")
tt_ICU_beds[] <- user()
ICU_beds[] <- user()
dim(tt_ICU_beds) <- user()
dim(ICU_beds) <- length(tt_ICU_beds)

prob_severe[] <- user() # probability of severe disease (requiring mechanical ventilation) by age
prob_non_severe_death_treatment[] <- user() # probability of dying from non-severe disease (i.e. requiring oxygen but not mechanical ventilation) by age given you receive appropriate treatment (proxy here is whether a general hospital bed is available)
prob_non_severe_death_no_treatment[] <- user() # probability of dying from non-severe disease (i.e. requiring oxygen but not mechanical ventilation) by age given you do NOT receive appropriate treatment (proxy here is whether a general hospital bed is available)
prob_severe_death_no_treatment[] <- user() # probability of dying from severe disease (i.e. requiring mechanical ventilation) by age given you do NOT receive appropriate treatment (proxy here is whether an ICU bed is available)

# Mechanical ventilation capacity and need
ICU_occ <- sum(IMVGetLive1) + sum(IMVGetLive2) + sum(IMVGetDie1) + sum(IMVGetDie2) # Summing number of infections in compartments that use ICU beds
number_requiring_IMV[] <- rbinom(n_ICase2_Hosp[i], prob_severe[i]) # Number of new hospitalisations that are going to require mechanical ventilation
total_number_requiring_IMV <- sum(number_requiring_IMV)
current_free_ICUs <- ICU_bed_capacity + sum(n_IMVGetLive2_Rec) + sum(n_IMVGetDie2_D) - ICU_occ # Number of ICU beds that are currently free
total_number_get_IMV <- if(current_free_ICUs <= 0) 0 else(if(current_free_ICUs - total_number_requiring_IMV >= 0) total_number_requiring_IMV else(current_free_ICUs)) # Working out the number of new ICU requiring infections that get a bed
number_get_IMV[] <- rmhyper(total_number_get_IMV, number_requiring_IMV)
n_IMVGetLive1[] <- number_get_IMV[i] - n_IMVGetDie1[i] # Number of individuals requiring mechanical ventilation and who receive it and who survive
number_notget_IMV[] <- number_requiring_IMV[i] - number_get_IMV[i] # Calculating the number of new ICU requiring cases who do not get an ICU bed
# Oxygen capacity and need
hosp_occ <- sum(IOxGetLive1) + sum(IOxGetLive2) + sum(IOxGetDie1) + sum(IOxGetDie2) + sum(IRec1) + sum(IRec2) # Summing number of infections in compartments that use general hospital beds
number_requiring_Ox[] <- n_ICase2_Hosp[i] - number_requiring_IMV[i] # Number of hospitalisations that are going to require oxygen
total_number_requiring_Ox <- sum(number_requiring_Ox)
current_free_hosp <- hosp_bed_capacity + sum(n_IOxGetDie2_D) + sum(n_IOxGetLive2_R) + sum(n_IRec2_R) - sum(n_IMVGetLive2_Rec) - hosp_occ # Number of hospital beds that are currently free
total_number_get_hosp <- if (current_free_hosp <= 0) 0 else (if(current_free_hosp - total_number_requiring_Ox >= 0) total_number_requiring_Ox else(current_free_hosp)) # Working out the number of new hospital bed requiring infections that get a bed
number_get_Ox[] <- rmhyper(total_number_get_hosp, number_requiring_Ox)
n_IOxGetDie1[] <- rbinom(number_get_Ox[i], prob_non_severe_death_treatment[i]) # Number of individuals requiring oxygen and who recieve it who die
n_IOxGetLive1[] <- number_get_Ox[i] - n_IOxGetDie1[i]  # Number of individuals requiring oxygen and who receive it and who survive
number_notget_Ox[] <- number_requiring_Ox[i] - number_get_Ox[i] # Calculating the number of cases requiring a hospital bed and who do not receive it
n_IOxNotGetDie1[] <- rbinom(number_notget_Ox[i], prob_non_severe_death_no_treatment[i]) # Number of individuals requiring oxygen but do not receive it and who die
n_IOxNotGetLive1[] <- number_notget_Ox[i] - n_IOxNotGetDie1[i] # Number of individuals requiring oxygen but who do not receive it and who survive
################################################################################
################################################################################

################################################################################
### Vaccination capacity #######################################################
################################################################################
vaccination_target[] <- user() # 0/1 index of targeted age groups
vaccine_efficacy_infection[] <- user() # Reduction in lambda for vaccinated individuals by age

# Interpolation of vaccination rate over time
mv <- interpolate(tt_vaccine, max_vaccine, "constant")
tt_vaccine[] <- user()
max_vaccine[] <- user()
dim(tt_vaccine) <- user()
dim(max_vaccine) <- length(tt_vaccine)

vr_temp[] <- S[i] * vaccination_target[i] + R[i] * vaccination_target[i]
dim(vr_temp) <- N_age
vr <- mv / sum(vr_temp) # Vaccination rate to achieve capacity given number in S or R
################################################################################
################################################################################

################################################################################
### FOI and contact matrix #####################################################
################################################################################

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
################################################################################
################################################################################

################################################################################
### Output #####################################################################
################################################################################
output(E) <- TRUE
output(IICU) <- TRUE
output(IHospital) <- TRUE
output(ICase) <- TRUE
output(R) <- TRUE
output(V) <- TRUE
output(EVac) <- TRUE
output(RVac) <- TRUE
output(IRec) <- TRUE
output(vaccines) <- TRUE
output(SVac) <- TRUE
output(N) <- TRUE
output(hospital_occupancy) <- TRUE
output(ICU_occupancy) <- TRUE
output(hospital_demand) <- TRUE
output(ICU_demand) <- TRUE
output(deaths) <- TRUE
output(infections) <- TRUE
output(time) <- TRUE

################################################################################
################################################################################

################################################################################
### Input ######################################################################
################################################################################
dt <- user() # Specified timestep
time <- step * dt # Tracking actual time
N_age <- user() # Number of age groups

# Initial states:
initial(S[]) <- S_0[i]
initial(SVac1[]) <- SVac1_0[i]
initial(SVac2[]) <- SVac2_0[i]
initial(E1[]) <- E1_0[i]
initial(E2[]) <- E2_0[i]
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
initial(D[]) <- D_0[i]
initial(R1[]) <- R1_0[i]
initial(R2[]) <- R2_0[i]
initial(V1[]) <- V1_0[i]
initial(V2[]) <- V2_0[i]
initial(EVac1[]) <- EVac1_0[i]
initial(EVac2[]) <- EVac2_0[i]
initial(RVac1[]) <- RVac1_0[i]
initial(RVac2[]) <- RVac2_0[i]

# Initial vectors
S_0[] <- user()
SVac1_0[] <- user()
SVac2_0[] <- user()
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
D_0[] <- user()
R1_0[] <- user()
R2_0[] <- user()
V1_0[] <- user()
V2_0[] <- user()
EVac1_0[] <- user()
EVac2_0[] <- user()
RVac1_0[] <- user()
RVac2_0[] <- user()

# State dimensions
dim(S) <- N_age
dim(SVac1) <- N_age
dim(SVac2) <- N_age
dim(E1) <- N_age
dim(E2) <- N_age
dim(E) <- N_age
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
dim(IICU) <- N_age
dim(IHospital) <- N_age
dim(IRec1) <- N_age
dim(IRec2) <- N_age
dim(D) <- N_age
dim(R1) <- N_age
dim(R2) <- N_age
dim(R) <- N_age
dim(V1) <- N_age
dim(V2) <- N_age
dim(V) <- N_age
dim(EVac1) <- N_age
dim(EVac2) <- N_age
dim(EVac) <- N_age
dim(IRec) <- N_age
dim(vaccines) <- N_age
dim(N) <- N_age
dim(hospital_occupancy) <- N_age
dim(ICU_occupancy) <- N_age
dim(hospital_demand) <- N_age
dim(ICU_demand) <- N_age
dim(deaths) <- N_age
dim(infections) <- N_age
dim(SVac) <- N_age
dim(RVac1) <- N_age
dim(RVac2) <- N_age
dim(RVac) <- N_age

# Initial value dimensions
dim(S_0) <- N_age
dim(SVac1_0) <- N_age
dim(SVac2_0) <- N_age
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
dim(D_0) <- N_age
dim(R1_0) <- N_age
dim(R2_0) <- N_age
dim(V1_0) <- N_age
dim(V2_0) <- N_age
dim(EVac1_0) <- N_age
dim(EVac2_0) <- N_age
dim(RVac1_0) <- N_age
dim(RVac2_0) <- N_age

# Delta dimensions
dim(delta_E1) <- N_age
dim(delta_E2) <- N_age
dim(delta_SVac1) <- N_age
dim(delta_SVac2) <- N_age
dim(delta_IMild) <- N_age
dim(delta_ICase1) <- N_age
dim(delta_ICase2) <- N_age
dim(delta_IOxGetLive1) <- N_age
dim(delta_IOxGetLive2) <- N_age
dim(delta_IOxNotGetLive1) <- N_age
dim(delta_IOxNotGetLive2) <- N_age
dim(delta_IOxGetDie1) <- N_age
dim(delta_IOxGetDie2) <- N_age
dim(delta_IOxNotGetDie1) <- N_age
dim(delta_IOxNotGetDie2) <- N_age
dim(delta_IMVGetLive1) <- N_age
dim(delta_IMVGetLive2) <- N_age
dim(delta_IMVNotGetLive1) <- N_age
dim(delta_IMVNotGetLive2) <- N_age
dim(delta_IMVGetDie1) <- N_age
dim(delta_IMVGetDie2) <- N_age
dim(delta_IMVNotGetDie1) <- N_age
dim(delta_IMVNotGetDie2) <- N_age
dim(delta_IRec1) <- N_age
dim(delta_IRec2) <- N_age
dim(delta_D) <- N_age
dim(delta_R1) <- N_age
dim(delta_R2) <- N_age
dim(delta_V1) <- N_age
dim(delta_V2) <- N_age
dim(delta_EVac1) <- N_age
dim(delta_EVac2) <- N_age
dim(delta_S) <- N_age
dim(delta_RVac1) <- N_age
dim(delta_RVac2) <- N_age

# n dimensions
dim(n_E1_E2) <- N_age
dim(n_E2_I) <- N_age
dim(n_E2_ICase1) <- N_age
dim(n_E2_IMild) <- N_age
dim(n_IMild_R) <- N_age
dim(n_ICase1_ICase2) <- N_age
dim(n_ICase2_Hosp) <- N_age
dim(number_requiring_IMV) <- N_age
dim(number_get_IMV) <- N_age
dim(n_IMVGetDie1) <- N_age
dim(n_IMVGetDie1_IMVGetDie2) <- N_age
dim(n_IMVGetDie2_D) <- N_age
dim(n_IMVGetLive1) <- N_age
dim(n_IMVGetLive1_IMVGetLive2) <- N_age
dim(n_IMVGetLive2_Rec) <- N_age
dim(number_notget_IMV) <- N_age
dim(n_IMVNotGetDie1) <- N_age
dim(n_IMVNotGetDie1_IMVNotGetDie2) <- N_age
dim(n_IMVNotGetDie2_D) <- N_age
dim(n_IMVNotGetLive1) <- N_age
dim(n_IMVNotGetLive1_IMVNotGetLive2) <- N_age
dim(n_IMVNotGetLive2_R) <- N_age
dim(number_requiring_Ox) <- N_age
dim(number_get_Ox) <- N_age
dim(n_IOxGetDie1) <- N_age
dim(n_IOxGetDie1_IOxGetDie2) <- N_age
dim(n_IOxGetDie2_D) <- N_age
dim(n_IOxGetLive1) <- N_age
dim(n_IOxGetLive1_IOxGetLive2) <- N_age
dim(n_IOxGetLive2_R) <- N_age
dim(number_notget_Ox) <- N_age
dim(n_IOxNotGetDie1) <- N_age
dim(n_IOxNotGetDie1_IOxNotGetDie2) <- N_age
dim(n_IOxNotGetDie2_D) <- N_age
dim(n_IOxNotGetLive1) <- N_age
dim(n_IOxNotGetLive1_IOxNotGetLive2) <- N_age
dim(n_IOxNotGetLive2_R) <- N_age
dim(n_IRec1_IRec2) <- N_age
dim(n_IRec2_R) <- N_age
dim(n_R1_R2) <- N_age
dim(n_R2_S) <- N_age
dim(n_S_E1) <- N_age
dim(n_V1_V2_Evac) <- N_age
dim(n_V1_V2) <- N_age
dim(n_V1_Evac) <- N_age
dim(n_V2_S_Evac) <- N_age
dim(n_V2_S) <- N_age
dim(n_V2_Evac) <- N_age
dim(n_EVac1_EVac2) <- N_age
dim(n_EVac2_I) <- N_age
dim(n_EVac2_ICase) <- N_age
dim(n_EVac2_IMild) <- N_age
dim(n_SVac1_SVac2) <- N_age
dim(n_SVac2_V1) <- N_age
dim(n_S_E1_SVac1) <- N_age
dim(n_S_SVac1) <- N_age
dim(n_RVac1_RVac2) <- N_age
dim(n_RVac2_V1) <- N_age
dim(n_R1_R2_RVac1) <- N_age
dim(n_R1_RVac1) <- N_age
dim(n_R2_S_RVac1) <- N_age
dim(n_R2_RVac1) <- N_age
dim(n_SVac1_EVac1_SVac2) <- N_age
dim(n_SVac2_EVac1_V1) <- N_age
dim(n_SVac1_EVac1) <- N_age
dim(n_SVac2_EVac1) <- N_age

# Severity Parameters
dim(prob_hosp) <- N_age
dim(prob_severe) <- N_age
dim(prob_non_severe_death_treatment) <- N_age
dim(prob_non_severe_death_no_treatment) <- N_age
dim(prob_severe_death_treatment) <- N_age
dim(prob_severe_death_no_treatment) <- N_age

# Vaccination parameters
dim(vaccination_target) <- N_age
dim(vaccine_efficacy_infection) <- N_age
dim(prob_hosp_vaccine) <- N_age

dim(p_leave_R) <- N_age
dim(p_R) <- N_age
dim(p_leave_V) <- N_age
dim(p_V) <- N_age
dim(p_leave_S) <- N_age
dim(p_E) <- N_age
dim(p_leave_SVac) <- N_age
dim(p_SVac1_SVac2) <- N_age
dim(p_SVac2_V1) <- N_age

# Related to Calculating Age-Structured Force of Infection
dim(lambda) <- N_age
dim(s_ij) <- c(N_age,N_age)
dim(temp) <- N_age
