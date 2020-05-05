##########################################################
## Setting parameters for the model:                    ##
##########################################################

dt <- user() # Specified timestep
time <- step * dt # Tracking actual time
output(time) <- TRUE
N_age <- user() # Number of age groups

## RATES
##------------------------------------------------------------------------------
gamma_E <- user() # rate of progression through latent infection
gamma_IMild <- user() # rate of progression from mild infection to recovery
gamma_ICase <- user() # rate of progression from symptom onset to requiring hospitalisation

# rate of progression through requiring oxygen compartment conditional on getting oxygen and surviving
gamma_get_ox_survive <- user()
# rate of progression through requiring oxygen compartment conditional on getting oxygen and dying
gamma_get_ox_die <- user()
# rate of progression through requiring oxygen compartment conditional on not getting oxygen and surviving
gamma_not_get_ox_survive <- user()
# rate of progression through requiring oxygen compartment conditional on not getting oxygen and dying
gamma_not_get_ox_die <- user()

# rate of progression through requiring mechanical ventilation compartment conditional on getting ventilation and surviving
gamma_get_mv_survive <- user()
# rate of progression through requiring mechanical ventilation compartment conditional on getting ventilation and dying
gamma_get_mv_die <- user()
# rate of progression through requiring mechanical ventilation compartment conditional on not getting ventilation and surviving
gamma_not_get_mv_survive <- user()
# rate of progression through requiring mechanical ventilation compartment conditional on not getting ventilation and dying
gamma_not_get_mv_die <- user()

gamma_rec <- user() # rate of progression through post-ICU recovery compartment

## Probabilities
# ------------------------------------------------------------------------------

prob_hosp[] <- user() # probability of requiring hospitalisation by age
prob_severe[] <- user() # probability of severe disease (requiring mechanical ventilation) by age
prob_non_severe_death_treatment[] <- user() # probability of dying from non-severe disease (i.e. requiring oxygen but not mechanical ventilation) by age given you receive appropriate treatment (proxy here is whether a general hospital bed is available)
prob_non_severe_death_no_treatment[] <- user() # probability of dying from non-severe disease (i.e. requiring oxygen but not mechanical ventilation) by age given you do NOT receive appropriate treatment (proxy here is whether a general hospital bed is available)
prob_severe_death_treatment[] <- user() # probability of dying from severe disease (i.e. requiring mechanical ventilation) by age given you receive appropriate treatment (proxy here is whether an ICU bed is available)
prob_severe_death_no_treatment[] <- user() # probability of dying from severe disease (i.e. requiring mechanical ventilation) by age given you do NOT receive appropriate treatment (proxy here is whether an ICU bed is available)
p_dist[] <- user() # distributing infections in given age class to available hosp/ICU beds (make all equal to make it random and not related to age)

##########################################################
## Core equations for transitions between compartments: ##
##########################################################

# Passage Through Initial Latent and Infection Stages
update(S[]) <- S[i] - n_S_E1[i]  # Susceptibles (1 comp)
update(E1[]) <- E1[i] + delta_E1[i]  # First of the latent infection compartments (2 comps)
update(E2[]) <- E2[i] + delta_E2[i]  # Second of the latent infection compartments (2 comps)
update(IMild[]) <- IMild[i] + delta_IMild[i]  # Mild infections (1 comp)
update(ICase1[]) <- ICase1[i] + delta_ICase1[i] # First of the compartments for infections that will require hospitalisation (2 comps)
update(ICase2[]) <- ICase2[i] + delta_ICase2[i] # Second of the compartments for infections that will require hospitalisation (2 comps)

# Passage Through Requiring Oxygen, Either Receiving It or Not, and Surviving or Not
update(IOxGetLive1[]) <- IOxGetLive1[i] + delta_IOxGetLive1[i] # First of the compartments for infections that will require oxygen, get it, and who survive (2 comps)
update(IOxGetLive2[]) <- IOxGetLive2[i] + delta_IOxGetLive2[i] # Second of the compartments for infections that will require oxygen, get it and who survive (2 comps)
update(IOxGetDie1[]) <- IOxGetDie1[i] + delta_IOxGetDie1[i] # First of the compartments for infections that will require oxygen, get it, and die (2 comps)
update(IOxGetDie2[]) <- IOxGetDie2[i] + delta_IOxGetDie2[i] # Second of the compartments for infections that will require oxygen, get it, and die (2 comps)
update(IOxNotGetLive1[]) <- IOxNotGetLive1[i] + delta_IOxNotGetLive1[i] # First of the compartments for infections that will require oxygen, do NOT get it, and live (2 comps)
update(IOxNotGetLive2[]) <- IOxNotGetLive2[i] + delta_IOxNotGetLive2[i] # Second of the compartments for infections that will require oxygen, do NOT get it, and live (2 comps)
update(IOxNotGetDie1[]) <- IOxNotGetDie1[i] + delta_IOxNotGetDie1[i] # First of the compartments for infections that will require oxygen, do NOT get it, and die (2 comps)
update(IOxNotGetDie2[]) <- IOxNotGetDie2[i] + delta_IOxNotGetDie2[i] # Second of the compartments for infections that will require oxygen, do NOT get it, and die (2 comps)

# Passage Through Requiring Mechanical Ventilation, Either Receiving It or Not, and Surviving or Not
update(IMVGetLive1[]) <- IMVGetLive1[i] + delta_IMVGetLive1[i] # First of the compartments for infections that will require mechanical ventilation, get it, and who survive (2 comps)
update(IMVGetLive2[]) <- IMVGetLive2[i] + delta_IMVGetLive2[i] # Second of the compartments for infections that will require mechanical ventilation, get it, and who survive (2 comps)
update(IMVGetDie1[]) <- IMVGetDie1[i] + delta_IMVGetDie1[i] # First of the compartments for infections that will require mechanical ventilation, get it, and die (2 comps)
update(IMVGetDie2[]) <- IMVGetDie2[i] + delta_IMVGetDie2[i] # Second of the compartments for infections that will require mechanical ventilation, get it, and die (2 comps)
update(IMVNotGetLive1[]) <- IMVNotGetLive1[i] + delta_IMVNotGetLive1[i] # First of the compartments for infections that will require mechanical ventilation, do NOT get it, and survive (2 comps)
update(IMVNotGetLive2[]) <- IMVNotGetLive2[i] + delta_IMVNotGetLive2[i] # Second of the compartments for infections that will require mechanical ventilation, do NOT get it, and survive (2 comps)
update(IMVNotGetDie1[]) <- IMVNotGetDie1[i] + delta_IMVNotGetDie1[i] # First of the compartments for infections that will require mechanical ventilation, do NOT get it, and die (2 comps)
update(IMVNotGetDie2[]) <- IMVNotGetDie2[i] + delta_IMVNotGetDie2[i] # Second of the compartments for infections that will require mechanical ventilation, do NOT get it, and die (2 comps)

# Passage Through Recovery, from Mild Infection, Requiring Oxygen or From ICU Post-Requiring Mechanical Ventilation
update(IRec1[]) <- IRec1[i] + delta_IRec1[i] # First of the compartments for those recovering from ICU (2 comps)
update(IRec2[]) <- IRec2[i] + delta_IRec2[i] # Second of the compartments for those recovering from ICU (2 comps)
update(R[]) <- R[i] + delta_R[i] # Recovered
update(D[]) <- D[i] + delta_D[i] # Deaths
total_deaths <- sum(delta_D)
output(total_deaths) <- TRUE

###########################################################################
## Defining individual probabilities of transition between compartments: ##
###########################################################################

## Transition Probabilities Up Until Hospitalisation/Recovery from Mild Infection
p_S_E1[] <- 1 - exp(-lambda[i] * dt) # Infection - age dependent FOI based on mixing patterns
p_E1_E2 <- 1 - exp(-gamma_E * dt) # Progression through latent infection
p_E2_I <- 1 - exp(-gamma_E * dt) # Progression to onset of infectiousness. Number split between I_Mild and I_Case
p_IMild_R <- 1 - exp(-gamma_IMild * dt) # Recovery from mild disease
p_ICase1_ICase2 <- 1 - exp(-gamma_ICase * dt) # Delay between symptom onset and requiring hospitalisation
p_ICase2_Hosp <- 1 - exp(-gamma_ICase * dt) # Progression to requiring hospitalisation. Number split between I_Oxygen and I_MV

# Transition Probabilities for Those Requiring Oxygen -> Recovery
p_IOxGetLive1_IOxGetLive2 <- 1 - exp(-gamma_get_ox_survive * dt) # Progression through requiring oxygen and receiving it -> Recovery
p_IOxGetLive2_R <- 1 - exp(-gamma_get_ox_survive * dt) # Progression through requiring oxygen and recieving it -> Recovery
p_IOxNotGetLive1_IOxNotGetLive2 <- 1 - exp(-gamma_not_get_ox_survive * dt) # Progression through requiring oxygen and not receiving it -> Recovery
p_IOxNotGetLive2_R <- 1 - exp(-gamma_not_get_ox_survive * dt) # Progression through requiring oxygen and not receiving it -> Recovery

# Transition Probabilities for Those Requiring Oxygen -> Death
p_IOxGetDie1_IOxGetDie2 <- 1 - exp(-gamma_get_ox_die * dt) # Progression through requiring oxygen and receiving it -> Death
p_IOxGetDie2_D <- 1 - exp(-gamma_get_ox_die * dt) # Progression through requiring oxygen and receiving it -> Death
p_IOxNotGetDie1_IOxNotGetDie2 <- 1 - exp(-gamma_not_get_ox_die * dt) # Progression through requiring oxygen and not receiving it -> Death
p_IOxNotGetDie2_D <- 1 - exp(-gamma_not_get_ox_die * dt) # Progression through requiring oxygen and not receiving it -> Death

# Transition Probabilities for Those Requiring Mechanical Ventilation -> Recovery
p_IMVGetLive1_IMVGetLive2 <- 1 - exp(-gamma_get_mv_survive * dt) # Progression through requiring mechanical ventilation and recieving it -> ICU recovery
p_IMVGetLive2_Rec <- 1 - exp(-gamma_get_mv_survive * dt) # Progression through requiring mechanical ventilation and recieving it -> ICU recovery
p_IMVNotGetLive1_IMVNotGetLive2 <- 1 - exp(-gamma_not_get_mv_survive * dt) # Progression through requiring mechanical ventilation and not recieving it -> Recovery
p_IMVNotGetLive2_R <- 1 - exp(-gamma_not_get_mv_survive * dt) # Progression through requiring mechanical ventilation and not recieving it -> Recovery

# Transition Probabilities for Those Requiring Mechanical Ventilation -> Death
p_IMVGetDie1_IMVGetDie2 <- 1 - exp(-gamma_get_mv_die * dt) # Progression through requiring mechanical ventilation and receving it -> Death
p_IMVGetDie2_D <- 1 - exp(-gamma_get_mv_die * dt) # Progression through requiring mechanical ventilation and receving it -> Death
p_IMVNotGetDie1_IMVNotGetDie2 <- 1 - exp(-gamma_not_get_mv_die * dt) # Progression through requiring mechanical ventilation and not receving it -> Death
p_IMVNotGetDie2_D <- 1 - exp(-gamma_not_get_mv_die * dt) # Progression through requiring mechanical ventilation and not receving it -> Death

# Transition Probabilities for Those Recovering from ICU
p_Rec1_Rec2 <- 1 - exp(-gamma_rec * dt) # Progression through recovery from ICU in hospital bed to eventual discharge (R)
p_Rec2_R <- 1 - exp(-gamma_rec * dt) # Progression through recovery from ICU in hospital bed to eventual discharge (R)

###########################################################################
## Draws for the number of individuals changing between compartments:    ##
###########################################################################

## Draws from binomial distributions for numbers changing between compartments:
n_S_E1[] <- rbinom(S[i], p_S_E1[i]) # Number of newly infected individuals
n_E1_E2[] <- rbinom(E1[i], p_E1_E2) # Number progressing through latent compartments
n_E2_I[] <- rbinom(E2[i], p_E2_I) # Number of new symptom onsets
output(n_E2_I[]) <- TRUE

n_E2_ICase1[] <- rbinom(n_E2_I[i], prob_hosp[i]) # Proportion of the new symptom onsets that will require hospitalisation (note: haven't entered hospital yet, delay between onset and hospitalisation)
output(n_E2_ICase1[]) <- TRUE
n_E2_IMild[] <- n_E2_I[i] - n_E2_ICase1[i] # 1 - Above, the rest of the infections, which we consider to be mild and not require hospitalisation
output(n_E2_IMild[]) <- TRUE
n_IMild_R[] <- rbinom(IMild[i], p_IMild_R) # Number of mild infections recovering
n_ICase1_ICase2[] <- rbinom(ICase1[i], p_ICase1_ICase2) # Number progressing through the onset but not hospitalised compartment
n_ICase2_Hosp[] <- rbinom(ICase2[i], p_ICase2_Hosp) # Number progressing to requiring hospitalisation

# Calculating Mechanical Ventilation Need and Current Capacity
ICU_occ <- sum(IMVGetLive1) + sum(IMVGetLive2) + sum(IMVGetDie1) + sum(IMVGetDie2) # Summing number of infections in compartments that use ICU beds
output(ICU_occ) <- TRUE
number_requiring_IMV[] <- rbinom(n_ICase2_Hosp[i], prob_severe[i]) # Number of new hospitalisations that are going to require mechanical ventilation
total_number_requiring_IMV <- sum(number_requiring_IMV)
output(total_number_requiring_IMV) <- TRUE
total_ICU_req <- ICU_occ + total_number_requiring_IMV
output(total_ICU_req) <- TRUE
current_free_ICUs <- ICU_bed_capacity + sum(n_IMVGetLive2_Rec) + sum(n_IMVGetDie2_D) - ICU_occ # Number of ICU beds that are currently free
total_number_get_IMV <- if(current_free_ICUs <= 0) 0 else(if(current_free_ICUs - total_number_requiring_IMV >= 0) total_number_requiring_IMV else(current_free_ICUs)) # Working out the number of new ICU requiring infections that get a bed

imv_multinom_prob[] <- if(total_number_requiring_IMV == 0) p_dist[i]/N_age else ((number_requiring_IMV[i]*p_dist[i])/total_number_requiring_IMV)
#number_get_IMV[] <-  rmultinom(total_number_get_IMV, imv_multinom_prob) # Assigning the new ICU requiring cases to the available ICU beds based numbers needing and probability of being chosen for bed

number_get_IMV[1] <- if (current_free_ICUs - total_number_requiring_IMV >= 0) number_requiring_IMV[1] else (if (total_number_get_IMV <= 0) 0 else min(number_requiring_IMV[1], rbinom(total_number_get_IMV, imv_multinom_prob[1]/sum(imv_multinom_prob[1:17]))))
number_get_IMV[2] <- if (current_free_ICUs - total_number_requiring_IMV >= 0) number_requiring_IMV[2] else (if (total_number_get_IMV - number_get_IMV[1] <= 0) 0 else min(number_requiring_IMV[2], rbinom(total_number_get_IMV - number_get_IMV[1], imv_multinom_prob[2]/sum(imv_multinom_prob[2:17]))))
number_get_IMV[3] <- if (current_free_ICUs - total_number_requiring_IMV >= 0) number_requiring_IMV[3] else (if ((total_number_get_IMV - sum(number_get_IMV[1:2])) <= 0) 0 else min(number_requiring_IMV[3], rbinom(total_number_get_IMV - sum(number_get_IMV[1:2]), imv_multinom_prob[3]/sum(imv_multinom_prob[3:17]))))
number_get_IMV[4] <- if (current_free_ICUs - total_number_requiring_IMV >= 0) number_requiring_IMV[4] else (if ((total_number_get_IMV - sum(number_get_IMV[1:3])) <= 0) 0 else min(number_requiring_IMV[4], rbinom(total_number_get_IMV - sum(number_get_IMV[1:3]), imv_multinom_prob[4]/sum(imv_multinom_prob[4:17]))))
number_get_IMV[5] <- if (current_free_ICUs - total_number_requiring_IMV >= 0) number_requiring_IMV[5] else (if ((total_number_get_IMV - sum(number_get_IMV[1:4])) <= 0) 0 else min(number_requiring_IMV[5], rbinom(total_number_get_IMV - sum(number_get_IMV[1:4]), imv_multinom_prob[5]/sum(imv_multinom_prob[5:17]))))
number_get_IMV[6] <- if (current_free_ICUs - total_number_requiring_IMV >= 0) number_requiring_IMV[6] else (if ((total_number_get_IMV - sum(number_get_IMV[1:5])) <= 0) 0 else min(number_requiring_IMV[6], rbinom(total_number_get_IMV - sum(number_get_IMV[1:5]), imv_multinom_prob[6]/sum(imv_multinom_prob[6:17]))))
number_get_IMV[7] <- if (current_free_ICUs - total_number_requiring_IMV >= 0) number_requiring_IMV[7] else (if ((total_number_get_IMV - sum(number_get_IMV[1:6])) <= 0) 0 else min(number_requiring_IMV[7], rbinom(total_number_get_IMV - sum(number_get_IMV[1:6]), imv_multinom_prob[7]/sum(imv_multinom_prob[7:17]))))
number_get_IMV[8] <- if (current_free_ICUs - total_number_requiring_IMV >= 0) number_requiring_IMV[8] else (if ((total_number_get_IMV - sum(number_get_IMV[1:7])) <= 0) 0 else min(number_requiring_IMV[8], rbinom(total_number_get_IMV - sum(number_get_IMV[1:7]), imv_multinom_prob[8]/sum(imv_multinom_prob[8:17]))))
number_get_IMV[9] <- if (current_free_ICUs - total_number_requiring_IMV >= 0) number_requiring_IMV[9] else (if ((total_number_get_IMV - sum(number_get_IMV[1:8])) <= 0) 0 else min(number_requiring_IMV[9], rbinom(total_number_get_IMV - sum(number_get_IMV[1:8]), imv_multinom_prob[9]/sum(imv_multinom_prob[9:17]))))
number_get_IMV[10] <- if (current_free_ICUs - total_number_requiring_IMV >= 0) number_requiring_IMV[10] else (if ((total_number_get_IMV - sum(number_get_IMV[1:9])) <= 0) 0 else min(number_requiring_IMV[10], rbinom(total_number_get_IMV - sum(number_get_IMV[1:9]), imv_multinom_prob[10]/sum(imv_multinom_prob[10:17]))))
number_get_IMV[11] <- if (current_free_ICUs - total_number_requiring_IMV >= 0) number_requiring_IMV[11] else (if ((total_number_get_IMV - sum(number_get_IMV[1:10])) <= 0) 0 else min(number_requiring_IMV[11], rbinom(total_number_get_IMV - sum(number_get_IMV[1:10]), imv_multinom_prob[11]/sum(imv_multinom_prob[11:17]))))
number_get_IMV[12] <- if (current_free_ICUs - total_number_requiring_IMV >= 0) number_requiring_IMV[12] else (if ((total_number_get_IMV - sum(number_get_IMV[1:11])) <= 0) 0 else min(number_requiring_IMV[12], rbinom(total_number_get_IMV - sum(number_get_IMV[1:11]), imv_multinom_prob[12]/sum(imv_multinom_prob[12:17]))))
number_get_IMV[13] <- if (current_free_ICUs - total_number_requiring_IMV >= 0) number_requiring_IMV[13] else (if ((total_number_get_IMV - sum(number_get_IMV[1:12])) <= 0) 0 else min(number_requiring_IMV[13], rbinom(total_number_get_IMV - sum(number_get_IMV[1:12]), imv_multinom_prob[13]/sum(imv_multinom_prob[13:17]))))
number_get_IMV[14] <- if (current_free_ICUs - total_number_requiring_IMV >= 0) number_requiring_IMV[14] else (if ((total_number_get_IMV - sum(number_get_IMV[1:13])) <= 0) 0 else min(number_requiring_IMV[14], rbinom(total_number_get_IMV - sum(number_get_IMV[1:13]), imv_multinom_prob[14]/sum(imv_multinom_prob[14:17]))))
number_get_IMV[15] <- if (current_free_ICUs - total_number_requiring_IMV >= 0) number_requiring_IMV[15] else (if ((total_number_get_IMV - sum(number_get_IMV[1:14])) <= 0) 0 else min(number_requiring_IMV[15], rbinom(total_number_get_IMV - sum(number_get_IMV[1:14]), imv_multinom_prob[15]/sum(imv_multinom_prob[15:17]))))
number_get_IMV[16] <- if (current_free_ICUs - total_number_requiring_IMV >= 0) number_requiring_IMV[16] else (if ((total_number_get_IMV - sum(number_get_IMV[1:15])) <= 0) 0 else min(number_requiring_IMV[16], rbinom(total_number_get_IMV - sum(number_get_IMV[1:15]), imv_multinom_prob[16]/sum(imv_multinom_prob[16:17]))))
number_get_IMV[17] <- if (current_free_ICUs - total_number_requiring_IMV >= 0) number_requiring_IMV[17] else (if ((total_number_get_IMV - sum(number_get_IMV[1:16])) <= 0) 0 else min(number_requiring_IMV[17], rbinom(total_number_get_IMV - sum(number_get_IMV[1:16]), imv_multinom_prob[17]/sum(imv_multinom_prob[17:17]))))

output(number_notget_IMV[]) <- TRUE
output(number_requiring_IMV[]) <- TRUE
output(number_get_IMV[]) <- TRUE
# output(imv_multinom_prob[]) <- TRUE
# output(total_number_get_IMV) <- TRUE

n_IMVGetDie1[] <- rbinom(number_get_IMV[i], prob_severe_death_treatment[i]) # Number of individuals requiring mechanical ventilation and who recieve it who die
n_IMVGetDie1_IMVGetDie2[] <- rbinom(IMVGetDie1[i], p_IMVGetDie1_IMVGetDie2) # Progression through the "require and receive mechanical ventilation but still die" compartment
n_IMVGetDie2_D[] <- rbinom(IMVGetDie2[i], p_IMVGetDie2_D) # Progression to death for those in the "require and receive mechanical ventilation but still die" compartment
n_IMVGetLive1[] <- number_get_IMV[i] - n_IMVGetDie1[i] # Number of individuals requiring mechanical ventilation and who receive it and who survive
n_IMVGetLive1_IMVGetLive2[] <- rbinom(IMVGetLive1[i], p_IMVGetLive1_IMVGetLive2) # Progression through the "require and receive mechanical ventilation and survive" compartment
n_IMVGetLive2_Rec[] <- rbinom(IMVGetLive2[i], p_IMVGetLive2_Rec) # Progression through the "require and receive mechanical ventilation and survive" compartment to the "recovering from ICU compartment"

number_notget_IMV[] <- number_requiring_IMV[i] - number_get_IMV[i] # Calculating the number of new ICU requiring cases who do not get an ICU bed
n_IMVNotGetDie1[] <- round(number_notget_IMV[i] * prob_severe_death_no_treatment[i]) # Number of individuals requiring mechanical ventilation, who do not receive it, and who die
n_IMVNotGetDie1_IMVNotGetDie2[] <- rbinom(IMVNotGetDie1[i], p_IMVNotGetDie1_IMVNotGetDie2)  # Progression through the "require but do not receive mechanical ventilation and die" compartment
n_IMVNotGetDie2_D[] <- rbinom(IMVNotGetDie2[i], p_IMVNotGetDie2_D) # Progression to death for those in the "require but do not receive mechanical ventilation and die" compartment
n_IMVNotGetLive1[] <- number_notget_IMV[i] - n_IMVNotGetDie1[i] # Number of individuals requiring mechanical ventilation but who do not receive it and who survive
n_IMVNotGetLive1_IMVNotGetLive2[] <- rbinom(IMVNotGetLive1[i], p_IMVNotGetLive1_IMVNotGetLive2) # Progression through the "require but do not receive mechanical ventilation and who survive" compartment
n_IMVNotGetLive2_R[] <- rbinom(IMVNotGetLive2[i], p_IMVNotGetLive2_R) # Progression through the "require but do not receive mechanical ventilation and who survive" compartment to "Recovered". Note they go to "Recovered" as these individuals never entered hospital

# Oxygen Related Bits
hosp_occ <- sum(IOxGetLive1) + sum(IOxGetLive2) + sum(IOxGetDie1) + sum(IOxGetDie2) + sum(IRec1) + sum(IRec2) # Summing number of infections in compartments that use general hospital beds
number_requiring_Ox[] <- n_ICase2_Hosp[i] - number_requiring_IMV[i] # Number of hospitalisations that are going to require oxygen
total_number_requiring_Ox <- sum(number_requiring_Ox)
current_free_hosp <- hosp_bed_capacity + sum(n_IOxGetDie2_D) + sum(n_IOxGetLive2_R) + sum(n_IRec2_R) - sum(n_IMVGetLive2_Rec) - hosp_occ # Number of hospital beds that are currently free
total_number_get_hosp <- if (current_free_hosp <= 0) 0 else (if(current_free_hosp - total_number_requiring_Ox >= 0) total_number_requiring_Ox else(current_free_hosp)) # Working out the number of new hospital bed requiring infections that get a bed

# multinomial probability determined by number in that age bin and the probability distrbution for choosing individuals from that age
ox_multinom_prob[] <- if(total_number_requiring_Ox == 0) p_dist[i]/N_age else ((number_requiring_Ox[i]*p_dist[i])/total_number_requiring_Ox)
#number_get_Ox[] <- rmultinom(total_number_get_hosp, ox_multinom_prob) # Assigning the new hospital bed requiring cases to the available hospital beds based numbers needing and probability of being chosen for bed

number_get_Ox[1] <- if (current_free_hosp - total_number_get_hosp >= 0) number_requiring_Ox[1] else (if (total_number_get_hosp <=0) 0 else min(number_requiring_Ox[1], rbinom(total_number_get_hosp, ox_multinom_prob[1]/sum(ox_multinom_prob[1:17]))))
number_get_Ox[2] <- if (current_free_hosp - total_number_get_hosp >= 0) number_requiring_Ox[2] else (if (total_number_get_hosp - number_get_Ox[1] <=0) 0 else min(number_requiring_Ox[2], rbinom(total_number_get_hosp - number_get_Ox[1], ox_multinom_prob[2]/sum(ox_multinom_prob[2:17]))))
number_get_Ox[3] <- if (current_free_hosp - total_number_get_hosp >= 0) number_requiring_Ox[3] else (if ((total_number_get_hosp - sum(number_get_Ox[1:2])) <=0) 0 else min(number_requiring_Ox[3], rbinom(total_number_get_hosp - sum(number_get_Ox[1:2]), ox_multinom_prob[3]/sum(ox_multinom_prob[3:17]))))
number_get_Ox[4] <- if (current_free_hosp - total_number_get_hosp >= 0) number_requiring_Ox[4] else (if ((total_number_get_hosp - sum(number_get_Ox[1:3])) <=0) 0 else min(number_requiring_Ox[4], rbinom(total_number_get_hosp - sum(number_get_Ox[1:3]), ox_multinom_prob[4]/sum(ox_multinom_prob[4:17]))))
number_get_Ox[5] <- if (current_free_hosp - total_number_get_hosp >= 0) number_requiring_Ox[5] else (if ((total_number_get_hosp - sum(number_get_Ox[1:4])) <=0) 0 else min(number_requiring_Ox[5], rbinom(total_number_get_hosp - sum(number_get_Ox[1:4]), ox_multinom_prob[5]/sum(ox_multinom_prob[5:17]))))
number_get_Ox[6] <- if (current_free_hosp - total_number_get_hosp >= 0) number_requiring_Ox[6] else (if ((total_number_get_hosp - sum(number_get_Ox[1:5])) <=0) 0 else min(number_requiring_Ox[6], rbinom(total_number_get_hosp - sum(number_get_Ox[1:5]), ox_multinom_prob[6]/sum(ox_multinom_prob[6:17]))))
number_get_Ox[7] <- if (current_free_hosp - total_number_get_hosp >= 0) number_requiring_Ox[7] else (if ((total_number_get_hosp - sum(number_get_Ox[1:6])) <=0) 0 else min(number_requiring_Ox[7], rbinom(total_number_get_hosp - sum(number_get_Ox[1:6]), ox_multinom_prob[7]/sum(ox_multinom_prob[7:17]))))
number_get_Ox[8] <- if (current_free_hosp - total_number_get_hosp >= 0) number_requiring_Ox[8] else (if ((total_number_get_hosp - sum(number_get_Ox[1:7])) <=0) 0 else min(number_requiring_Ox[8], rbinom(total_number_get_hosp - sum(number_get_Ox[1:7]), ox_multinom_prob[8]/sum(ox_multinom_prob[8:17]))))
number_get_Ox[9] <- if (current_free_hosp - total_number_get_hosp >= 0) number_requiring_Ox[9] else (if ((total_number_get_hosp - sum(number_get_Ox[1:8])) <=0) 0 else min(number_requiring_Ox[9], rbinom(total_number_get_hosp - sum(number_get_Ox[1:8]), ox_multinom_prob[9]/sum(ox_multinom_prob[9:17]))))
number_get_Ox[10] <- if (current_free_hosp - total_number_get_hosp >= 0) number_requiring_Ox[10] else (if ((total_number_get_hosp - sum(number_get_Ox[1:9])) <=0) 0 else min(number_requiring_Ox[10], rbinom(total_number_get_hosp - sum(number_get_Ox[1:9]), ox_multinom_prob[10]/sum(ox_multinom_prob[10:17]))))
number_get_Ox[11] <- if (current_free_hosp - total_number_get_hosp >= 0) number_requiring_Ox[11] else (if ((total_number_get_hosp - sum(number_get_Ox[1:10])) <=0) 0 else min(number_requiring_Ox[11], rbinom(total_number_get_hosp - sum(number_get_Ox[1:10]), ox_multinom_prob[11]/sum(ox_multinom_prob[11:17]))))
number_get_Ox[12] <- if (current_free_hosp - total_number_get_hosp >= 0) number_requiring_Ox[12] else (if ((total_number_get_hosp - sum(number_get_Ox[1:11])) <=0) 0 else min(number_requiring_Ox[12], rbinom(total_number_get_hosp - sum(number_get_Ox[1:11]), ox_multinom_prob[12]/sum(ox_multinom_prob[12:17]))))
number_get_Ox[13] <- if (current_free_hosp - total_number_get_hosp >= 0) number_requiring_Ox[13] else (if ((total_number_get_hosp - sum(number_get_Ox[1:12])) <=0) 0 else min(number_requiring_Ox[13], rbinom(total_number_get_hosp - sum(number_get_Ox[1:12]), ox_multinom_prob[13]/sum(ox_multinom_prob[13:17]))))
number_get_Ox[14] <- if (current_free_hosp - total_number_get_hosp >= 0) number_requiring_Ox[14] else (if ((total_number_get_hosp - sum(number_get_Ox[1:13])) <=0) 0 else min(number_requiring_Ox[14], rbinom(total_number_get_hosp - sum(number_get_Ox[1:13]), ox_multinom_prob[14]/sum(ox_multinom_prob[14:17]))))
number_get_Ox[15] <- if (current_free_hosp - total_number_get_hosp >= 0) number_requiring_Ox[15] else (if ((total_number_get_hosp - sum(number_get_Ox[1:14])) <=0) 0 else min(number_requiring_Ox[15], rbinom(total_number_get_hosp - sum(number_get_Ox[1:14]), ox_multinom_prob[15]/sum(ox_multinom_prob[15:17]))))
number_get_Ox[16] <- if (current_free_hosp - total_number_get_hosp >= 0) number_requiring_Ox[16] else (if ((total_number_get_hosp - sum(number_get_Ox[1:15])) <=0) 0 else min(number_requiring_Ox[16], rbinom(total_number_get_hosp - sum(number_get_Ox[1:15]), ox_multinom_prob[16]/sum(ox_multinom_prob[16:17]))))
number_get_Ox[17] <- if (current_free_hosp - total_number_get_hosp >= 0) number_requiring_Ox[17] else (if ((total_number_get_hosp - sum(number_get_Ox[1:16])) <=0) 0 else min(number_requiring_Ox[17], rbinom(total_number_get_hosp - sum(number_get_Ox[1:16]), ox_multinom_prob[17]/sum(ox_multinom_prob[17:17]))))

n_IOxGetDie1[] <- round(number_get_Ox[i] * prob_non_severe_death_treatment[i]) # Number of individuals requiring oxygen and who recieve it who die
n_IOxGetDie1_IOxGetDie2[] <- rbinom(IOxGetDie1[i], p_IOxGetDie1_IOxGetDie2) # Progression through the "require and receive oxygen but still die" compartment
n_IOxGetDie2_D[] <- rbinom(IOxGetDie2[i], p_IOxGetDie2_D) # Progression through the "require and receive oxygen but still die" compartment to death
n_IOxGetLive1[] <- number_get_Ox[i] - n_IOxGetDie1[i]  # Number of individuals requiring oxygen and who receive it and who survive
n_IOxGetLive1_IOxGetLive2[] <- rbinom(IOxGetLive1[i], p_IOxGetLive1_IOxGetLive2) # Progression through the "require and receive oxygen and survive" compartment
n_IOxGetLive2_R[] <- rbinom(IOxGetLive2[i], p_IOxGetLive2_R) # Progression through the "require and receive oxygen and survive" compartment to recovery

number_notget_Ox[] <- number_requiring_Ox[i] - number_get_Ox[i] # Calculating the number of cases requiring a hospital bed and who do not receive it
# output(number_notget_Ox[]) <- TRUE
# output(number_requiring_Ox[]) <- TRUE
# output(number_get_Ox[]) <- TRUE
# output(ox_multinom_prob[]) <- TRUE
# output(total_number_get_hosp) <- TRUE
# output(n_IOxNotGetDie1[]) <- TRUE

n_IOxNotGetDie1[] <- rbinom(number_notget_Ox[i], prob_non_severe_death_no_treatment[i]) # Number of individuals requiring oxygen but do not receive it and who die
n_IOxNotGetDie1_IOxNotGetDie2[] <- rbinom(IOxNotGetDie1[i], p_IOxNotGetDie1_IOxNotGetDie2) # Progression through the "require but do not receive oxygen and die" compartment
n_IOxNotGetDie2_D[] <- rbinom(IOxNotGetDie2[i], p_IOxNotGetDie2_D) # Progression through the "require but do not receive oxygen and die" compartment to death
n_IOxNotGetLive1[] <- number_notget_Ox[i] - n_IOxNotGetDie1[i] # Number of individuals requiring oxygen but who do not receive it and who survive
n_IOxNotGetLive1_IOxNotGetLive2[] <- rbinom(IOxNotGetLive1[i], p_IOxNotGetLive1_IOxNotGetLive2) # Progression through the "require but do not receive oxygen and survive" compartment
n_IOxNotGetLive2_R[] <- rbinom(IOxNotGetLive2[i], p_IOxNotGetLive2_R) # Progression through the "require but do not receive oxygen and survive" compartment to recovery

n_IRec1_IRec2[] <- rbinom(IRec1[i], p_Rec1_Rec2) # Number progressing through ICU recovery compartment
n_IRec2_R[] <- rbinom(IRec2[i], p_Rec2_R) # Number recovering completely

###########################################################################
##  Totalling up the flows in and out of each compartment                ##
###########################################################################

delta_E1[] <- n_S_E1[i] - n_E1_E2[i]
delta_E2[] <- n_E1_E2[i] - n_E2_I[i]
delta_IMild[] <- n_E2_IMild[i] - n_IMild_R[i]
delta_ICase1[] <- n_E2_ICase1[i] - n_ICase1_ICase2[i]
delta_ICase2[] <- n_ICase1_ICase2[i] - n_ICase2_Hosp[i]

delta_IOxGetLive1[] <- n_IOxGetLive1[i] - n_IOxGetLive1_IOxGetLive2[i]
delta_IOxGetLive2[] <- n_IOxGetLive1_IOxGetLive2[i] - n_IOxGetLive2_R[i]
delta_IOxNotGetLive1[] <- n_IOxNotGetLive1[i] - n_IOxNotGetLive1_IOxNotGetLive2[i]
delta_IOxNotGetLive2[] <- n_IOxNotGetLive1_IOxNotGetLive2[i] - n_IOxNotGetLive2_R[i]
delta_IOxGetDie1[] <- n_IOxGetDie1[i] - n_IOxGetDie1_IOxGetDie2[i]
delta_IOxGetDie2[] <- n_IOxGetDie1_IOxGetDie2[i] - n_IOxGetDie2_D[i]
delta_IOxNotGetDie1[] <- n_IOxNotGetDie1[i] - n_IOxNotGetDie1_IOxNotGetDie2[i]
delta_IOxNotGetDie2[] <- n_IOxNotGetDie1_IOxNotGetDie2[i] - n_IOxNotGetDie2_D[i]

delta_IMVGetLive1[] <- n_IMVGetLive1[i] - n_IMVGetLive1_IMVGetLive2[i]
delta_IMVGetLive2[] <- n_IMVGetLive1_IMVGetLive2[i] - n_IMVGetLive2_Rec[i]
delta_IMVNotGetLive1[] <- n_IMVNotGetLive1[i] - n_IMVNotGetLive1_IMVNotGetLive2[i]
delta_IMVNotGetLive2[] <- n_IMVNotGetLive1_IMVNotGetLive2[i] - n_IMVNotGetLive2_R[i]
delta_IMVGetDie1[] <- n_IMVGetDie1[i] - n_IMVGetDie1_IMVGetDie2[i]
delta_IMVGetDie2[] <- n_IMVGetDie1_IMVGetDie2[i] - n_IMVGetDie2_D[i]
delta_IMVNotGetDie1[] <- n_IMVNotGetDie1[i] - n_IMVNotGetDie1_IMVNotGetDie2[i]
delta_IMVNotGetDie2[] <-  n_IMVNotGetDie1_IMVNotGetDie2[i] - n_IMVNotGetDie2_D[i]

delta_IRec1[] <- n_IMVGetLive2_Rec[i] - n_IRec1_IRec2[i]
delta_IRec2[] <- n_IRec1_IRec2[i] - n_IRec2_R[i]
delta_R[] <- n_IOxGetLive2_R[i] + n_IOxNotGetLive2_R[i] + n_IRec2_R[i] + n_IMVNotGetLive2_R[i] + n_IMild_R[i]
delta_D[] <- n_IOxGetDie2_D[i] + n_IOxNotGetDie2_D[i] + n_IMVGetDie2_D[i] + n_IMVNotGetDie2_D[i]
output(delta_D[]) <- TRUE

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

## Interpolation for Hospital and ICU Capacity
hosp_bed_capacity <- interpolate(tt_hosp_beds, hosp_beds, "constant")
#output(hosp_bed_capacity) <- TRUE
tt_hosp_beds[] <- user()
hosp_beds[] <- user()
dim(tt_hosp_beds) <- user()
dim(hosp_beds) <- length(tt_hosp_beds)

ICU_bed_capacity <- interpolate(tt_ICU_beds, ICU_beds, "constant")
#output(ICU_bed_capacity) <- TRUE
tt_ICU_beds[] <- user()
ICU_beds[] <- user()
dim(tt_ICU_beds) <- user()
dim(ICU_beds) <- length(tt_ICU_beds)

## Initial states:
initial(S[]) <- S_0[i]
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
initial(R[]) <- R_0[i]
initial(D[]) <- D_0[i]

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

# For the Flows Between State Variables
dim(delta_E1) <- N_age
dim(delta_E2) <- N_age
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
dim(number_requiring_IMV) <- N_age
dim(number_get_IMV) <- N_age
dim(imv_multinom_prob) <- N_age
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
dim(ox_multinom_prob) <- N_age
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

# Related to Calculating Age-Structured Force of Infection
dim(p_S_E1) <- N_age
dim(n_S_E1) <- N_age
dim(lambda) <- N_age
dim(s_ij) <- c(N_age,N_age)
dim(temp) <- N_age

# Severity Parameters
dim(prob_hosp) <- N_age
dim(prob_severe) <- N_age
dim(prob_non_severe_death_treatment) <- N_age
dim(prob_non_severe_death_no_treatment) <- N_age
dim(prob_severe_death_treatment) <- N_age
dim(prob_severe_death_no_treatment) <- N_age
dim(p_dist) <- N_age

