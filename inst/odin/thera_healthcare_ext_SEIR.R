# KNOWN ISSUES OR RELATED:
# 1) WHAT TO DO ABOUT IREC CASES AND THEIR OXYGEN REQUIREMENTS (AND THEIR HOSPITAL BED REQUIREMENTS, PER THE PREVIOUS ISSUE)???
# 2)

## TIMESTEP RELATED PARAMETERS
##------------------------------------------------------------------------------
dt <- user() # Specified timestep
time <- step * dt # Tracking actual time
N_age <- user() # Number of age groups

## RATES
##------------------------------------------------------------------------------
gamma_E <- user() # passage through latent infection
gamma_IMild <- user() # mild infection to recovery
gamma_ICase <- user() # symptom onset to requiring hospitalisation
gamma_rec <- user() # rate of progression through post-ICU recovery compartment

# Rates Related to Requiring Hospital Bed and Oxygen
gamma_IMod_GetHosp_GetOx_Surv <- user() # through requiring hosp bed and oxygen compartment conditional on getting hosp bed and oxygen and surviving
gamma_IMod_GetHosp_GetOx_Die <- user() # through requiring hosp bed and oxygen compartment conditional on getting hosp bed and oxygen and dying
gamma_IMod_GetHosp_NoOx_Surv <- user() # through requiring hosp bed and oxygen compartment conditional on getting hosp bed but NOT oxygen and surviving
gamma_IMod_GetHosp_NoOx_Die <- user() # through requiring hosp bed and oxygen compartment conditional on getting  hosp bed but NOT oxygen and dying
gamma_IMod_NoHosp_NoOx_Surv <- user() # through requiring hosp bed and oxygen compartment conditional on NOT getting hosp bed and NOT oxygen and surviving
gamma_IMod_NoHosp_NoOx_Die <- user() # through requiring hosp bed and oxygen compartment conditional on NOT getting hosp bed and NOT oxygen and dying

# Rates Related to Requiring ICU Bed and Oxygen
gamma_ISev_GetICU_GetOx_Surv <- user() # through requiring ICU bed and oxygen compartment conditional on getting ICU bed and oxygen and surviving
gamma_ISev_GetICU_GetOx_Die <- user() # through requiring ICU bed and oxygen compartment conditional on getting ICU bed and oxygen and dying
gamma_ISev_GetICU_NoOx_Surv <- user() # through requiring ICU bed and oxygen compartment conditional on getting ICU bed but NOT oxygen and surviving
gamma_ISev_GetICU_NoOx_Die <- user() # through requiring ICU bed and oxygen compartment conditional on getting ICU bed but NOT oxygen and dying
gamma_ISev_NoICU_NoOx_Surv <- user() # through requiring ICU bed and oxygen compartment conditional on NOT getting ICU bed and NOT oxygen and surviving
gamma_ISev_NoICU_NoOx_Die <- user() # through requiring ICU bed and oxygen compartment conditional on NOT getting ICU bed and NOT oxygen and dying
gamma_ICrit_GetICU_GetOx_GetMV_Surv <- user() # through requiring ICU bed, oxygen and MV compartment conditional on getting ICU bed, oxygen and MV and surviving

# Rates Related to Requiring ICU Bed, Oxygen and Mechanical Ventilation
gamma_ICrit_GetICU_GetOx_GetMV_Die <- user() # through requiring ICU bed, oxygen and MV compartment conditional on getting ICU bed, oxygen and MV and dying
gamma_ICrit_GetICU_GetOx_NoMV_Surv <- user() # through requiring ICU bed, oxygen and MV compartment conditional on getting ICU bed and oxygen, but NOT MV and surviving
gamma_ICrit_GetICU_GetOx_NoMV_Die <- user() # through requiring ICU bed, oxygen and MV compartment conditional on getting ICU bed and oxygen, but NOT MV and dying
gamma_ICrit_GetICU_NoOx_NoMV_Surv <- user() # through requiring ICU bed, oxygen and MV compartment conditional on getting ICU bed, but NOT oxygen and NOT MV and surviving
gamma_ICrit_GetICU_NoOx_NoMV_Die <- user() # through requiring ICU bed, oxygen and MV compartment conditional on getting ICU bed, but NOT oxygen and NOT MV and dying
gamma_ICrit_NoICU_NoOx_NoMV_Surv <- user() # through requiring ICU bed, oxygen and MV compartment conditional on NOT getting ICU bed, NOT oxygen and NOT MV and surviving
gamma_ICrit_NoICU_NoOx_NoMV_Die <- user() # through requiring ICU bed, oxygen and MV compartment conditional on NOT getting ICU bed, NOT oxygen and NOT MV and dying

## PROBABILITIES
##------------------------------------------------------------------------------
prob_hosp[] <- user() # probability of requiring hospitalisation by age
prob_severe[] <- user() # probability of severe disease (requiring ICU bed) by age
prob_critical[] <- user() # probability of critical disease (requiring ICU bed AND MV) by age, conditional on having severe disease

# Probabilities Related to Requiring Hospital Bed and Oxygen
prob_moderate_death_get_hosp_get_ox[] <- user() # probability of dying from moderate disease (i.e. requiring hospital bed and oxygen) by age given you receive a hospital bed AND oxygen)
prob_moderate_death_get_hosp_no_ox[] <- user() # probability of dying from moderate disease (i.e. requiring hospital bed and oxygen) by age given you receive a hospital bed BUT no oxygen)
prob_moderate_death_no_hosp_no_ox[] <- user() # probability of dying from moderate disease (i.e. requiring hospital bed and oxygen) by age given you do NOT receive a hospital bed and you do NOT receive oxygen

# Probabilities Related to Requiring ICU Bed and Oxygen
prob_severe_death_get_ICU_get_ox[] <- user() # probability of dying from severe disease (i.e. requiring ICU bed and oxygen) by age given you receive an ICU bed AND oxygen)
prob_severe_death_get_ICU_no_ox[] <- user() # probability of dying from severe disease (i.e. requiring ICU bed and oxygen) by age given you receive an ICU bed BUT no oxygen)
prob_severe_death_no_ICU_no_ox[] <- user() # probability of dying from severe disease (i.e. requiring ICU bed and oxygen) by age given you do NOT receive an ICU bed and you do NOT receive oxygen

# Probabilities Related to Requiring ICU Bed, Oxygen and Mechanical Ventilation
prob_critical_death_get_ICU_get_ox_get_MV[] <- user() # probability of dying from critical disease (i.e. requiring ICU bed, oxygen and MV) by age given you receive an ICU bed AND oxygen AND MV)
prob_critical_death_get_ICU_get_ox_no_MV[] <- user() # probability of dying from critical disease (i.e. requiring ICU bed, oxygen and MV) by age given you receive an ICU bed AND oxygen BUT no MV)
prob_critical_death_get_ICU_no_ox_no_MV[] <- user() # probability of dying from critical disease (i.e. requiring ICU bed, oxygen and MV) by age given you receive an ICU bed BUT no oxygen and you do NOT receive MV
prob_critical_death_no_ICU_no_ox_no_MV[] <- user() # probability of dying from critical disease (i.e. requiring ICU bed, oxygen and MV) by age given you do NOT receive an ICU bed, you do NOT receive oxygen, and you do NOT receive MV

## INDIVIDUAL PROBABILITIES OF TRANSITION BETWEEN COMPARTMENTS
##------------------------------------------------------------------------------
# Transition Probabilities Up Until Hospitalisation/Recovery from Mild Infection
p_S_E1[] <- 1 - exp(-lambda[i] * dt) # Infection - age dependent FOI based on mixing patterns
p_E1_E2 <- 1 - exp(-gamma_E * dt) # Progression through latent infection
p_E2_I <- 1 - exp(-gamma_E * dt) # Progression to onset of infectiousness. Number split between I_Mild and I_Case
p_IMild_R <- 1 - exp(-gamma_IMild * dt) # Recovery from mild disease
p_ICase1_ICase2 <- 1 - exp(-gamma_ICase * dt) # Delay between symptom onset and requiring hospitalisation
p_ICase2_Hosp <- 1 - exp(-gamma_ICase * dt) # Progression to requiring hospitalisation. Number split between I_Oxygen and I_MV

# Transition Probabilities for Those Recovering from ICU
p_Rec1_Rec2 <- 1 - exp(-gamma_rec * dt) # Progression through recovery from ICU in hospital bed to eventual discharge (R)
p_Rec2_R <- 1 - exp(-gamma_rec * dt) # Progression through recovery from ICU in hospital bed to eventual discharge (R)

# Transition Probabilities for Those Requiring Hospital Bed and Oxygen -> Recovery/Death
p_IMod_GetHosp_GetOx_Surv <- 1 - exp(-gamma_IMod_GetHosp_GetOx_Surv * dt) # Progression through requiring hosp bed and oxygen and receiving both -> Recovery
p_IMod_GetHosp_GetOx_Die <- 1 - exp(-gamma_IMod_GetHosp_GetOx_Die * dt) # Progression through requiring hosp bed and oxygen and receiving both -> Dying
p_IMod_GetHosp_NoOx_Surv <- 1 - exp(-gamma_IMod_GetHosp_NoOx_Surv * dt) # Progression through requiring hosp bed and oxygen, receiving hosp bed only -> Recovery
p_IMod_GetHosp_NoOx_Die <- 1 - exp(-gamma_IMod_GetHosp_NoOx_Die * dt) # Progression through requiring hosp bed and oxygen, receiving hosp bed only -> Dying
p_IMod_NoHosp_NoOx_Surv <- 1 - exp(-gamma_IMod_NoHosp_NoOx_Surv * dt) # Progression through requiring hosp bed and oxygen, receiving neither -> Recovery
p_IMod_NoHosp_NoOx_Die <- 1 - exp(-gamma_IMod_NoHosp_NoOx_Die * dt) # Progression through requiring hosp bed and oxygen, receiving neither -> Dying

# Transition Probabilities for Those Requiring ICU Bed and Oxygen -> Recovery/Death
p_ISev_GetICU_GetOx_Surv <- 1 - exp(-gamma_ISev_GetICU_GetOx_Surv * dt) # Progression through requiring ICU bed and oxygen and receiving both -> Recovery
p_ISev_GetICU_GetOx_Die <- 1 - exp(-gamma_ISev_GetICU_GetOx_Die * dt) # Progression through requiring ICU bed and oxygen and receiving both -> Dying
p_ISev_GetICU_NoOx_Surv <- 1 - exp(-gamma_ISev_GetICU_NoOx_Surv * dt) # Progression through requiring ICU bed and oxygen, receiving ICU bed only -> Recovery
p_ISev_GetICU_NoOx_Die <- 1 - exp(-gamma_ISev_GetICU_NoOx_Die * dt) # Progression through requiring hosp bed and oxygen, receiving ICU bed only -> Dying
p_ISev_NoICU_NoOx_Surv <- 1 - exp(-gamma_ISev_NoICU_NoOx_Surv * dt) # Progression through requiring ICU bed and oxygen, receiving neither -> Recovery
p_ISev_NoICU_NoOx_Die <- 1 - exp(-gamma_ISev_NoICU_NoOx_Die * dt) # Progression through requiring ICU bed and oxygen, receiving neither -> Dying

# Transition Probabilities for Those Requiring ICU Bed, Oxygen and Mechanical Ventilation -> Recovery/Death
p_ICrit_GetICU_GetOx_GetMV_Surv <- 1 - exp(-gamma_ICrit_GetICU_GetOx_GetMV_Surv * dt) # Progression through requiring ICU bed, oxygen and MV, and receiving all -> Recovery
p_ICrit_GetICU_GetOx_GetMV_Die <- 1 - exp(-gamma_ICrit_GetICU_GetOx_GetMV_Die * dt) # Progression through requiring ICU bed, oxygen and MV, and receiving all -> Dying
p_ICrit_GetICU_GetOx_NoMV_Surv <- 1 - exp(-gamma_ICrit_GetICU_GetOx_NoMV_Surv * dt) # Progression through requiring ICU bed, oxygen and MV, and receiving ICU bed and oxygen only -> Recovery
p_ICrit_GetICU_GetOx_NoMV_Die <- 1 - exp(-gamma_ICrit_GetICU_GetOx_NoMV_Die * dt) # Progression through requiring ICU bed, oxygen and MV, and receiving ICU bed and oxygen only -> Dying
p_ICrit_GetICU_NoOx_NoMV_Surv <- 1 - exp(-gamma_ICrit_GetICU_NoOx_NoMV_Surv * dt) # Progression through requiring ICU bed, oxygen and MV, receiving ICU bed only -> Recovery
p_ICrit_GetICU_NoOx_NoMV_Die <- 1 - exp(-gamma_ICrit_GetICU_NoOx_NoMV_Die * dt) # Progression through requiring ICU bed, oxygen and MV, receiving ICU bed only -> Dying
p_ICrit_NoICU_NoOx_NoMV_Surv <- 1 - exp(-gamma_ICrit_NoICU_NoOx_NoMV_Surv * dt) # Progression through requiring ICU bed, oxygen and MV, receiving nothing -> Recovery
p_ICrit_NoICU_NoOx_NoMV_Die <- 1 - exp(-gamma_ICrit_NoICU_NoOx_NoMV_Die * dt) # Progression through requiring ICU bed, oxygen and MV, receiving nothing -> Dying


## NUMBER OF INDIVIDUALS LEAVING DIFFERENT COMPARTMENTS
##------------------------------------------------------------------------------

## DRAWS FOR NUMBER OF INDIVIDUALS MOVING BETWEEN NON-HOSPITAL/ICU BED RELATED COMPARTMENTS
##-----------------------------------------------------------------------------------------
n_S_E1[] <- rbinom(S[i], p_S_E1[i]) # Number of newly infected individuals
n_E1_E2[] <- rbinom(E1[i], p_E1_E2) # Number progressing through latent compartments
n_E2_I[] <- rbinom(E2[i], p_E2_I) # Number of new symptom onsets
n_E2_ICase1[] <- rbinom(n_E2_I[i], prob_hosp[i]) # Proportion of the new symptom onsets that will require hospitalisation (note: haven't entered hospital yet, delay between onset and hospitalisation)
n_E2_IMild[] <- n_E2_I[i] - n_E2_ICase1[i] # 1 - Above, the rest of the infections, which we consider to be mild and not require hospitalisation
n_IMild_R[] <- rbinom(IMild[i], p_IMild_R) # Number of mild infections recovering
n_ICase1_ICase2[] <- rbinom(ICase1[i], p_ICase1_ICase2) # Number progressing through the onset but not hospitalised compartment
n_ICase2_Hosp[] <- rbinom(ICase2[i], p_ICase2_Hosp) # Number progressing to requiring hospitalisation
n_IRec1_IRec2[] <- rbinom(IRec1[i], p_Rec1_Rec2) # Number progressing through ICU recovery compartment
n_IRec2_R[] <- rbinom(IRec2[i], p_Rec2_R) # Number recovering completely


##  This section is non-trivial and so a brief description of everything that occurs below
##  is provided:
##    1) The number of ICU beds available is calculated, compared to the number of individuals
##       newly requiring ICU beds, and these individuals are assigned to receive/not get an ICU
##       bed as appropriate.
##    2) The number of hospital beds available is calculated, compared to the number of individuals
##       newly requiring hospital beds, and these individuals are assigned to receive/not get a hospital
##       bed as appropriate.
##    3) The amount of oxygen available is calculated and split amongst hospital bed and ICU bed patients
##       in a manner proportional to the numbers in each group. The amount of people in hospital beds/ICU
##       beds receiving or not receiving oxygen is then calculated.
##    4) For those in ICU beds receiving oxygen, the number of those who are severe (no MV required) and
##       those who are critical (MV required) is calculated and the number of available mechanical ventilators
##       distributed amongst the critical cases in a manner dependent on whether these critical cases
##       received oxygen or not.
##------------------------------------------------------------------------------

## WORKING OUT NUMBER OF ICU BEDS AVAILABILE AND HOW MANY INDIVIDUALS RECEIVE THEM
##--------------------------------------------------------------------------------
number_req_ICU[] <- rbinom(n_ICase2_Hosp[i], prob_severe[i]) # Number of new hospitalisations that are going to require an ICU bed (either with or w/o mechanical ventilation)
total_req_ICU <- sum(number_req_ICU) # Totalling number newly requiring an ICU bed over age groups

# Calculating Current ICU Occupancy and New Occupancy After Taking Into Account Individuals Leaving ICU Beds This Timestep
ICU_occ <- sum(ISev_GetICU_GetOx_Surv1) + sum(ISev_GetICU_GetOx_Surv2) + sum(ISev_GetICU_GetOx_Die1) + sum(ISev_GetICU_GetOx_Die2) +
           sum(ISev_GetICU_NoOx_Surv1) + sum(ISev_GetICU_NoOx_Surv2) + sum(ISev_GetICU_NoOx_Die1) + sum(ISev_GetICU_NoOx_Die2) +
           sum(ICrit_GetICU_GetOx_GetMV_Surv1) + sum(ICrit_GetICU_GetOx_GetMV_Surv2) + sum(ICrit_GetICU_GetOx_GetMV_Die1) + sum(ICrit_GetICU_GetOx_GetMV_Die2) +
           sum(ICrit_GetICU_GetOx_NoMV_Surv1) + sum(ICrit_GetICU_GetOx_NoMV_Surv2) + sum(ICrit_GetICU_GetOx_NoMV_Die1) + sum(ICrit_GetICU_GetOx_NoMV_Die2) +
           sum(ICrit_GetICU_NoOx_NoMV_Surv1) + sum(ICrit_GetICU_NoOx_NoMV_Surv2) + sum(ICrit_GetICU_NoOx_NoMV_Die1) + sum(ICrit_GetICU_NoOx_NoMV_Die2)
current_free_ICU <- ICU_bed_capacity +
                    sum(n_ISev_GetICU_GetOx_Surv2_Rec) + sum(n_ISev_GetICU_GetOx_Die2_D_Hospital) +
                    sum(n_ISev_GetICU_NoOx_Surv2_Rec) + sum(n_ISev_GetICU_NoOx_Die2_D_Hospital) +
                    sum(n_ICrit_GetICU_GetOx_GetMV_Surv2_Rec) + sum(n_ICrit_GetICU_GetOx_GetMV_Die2_D_Hospital) +
                    sum(n_ICrit_GetICU_GetOx_NoMV_Surv2_Rec) + sum(n_ICrit_GetICU_GetOx_NoMV_Die2_D_Hospital) +
                    sum(n_ICrit_GetICU_NoOx_NoMV_Surv2_Rec) + sum(n_ICrit_GetICU_NoOx_NoMV_Die2_D_Hospital) - ICU_occ

# Individuals Getting and Not Getting ICU Beds, and Their Associated Disease Severities
total_GetICU <- if(current_free_ICU <= 0) 0 else(if(current_free_ICU - total_req_ICU >= 0) total_req_ICU else(current_free_ICU)) # Working out the number of new ICU requiring infections that get a bed
number_GetICU[] <- rmhyper(total_GetICU, number_req_ICU) # number who get an ICU bed

number_req_ICU_MV[] <- rbinom(number_GetICU[i], prob_critical[i]) # Number of new ICU admissions that are going to require oxygen and mechanical ventilation
number_req_ICU_Ox[] <- number_GetICU[i] - number_req_ICU_MV[i] # Number of new ICU admissions that going to require oxygen only
total_req_ICU_MV <- sum(number_req_ICU_MV)
total_req_ICU_Ox <- sum(number_req_ICU_Ox)

number_NotICU[] <- number_req_ICU[i] - number_GetICU[i] # number who do not get an ICU bed
number_NotICU_NotOx_NotMV[] <- rbinom(number_NotICU[i], prob_critical[i]) # number who do not get an ICU bed and who require both oxygen and mechanical ventilation
number_NotICU_NotOx[] <- number_NotICU[i] - number_NotICU_NotOx_NotMV[i] # number who do not get an ICU bed and who require oxygen only


## WORKING OUT NUMBER OF HOSPITAL BEDS AVAILABILE AND HOW MANY INDIVIDUALS RECEIVE THEM
##-------------------------------------------------------------------------------------
number_req_Hosp[] <- n_ICase2_Hosp[i] - number_req_ICU[i]  # Number of new hospitalisations that are going to require a hospital bed IS THIS UNCLEAR AND SHOULD IT BE CALLED REQ_HOSP_BED CHANGE
total_req_Hosp <- sum(number_req_Hosp) # Totalling number newly requiring a hospital bed over age groups

# Current Hospital Bed Occupancy
hosp_occ <- sum(IMod_GetHosp_GetOx_Surv1) + sum(IMod_GetHosp_GetOx_Surv2) + sum(IMod_GetHosp_GetOx_Die1) + sum(IMod_GetHosp_GetOx_Die2) +
            sum(IMod_GetHosp_NoOx_Surv1) + sum(IMod_GetHosp_NoOx_Surv2) + sum(IMod_GetHosp_NoOx_Die1) + sum(IMod_GetHosp_NoOx_Die2) +
            sum(IRec1) + sum(IRec2)

# Totting Hospital Bed Occupancy Up After Taking Account of Individuals Leaving Hospital Beds to Recovery and Entering from ICU
current_free_hosp <- hosp_bed_capacity +
                     sum(n_IMod_GetHosp_GetOx_Surv2_R) + sum(n_IMod_GetHosp_GetOx_Die2_D_Hospital) +
                     sum(n_IMod_GetHosp_NoOx_Surv2_R) + sum(n_IMod_GetHosp_NoOx_Die2_D_Hospital) +
                     sum(n_IRec2_R) -
                     sum(n_ISev_GetICU_GetOx_Surv2_Rec) - sum(n_ISev_GetICU_NoOx_Surv2_Rec) -
                     sum(n_ICrit_GetICU_GetOx_GetMV_Surv2_Rec) - sum(n_ICrit_GetICU_GetOx_NoMV_Surv2_Rec) - sum(n_ICrit_GetICU_NoOx_NoMV_Surv2_Rec) - hosp_occ

# Individuals Getting and Not Get Hospital Beds
total_GetHosp <- if (current_free_hosp <= 0) 0 else (if(current_free_hosp - total_req_Hosp >= 0) total_req_Hosp else(current_free_hosp)) # Working out the number of new hospital bed requiring infections that get a bed
number_GetHosp[] <- rmhyper(total_GetHosp, number_req_Hosp)
number_NotHosp[] <- number_req_Hosp[i] - number_GetHosp[i]


## WORKING OUT HOW MUCH OXYGEN IS AVAILABILE AND HOW MANY INDIVIDUALS REQUIRING HOSPITAL/ICU BED RECEIVE IT
##---------------------------------------------------------------------------------------------------------
# Updating Oxyen Availability With New Supply At Each Timestep, Subtract Off Baseline Demand and Add In Any O2 Leftover From Previous Timestep
update(oxygen_availability) <- oxygen_supply + leftover - baseline_oxygen_demand

# Working Out Proportion of Oxygen Going to Hospital Beds vs ICU Beds, and Splitting ICU Oxygen Into Amounts for Each Disease Severity Category
prop_ox_hosp_beds <- if (total_GetHosp == 0 && total_GetICU == 0) 0 else (total_GetHosp/(total_GetHosp + total_GetICU * severe_critical_case_oxygen_consumption_multiplier))
available_oxygen_for_hosp_beds <- round(prop_ox_hosp_beds * oxygen_availability)
available_oxygen_for_ICU_beds <- floor((oxygen_availability - available_oxygen_for_hosp_beds)/severe_critical_case_oxygen_consumption_multiplier)
available_oxygen_for_ICU_MV <- if(total_req_ICU_MV == 0 && total_req_ICU_Ox == 0) 0 else (round(available_oxygen_for_ICU_beds * total_req_ICU_MV/(total_req_ICU_MV + total_req_ICU_Ox))) # if these are 0s we get NAs maybe!!!
available_oxygen_for_ICU_Ox <- available_oxygen_for_ICU_beds - available_oxygen_for_ICU_MV

# Number Getting Hospital Beds Who Receive/Don't Receive Oxygen
total_GetHosp_GetOx <- if(available_oxygen_for_hosp_beds <= 0) 0 else(if(available_oxygen_for_hosp_beds - total_GetHosp >= 0) total_GetHosp else(available_oxygen_for_hosp_beds)) # Working out the number of new ICU requiring infections that get a bed
number_GetHosp_Ox[] <- rmhyper(total_GetHosp_GetOx, number_GetHosp)
number_GetHosp_NoOx[] <- number_GetHosp[i] - number_GetHosp_Ox[i]

# Calculating the Number of Severe Cases (no MV required) Who Get Oxygen
total_GetICU_GetOx_Only <- if(available_oxygen_for_ICU_Ox <= 0) 0 else(if(available_oxygen_for_ICU_Ox - total_req_ICU_Ox >= 0) total_req_ICU_Ox else(available_oxygen_for_ICU_Ox))
number_GetICU_GetOx[] <- rmhyper(total_GetICU_GetOx_Only, number_req_ICU_Ox)
number_GetICU_NoOx[] <- number_req_ICU_Ox[i] - number_GetICU_GetOx[i]

# Calculating the Number of Critical Cases (MV requied) Who Get Oxygen
total_GetICU_GetOx_Need_MV <- if(available_oxygen_for_ICU_MV <= 0) 0 else(if(available_oxygen_for_ICU_MV - total_req_ICU_MV >= 0) total_req_ICU_MV else(available_oxygen_for_ICU_MV))
number_GetICU_GetOx_NeedMV[] <- rmhyper(total_GetICU_GetOx_Need_MV, number_req_ICU_MV)
number_GetICU_NoOx_NeedMV[] <- number_req_ICU_MV[i] - number_GetICU_GetOx_NeedMV[i]

## WORKING OUT HOW MANY MECHANICAL VENTILATORS ARE AVAILABILE AND HOW MANY INDIVIDUALS REQUIRING THEM RECEIVE THEM
##----------------------------------------------------------------------------------------------------------------
# Calculating the Number of Critical Cases Who Have Been Assigned to Receive Oxygen (Hence Are Eligible to Get MV) Who Also Get MV
MV_occ <- sum(ICrit_GetICU_GetOx_GetMV_Surv1) + sum(ICrit_GetICU_GetOx_GetMV_Surv2) + sum(ICrit_GetICU_GetOx_GetMV_Die1) + sum(ICrit_GetICU_GetOx_GetMV_Die2) # Current Mechanical Ventilator Usage
current_free_MV <- MV_capacity + sum(n_ICrit_GetICU_GetOx_GetMV_Surv2_Rec) + sum(n_ICrit_GetICU_GetOx_GetMV_Die2_D_Hospital) - MV_occ # Number of mechanical ventilators that are currently free
total_GetICU_GetOx_GetMV <- if(current_free_MV <= 0) 0 else(if(current_free_MV - total_GetICU_GetOx_Need_MV >= 0) total_GetICU_GetOx_Need_MV else(current_free_MV))
number_GetICU_GetOx_GetMV[] <-  rmhyper(total_GetICU_GetOx_GetMV, number_GetICU_GetOx_NeedMV)
number_GetICU_GetOx_NoMV[] <- number_GetICU_GetOx_NeedMV[i] - number_GetICU_GetOx_GetMV[i]

## TALLYING UP USED AND REMAINING OXYGEN, INCLUDING ANY LEFTOVER, WHICH MAY OR MAY NOT BE CARRIED OVER INTO NEXT TIMESTEP
##----------------------------------------------------------------------------------------------------------------
temp_leftover <- oxygen_supply - baseline_oxygen_demand - sum(number_GetHosp_Ox) - (sum(number_GetICU_GetOx_NeedMV) + sum(number_GetICU_GetOx)) * severe_critical_case_oxygen_consumption_multiplier
leftover <- if (temp_leftover < 0) 0 else (if(temp_leftover >= max_leftover) max_leftover else temp_leftover)
oxygen_needed_overall <- sum(number_req_Hosp) + (sum(number_req_ICU_MV) + sum(number_req_ICU_Ox)) * severe_critical_case_oxygen_consumption_multiplier
oxygen_used <- sum(number_GetHosp_Ox) + (sum(number_GetICU_GetOx_NeedMV) + sum(number_GetICU_GetOx)) * severe_critical_case_oxygen_consumption_multiplier

## CALCULATING THE NUMBER OF INDIVIDUALS MOVING OUT OF HOSPITAL/ICU BED RELATED COMPARTMENTS
##------------------------------------------------------------------------------------------
# Numbers changing between hospital bed related compartments
n_IMod_GetHosp_GetOx_Die1[] <- rbinom(number_GetHosp_Ox[i], prob_moderate_death_get_hosp_get_ox[i])
n_IMod_GetHosp_GetOx_Die1_IMod_GetHosp_GetOx_Die2[] <- rbinom(IMod_GetHosp_GetOx_Die1[i], p_IMod_GetHosp_GetOx_Die) # Number progressing through requiring hosp bed and oxygen and receiving both -> Dying
n_IMod_GetHosp_GetOx_Die2_D_Hospital[] <- rbinom(IMod_GetHosp_GetOx_Die2[i], p_IMod_GetHosp_GetOx_Die) # Number progressing through requiring hosp bed and oxygen and receiving both -> Dying
n_IMod_GetHosp_GetOx_Surv1[] <- number_GetHosp_Ox[i] - n_IMod_GetHosp_GetOx_Die1[i]
n_IMod_GetHosp_GetOx_Surv1_IMod_GetHosp_GetOx_Surv2[] <- rbinom(IMod_GetHosp_GetOx_Surv1[i], p_IMod_GetHosp_GetOx_Surv) # Number progressing through requiring hosp bed and oxygen and receiving both -> Recovery
n_IMod_GetHosp_GetOx_Surv2_R[] <- rbinom(IMod_GetHosp_GetOx_Surv2[i], p_IMod_GetHosp_GetOx_Surv) # Number progressing through requiring hosp bed and oxygen and receiving both -> Recovery

n_IMod_GetHosp_NoOx_Die1[] <- rbinom(number_GetHosp_NoOx[i], prob_moderate_death_get_hosp_no_ox[i])
n_IMod_GetHosp_NoOx_Die1_IMod_GetHosp_NoOx_Die2[] <- rbinom(IMod_GetHosp_NoOx_Die1[i], p_IMod_GetHosp_NoOx_Die) # Number progressing through requiring hosp bed and oxygen, receiving hosp bed only -> Dying
n_IMod_GetHosp_NoOx_Die2_D_Hospital[] <- rbinom(IMod_GetHosp_NoOx_Die2[i], p_IMod_GetHosp_NoOx_Die) # Number progressing through requiring hosp bed and oxygen, receiving hosp bed only -> Dying
n_IMod_GetHosp_NoOx_Surv1[] <- number_GetHosp_NoOx[i] - n_IMod_GetHosp_NoOx_Die1[i]
n_IMod_GetHosp_NoOx_Surv1_IMod_GetHosp_NoOx_Surv2[] <- rbinom(IMod_GetHosp_NoOx_Surv1[i], p_IMod_GetHosp_NoOx_Surv) # Number progressing through requiring hosp bed and oxygen, receiving hosp bed only -> Recovery
n_IMod_GetHosp_NoOx_Surv2_R[] <- rbinom(IMod_GetHosp_NoOx_Surv2[i], p_IMod_GetHosp_NoOx_Surv) # Number progressing through requiring hosp bed and oxygen, receiving hosp bed only -> Recovery

n_IMod_NoHosp_NoOx_Die1[] <- rbinom(number_NotHosp[i], prob_moderate_death_no_hosp_no_ox[i])
n_IMod_NoHosp_NoOx_Die1_IMod_NoHosp_NoOx_Die2[] <- rbinom(IMod_NoHosp_NoOx_Die1[i], p_IMod_NoHosp_NoOx_Die) # Number progressing through requiring hosp bed and oxygen, receiving neither -> Dying
n_IMod_NoHosp_NoOx_Die2_D_Community[] <- rbinom(IMod_NoHosp_NoOx_Die2[i], p_IMod_NoHosp_NoOx_Die) # Number progressing through requiring hosp bed and oxygen, receiving neither -> Dying
n_IMod_NoHosp_NoOx_Surv1[] <- number_NotHosp[i] - n_IMod_NoHosp_NoOx_Die1[i]
n_IMod_NoHosp_NoOx_Surv1_IMod_NoHosp_NoOx_Surv2[] <- rbinom(IMod_NoHosp_NoOx_Surv1[i], p_IMod_NoHosp_NoOx_Surv) # Number progressing through requiring hosp bed and oxygen, receiving neither -> Recovery
n_IMod_NoHosp_NoOx_Surv2_R[] <- rbinom(IMod_NoHosp_NoOx_Surv2[i], p_IMod_NoHosp_NoOx_Surv) # Number progressing through requiring hosp bed and oxygen, receiving neither -> Recovery

# Numbers changing between ICU bed/non-mechanical ventilation related compartments
n_ISev_GetICU_GetOx_Die1[] <- rbinom(number_GetICU_GetOx[i], prob_severe_death_get_ICU_get_ox[i])
n_ISev_GetICU_GetOx_Die1_ISev_GetICU_GetOx_Die2[] <- rbinom(ISev_GetICU_GetOx_Die1[i], p_ISev_GetICU_GetOx_Die) # Number progressing through requiring ICU bed and oxygen and receiving both -> Dying
n_ISev_GetICU_GetOx_Die2_D_Hospital[] <- rbinom(ISev_GetICU_GetOx_Die2[i], p_ISev_GetICU_GetOx_Die) # Number progressing through requiring ICU bed and oxygen and receiving both -> Dying
n_ISev_GetICU_GetOx_Surv1[] <- number_GetICU_GetOx[i] - n_ISev_GetICU_GetOx_Die1[i]
n_ISev_GetICU_GetOx_Surv1_ISev_GetICU_GetOx_Surv2[] <- rbinom(ISev_GetICU_GetOx_Surv1[i], p_ISev_GetICU_GetOx_Surv) # Number progressing through requiring ICU bed and oxygen and receiving both -> Recovery
n_ISev_GetICU_GetOx_Surv2_Rec[] <- rbinom(ISev_GetICU_GetOx_Surv2[i], p_ISev_GetICU_GetOx_Surv) # Number progressing through requiring ICU bed and oxygen and receiving both -> Recovery

n_ISev_GetICU_NoOx_Die1[] <-  rbinom(number_GetICU_NoOx[i], prob_severe_death_get_ICU_no_ox[i])
n_ISev_GetICU_NoOx_Die1_ISev_GetICU_NoOx_Die2[] <- rbinom(ISev_GetICU_NoOx_Die1[i], p_ISev_GetICU_NoOx_Die) # Number progressing through requiring hosp bed and oxygen, receiving ICU bed only -> Dying
n_ISev_GetICU_NoOx_Die2_D_Hospital[] <- rbinom(ISev_GetICU_NoOx_Die2[i], p_ISev_GetICU_NoOx_Die) # Number progressing through requiring hosp bed and oxygen, receiving ICU bed only -> Dying
n_ISev_GetICU_NoOx_Surv1[] <- number_GetICU_NoOx[i] - n_ISev_GetICU_NoOx_Die1[i]
n_ISev_GetICU_NoOx_Surv1_ISev_GetICU_NoOx_Surv2[] <- rbinom(ISev_GetICU_NoOx_Surv1[i], p_ISev_GetICU_NoOx_Surv) # Number progressing through requiring ICU bed and oxygen, receiving ICU bed only -> Recovery
n_ISev_GetICU_NoOx_Surv2_Rec[] <- rbinom(ISev_GetICU_NoOx_Surv2[i], p_ISev_GetICU_NoOx_Surv) # Number progressing through requiring ICU bed and oxygen, receiving ICU bed only -> Recovery

n_ISev_NoICU_NoOx_Die1[] <- rbinom(number_NotICU_NotOx[i], prob_severe_death_no_ICU_no_ox[i])
n_ISev_NoICU_NoOx_Die1_ISev_NoICU_NoOx_Die2[] <- rbinom(ISev_NoICU_NoOx_Die1[i], p_ISev_NoICU_NoOx_Die) # Number progressing through requiring ICU bed and oxygen, receiving neither -> Dying
n_ISev_NoICU_NoOx_Die2_D_Community[] <- rbinom(ISev_NoICU_NoOx_Die2[i], p_ISev_NoICU_NoOx_Die) # Number progressing through requiring ICU bed and oxygen, receiving neither -> Dying
n_ISev_NoICU_NoOx_Surv1[] <- number_NotICU_NotOx[i] - n_ISev_NoICU_NoOx_Die1[i]
n_ISev_NoICU_NoOx_Surv1_ISev_NoICU_NoOx_Surv2[] <- rbinom(ISev_NoICU_NoOx_Surv1[i], p_ISev_NoICU_NoOx_Surv) # Number progressing through requiring ICU bed and oxygen, receiving neither -> Recovery
n_ISev_NoICU_NoOx_Surv2_R[] <- rbinom(ISev_NoICU_NoOx_Surv2[i], p_ISev_NoICU_NoOx_Surv) # Number progressing through requiring ICU bed and oxygen, receiving neither -> Recovery

# Numbers changing between ICU bed/mechanical ventilation related compartments
n_ICrit_GetICU_GetOx_GetMV_Die1[] <- rbinom(number_GetICU_GetOx_GetMV[i], prob_critical_death_get_ICU_get_ox_get_MV[i])
n_ICrit_GetICU_GetOx_GetMV_Die1_ICrit_GetICU_GetOx_GetMV_Die2[] <- rbinom(ICrit_GetICU_GetOx_GetMV_Die1[i], p_ICrit_GetICU_GetOx_GetMV_Die) # Number progressing through requiring ICU bed, oxygen and MV, and receiving all -> Dying
n_ICrit_GetICU_GetOx_GetMV_Die2_D_Hospital[] <- rbinom(ICrit_GetICU_GetOx_GetMV_Die2[i], p_ICrit_GetICU_GetOx_GetMV_Die) # Number progressing through requiring ICU bed, oxygen and MV, and receiving all -> Recovery
n_ICrit_GetICU_GetOx_GetMV_Surv1[] <- number_GetICU_GetOx_GetMV[i] - n_ICrit_GetICU_GetOx_GetMV_Die1[i]
n_ICrit_GetICU_GetOx_GetMV_Surv1_ICrit_GetICU_GetOx_GetMV_Surv2[] <- rbinom(ICrit_GetICU_GetOx_GetMV_Surv1[i], p_ICrit_GetICU_GetOx_GetMV_Surv) # Number progressing through requiring ICU bed, oxygen and MV, and receiving all -> Recovery
n_ICrit_GetICU_GetOx_GetMV_Surv2_Rec[] <- rbinom(ICrit_GetICU_GetOx_GetMV_Surv2[i], p_ICrit_GetICU_GetOx_GetMV_Surv) # Number progressing through requiring ICU bed, oxygen and MV, and receiving all -> Recovery

n_ICrit_GetICU_GetOx_NoMV_Die1[] <- rbinom(number_GetICU_GetOx_NoMV[i], prob_critical_death_get_ICU_get_ox_no_MV[i])
n_ICrit_GetICU_GetOx_NoMV_Die1_ICrit_GetICU_GetOx_NoMV_Die2[] <- rbinom(ICrit_GetICU_GetOx_NoMV_Die1[i], p_ICrit_GetICU_GetOx_NoMV_Die) # Number progressing through requiring ICU bed, oxygen and MV, and receiving ICU bed and oxygen only -> Dying
n_ICrit_GetICU_GetOx_NoMV_Die2_D_Hospital[] <- rbinom(ICrit_GetICU_GetOx_NoMV_Die2[i], p_ICrit_GetICU_GetOx_NoMV_Die) # Number progressing through requiring ICU bed, oxygen and MV, and receiving ICU bed and oxygen only -> Dying
n_ICrit_GetICU_GetOx_NoMV_Surv1[] <- number_GetICU_GetOx_NoMV[i] - n_ICrit_GetICU_GetOx_NoMV_Die1[i]
n_ICrit_GetICU_GetOx_NoMV_Surv1_ICrit_GetICU_GetOx_NoMV_Surv2[] <- rbinom(ICrit_GetICU_GetOx_NoMV_Surv1[i], p_ICrit_GetICU_GetOx_NoMV_Surv) # Number progressing through requiring ICU bed, oxygen and MV, and receiving ICU bed and oxygen only -> Recovery
n_ICrit_GetICU_GetOx_NoMV_Surv2_Rec[] <- rbinom(ICrit_GetICU_GetOx_NoMV_Surv2[i], p_ICrit_GetICU_GetOx_NoMV_Surv) # Number progressing through requiring ICU bed, oxygen and MV, and receiving ICU bed and oxygen only -> Recovery

n_ICrit_GetICU_NoOx_NoMV_Die1[] <- rbinom(number_GetICU_NoOx_NeedMV[i], prob_critical_death_get_ICU_no_ox_no_MV[i])
n_ICrit_GetICU_NoOx_NoMV_Die1_ICrit_GetICU_NoOx_NoMV_Die2[] <- rbinom(ICrit_GetICU_NoOx_NoMV_Die1[i], p_ICrit_GetICU_NoOx_NoMV_Die) # Number progressing through requiring ICU bed, oxygen and MV, receiving ICU bed only -> Dying
n_ICrit_GetICU_NoOx_NoMV_Die2_D_Hospital[] <- rbinom(ICrit_GetICU_NoOx_NoMV_Die2[i], p_ICrit_GetICU_NoOx_NoMV_Die) # Number progressing through requiring ICU bed, oxygen and MV, receiving ICU bed only -> Dying
n_ICrit_GetICU_NoOx_NoMV_Surv1[] <- number_GetICU_NoOx_NeedMV[i] - n_ICrit_GetICU_NoOx_NoMV_Die1[i]
n_ICrit_GetICU_NoOx_NoMV_Surv1_ICrit_GetICU_NoOx_NoMV_Surv2[] <- rbinom(ICrit_GetICU_NoOx_NoMV_Surv1[i], p_ICrit_GetICU_NoOx_NoMV_Surv) # Number progressing through requiring ICU bed, oxygen and MV, receiving ICU bed only -> Recovery
n_ICrit_GetICU_NoOx_NoMV_Surv2_Rec[] <- rbinom(ICrit_GetICU_NoOx_NoMV_Surv2[i], p_ICrit_GetICU_NoOx_NoMV_Surv) # Number progressing through requiring ICU bed, oxygen and MV, receiving ICU bed only -> Recovery

n_ICrit_NoICU_NoOx_NoMV_Die1[] <- rbinom(number_NotICU_NotOx_NotMV[i], prob_critical_death_no_ICU_no_ox_no_MV[i])
n_ICrit_NoICU_NoOx_NoMV_Die1_ICrit_NoICU_NoOx_NoMV_Die2[] <- rbinom(ICrit_NoICU_NoOx_NoMV_Die1[i], p_ICrit_NoICU_NoOx_NoMV_Die) # Number progressing through requiring ICU bed, oxygen and MV, receiving nothing -> Dying
n_ICrit_NoICU_NoOx_NoMV_Die2_D_Community[] <- rbinom(ICrit_NoICU_NoOx_NoMV_Die2[i], p_ICrit_NoICU_NoOx_NoMV_Die) # Number progressing through requiring ICU bed, oxygen and MV, receiving nothing -> Dying
n_ICrit_NoICU_NoOx_NoMV_Surv1[] <- number_NotICU_NotOx_NotMV[i] - n_ICrit_NoICU_NoOx_NoMV_Die1[i]
n_ICrit_NoICU_NoOx_NoMV_Surv1_ICrit_NoICU_NoOx_NoMV_Surv2[] <- rbinom(ICrit_NoICU_NoOx_NoMV_Surv1[i], p_ICrit_NoICU_NoOx_NoMV_Surv) # Number progressing through requiring ICU bed, oxygen and MV, receiving nothing -> Recovery
n_ICrit_NoICU_NoOx_NoMV_Surv2_R[] <- rbinom(ICrit_NoICU_NoOx_NoMV_Surv2[i], p_ICrit_NoICU_NoOx_NoMV_Surv) # Number progressing through requiring ICU bed, oxygen and MV, receiving nothing -> Recovery

## TOTALLING UP THE FLOWS IN AND OUT OF EACH COMPARTMENT
##------------------------------------------------------
# Non-Hospital/ICU Bed Related Compartments
delta_E1[] <- n_S_E1[i] - n_E1_E2[i]
delta_E2[] <- n_E1_E2[i] - n_E2_I[i]
delta_IMild[] <- n_E2_IMild[i] - n_IMild_R[i]
delta_ICase1[] <- n_E2_ICase1[i] - n_ICase1_ICase2[i]
delta_ICase2[] <- n_ICase1_ICase2[i] - n_ICase2_Hosp[i]

# Stepdown Bed, Recovery and Death Related Compartments
# NOTE THAT IF NUMBER ENTERING IREC FROM ICU BEDS IS > NUMBER LEAVING HOSPITAL BEDS, WE
# GO OVER CAPACITY - AS NOTED FOR SQUIRE PREVIOUSLY. ISSUE ISN'T DIFFERENT BUT WE MIGHT WANT
# TO THINK ABOUT ADDRESSING.
delta_IRec1[] <- n_ISev_GetICU_GetOx_Surv2_Rec[i] + n_ISev_GetICU_NoOx_Surv2_Rec[i] +
                 n_ICrit_GetICU_GetOx_GetMV_Surv2_Rec[i]  + n_ICrit_GetICU_GetOx_NoMV_Surv2_Rec[i] + n_ICrit_GetICU_NoOx_NoMV_Surv2_Rec[i] -
                 n_IRec1_IRec2[i]
delta_IRec2[] <- n_IRec1_IRec2[i] - n_IRec2_R[i]
delta_R[] <- n_IMild_R[i] +
             n_IRec2_R[i] +
             n_IMod_GetHosp_GetOx_Surv2_R[i] + n_IMod_GetHosp_NoOx_Surv2_R[i] + n_IMod_NoHosp_NoOx_Surv2_R[i] +
             n_ISev_NoICU_NoOx_Surv2_R[i] +
             n_ICrit_NoICU_NoOx_NoMV_Surv2_R[i]
delta_D_Community[] <- n_IMod_NoHosp_NoOx_Die2_D_Community[i] + n_ISev_NoICU_NoOx_Die2_D_Community[i] + n_ICrit_NoICU_NoOx_NoMV_Die2_D_Community[i]
delta_D_Hospital[] <- n_IMod_GetHosp_GetOx_Die2_D_Hospital[i] + n_IMod_GetHosp_NoOx_Die2_D_Hospital[i] +
                      n_ISev_GetICU_GetOx_Die2_D_Hospital[i] + n_ISev_GetICU_NoOx_Die2_D_Hospital[i] +
                      n_ICrit_GetICU_GetOx_GetMV_Die2_D_Hospital[i] + n_ICrit_GetICU_GetOx_NoMV_Die2_D_Hospital[i] + n_ICrit_GetICU_NoOx_NoMV_Die2_D_Hospital[i]

# Flows In and Out of Hospital Bed Related Compartments
delta_IMod_GetHosp_GetOx_Die1[] <- n_IMod_GetHosp_GetOx_Die1[i] - n_IMod_GetHosp_GetOx_Die1_IMod_GetHosp_GetOx_Die2[i]
delta_IMod_GetHosp_GetOx_Die2[] <- n_IMod_GetHosp_GetOx_Die1_IMod_GetHosp_GetOx_Die2[i] - n_IMod_GetHosp_GetOx_Die2_D_Hospital[i]
delta_IMod_GetHosp_GetOx_Surv1[] <- n_IMod_GetHosp_GetOx_Surv1[i] - n_IMod_GetHosp_GetOx_Surv1_IMod_GetHosp_GetOx_Surv2[i]
delta_IMod_GetHosp_GetOx_Surv2[] <- n_IMod_GetHosp_GetOx_Surv1_IMod_GetHosp_GetOx_Surv2[i] - n_IMod_GetHosp_GetOx_Surv2_R[i]
delta_IMod_GetHosp_NoOx_Die1[] <- n_IMod_GetHosp_NoOx_Die1[i] - n_IMod_GetHosp_NoOx_Die1_IMod_GetHosp_NoOx_Die2[i]
delta_IMod_GetHosp_NoOx_Die2[] <- n_IMod_GetHosp_NoOx_Die1_IMod_GetHosp_NoOx_Die2[i] - n_IMod_GetHosp_NoOx_Die2_D_Hospital[i]
delta_IMod_GetHosp_NoOx_Surv1[] <- n_IMod_GetHosp_NoOx_Surv1[i] - n_IMod_GetHosp_NoOx_Surv1_IMod_GetHosp_NoOx_Surv2[i]
delta_IMod_GetHosp_NoOx_Surv2[] <- n_IMod_GetHosp_NoOx_Surv1_IMod_GetHosp_NoOx_Surv2[i] - n_IMod_GetHosp_NoOx_Surv2_R[i]
delta_IMod_NoHosp_NoOx_Die1[] <- n_IMod_NoHosp_NoOx_Die1[i] - n_IMod_NoHosp_NoOx_Die1_IMod_NoHosp_NoOx_Die2[i]
delta_IMod_NoHosp_NoOx_Die2[] <- n_IMod_NoHosp_NoOx_Die1_IMod_NoHosp_NoOx_Die2[i] - n_IMod_NoHosp_NoOx_Die2_D_Community[i]
delta_IMod_NoHosp_NoOx_Surv1[] <- n_IMod_NoHosp_NoOx_Surv1[i] - n_IMod_NoHosp_NoOx_Surv1_IMod_NoHosp_NoOx_Surv2[i]
delta_IMod_NoHosp_NoOx_Surv2[] <- n_IMod_NoHosp_NoOx_Surv1_IMod_NoHosp_NoOx_Surv2[i] - n_IMod_NoHosp_NoOx_Surv2_R[i]

# Flows In and Out of ICU Bed & Oxygen Only Related Compartments
delta_ISev_GetICU_GetOx_Die1[] <- n_ISev_GetICU_GetOx_Die1[i] - n_ISev_GetICU_GetOx_Die1_ISev_GetICU_GetOx_Die2[i]
delta_ISev_GetICU_GetOx_Die2[] <- n_ISev_GetICU_GetOx_Die1_ISev_GetICU_GetOx_Die2[i] - n_ISev_GetICU_GetOx_Die2_D_Hospital[i]
delta_ISev_GetICU_GetOx_Surv1[] <- n_ISev_GetICU_GetOx_Surv1[i] - n_ISev_GetICU_GetOx_Surv1_ISev_GetICU_GetOx_Surv2[i]
delta_ISev_GetICU_GetOx_Surv2[] <- n_ISev_GetICU_GetOx_Surv1_ISev_GetICU_GetOx_Surv2[i] - n_ISev_GetICU_GetOx_Surv2_Rec[i]
delta_ISev_GetICU_NoOx_Die1[] <- n_ISev_GetICU_NoOx_Die1[i] - n_ISev_GetICU_NoOx_Die1_ISev_GetICU_NoOx_Die2[i]
delta_ISev_GetICU_NoOx_Die2[] <- n_ISev_GetICU_NoOx_Die1_ISev_GetICU_NoOx_Die2[i] - n_ISev_GetICU_NoOx_Die2_D_Hospital[i]
delta_ISev_GetICU_NoOx_Surv1[] <- n_ISev_GetICU_NoOx_Surv1[i] - n_ISev_GetICU_NoOx_Surv1_ISev_GetICU_NoOx_Surv2[i]
delta_ISev_GetICU_NoOx_Surv2[] <- n_ISev_GetICU_NoOx_Surv1_ISev_GetICU_NoOx_Surv2[i] - n_ISev_GetICU_NoOx_Surv2_Rec[i]
delta_ISev_NoICU_NoOx_Die1[] <- n_ISev_NoICU_NoOx_Die1[i] - n_ISev_NoICU_NoOx_Die1_ISev_NoICU_NoOx_Die2[i]
delta_ISev_NoICU_NoOx_Die2[] <- n_ISev_NoICU_NoOx_Die1_ISev_NoICU_NoOx_Die2[i] - n_ISev_NoICU_NoOx_Die2_D_Community[i]
delta_ISev_NoICU_NoOx_Surv1[] <- n_ISev_NoICU_NoOx_Surv1[i] - n_ISev_NoICU_NoOx_Surv1_ISev_NoICU_NoOx_Surv2[i]
delta_ISev_NoICU_NoOx_Surv2[] <- n_ISev_NoICU_NoOx_Surv1_ISev_NoICU_NoOx_Surv2[i] - n_ISev_NoICU_NoOx_Surv2_R[i]

# Flows In and Out of ICU Bed, Oxygen and Mechanical Ventilation Related Compartments
delta_ICrit_GetICU_GetOx_GetMV_Die1[] <- n_ICrit_GetICU_GetOx_GetMV_Die1[i] - n_ICrit_GetICU_GetOx_GetMV_Die1_ICrit_GetICU_GetOx_GetMV_Die2[i]
delta_ICrit_GetICU_GetOx_GetMV_Die2[] <- n_ICrit_GetICU_GetOx_GetMV_Die1_ICrit_GetICU_GetOx_GetMV_Die2[i] - n_ICrit_GetICU_GetOx_GetMV_Die2_D_Hospital[i]
delta_ICrit_GetICU_GetOx_GetMV_Surv1[] <- n_ICrit_GetICU_GetOx_GetMV_Surv1[i] - n_ICrit_GetICU_GetOx_GetMV_Surv1_ICrit_GetICU_GetOx_GetMV_Surv2[i]
delta_ICrit_GetICU_GetOx_GetMV_Surv2[] <- n_ICrit_GetICU_GetOx_GetMV_Surv1_ICrit_GetICU_GetOx_GetMV_Surv2[i] - n_ICrit_GetICU_GetOx_GetMV_Surv2_Rec[i]
delta_ICrit_GetICU_GetOx_NoMV_Die1[] <- n_ICrit_GetICU_GetOx_NoMV_Die1[i] - n_ICrit_GetICU_GetOx_NoMV_Die1_ICrit_GetICU_GetOx_NoMV_Die2[i]
delta_ICrit_GetICU_GetOx_NoMV_Die2[] <- n_ICrit_GetICU_GetOx_NoMV_Die1_ICrit_GetICU_GetOx_NoMV_Die2[i] - n_ICrit_GetICU_GetOx_NoMV_Die2_D_Hospital[i]
delta_ICrit_GetICU_GetOx_NoMV_Surv1[] <- n_ICrit_GetICU_GetOx_NoMV_Surv1[i] - n_ICrit_GetICU_GetOx_NoMV_Surv1_ICrit_GetICU_GetOx_NoMV_Surv2[i]
delta_ICrit_GetICU_GetOx_NoMV_Surv2[] <- n_ICrit_GetICU_GetOx_NoMV_Surv1_ICrit_GetICU_GetOx_NoMV_Surv2[i] - n_ICrit_GetICU_GetOx_NoMV_Surv2_Rec[i]
delta_ICrit_GetICU_NoOx_NoMV_Die1[] <- n_ICrit_GetICU_NoOx_NoMV_Die1[i] - n_ICrit_GetICU_NoOx_NoMV_Die1_ICrit_GetICU_NoOx_NoMV_Die2[i]
delta_ICrit_GetICU_NoOx_NoMV_Die2[] <- n_ICrit_GetICU_NoOx_NoMV_Die1_ICrit_GetICU_NoOx_NoMV_Die2[i] - n_ICrit_GetICU_NoOx_NoMV_Die2_D_Hospital[i]
delta_ICrit_GetICU_NoOx_NoMV_Surv1[] <- n_ICrit_GetICU_NoOx_NoMV_Surv1[i] - n_ICrit_GetICU_NoOx_NoMV_Surv1_ICrit_GetICU_NoOx_NoMV_Surv2[i]
delta_ICrit_GetICU_NoOx_NoMV_Surv2[] <- n_ICrit_GetICU_NoOx_NoMV_Surv1_ICrit_GetICU_NoOx_NoMV_Surv2[i] - n_ICrit_GetICU_NoOx_NoMV_Surv2_Rec[i]
delta_ICrit_NoICU_NoOx_NoMV_Die1[] <- n_ICrit_NoICU_NoOx_NoMV_Die1[i] - n_ICrit_NoICU_NoOx_NoMV_Die1_ICrit_NoICU_NoOx_NoMV_Die2[i]
delta_ICrit_NoICU_NoOx_NoMV_Die2[] <- n_ICrit_NoICU_NoOx_NoMV_Die1_ICrit_NoICU_NoOx_NoMV_Die2[i] - n_ICrit_NoICU_NoOx_NoMV_Die2_D_Community[i]
delta_ICrit_NoICU_NoOx_NoMV_Surv1[] <- n_ICrit_NoICU_NoOx_NoMV_Surv1[i] - n_ICrit_NoICU_NoOx_NoMV_Surv1_ICrit_NoICU_NoOx_NoMV_Surv2[i]
delta_ICrit_NoICU_NoOx_NoMV_Surv2[] <- n_ICrit_NoICU_NoOx_NoMV_Surv1_ICrit_NoICU_NoOx_NoMV_Surv2[i] - n_ICrit_NoICU_NoOx_NoMV_Surv2_R[i]

## UPDATING STATE VARIABLES WITH THE OVERALL TRANSITIONS IN AND OUT OF EACH COMPARTMENT
##-------------------------------------------------------------------------------------
# Passage Through Initial Latent and Infection Stages
update(S[]) <- S[i] - n_S_E1[i]  # Susceptibles (1 comp)
update(E1[]) <- E1[i] + delta_E1[i]  # First of the latent infection compartments (2 comps)
update(E2[]) <- E2[i] + delta_E2[i]  # Second of the latent infection compartments (2 comps)
update(IMild[]) <- IMild[i] + delta_IMild[i]  # Mild infections (1 comp)
update(ICase1[]) <- ICase1[i] + delta_ICase1[i] # First of the compartments for infections that will require hospitalisation (2 comps)
update(ICase2[]) <- ICase2[i] + delta_ICase2[i] # Second of the compartments for infections that will require hospitalisation (2 comps)

# Passage Through Requiring Hospital Bed and Oxygen, Either Receiving Both, Oxygen or Neither, and Surviving or Not
update(IMod_GetHosp_GetOx_Die1[]) <- IMod_GetHosp_GetOx_Die1[i] + delta_IMod_GetHosp_GetOx_Die1[i] # Require hosp bed and oxygen, get both, die (1st)
update(IMod_GetHosp_GetOx_Die2[]) <- IMod_GetHosp_GetOx_Die2[i] + delta_IMod_GetHosp_GetOx_Die2[i] # Require hosp bed and oxygen, get both, die (2nd)
update(IMod_GetHosp_GetOx_Surv1[]) <- IMod_GetHosp_GetOx_Surv1[i] + delta_IMod_GetHosp_GetOx_Surv1[i] # Require hosp bed and oxygen, get both, survive (1st)
update(IMod_GetHosp_GetOx_Surv2[]) <- IMod_GetHosp_GetOx_Surv2[i] + delta_IMod_GetHosp_GetOx_Surv2[i] # Require hosp bed and oxygen, get both, survive (2nd)
update(IMod_GetHosp_NoOx_Die1[]) <- IMod_GetHosp_NoOx_Die1[i] + delta_IMod_GetHosp_NoOx_Die1[i] # Require hosp bed and oxygen, get bed only, die (1st)
update(IMod_GetHosp_NoOx_Die2[]) <- IMod_GetHosp_NoOx_Die2[i] + delta_IMod_GetHosp_NoOx_Die2[i] # Require hosp bed and oxygen, get bed only, die (2nd)
update(IMod_GetHosp_NoOx_Surv1[]) <- IMod_GetHosp_NoOx_Surv1[i] + delta_IMod_GetHosp_NoOx_Surv1[i] # Require hosp bed and oxygen, get bed only, survive (1st)
update(IMod_GetHosp_NoOx_Surv2[]) <- IMod_GetHosp_NoOx_Surv2[i] + delta_IMod_GetHosp_NoOx_Surv2[i] # Require hosp bed and oxygen, get bed only, survive (2nd)
update(IMod_NoHosp_NoOx_Die1[]) <- IMod_NoHosp_NoOx_Die1[i] + delta_IMod_NoHosp_NoOx_Die1[i] # Require hosp bed and oxygen, get neither, die (1st)
update(IMod_NoHosp_NoOx_Die2[]) <- IMod_NoHosp_NoOx_Die2[i] + delta_IMod_NoHosp_NoOx_Die2[i] # Require hosp bed and oxygen, get neither, survive (2nd)
update(IMod_NoHosp_NoOx_Surv1[]) <- IMod_NoHosp_NoOx_Surv1[i] + delta_IMod_NoHosp_NoOx_Surv1[i] # Require hosp bed and oxygen, get neither, survive (1st)
update(IMod_NoHosp_NoOx_Surv2[]) <- IMod_NoHosp_NoOx_Surv2[i] + delta_IMod_NoHosp_NoOx_Surv2[i] # Require hosp bed and oxygen, get neither, survive (2nd)

# Passage Through Requiring ICU Bed and Oxygen, Either Receiving Both, Oxygen or Neither, and Surviving or Not
update(ISev_GetICU_GetOx_Die1[]) <- ISev_GetICU_GetOx_Die1[i] + delta_ISev_GetICU_GetOx_Die1[i] # Require ICU bed and oxygen, get both, die (1st)
update(ISev_GetICU_GetOx_Die2[]) <- ISev_GetICU_GetOx_Die2[i] + delta_ISev_GetICU_GetOx_Die2[i] # Require ICU bed and oxygen, get both, die (2nd)
update(ISev_GetICU_GetOx_Surv1[]) <- ISev_GetICU_GetOx_Surv1[i] + delta_ISev_GetICU_GetOx_Surv1[i] # Require ICU bed and oxygen, get both, survive (1st)
update(ISev_GetICU_GetOx_Surv2[]) <- ISev_GetICU_GetOx_Surv2[i] + delta_ISev_GetICU_GetOx_Surv2[i] # Require ICU bed and oxygen, get both, survive (2nd)
update(ISev_GetICU_NoOx_Die1[]) <- ISev_GetICU_NoOx_Die1[i] + delta_ISev_GetICU_NoOx_Die1[i] # Require ICU bed and oxygen, get ICU bed only, die (1st)
update(ISev_GetICU_NoOx_Die2[]) <- ISev_GetICU_NoOx_Die2[i] + delta_ISev_GetICU_NoOx_Die2[i] # Require ICU bed and oxygen, get ICU bed only, die (2nd)
update(ISev_GetICU_NoOx_Surv1[]) <- ISev_GetICU_NoOx_Surv1[i] + delta_ISev_GetICU_NoOx_Surv1[i] # Require ICU bed and oxygen, get ICU bed only, survive (1st)
update(ISev_GetICU_NoOx_Surv2[]) <- ISev_GetICU_NoOx_Surv2[i] + delta_ISev_GetICU_NoOx_Surv2[i] # Require ICU bed and oxygen, get ICU bed only, survive (2nd)
update(ISev_NoICU_NoOx_Die1[]) <- ISev_NoICU_NoOx_Die1[i] + delta_ISev_NoICU_NoOx_Die1[i] # Require ICU bed and oxygen, get neither, die (1st)
update(ISev_NoICU_NoOx_Die2[]) <- ISev_NoICU_NoOx_Die2[i] + delta_ISev_NoICU_NoOx_Die2[i] # Require ICU bed and oxygen, get neither, die (2nd)
update(ISev_NoICU_NoOx_Surv1[]) <- ISev_NoICU_NoOx_Surv1[i] + delta_ISev_NoICU_NoOx_Surv1[i] # Require ICU bed and oxygen, get neither, suvive (1st)
update(ISev_NoICU_NoOx_Surv2[]) <- ISev_NoICU_NoOx_Surv2[i] + delta_ISev_NoICU_NoOx_Surv2[i] # Require ICU bed and oxygen, get neither, suvive (2nd)

# Passage Through Requiring ICU Bed, Oxygen and Mechanical Ventilation, Either Receiving All, ICU Bed and Oxygen, ICU Bed Only or Nothing, and Surviving or Not
update(ICrit_GetICU_GetOx_GetMV_Die1[]) <- ICrit_GetICU_GetOx_GetMV_Die1[i] + delta_ICrit_GetICU_GetOx_GetMV_Die1[i] # Require ICU bed, oxygen and MV, get all, die (1st)
update(ICrit_GetICU_GetOx_GetMV_Die2[]) <- ICrit_GetICU_GetOx_GetMV_Die2[i] + delta_ICrit_GetICU_GetOx_GetMV_Die2[i] # Require ICU bed, oxygen and MV, get all, die (2nd)
update(ICrit_GetICU_GetOx_GetMV_Surv1[]) <- ICrit_GetICU_GetOx_GetMV_Surv1[i] + delta_ICrit_GetICU_GetOx_GetMV_Surv1[i] # Require ICU bed, oxygen and MV, get all, survive (1st)
update(ICrit_GetICU_GetOx_GetMV_Surv2[]) <- ICrit_GetICU_GetOx_GetMV_Surv2[i] + delta_ICrit_GetICU_GetOx_GetMV_Surv2[i] # Require ICU bed, oxygen and MV, get all, survive (2nd)
update(ICrit_GetICU_GetOx_NoMV_Die1[]) <- ICrit_GetICU_GetOx_NoMV_Die1[i] + delta_ICrit_GetICU_GetOx_NoMV_Die1[i] # Require ICU bed, oxygen and MV, get ICU bed and oxygen only, die (1st)
update(ICrit_GetICU_GetOx_NoMV_Die2[]) <- ICrit_GetICU_GetOx_NoMV_Die2[i] + delta_ICrit_GetICU_GetOx_NoMV_Die2[i] # Require ICU bed, oxygen and MV, get ICU bed and oxygen only, die (2nd)
update(ICrit_GetICU_GetOx_NoMV_Surv1[]) <- ICrit_GetICU_GetOx_NoMV_Surv1[i] + delta_ICrit_GetICU_GetOx_NoMV_Surv1[i] # Require ICU bed, oxygen and MV, get ICU bed and oxygen only, survive (1st)
update(ICrit_GetICU_GetOx_NoMV_Surv2[]) <- ICrit_GetICU_GetOx_NoMV_Surv2[i] + delta_ICrit_GetICU_GetOx_NoMV_Surv2[i] # Require ICU bed, oxygen and MV, get ICU bed and oxygen only, survive (2nd)
update(ICrit_GetICU_NoOx_NoMV_Die1[]) <- ICrit_GetICU_NoOx_NoMV_Die1[i] + delta_ICrit_GetICU_NoOx_NoMV_Die1[i] # Require ICU bed, oxygen and MV, get ICU bed only, die (1st)
update(ICrit_GetICU_NoOx_NoMV_Die2[]) <- ICrit_GetICU_NoOx_NoMV_Die2[i] + delta_ICrit_GetICU_NoOx_NoMV_Die2[i] # Require ICU bed, oxygen and MV, get ICU bed only, die (2nd)
update(ICrit_GetICU_NoOx_NoMV_Surv1[]) <- ICrit_GetICU_NoOx_NoMV_Surv1[i] + delta_ICrit_GetICU_NoOx_NoMV_Surv1[i] # Require ICU bed, oxygen and MV, get ICU bed only, survive (1st)
update(ICrit_GetICU_NoOx_NoMV_Surv2[]) <- ICrit_GetICU_NoOx_NoMV_Surv2[i] + delta_ICrit_GetICU_NoOx_NoMV_Surv2[i] # Require ICU bed, oxygen and MV, get ICU bed only, survive (2nd)
update(ICrit_NoICU_NoOx_NoMV_Die1[]) <- ICrit_NoICU_NoOx_NoMV_Die1[i] + delta_ICrit_NoICU_NoOx_NoMV_Die1[i] # Require ICU bed, oxygen and MV, get nothing, die (1st)
update(ICrit_NoICU_NoOx_NoMV_Die2[]) <- ICrit_NoICU_NoOx_NoMV_Die2[i] + delta_ICrit_NoICU_NoOx_NoMV_Die2[i] # Require ICU bed, oxygen and MV, get nothing, die (2nd)
update(ICrit_NoICU_NoOx_NoMV_Surv1[]) <- ICrit_NoICU_NoOx_NoMV_Surv1[i] + delta_ICrit_NoICU_NoOx_NoMV_Surv1[i] # Require ICU bed, oxygen and MV, get nothing, survive (1st)
update(ICrit_NoICU_NoOx_NoMV_Surv2[]) <- ICrit_NoICU_NoOx_NoMV_Surv2[i] + delta_ICrit_NoICU_NoOx_NoMV_Surv2[i] # Require ICU bed, oxygen and MV, get nothing, survive (2nd)

# Passage Through Recovery, from Mild Infection, Requiring Oxygen or From ICU Post-Requiring Mechanical Ventilation
update(IRec1[]) <- IRec1[i] + delta_IRec1[i] # First of the compartments for those recovering from ICU (2 comps)
update(IRec2[]) <- IRec2[i] + delta_IRec2[i] # Second of the compartments for those recovering from ICU (2 comps)
update(R[]) <- R[i] + delta_R[i] # Recovered
update(D_Community[]) <- D_Community[i] + delta_D_Community[i] # Deaths in the community
update(D_Hospital[]) <- D_Hospital[i] + delta_D_Hospital[i] # Deaths in the community

## COMPUTING THE FORCE OF INFECTION AND INTERPOLATION FOR MIXING MATRIX AND BETA
##------------------------------------------------------------------------------
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

## INTERPOLATION FOR CHANGING HOSPITAL AND ICU BED CAPACITY OVER TIME
##------------------------------------------------------------------------------
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

##  INTERPOLATION FOR PARAMETERS DESCRIBING THE AVAILABILITY OF HEALTHCARE MATERIALS
##----------------------------------------------------------------------------------
oxygen_supply <- interpolate(tt_oxygen_supply, input_oxygen_supply, "constant") # rate of resupply of oxygen
tt_oxygen_supply[] <- user()
input_oxygen_supply[] <- user()
dim(tt_oxygen_supply) <- user()
dim(input_oxygen_supply) <- length(tt_oxygen_supply)

baseline_oxygen_demand <- interpolate(tt_baseline_oxygen_demand, input_baseline_oxygen_demand, "constant") # rate of demand of oxygen
tt_baseline_oxygen_demand[] <- user()
input_baseline_oxygen_demand[] <- user()
dim(tt_baseline_oxygen_demand) <- user()
dim(input_baseline_oxygen_demand) <- length(tt_baseline_oxygen_demand)

severe_critical_case_oxygen_consumption_multiplier <- user() # consumption of oxygen for severe/critical covid-19 cases compared to moderate cases
max_leftover <- user()
MV_capacity <- user() # number of mechanical ventilators available


## DEFINING INITIAL STATES
##------------------------------------------------------------------------------
initial(oxygen_availability) <- oxygen_availability_0
initial(S[]) <- S_0[i]
initial(E1[]) <- E1_0[i]
initial(E2[]) <- E2_0[i]
initial(IMild[]) <- IMild_0[i]
initial(ICase1[]) <- ICase1_0[i]
initial(ICase2[]) <- ICase2_0[i]
initial(IRec1[]) <- IRec1_0[i]
initial(IRec2[]) <- IRec2_0[i]
initial(R[]) <- R_0[i]
initial(D_Community[]) <- D_Community_0[i]
initial(D_Hospital[]) <- D_Hospital_0[i]

initial(IMod_GetHosp_GetOx_Surv1[]) <- IMod_GetHosp_GetOx_Surv1_0[i]
initial(IMod_GetHosp_GetOx_Surv2[]) <- IMod_GetHosp_GetOx_Surv2_0[i]
initial(IMod_GetHosp_GetOx_Die1[]) <- IMod_GetHosp_GetOx_Die1_0[i]
initial(IMod_GetHosp_GetOx_Die2[]) <- IMod_GetHosp_GetOx_Die2_0[i]
initial(IMod_GetHosp_NoOx_Surv1[]) <- IMod_GetHosp_NoOx_Surv1_0[i]
initial(IMod_GetHosp_NoOx_Surv2[]) <- IMod_GetHosp_NoOx_Surv2_0[i]
initial(IMod_GetHosp_NoOx_Die1[]) <- IMod_GetHosp_NoOx_Die1_0[i]
initial(IMod_GetHosp_NoOx_Die2[]) <- IMod_GetHosp_NoOx_Die2_0[i]
initial(IMod_NoHosp_NoOx_Surv1[]) <- IMod_NoHosp_NoOx_Surv1_0[i]
initial(IMod_NoHosp_NoOx_Surv2[]) <- IMod_NoHosp_NoOx_Surv2_0[i]
initial(IMod_NoHosp_NoOx_Die1[]) <- IMod_NoHosp_NoOx_Die1_0[i]
initial(IMod_NoHosp_NoOx_Die2[]) <- IMod_NoHosp_NoOx_Die2_0[i]

initial(ISev_GetICU_GetOx_Surv1[]) <- ISev_GetICU_GetOx_Surv1_0[i]
initial(ISev_GetICU_GetOx_Surv2[]) <- ISev_GetICU_GetOx_Surv2_0[i]
initial(ISev_GetICU_GetOx_Die1[]) <- ISev_GetICU_GetOx_Die1_0[i]
initial(ISev_GetICU_GetOx_Die2[]) <- ISev_GetICU_GetOx_Die2_0[i]
initial(ISev_GetICU_NoOx_Surv1[]) <- ISev_GetICU_NoOx_Surv1_0[i]
initial(ISev_GetICU_NoOx_Surv2[]) <- ISev_GetICU_NoOx_Surv2_0[i]
initial(ISev_GetICU_NoOx_Die1[]) <- ISev_GetICU_NoOx_Die1_0[i]
initial(ISev_GetICU_NoOx_Die2[]) <- ISev_GetICU_NoOx_Die2_0[i]
initial(ISev_NoICU_NoOx_Surv1[]) <- ISev_NoICU_NoOx_Surv1_0[i]
initial(ISev_NoICU_NoOx_Surv2[]) <- ISev_NoICU_NoOx_Surv2_0[i]
initial(ISev_NoICU_NoOx_Die1[]) <- ISev_NoICU_NoOx_Die1_0[i]
initial(ISev_NoICU_NoOx_Die2[]) <- ISev_NoICU_NoOx_Die2_0[i]

initial(ICrit_GetICU_GetOx_GetMV_Surv1[]) <- ICrit_GetICU_GetOx_GetMV_Surv1_0[i]
initial(ICrit_GetICU_GetOx_GetMV_Surv2[]) <- ICrit_GetICU_GetOx_GetMV_Surv2_0[i]
initial(ICrit_GetICU_GetOx_GetMV_Die1[]) <- ICrit_GetICU_GetOx_GetMV_Die1_0[i]
initial(ICrit_GetICU_GetOx_GetMV_Die2[]) <- ICrit_GetICU_GetOx_GetMV_Die2_0[i]
initial(ICrit_GetICU_GetOx_NoMV_Surv1[]) <- ICrit_GetICU_GetOx_NoMV_Surv1_0[i]
initial(ICrit_GetICU_GetOx_NoMV_Surv2[]) <- ICrit_GetICU_GetOx_NoMV_Surv2_0[i]
initial(ICrit_GetICU_GetOx_NoMV_Die1[]) <- ICrit_GetICU_GetOx_NoMV_Die1_0[i]
initial(ICrit_GetICU_GetOx_NoMV_Die2[]) <- ICrit_GetICU_GetOx_NoMV_Die2_0[i]
initial(ICrit_GetICU_NoOx_NoMV_Surv1[]) <- ICrit_GetICU_NoOx_NoMV_Surv1_0[i]
initial(ICrit_GetICU_NoOx_NoMV_Surv2[]) <- ICrit_GetICU_NoOx_NoMV_Surv2_0[i]
initial(ICrit_GetICU_NoOx_NoMV_Die1[]) <- ICrit_GetICU_NoOx_NoMV_Die1_0[i]
initial(ICrit_GetICU_NoOx_NoMV_Die2[]) <- ICrit_GetICU_NoOx_NoMV_Die2_0[i]
initial(ICrit_NoICU_NoOx_NoMV_Surv1[]) <- ICrit_NoICU_NoOx_NoMV_Surv1_0[i]
initial(ICrit_NoICU_NoOx_NoMV_Surv2[]) <- ICrit_NoICU_NoOx_NoMV_Surv2_0[i]
initial(ICrit_NoICU_NoOx_NoMV_Die1[]) <- ICrit_NoICU_NoOx_NoMV_Die1_0[i]
initial(ICrit_NoICU_NoOx_NoMV_Die2[]) <- ICrit_NoICU_NoOx_NoMV_Die2_0[i]

##Initial vectors
oxygen_availability_0 <- user()
S_0[] <- user()
E1_0[] <- user()
E2_0[] <- user()
IMild_0[] <- user()
ICase1_0[] <- user()
ICase2_0[] <- user()
IRec1_0[] <- user()
IRec2_0[] <- user()
R_0[] <- user()
D_Community_0[] <- user()
D_Hospital_0[] <- user()

IMod_GetHosp_GetOx_Surv1_0[] <- user()
IMod_GetHosp_GetOx_Surv2_0[] <- user()
IMod_GetHosp_GetOx_Die1_0[] <- user()
IMod_GetHosp_GetOx_Die2_0[] <- user()
IMod_GetHosp_NoOx_Surv1_0[] <- user()
IMod_GetHosp_NoOx_Surv2_0[] <- user()
IMod_GetHosp_NoOx_Die1_0[] <- user()
IMod_GetHosp_NoOx_Die2_0[] <- user()
IMod_NoHosp_NoOx_Surv1_0[] <- user()
IMod_NoHosp_NoOx_Surv2_0[] <- user()
IMod_NoHosp_NoOx_Die1_0[] <- user()
IMod_NoHosp_NoOx_Die2_0[] <- user()

ISev_GetICU_GetOx_Surv1_0[] <- user()
ISev_GetICU_GetOx_Surv2_0[] <- user()
ISev_GetICU_GetOx_Die1_0[] <- user()
ISev_GetICU_GetOx_Die2_0[] <- user()
ISev_GetICU_NoOx_Surv1_0[] <- user()
ISev_GetICU_NoOx_Surv2_0[] <- user()
ISev_GetICU_NoOx_Die1_0[] <- user()
ISev_GetICU_NoOx_Die2_0[] <- user()
ISev_NoICU_NoOx_Surv1_0[] <- user()
ISev_NoICU_NoOx_Surv2_0[] <- user()
ISev_NoICU_NoOx_Die1_0[] <- user()
ISev_NoICU_NoOx_Die2_0[] <- user()

ICrit_GetICU_GetOx_GetMV_Surv1_0[] <- user()
ICrit_GetICU_GetOx_GetMV_Surv2_0[] <- user()
ICrit_GetICU_GetOx_GetMV_Die1_0[] <- user()
ICrit_GetICU_GetOx_GetMV_Die2_0[] <- user()
ICrit_GetICU_GetOx_NoMV_Surv1_0[] <- user()
ICrit_GetICU_GetOx_NoMV_Surv2_0[] <- user()
ICrit_GetICU_GetOx_NoMV_Die1_0[] <- user()
ICrit_GetICU_GetOx_NoMV_Die2_0[] <- user()
ICrit_GetICU_NoOx_NoMV_Surv1_0[] <- user()
ICrit_GetICU_NoOx_NoMV_Surv2_0[] <- user()
ICrit_GetICU_NoOx_NoMV_Die1_0[] <- user()
ICrit_GetICU_NoOx_NoMV_Die2_0[] <- user()
ICrit_NoICU_NoOx_NoMV_Surv1_0[] <- user()
ICrit_NoICU_NoOx_NoMV_Surv2_0[] <- user()
ICrit_NoICU_NoOx_NoMV_Die1_0[] <- user()
ICrit_NoICU_NoOx_NoMV_Die2_0[] <- user()

##Dimensions of the different "vectors" used
# For the State Variables
dim(S) <- N_age
dim(E1) <- N_age
dim(E2) <- N_age
dim(IMild) <- N_age
dim(ICase1) <- N_age
dim(ICase2) <- N_age
dim(IRec1) <- N_age
dim(IRec2) <- N_age
dim(R) <- N_age
dim(D_Community) <- N_age
dim(D_Hospital) <- N_age

dim(IMod_GetHosp_GetOx_Surv1) <- N_age
dim(IMod_GetHosp_GetOx_Surv2) <- N_age
dim(IMod_GetHosp_GetOx_Die1) <- N_age
dim(IMod_GetHosp_GetOx_Die2) <- N_age
dim(IMod_GetHosp_NoOx_Surv1) <- N_age
dim(IMod_GetHosp_NoOx_Surv2) <- N_age
dim(IMod_GetHosp_NoOx_Die1) <- N_age
dim(IMod_GetHosp_NoOx_Die2) <- N_age
dim(IMod_NoHosp_NoOx_Surv1) <- N_age
dim(IMod_NoHosp_NoOx_Surv2) <- N_age
dim(IMod_NoHosp_NoOx_Die1) <- N_age
dim(IMod_NoHosp_NoOx_Die2) <- N_age

dim(ISev_GetICU_GetOx_Surv1) <- N_age
dim(ISev_GetICU_GetOx_Surv2) <- N_age
dim(ISev_GetICU_GetOx_Die1) <- N_age
dim(ISev_GetICU_GetOx_Die2) <- N_age
dim(ISev_GetICU_NoOx_Surv1) <- N_age
dim(ISev_GetICU_NoOx_Surv2) <- N_age
dim(ISev_GetICU_NoOx_Die1) <- N_age
dim(ISev_GetICU_NoOx_Die2) <- N_age
dim(ISev_NoICU_NoOx_Surv1) <- N_age
dim(ISev_NoICU_NoOx_Surv2) <- N_age
dim(ISev_NoICU_NoOx_Die1) <- N_age
dim(ISev_NoICU_NoOx_Die2) <- N_age

dim(ICrit_GetICU_GetOx_GetMV_Surv1) <- N_age
dim(ICrit_GetICU_GetOx_GetMV_Surv2) <- N_age
dim(ICrit_GetICU_GetOx_GetMV_Die1) <- N_age
dim(ICrit_GetICU_GetOx_GetMV_Die2) <- N_age
dim(ICrit_GetICU_GetOx_NoMV_Surv1) <- N_age
dim(ICrit_GetICU_GetOx_NoMV_Surv2) <- N_age
dim(ICrit_GetICU_GetOx_NoMV_Die1) <- N_age
dim(ICrit_GetICU_GetOx_NoMV_Die2) <- N_age
dim(ICrit_GetICU_NoOx_NoMV_Surv1) <- N_age
dim(ICrit_GetICU_NoOx_NoMV_Surv2) <- N_age
dim(ICrit_GetICU_NoOx_NoMV_Die1) <- N_age
dim(ICrit_GetICU_NoOx_NoMV_Die2) <- N_age
dim(ICrit_NoICU_NoOx_NoMV_Surv1) <- N_age
dim(ICrit_NoICU_NoOx_NoMV_Surv2) <- N_age
dim(ICrit_NoICU_NoOx_NoMV_Die1) <- N_age
dim(ICrit_NoICU_NoOx_NoMV_Die2) <- N_age

# For the Initial Values
dim(S_0) <- N_age
dim(E1_0) <- N_age
dim(E2_0) <- N_age
dim(IMild_0) <- N_age
dim(ICase1_0) <- N_age
dim(ICase2_0) <- N_age
dim(IRec1_0) <- N_age
dim(IRec2_0) <- N_age
dim(R_0) <- N_age
dim(D_Community_0) <- N_age
dim(D_Hospital_0) <- N_age

dim(IMod_GetHosp_GetOx_Surv1_0) <- N_age
dim(IMod_GetHosp_GetOx_Surv2_0) <- N_age
dim(IMod_GetHosp_GetOx_Die1_0) <- N_age
dim(IMod_GetHosp_GetOx_Die2_0) <- N_age
dim(IMod_GetHosp_NoOx_Surv1_0) <- N_age
dim(IMod_GetHosp_NoOx_Surv2_0) <- N_age
dim(IMod_GetHosp_NoOx_Die1_0) <- N_age
dim(IMod_GetHosp_NoOx_Die2_0) <- N_age
dim(IMod_NoHosp_NoOx_Surv1_0) <- N_age
dim(IMod_NoHosp_NoOx_Surv2_0) <- N_age
dim(IMod_NoHosp_NoOx_Die1_0) <- N_age
dim(IMod_NoHosp_NoOx_Die2_0) <- N_age

dim(ISev_GetICU_GetOx_Surv1_0) <- N_age
dim(ISev_GetICU_GetOx_Surv2_0) <- N_age
dim(ISev_GetICU_GetOx_Die1_0) <- N_age
dim(ISev_GetICU_GetOx_Die2_0) <- N_age
dim(ISev_GetICU_NoOx_Surv1_0) <- N_age
dim(ISev_GetICU_NoOx_Surv2_0) <- N_age
dim(ISev_GetICU_NoOx_Die1_0) <- N_age
dim(ISev_GetICU_NoOx_Die2_0) <- N_age
dim(ISev_NoICU_NoOx_Surv1_0) <- N_age
dim(ISev_NoICU_NoOx_Surv2_0) <- N_age
dim(ISev_NoICU_NoOx_Die1_0) <- N_age
dim(ISev_NoICU_NoOx_Die2_0) <- N_age

dim(ICrit_GetICU_GetOx_GetMV_Surv1_0) <- N_age
dim(ICrit_GetICU_GetOx_GetMV_Surv2_0) <- N_age
dim(ICrit_GetICU_GetOx_GetMV_Die1_0) <- N_age
dim(ICrit_GetICU_GetOx_GetMV_Die2_0) <- N_age
dim(ICrit_GetICU_GetOx_NoMV_Surv1_0) <- N_age
dim(ICrit_GetICU_GetOx_NoMV_Surv2_0) <- N_age
dim(ICrit_GetICU_GetOx_NoMV_Die1_0) <- N_age
dim(ICrit_GetICU_GetOx_NoMV_Die2_0) <- N_age
dim(ICrit_GetICU_NoOx_NoMV_Surv1_0) <- N_age
dim(ICrit_GetICU_NoOx_NoMV_Surv2_0) <- N_age
dim(ICrit_GetICU_NoOx_NoMV_Die1_0) <- N_age
dim(ICrit_GetICU_NoOx_NoMV_Die2_0) <- N_age
dim(ICrit_NoICU_NoOx_NoMV_Surv1_0) <- N_age
dim(ICrit_NoICU_NoOx_NoMV_Surv2_0) <- N_age
dim(ICrit_NoICU_NoOx_NoMV_Die1_0) <- N_age
dim(ICrit_NoICU_NoOx_NoMV_Die2_0) <- N_age

# For the Flows Between State Variables
dim(delta_E1) <- N_age
dim(delta_E2) <- N_age
dim(delta_IMild) <- N_age
dim(delta_ICase1) <- N_age
dim(delta_ICase2) <- N_age
dim(delta_IRec1) <- N_age
dim(delta_IRec2) <- N_age
dim(delta_R) <- N_age
dim(delta_D_Community) <- N_age
dim(delta_D_Hospital) <- N_age

dim(delta_IMod_GetHosp_GetOx_Surv1) <- N_age
dim(delta_IMod_GetHosp_GetOx_Surv2) <- N_age
dim(delta_IMod_GetHosp_GetOx_Die1) <- N_age
dim(delta_IMod_GetHosp_GetOx_Die2) <- N_age
dim(delta_IMod_GetHosp_NoOx_Surv1) <- N_age
dim(delta_IMod_GetHosp_NoOx_Surv2) <- N_age
dim(delta_IMod_GetHosp_NoOx_Die1) <- N_age
dim(delta_IMod_GetHosp_NoOx_Die2) <- N_age
dim(delta_IMod_NoHosp_NoOx_Surv1) <- N_age
dim(delta_IMod_NoHosp_NoOx_Surv2) <- N_age
dim(delta_IMod_NoHosp_NoOx_Die1) <- N_age
dim(delta_IMod_NoHosp_NoOx_Die2) <- N_age

dim(delta_ISev_GetICU_GetOx_Surv1) <- N_age
dim(delta_ISev_GetICU_GetOx_Surv2) <- N_age
dim(delta_ISev_GetICU_GetOx_Die1) <- N_age
dim(delta_ISev_GetICU_GetOx_Die2) <- N_age
dim(delta_ISev_GetICU_NoOx_Surv1) <- N_age
dim(delta_ISev_GetICU_NoOx_Surv2) <- N_age
dim(delta_ISev_GetICU_NoOx_Die1) <- N_age
dim(delta_ISev_GetICU_NoOx_Die2) <- N_age
dim(delta_ISev_NoICU_NoOx_Surv1) <- N_age
dim(delta_ISev_NoICU_NoOx_Surv2) <- N_age
dim(delta_ISev_NoICU_NoOx_Die1) <- N_age
dim(delta_ISev_NoICU_NoOx_Die2) <- N_age

dim(delta_ICrit_GetICU_GetOx_GetMV_Surv1) <- N_age
dim(delta_ICrit_GetICU_GetOx_GetMV_Surv2) <- N_age
dim(delta_ICrit_GetICU_GetOx_GetMV_Die1) <- N_age
dim(delta_ICrit_GetICU_GetOx_GetMV_Die2) <- N_age
dim(delta_ICrit_GetICU_GetOx_NoMV_Surv1) <- N_age
dim(delta_ICrit_GetICU_GetOx_NoMV_Surv2) <- N_age
dim(delta_ICrit_GetICU_GetOx_NoMV_Die1) <- N_age
dim(delta_ICrit_GetICU_GetOx_NoMV_Die2) <- N_age
dim(delta_ICrit_GetICU_NoOx_NoMV_Surv1) <- N_age
dim(delta_ICrit_GetICU_NoOx_NoMV_Surv2) <- N_age
dim(delta_ICrit_GetICU_NoOx_NoMV_Die1) <- N_age
dim(delta_ICrit_GetICU_NoOx_NoMV_Die2) <- N_age
dim(delta_ICrit_NoICU_NoOx_NoMV_Surv1) <- N_age
dim(delta_ICrit_NoICU_NoOx_NoMV_Surv2) <- N_age
dim(delta_ICrit_NoICU_NoOx_NoMV_Die1) <- N_age
dim(delta_ICrit_NoICU_NoOx_NoMV_Die2) <- N_age

# For the Number of People Moving In and Out of Compartments
dim(n_E1_E2) <- N_age
dim(n_E2_I) <- N_age
dim(n_E2_ICase1) <- N_age
dim(n_E2_IMild) <- N_age
dim(n_IMild_R) <- N_age
dim(n_ICase1_ICase2) <- N_age
dim(n_ICase2_Hosp) <- N_age
dim(n_IRec1_IRec2) <- N_age
dim(n_IRec2_R) <- N_age

dim(n_IMod_GetHosp_GetOx_Die1) <- N_age
dim(n_IMod_GetHosp_GetOx_Die1_IMod_GetHosp_GetOx_Die2) <- N_age
dim(n_IMod_GetHosp_GetOx_Die2_D_Hospital) <- N_age
dim(n_IMod_GetHosp_GetOx_Surv1) <- N_age
dim(n_IMod_GetHosp_GetOx_Surv1_IMod_GetHosp_GetOx_Surv2) <- N_age
dim(n_IMod_GetHosp_GetOx_Surv2_R) <- N_age
dim(n_IMod_GetHosp_NoOx_Die1) <- N_age
dim(n_IMod_GetHosp_NoOx_Die1_IMod_GetHosp_NoOx_Die2) <- N_age
dim(n_IMod_GetHosp_NoOx_Die2_D_Hospital) <- N_age
dim(n_IMod_GetHosp_NoOx_Surv1) <- N_age
dim(n_IMod_GetHosp_NoOx_Surv1_IMod_GetHosp_NoOx_Surv2) <- N_age
dim(n_IMod_GetHosp_NoOx_Surv2_R) <- N_age
dim(n_IMod_NoHosp_NoOx_Die1) <- N_age
dim(n_IMod_NoHosp_NoOx_Die1_IMod_NoHosp_NoOx_Die2) <- N_age
dim(n_IMod_NoHosp_NoOx_Die2_D_Community) <- N_age
dim(n_IMod_NoHosp_NoOx_Surv1) <- N_age
dim(n_IMod_NoHosp_NoOx_Surv1_IMod_NoHosp_NoOx_Surv2) <- N_age
dim(n_IMod_NoHosp_NoOx_Surv2_R) <- N_age

dim(n_ISev_GetICU_GetOx_Die1) <- N_age
dim(n_ISev_GetICU_GetOx_Die1_ISev_GetICU_GetOx_Die2) <- N_age
dim(n_ISev_GetICU_GetOx_Die2_D_Hospital) <- N_age
dim(n_ISev_GetICU_GetOx_Surv1) <- N_age
dim(n_ISev_GetICU_GetOx_Surv1_ISev_GetICU_GetOx_Surv2) <- N_age
dim(n_ISev_GetICU_GetOx_Surv2_Rec) <- N_age
dim(n_ISev_GetICU_NoOx_Die1) <- N_age
dim(n_ISev_GetICU_NoOx_Die1_ISev_GetICU_NoOx_Die2) <- N_age
dim(n_ISev_GetICU_NoOx_Die2_D_Hospital) <- N_age
dim(n_ISev_GetICU_NoOx_Surv1) <- N_age
dim(n_ISev_GetICU_NoOx_Surv1_ISev_GetICU_NoOx_Surv2) <- N_age
dim(n_ISev_GetICU_NoOx_Surv2_Rec) <- N_age
dim(n_ISev_NoICU_NoOx_Die1) <- N_age
dim(n_ISev_NoICU_NoOx_Die1_ISev_NoICU_NoOx_Die2) <- N_age
dim(n_ISev_NoICU_NoOx_Die2_D_Community) <- N_age
dim(n_ISev_NoICU_NoOx_Surv1) <- N_age
dim(n_ISev_NoICU_NoOx_Surv1_ISev_NoICU_NoOx_Surv2) <- N_age
dim(n_ISev_NoICU_NoOx_Surv2_R) <- N_age

dim(n_ICrit_GetICU_GetOx_GetMV_Die1) <- N_age
dim(n_ICrit_GetICU_GetOx_GetMV_Die1_ICrit_GetICU_GetOx_GetMV_Die2) <- N_age
dim(n_ICrit_GetICU_GetOx_GetMV_Die2_D_Hospital) <- N_age
dim(n_ICrit_GetICU_GetOx_GetMV_Surv1) <- N_age
dim(n_ICrit_GetICU_GetOx_GetMV_Surv1_ICrit_GetICU_GetOx_GetMV_Surv2) <- N_age
dim(n_ICrit_GetICU_GetOx_GetMV_Surv2_Rec) <- N_age
dim(n_ICrit_GetICU_GetOx_NoMV_Die1) <- N_age
dim(n_ICrit_GetICU_GetOx_NoMV_Die1_ICrit_GetICU_GetOx_NoMV_Die2) <- N_age
dim(n_ICrit_GetICU_GetOx_NoMV_Die2_D_Hospital) <- N_age
dim(n_ICrit_GetICU_GetOx_NoMV_Surv1) <- N_age
dim(n_ICrit_GetICU_GetOx_NoMV_Surv1_ICrit_GetICU_GetOx_NoMV_Surv2) <- N_age
dim(n_ICrit_GetICU_GetOx_NoMV_Surv2_Rec) <- N_age
dim(n_ICrit_GetICU_NoOx_NoMV_Die1) <- N_age
dim(n_ICrit_GetICU_NoOx_NoMV_Die1_ICrit_GetICU_NoOx_NoMV_Die2) <- N_age
dim(n_ICrit_GetICU_NoOx_NoMV_Die2_D_Hospital) <- N_age
dim(n_ICrit_GetICU_NoOx_NoMV_Surv1) <- N_age
dim(n_ICrit_GetICU_NoOx_NoMV_Surv1_ICrit_GetICU_NoOx_NoMV_Surv2) <- N_age
dim(n_ICrit_GetICU_NoOx_NoMV_Surv2_Rec) <- N_age
dim(n_ICrit_NoICU_NoOx_NoMV_Die1) <- N_age
dim(n_ICrit_NoICU_NoOx_NoMV_Die1_ICrit_NoICU_NoOx_NoMV_Die2) <- N_age
dim(n_ICrit_NoICU_NoOx_NoMV_Die2_D_Community) <- N_age
dim(n_ICrit_NoICU_NoOx_NoMV_Surv1) <- N_age
dim(n_ICrit_NoICU_NoOx_NoMV_Surv1_ICrit_NoICU_NoOx_NoMV_Surv2) <- N_age
dim(n_ICrit_NoICU_NoOx_NoMV_Surv2_R) <- N_age

dim(number_req_ICU) <- N_age
dim(number_NotICU) <- N_age
dim(number_NotICU_NotOx_NotMV) <- N_age
dim(number_NotICU_NotOx) <- N_age
dim(number_req_Hosp) <- N_age
dim(number_NotHosp) <- N_age
dim(number_GetHosp) <- N_age
dim(number_GetHosp_Ox) <- N_age
dim(number_GetHosp_NoOx) <- N_age
dim(number_req_ICU_MV) <- N_age
dim(number_req_ICU_Ox) <- N_age
dim(number_GetICU) <- N_age
dim(number_GetICU_GetOx_NoMV) <- N_age
dim(number_GetICU_NoOx_NeedMV) <- N_age
dim(number_GetICU_NoOx) <- N_age
dim(number_GetICU_GetOx_GetMV) <- N_age
dim(number_GetICU_GetOx_NeedMV) <- N_age
dim(number_GetICU_GetOx) <- N_age

# Related to Calculating Age-Structured Force of Infection
dim(p_S_E1) <- N_age
dim(n_S_E1) <- N_age
dim(lambda) <- N_age
dim(s_ij) <- c(N_age,N_age)
dim(temp) <- N_age

# Severity Parameters
dim(prob_hosp) <- N_age
dim(prob_severe) <- N_age
dim(prob_critical) <- N_age
dim(prob_moderate_death_get_hosp_get_ox) <- N_age
dim(prob_moderate_death_get_hosp_no_ox) <- N_age
dim(prob_moderate_death_no_hosp_no_ox) <- N_age
dim(prob_severe_death_get_ICU_get_ox) <- N_age
dim(prob_severe_death_get_ICU_no_ox) <- N_age
dim(prob_severe_death_no_ICU_no_ox) <- N_age
dim(prob_critical_death_get_ICU_get_ox_get_MV) <- N_age
dim(prob_critical_death_get_ICU_get_ox_no_MV) <- N_age
dim(prob_critical_death_get_ICU_no_ox_no_MV) <- N_age
dim(prob_critical_death_no_ICU_no_ox_no_MV) <- N_age

# Extra Non-State Variables Outputted by the Model
output(time) <- TRUE
output(n_S_E1) <- TRUE
output(n_E1_E2) <- TRUE
output(n_E2_I) <- TRUE
output(n_E2_ICase1) <- TRUE
output(n_E2_IMild) <- TRUE
output(n_IMild_R) <- TRUE
output(n_ICase1_ICase2) <- TRUE
output(n_ICase2_Hosp) <- TRUE
output(n_IRec1_IRec2) <- TRUE
output(n_IRec2_R) <- TRUE
output(number_req_ICU) <- TRUE
output(total_req_ICU) <- TRUE
output(ICU_occ) <- TRUE
output(current_free_ICU) <- TRUE
output(total_GetICU) <- TRUE
output(number_GetICU) <- TRUE
output(number_NotICU) <- TRUE
output(number_NotICU_NotOx_NotMV) <- TRUE
output(number_NotICU_NotOx) <- TRUE
output(number_req_Hosp) <- TRUE
output(total_req_Hosp) <- TRUE
output(hosp_occ) <- TRUE
output(current_free_hosp) <- TRUE
output(total_GetHosp) <- TRUE
output(number_GetHosp) <- TRUE
output(number_NotHosp) <- TRUE
output(oxygen_supply) <- TRUE
output(leftover) <- TRUE
output(baseline_oxygen_demand) <- TRUE
output(prop_ox_hosp_beds) <- TRUE
output(available_oxygen_for_hosp_beds) <- TRUE
output(available_oxygen_for_ICU_beds) <- TRUE
output(total_GetHosp_GetOx) <- TRUE
output(number_GetHosp_Ox) <- TRUE
output(number_GetHosp_NoOx) <- TRUE
output(number_req_ICU_MV) <- TRUE
output(number_req_ICU_Ox) <- TRUE
output(total_req_ICU_MV) <- TRUE
output(total_req_ICU_Ox) <- TRUE
output(MV_occ) <- TRUE
output(current_free_MV) <- TRUE
output(available_oxygen_for_ICU_MV) <- TRUE
output(available_oxygen_for_ICU_Ox) <- TRUE
output(number_GetICU_GetOx_NeedMV) <-TRUE
output(total_GetICU_GetOx_Need_MV) <- TRUE
output(total_GetICU_GetOx_GetMV) <- TRUE
output(number_GetICU_NoOx) <- TRUE
output(number_GetICU_NoOx_NeedMV) <- TRUE
output(delta_IMild) <- TRUE
output(delta_ICase1) <- TRUE
output(delta_ICase2) <- TRUE
output(number_GetICU_GetOx_GetMV) <- TRUE
output(number_GetICU_GetOx_NoMV) <- TRUE
output(oxygen_used) <- TRUE
output(number_GetICU_GetOx) <- TRUE
output(oxygen_needed_overall) <- TRUE
output(temp_leftover) <- TRUE
output(n_IMod_GetHosp_GetOx_Die2_D_Hospital) <- TRUE
output(n_IMod_GetHosp_GetOx_Surv2_R) <- TRUE
output(n_IMod_GetHosp_NoOx_Die2_D_Hospital) <- TRUE
output(n_IMod_GetHosp_NoOx_Surv2_R) <- TRUE
output(n_ISev_GetICU_GetOx_Die2_D_Hospital) <- TRUE
output(n_ISev_GetICU_GetOx_Surv2_Rec) <- TRUE
output(n_ISev_GetICU_NoOx_Die2_D_Hospital) <- TRUE
output(n_ISev_GetICU_NoOx_Surv2_Rec) <- TRUE
output(n_ICrit_GetICU_GetOx_GetMV_Die2_D_Hospital) <- TRUE
output(n_ICrit_GetICU_GetOx_GetMV_Surv2_Rec) <- TRUE
output(n_ICrit_GetICU_GetOx_NoMV_Die2_D_Hospital) <- TRUE
output(n_ICrit_GetICU_GetOx_NoMV_Surv2_Rec) <- TRUE
output(n_ICrit_GetICU_NoOx_NoMV_Die2_D_Hospital) <- TRUE
output(n_ICrit_GetICU_NoOx_NoMV_Surv2_Rec) <- TRUE
