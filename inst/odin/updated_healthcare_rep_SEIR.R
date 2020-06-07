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
gamma_moderate_get_hosp_get_ox_survive <- user() # through requiring hosp bed and oxygen compartment conditional on getting hosp bed and oxygen and surviving
gamma_moderate_get_hosp_get_ox_die <- user() # through requiring hosp bed and oxygen compartment conditional on getting hosp bed and oxygen and dying
gamma_moderate_get_hosp_no_ox_survive <- user() # through requiring hosp bed and oxygen compartment conditional on getting hosp bed but NOT oxygen and surviving
gamma_moderate_get_hosp_no_ox_die <- user() # through requiring hosp bed and oxygen compartment conditional on getting  hosp bed but NOT oxygen and dying
gamma_moderate_no_hosp_no_ox_survive <- user() # through requiring hosp bed and oxygen compartment conditional on NOT getting hosp bed and NOT oxygen and surviving
gamma_moderate_no_hosp_no_ox_die <- user() # through requiring hosp bed and oxygen compartment conditional on NOT getting hosp bed and NOT oxygen and dying
gamma_severe_get_ICU_get_ox_survive <- user() # through requiring ICU bed and oxygen compartment conditional on getting ICU bed and oxygen and surviving
gamma_severe_get_ICU_get_ox_die <- user() # through requiring ICU bed and oxygen compartment conditional on getting ICU bed and oxygen and dying
gamma_severe_get_ICU_no_ox_survive <- user() # through requiring ICU bed and oxygen compartment conditional on getting ICU bed but NOT oxygen and surviving
gamma_severe_get_ICU_no_ox_die <- user() # through requiring ICU bed and oxygen compartment conditional on getting ICU bed but NOT oxygen and dying
gamma_severe_no_ICU_no_ox_survive <- user() # through requiring ICU bed and oxygen compartment conditional on NOT getting ICU bed and NOT oxygen and surviving
gamma_severe_no_ICU_no_ox_die <- user() # through requiring ICU bed and oxygen compartment conditional on NOT getting ICU bed and NOT oxygen and dying
gamma_critical_get_ICU_get_ox_get_mv_survive <- user() # through requiring ICU bed, oxygen and MV compartment conditional on getting ICU bed, oxygen and MV and surviving
gamma_critical_get_ICU_get_ox_get_mv_die <- user() # through requiring ICU bed, oxygen and MV compartment conditional on getting ICU bed, oxygen and MV and dying
gamma_critical_get_ICU_get_ox_no_mv_survive <- user() # through requiring ICU bed, oxygen and MV compartment conditional on getting ICU bed and oxygen, but NOT MV and surviving
gamma_critical_get_ICU_get_ox_no_mv_die <- user() # through requiring ICU bed, oxygen and MV compartment conditional on getting ICU bed and oxygen, but NOT MV and dying
gamma_critical_get_ICU_no_ox_no_mv_survive <- user() # through requiring ICU bed, oxygen and MV compartment conditional on getting ICU bed, but NOT oxygen and NOT MV and surviving
gamma_critical_get_ICU_no_ox_no_mv_die <- user() # through requiring ICU bed, oxygen and MV compartment conditional on getting ICU bed, but NOT oxygen and NOT MV and dying
gamma_critical_no_ICU_no_ox_no_mv_survive <- user() # through requiring ICU bed, oxygen and MV compartment conditional on NOT getting ICU bed, NOT oxygen and NOT MV and surviving
gamma_critical_no_ICU_no_ox_no_mv_die <- user() # through requiring ICU bed, oxygen and MV compartment conditional on NOT getting ICU bed, NOT oxygen and NOT MV and dying

## PROBABILITIES
##------------------------------------------------------------------------------
prob_hosp[] <- user() # probability of requiring hospitalisation by age
prob_severe[] <- user() # probability of severe disease (requiring ICU bed) by age
prob_critical[] <- user() # probability of critical disease (requiring ICU bed AND MV) by age, conditional on having severe disease
prob_moderate_death_get_hosp_get_ox[] <- user() # probability of dying from moderate disease (i.e. requiring hospital bed and oxygen) by age given you receive a hospital bed AND oxygen)
prob_moderate_death_get_hosp_no_ox[] <- user() # probability of dying from moderate disease (i.e. requiring hospital bed and oxygen) by age given you receive a hospital bed BUT no oxygen)
prob_moderate_death_no_hosp_no_ox[] <- user() # probability of dying from moderate disease (i.e. requiring hospital bed and oxygen) by age given you do NOT receive a hospital bed and you do NOT receive oxygen
prob_severe_death_get_ICU_get_ox[] <- user() # probability of dying from severe disease (i.e. requiring ICU bed and oxygen) by age given you receive an ICU bed AND oxygen)
prob_severe_death_get_ICU_no_ox[] <- user() # probability of dying from severe disease (i.e. requiring ICU bed and oxygen) by age given you receive an ICU bed BUT no oxygen)
prob_severe_death_no_ICU_no_ox[] <- user() # probability of dying from severe disease (i.e. requiring ICU bed and oxygen) by age given you do NOT receive an ICU bed and you do NOT receive oxygen
prob_critical_death_get_ICU_get_ox_get_MV[] <- user() # probability of dying from critical disease (i.e. requiring ICU bed, oxygen and MV) by age given you receive an ICU bed AND oxygen AND MV)
prob_critical_death_get_ICU_get_ox_no_MV[] <- user() # probability of dying from critical disease (i.e. requiring ICU bed, oxygen and MV) by age given you receive an ICU bed AND oxygen BUT no MV)
prob_critical_death_get_ICU_no_ox_no_MV[] <- user() # probability of dying from critical disease (i.e. requiring ICU bed, oxygen and MV) by age given you receive an ICU bed BUT no oxygen and you do NOT receive MV
prob_critical_death_no_ICU_no_ox_no_MV[] <- user() # probability of dying from critical disease (i.e. requiring ICU bed, oxygen and MV) by age given you do NOT receive an ICU bed, you do NOT receive oxygen, and you do NOT receive MV

## EQUATIONS FOR TRANSITIONS BETWEEN COMPARTMENTS
##------------------------------------------------------------------------------
# Passage Through Initial Latent and Infection Stages
update(S[]) <- S[i] - n_S_E1[i]  # Susceptibles (1 comp)
update(E1[]) <- E1[i] + delta_E1[i]  # First of the latent infection compartments (2 comps)
update(E2[]) <- E2[i] + delta_E2[i]  # Second of the latent infection compartments (2 comps)
update(IMild[]) <- IMild[i] + delta_IMild[i]  # Mild infections (1 comp)
update(ICase1[]) <- ICase1[i] + delta_ICase1[i] # First of the compartments for infections that will require hospitalisation (2 comps)
update(ICase2[]) <- ICase2[i] + delta_ICase2[i] # Second of the compartments for infections that will require hospitalisation (2 comps)

# Passage Through Requiring Hospital Bed and Oxygen, Either Receiving Both, Oxygen or Neither, and Surviving or Not
update(IModerate_Get_Hosp_Get_Ox_Survive1) <- IModerate_Get_Hosp_Get_Ox_Survive1[i] + delta_IModerate_Get_Hosp_Get_Ox_Survive1[i] # Require hosp bed and oxygen, get both, survive (1st)
update(IModerate_Get_Hosp_Get_Ox_Survive2) <- IModerate_Get_Hosp_Get_Ox_Survive2[i] + delta_IModerate_Get_Hosp_Get_Ox_Survive2[i] # Require hosp bed and oxygen, get both, survive (2nd)
update(IModerate_Get_Hosp_Get_Ox_Die1) <- IModerate_Get_Hosp_Get_Ox_Die1[i] + delta_IModerate_Get_Hosp_Get_Ox_Die1[i] # Require hosp bed and oxygen, get both, die (1st)
update(IModerate_Get_Hosp_Get_Ox_Die2) <- IModerate_Get_Hosp_Get_Ox_Die2[i] + delta_IModerate_Get_Hosp_Get_Ox_Die2[i] # Require hosp bed and oxygen, get both, die (2nd)
update(IModerate_Get_Hosp_No_Ox_Survive1) <- IModerate_Get_Hosp_No_Ox_Survive1[i] + delta_IModerate_Get_Hosp_No_Ox_Survive1[i] # Require hosp bed and oxygen, get bed only, survive (1st)
update(IModerate_Get_Hosp_No_Ox_Survive2) <- IModerate_Get_Hosp_No_Ox_Survive2[i] + delta_IModerate_Get_Hosp_No_Ox_Survive2[i] # Require hosp bed and oxygen, get bed only, survive (2nd)
update(IModerate_Get_Hosp_No_Ox_Die1) <- IModerate_Get_Hosp_No_Ox_Die1[i] + delta_IModerate_Get_Hosp_No_Ox_Die1[i] # Require hosp bed and oxygen, get bed only, die (1st)
update(IModerate_Get_Hosp_No_Ox_Die2) <- IModerate_Get_Hosp_No_Ox_Die2[i] + delta_IModerate_Get_Hosp_No_Ox_Die2[i] # Require hosp bed and oxygen, get bed only, die (2nd)
update(IModerate_No_Hosp_No_Ox_Survive1) <- IModerate_No_Hosp_No_Ox_Survive1[i] + delta_IModerate_No_Hosp_No_Ox_Survive1[i] # Require hosp bed and oxygen, get neither, survive (1st)
update(IModerate_No_Hosp_No_Ox_Survive2) <- IModerate_No_Hosp_No_Ox_Survive2[i] + delta_IModerate_No_Hosp_No_Ox_Survive2[i] # Require hosp bed and oxygen, get neither, survive (2nd)
update(IModerate_No_Hosp_No_Ox_Die1) <- IModerate_No_Hosp_No_Ox_Die1[i] + delta_IModerate_No_Hosp_No_Ox_Die1[i] # Require hosp bed and oxygen, get neither, die (1st)
update(IModerate_No_Hosp_No_Ox_Die2) <- IModerate_No_Hosp_No_Ox_Die2[i] + delta_IModerate_No_Hosp_No_Ox_Die2[i] # Require hosp bed and oxygen, get neither, survive (2nd)

# Passage Through Requiring ICU Bed and Oxygen, Either Receiving Both, Oxygen or Neither, and Surviving or Not
update(ISevere_Get_ICU_Get_Ox_Survive1) <- ISevere_Get_ICU_Get_Ox_Survive1[i] + delta_ISevere_Get_ICU_Get_Ox_Survive1[i] # Require ICU bed and oxygen, get both, survive (1st)
update(ISevere_Get_ICU_Get_Ox_Survive2) <- ISevere_Get_ICU_Get_Ox_Survive2[i] + delta_ISevere_Get_ICU_Get_Ox_Survive2[i] # Require ICU bed and oxygen, get both, survive (2nd)
update(ISevere_Get_ICU_Get_Ox_Die1) <- ISevere_Get_ICU_Get_Ox_Die1[i] + delta_ISevere_Get_ICU_Get_Ox_Die1[i] # Require ICU bed and oxygen, get both, die (1st)
update(ISevere_Get_ICU_Get_Ox_Die2) <- ISevere_Get_ICU_Get_Ox_Die2[i] + delta_ISevere_Get_ICU_Get_Ox_Die2[i] # Require ICU bed and oxygen, get both, die (2nd)
update(ISevere_Get_ICU_No_Ox_Survive1) <- ISevere_Get_ICU_No_Ox_Survive1[i] + delta_ISevere_Get_ICU_No_Ox_Survive1[i] # Require ICU bed and oxygen, get ICU bed only, survive (1st)
update(ISevere_Get_ICU_No_Ox_Survive2) <- ISevere_Get_ICU_No_Ox_Survive2[i] + delta_ISevere_Get_ICU_No_Ox_Survive2[i] # Require ICU bed and oxygen, get ICU bed only, survive (2nd)
update(ISevere_Get_ICU_No_Ox_Die1) <- ISevere_Get_ICU_No_Ox_Die1[i] + delta_ISevere_Get_ICU_No_Ox_Die1[i] # Require ICU bed and oxygen, get ICU bed only, die (1st)
update(ISevere_Get_ICU_No_Ox_Die2) <- ISevere_Get_ICU_No_Ox_Die2[i] + delta_ISevere_Get_ICU_No_Ox_Die2[i] # Require ICU bed and oxygen, get ICU bed only, die (2nd)
update(ISevere_No_ICU_No_Ox_Survive1) <- ISevere_No_ICU_No_Ox_Survive1[i] + delta_ISevere_No_ICU_No_Ox_Survive1[i] # Require ICU bed and oxygen, get neither, suvive (1st)
update(ISevere_No_ICU_No_Ox_Survive2) <- ISevere_No_ICU_No_Ox_Survive2[i] + delta_ISevere_No_ICU_No_Ox_Survive2[i] # Require ICU bed and oxygen, get neither, suvive (2nd)
update(ISevere_No_ICU_No_Ox_Die1) <- ISevere_No_ICU_No_Ox_Die1[i] + delta_ISevere_No_ICU_No_Ox_Die1[i] # Require ICU bed and oxygen, get neither, die (1st)
update(ISevere_No_ICU_No_Ox_Die2) <- ISevere_No_ICU_No_Ox_Die2[i] + delta_ISevere_No_ICU_No_Ox_Die2[i] # Require ICU bed and oxygen, get neither, die (2nd)

# Passage Through Requiring ICU Bed, Oxygen and Mechanical Ventilation, Either Receiving All, ICU Bed and Oxygen, ICU Bed Only or Nothing, and Surviving or Not
update(ICritical_Get_ICU_Get_Ox_Get_MV_Survive1) <- ICritical_Get_ICU_Get_Ox_Get_MV_Survive1[i] + delta_ICritical_Get_ICU_Get_Ox_Get_MV_Survive1[i] # Require ICU bed, oxygen and MV, get all, survive (1st)
update(ICritical_Get_ICU_Get_Ox_Get_MV_Survive2) <- ICritical_Get_ICU_Get_Ox_Get_MV_Survive2[i] + delta_ICritical_Get_ICU_Get_Ox_Get_MV_Survive2[i] # Require ICU bed, oxygen and MV, get all, survive (2nd)
update(ICritical_Get_ICU_Get_Ox_Get_MV_Die1) <- ICritical_Get_ICU_Get_Ox_Get_MV_Die1[i] + delta_ICritical_Get_ICU_Get_Ox_Get_MV_Die1[i] # Require ICU bed, oxygen and MV, get all, die (1st)
update(ICritical_Get_ICU_Get_Ox_Get_MV_Die2) <- ICritical_Get_ICU_Get_Ox_Get_MV_Die2[i] + delta_ICritical_Get_ICU_Get_Ox_Get_MV_Die2[i] # Require ICU bed, oxygen and MV, get all, die (2nd)
update(ICritical_Get_ICU_Get_Ox_No_MV_Survive1) <- ICritical_Get_ICU_Get_Ox_No_MV_Survive1[i] + delta_ICritical_Get_ICU_Get_Ox_No_MV_Survive1[i] # Require ICU bed, oxygen and MV, get ICU bed and oxygen only, survive (1st)
update(ICritical_Get_ICU_Get_Ox_No_MV_Survive2) <- ICritical_Get_ICU_Get_Ox_No_MV_Survive2[i] + delta_ICritical_Get_ICU_Get_Ox_No_MV_Survive2[i] # Require ICU bed, oxygen and MV, get ICU bed and oxygen only, survive (2nd)
update(ICritical_Get_ICU_Get_Ox_No_MV_Die1) <- ICritical_Get_ICU_Get_Ox_No_MV_Die1[i] + delta_ICritical_Get_ICU_Get_Ox_No_MV_Die1[i] # Require ICU bed, oxygen and MV, get ICU bed and oxygen only, die (1st)
update(ICritical_Get_ICU_Get_Ox_No_MV_Die2) <- ICritical_Get_ICU_Get_Ox_No_MV_Die2[i] + delta_ICritical_Get_ICU_Get_Ox_No_MV_Die2[i] # Require ICU bed, oxygen and MV, get ICU bed and oxygen only, die (2nd)
update(ICritical_Get_ICU_No_Ox_No_MV_Survive1) <- ICritical_Get_ICU_No_Ox_No_MV_Survive1[i] + delta_ICritical_Get_ICU_No_Ox_No_MV_Survive1[i] # Require ICU bed, oxygen and MV, get ICU bed only, survive (1st)
update(ICritical_Get_ICU_No_Ox_No_MV_Survive2) <- ICritical_Get_ICU_No_Ox_No_MV_Survive2[i] + delta_ICritical_Get_ICU_No_Ox_No_MV_Survive2[i] # Require ICU bed, oxygen and MV, get ICU bed only, survive (2nd)
update(ICritical_Get_ICU_No_Ox_No_MV_Die1) <- ICritical_Get_ICU_No_Ox_No_MV_Die1[i] + delta_ICritical_Get_ICU_No_Ox_No_MV_Die1[i] # Require ICU bed, oxygen and MV, get ICU bed only, die (1st)
update(ICritical_Get_ICU_No_Ox_No_MV_Die2) <- ICritical_Get_ICU_No_Ox_No_MV_Die2[i] + delta_ICritical_Get_ICU_No_Ox_No_MV_Die2[i] # Require ICU bed, oxygen and MV, get ICU bed only, die (2nd)
update(ICritical_No_ICU_No_Ox_No_MV_Survive1) <- ICritical_No_ICU_No_Ox_No_MV_Survive1[i] + delta_ICritical_No_ICU_No_Ox_No_MV_Survive1[i] # Require ICU bed, oxygen and MV, get nothing, survive (1st)
update(ICritical_No_ICU_No_Ox_No_MV_Survive2) <- ICritical_No_ICU_No_Ox_No_MV_Survive2[i] + delta_ICritical_No_ICU_No_Ox_No_MV_Survive2[i] # Require ICU bed, oxygen and MV, get nothing, survive (2nd)
update(ICritical_No_ICU_No_Ox_No_MV_Die1) <- ICritical_No_ICU_No_Ox_No_MV_Die1[i] + delta_ICritical_No_ICU_No_Ox_No_MV_Die1[i] # Require ICU bed, oxygen and MV, get nothing, die (1st)
update(ICritical_No_ICU_No_Ox_No_MV_Die2) <- ICritical_No_ICU_No_Ox_No_MV_Die2[i] + delta_ICritical_No_ICU_No_Ox_No_MV_Die2[i] # Require ICU bed, oxygen and MV, get nothing, die (2nd)

# Passage Through Recovery, from Mild Infection, Requiring Oxygen or From ICU Post-Requiring Mechanical Ventilation
update(IRec1[]) <- IRec1[i] + delta_IRec1[i] # First of the compartments for those recovering from ICU (2 comps)
update(IRec2[]) <- IRec2[i] + delta_IRec2[i] # Second of the compartments for those recovering from ICU (2 comps)
update(R[]) <- R[i] + delta_R[i] # Recovered
update(D_Community[]) <- D_Community[i] + delta_D_Community[i] # Deaths in the community
update(D_Hospital[]) <- D_Hospital[i] + delta_D_Hospital[i] # Deaths in the community


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
p_IModerate_Get_Hosp_Get_Ox_Survive1_IModerate_Get_Hosp_Get_Ox_Survive2 <- 1 - exp(-gamma_moderate_get_hosp_get_ox_survive * dt) # Progression through requiring hosp bed and oxygen and receiving both -> Recovery
p_IModerate_Get_Hosp_Get_Ox_Survive2_R <- 1 - exp(-gamma_moderate_get_hosp_get_ox_survive * dt) # Progression through requiring hosp bed and oxygen and receiving both -> Recovery
p_IModerate_Get_Hosp_Get_Ox_Die1_IModerate_Get_Hosp_Get_Ox_Die2 <- 1 - exp(-gamma_moderate_get_hosp_get_ox_die * dt) # Progression through requiring hosp bed and oxygen and receiving both -> Dying
p_IModerate_Get_Hosp_Get_Ox_Die2_D_Hospital <- 1 - exp(-gamma_moderate_get_hosp_get_ox_die * dt) # Progression through requiring hosp bed and oxygen and receiving both -> Dying
p_IModerate_Get_Hosp_No_Ox_Survive1_IModerate_Get_Hosp_No_Ox_Survive2 <- 1 - exp(-gamma_moderate_get_hosp_no_ox_survive * dt) # Progression through requiring hosp bed and oxygen, receiving hosp bed only -> Recovery
p_IModerate_Get_Hosp_No_Ox_Survive2_R <- 1 - exp(-gamma_moderate_get_hosp_no_ox_survive * dt) # Progression through requiring hosp bed and oxygen, receiving hosp bed only -> Recovery
p_IModerate_Get_Hosp_No_Ox_Die1_IModerate_Get_Hosp_No_Ox_Die2 <- 1 - exp(-gamma_moderate_get_hosp_no_ox_die * dt) # Progression through requiring hosp bed and oxygen, receiving hosp bed only -> Dying
p_IModerate_Get_Hosp_No_Ox_Die2_D_Hospital <- 1 - exp(-gamma_moderate_get_hosp_no_ox_die * dt) # Progression through requiring hosp bed and oxygen, receiving hosp bed only -> Dying
p_IModerate_No_Hosp_No_Ox_Survive1_IModerate_No_Hosp_No_Ox_Survive2 <- 1 - exp(-gamma_moderate_no_hosp_no_ox_survive * dt) # Progression through requiring hosp bed and oxygen, receiving neither -> Recovery
p_IModerate_No_Hosp_No_Ox_Survive2_R <- 1 - exp(-gamma_moderate_no_hosp_no_ox_survive * dt) # Progression through requiring hosp bed and oxygen, receiving neither -> Recovery
p_IModerate_No_Hosp_No_Ox_Die1_IModerate_No_Hosp_No_Ox_Die2 <- 1 - exp(-gamma_moderate_no_hosp_no_ox_die * dt) # Progression through requiring hosp bed and oxygen, receiving neither -> Dying
p_IModerate_No_Hosp_No_Ox_Die2_D_Community <- 1 - exp(-gamma_moderate_no_hosp_no_ox_die * dt) # Progression through requiring hosp bed and oxygen, receiving neither -> Dying

# Transition Probabilities for Those Requiring ICU Bed and Oxygen -> Recovery/Death
p_ISevere_Get_ICU_Get_Ox_Survive1_ISevere_Get_ICU_Get_Ox_Survive2 <- 1 - exp(-gamma_severe_get_ICU_get_ox_survive * dt) # Progression through requiring ICU bed and oxygen and receiving both -> Recovery
p_ISevere_Get_ICU_Get_Ox_Survive2_R <- 1 - exp(-gamma_severe_get_ICU_get_ox_survive * dt) # Progression through requiring ICU bed and oxygen and receiving both -> Recovery
p_ISevere_Get_ICU_Get_Ox_Die1_ISevere_Get_ICU_Get_Ox_Die2 <- 1 - exp(-gamma_severe_get_ICU_get_ox_die * dt) # Progression through requiring ICU bed and oxygen and receiving both -> Dying
p_ISevere_Get_ICU_Get_Ox_Die2_D_Hospital <- 1 - exp(-gamma_severe_get_ICU_get_ox_die * dt) # Progression through requiring ICU bed and oxygen and receiving both -> Dying
p_ISevere_Get_ICU_No_Ox_Survive1_ISevere_Get_ICU_No_Ox_Survive2 <- 1 - exp(-gamma_severe_get_ICU_no_ox_survive * dt) # Progression through requiring ICU bed and oxygen, receiving ICU bed only -> Recovery
p_ISevere_Get_ICU_No_Ox_Survive2_R <- 1 - exp(-gamma_severe_get_ICU_no_ox_survive * dt) # Progression through requiring ICU bed and oxygen, receiving ICU bed only -> Recovery
p_ISevere_Get_ICU_No_Ox_Die1_ISevere_Get_ICU_No_Ox_Die2 <- 1 - exp(-gamma_severe_get_ICU_no_ox_die * dt) # Progression through requiring hosp bed and oxygen, receiving ICU bed only -> Dying
p_ISevere_Get_ICU_No_Ox_Die2_D_Hospital <- 1 - exp(-gamma_severe_get_ICU_no_ox_die * dt) # Progression through requiring hosp bed and oxygen, receiving ICU bed only -> Dying
p_ISevere_No_ICU_No_Ox_Survive1_ISevere_No_ICU_No_Ox_Survive2 <- 1 - exp(-gamma_severe_no_ICU_no_ox_survive * dt) # Progression through requiring ICU bed and oxygen, receiving neither -> Recovery
p_ISevere_No_ICU_No_Ox_Survive2_R <- 1 - exp(-gamma_severe_no_ICU_no_ox_survive * dt) # Progression through requiring ICU bed and oxygen, receiving neither -> Recovery
p_ISevere_No_ICU_No_Ox_Die1_ISevere_No_ICU_No_Ox_Die2 <- 1 - exp(-gamma_severe_no_ICU_no_ox_die * dt) # Progression through requiring ICU bed and oxygen, receiving neither -> Dying
p_ISevere_No_ICU_No_Ox_Die2_D_Community <- 1 - exp(-gamma_severe_no_ICU_no_ox_die * dt) # Progression through requiring ICU bed and oxygen, receiving neither -> Dying

# Transition Probabilities for Those Requiring ICU Bed, Oxygen and Mechanical Ventilation -> Recovery/Death
p_ICritical_Get_ICU_Get_Ox_Get_MV_Survive1_ICritical_Get_ICU_Get_Ox_Get_MV_Survive2 <- 1 - exp(-gamma_critical_get_ICU_get_ox_get_mv_survive * dt) # Progression through requiring ICU bed, oxygen and MV, and receiving all -> Recovery
p_ICritical_Get_ICU_Get_Ox_Get_MV_Survive2_R <- 1 - exp(-gamma_critical_get_ICU_get_ox_get_mv_survive * dt) # Progression through requiring ICU bed, oxygen and MV, and receiving all -> Recovery
p_ICritical_Get_ICU_Get_Ox_Get_MV_Die1_ICritical_Get_ICU_Get_Ox_Get_MV_Die2 <- 1 - exp(-gamma_critical_get_ICU_get_ox_get_mv_die * dt) # Progression through requiring ICU bed, oxygen and MV, and receiving all -> Dying
p_ICritical_Get_ICU_Get_Ox_Get_MV_Die2_D_Hospital <- 1 - exp(-gamma_critical_get_ICU_get_ox_get_mv_die * dt) # Progression through requiring ICU bed, oxygen and MV, and receiving all -> Recovery
p_ICritical_Get_ICU_Get_Ox_No_MV_Survive1_ICritical_Get_ICU_Get_Ox_No_MV_Survive2 <- 1 - exp(-gamma_critical_get_ICU_get_ox_no_mv_survive * dt) # Progression through requiring ICU bed, oxygen and MV, and receiving ICU bed and oxygen only -> Recovery
p_ICritical_Get_ICU_Get_Ox_No_MV_Survive2_R <- 1 - exp(-gamma_critical_get_ICU_get_ox_no_mv_survive * dt) # Progression through requiring ICU bed, oxygen and MV, and receiving ICU bed and oxygen only -> Recovery
p_ICritical_Get_ICU_Get_Ox_No_MV_Die1_ICritical_Get_ICU_Get_Ox_No_MV_Die2 <- 1 - exp(-gamma_critical_get_ICU_get_ox_no_mv_die * dt) # Progression through requiring ICU bed, oxygen and MV, and receiving ICU bed and oxygen only -> Dying
p_ICritical_Get_ICU_Get_Ox_No_MV_Die2_D_Hospital <- 1 - exp(-gamma_critical_get_ICU_get_ox_no_mv_die * dt) # Progression through requiring ICU bed, oxygen and MV, and receiving ICU bed and oxygen only -> Dying
p_ICritical_Get_ICU_No_Ox_No_MV_Survive1_ICritical_Get_ICU_No_Ox_No_MV_Survive2 <- 1 - exp(-gamma_critical_get_ICU_no_ox_no_mv_survive * dt) # Progression through requiring ICU bed, oxygen and MV, receiving ICU bed only -> Recovery
p_ICritical_Get_ICU_No_Ox_No_MV_Survive2_R <- 1 - exp(-gamma_critical_get_ICU_no_ox_no_mv_survive * dt) # Progression through requiring ICU bed, oxygen and MV, receiving ICU bed only -> Recovery
p_ICritical_Get_ICU_No_Ox_No_MV_Die1_ICritical_Get_ICU_No_Ox_No_MV_Die2 <- 1 - exp(-gamma_critical_get_ICU_no_ox_no_mv_survive * dt) # Progression through requiring ICU bed, oxygen and MV, receiving ICU bed only -> Dying
p_ICritical_Get_ICU_No_Ox_No_MV_Die2_D_Hospital <- 1 - exp(-gamma_critical_get_ICU_no_ox_no_mv_survive * dt) # Progression through requiring ICU bed, oxygen and MV, receiving ICU bed only -> Dying
p_ICritical_No_ICU_No_Ox_No_MV_Survive1_ICritical_No_ICU_No_Ox_No_MV_Survive2 <- 1 - exp(-gamma_critical_no_ICU_no_ox_no_mv_survive * dt) # Progression through requiring ICU bed, oxygen and MV, receiving nothing -> Recovery
p_ICritical_No_ICU_No_Ox_No_MV_Survive2_R <- 1 - exp(-gamma_critical_no_ICU_no_ox_no_mv_survive * dt) # Progression through requiring ICU bed, oxygen and MV, receiving nothing -> Recovery
p_ICritical_No_ICU_No_Ox_No_MV_Die1_ICritical_No_ICU_No_Ox_No_MV_Die2 <- 1 - exp(-gamma_critical_no_ICU_no_ox_no_mv_survive * dt) # Progression through requiring ICU bed, oxygen and MV, receiving nothing -> Dying
p_ICritical_No_ICU_No_Ox_No_MV_Die2_D_Community <- 1 - exp(-gamma_critical_no_ICU_no_ox_no_mv_survive * dt) # Progression through requiring ICU bed, oxygen and MV, receiving nothing -> Dying


## DRAWS FOR NUMBER OF INDIVIDUALS MOVING BETWEEN COMPARTMENTS
##------------------------------------------------------------------------------
# Numbers changing between non-hospital related compartments:
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

# Numbers changing between hospital bed related compartments
n_IModerate_Get_Hosp_Get_Ox_Die1[] <- rbinom(number_get_hosp_Ox[i], prob_moderate_death_get_hosp_get_ox)
n_IModerate_Get_Hosp_Get_Ox_Die1_IModerate_Get_Hosp_Get_Ox_Die2[] <- rbinom(IModerate_Get_Hosp_Get_Ox_Die1[i], p_IModerate_Get_Hosp_Get_Ox_Die1_IModerate_Get_Hosp_Get_Ox_Die2) # Number progressing through requiring hosp bed and oxygen and receiving both -> Dying
n_IModerate_Get_Hosp_Get_Ox_Die2_D_Hospital[] <- rbinom(IModerate_Get_Hosp_Get_Ox_Die2[i], p_IModerate_Get_Hosp_Get_Ox_Die2_D_Hospital) # Number progressing through requiring hosp bed and oxygen and receiving both -> Dying
n_IModerate_Get_Hosp_Get_Ox_Survive1[] <- number_get_hosp_Ox[i] - n_IModerate_Get_Hosp_Get_Ox_Die1[i]
n_IModerate_Get_Hosp_Get_Ox_Survive1_IModerate_Get_Hosp_Get_Ox_Survive2[] <- rbinom(IModerate_Get_Hosp_Get_Ox_Survive1[i], p_IModerate_Get_Hosp_Get_Ox_Survive1_IModerate_Get_Hosp_Get_Ox_Survive2) # Number progressing through requiring hosp bed and oxygen and receiving both -> Recovery
n_IModerate_Get_Hosp_Get_Ox_Survive2_R[] <- rbinom(IModerate_Get_Hosp_Get_Ox_Survive2_R[i], p_IModerate_Get_Hosp_Get_Ox_Survive2_R) # Number progressing through requiring hosp bed and oxygen and receiving both -> Recovery

n_IModerate_Get_Hosp_No_Ox_Die1[] <- rbinom(number_get_hosp_no_Ox[i], prob_moderate_death_get_hosp_no_ox)
n_IModerate_Get_Hosp_No_Ox_Die1_IModerate_Get_Hosp_No_Ox_Die2[] <- rbinom(IModerate_Get_Hosp_No_Ox_Die1[i], p_IModerate_Get_Hosp_No_Ox_Die1_IModerate_Get_Hosp_No_Ox_Die2) # Number progressing through requiring hosp bed and oxygen, receiving hosp bed only -> Dying
n_IModerate_Get_Hosp_No_Ox_Die2_D_Hospital[] <- rbinom(IModerate_Get_Hosp_No_Ox_Die2[i], p_IModerate_Get_Hosp_No_Ox_Die2_D_Hospital) # Number progressing through requiring hosp bed and oxygen, receiving hosp bed only -> Dying
n_IModerate_Get_Hosp_No_Ox_Survive1[] <- number_get_hosp_no_Ox[i] - n_IModerate_Get_Hosp_No_Ox_Die1[i]
n_IModerate_Get_Hosp_No_Ox_Survive1_IModerate_Get_Hosp_No_Ox_Survive2[] <- rbinom(IModerate_Get_Hosp_No_Ox_Survive1[i], p_IModerate_Get_Hosp_No_Ox_Survive1_IModerate_Get_Hosp_No_Ox_Survive2) # Number progressing through requiring hosp bed and oxygen, receiving hosp bed only -> Recovery
n_IModerate_Get_Hosp_No_Ox_Survive2_R[] <- rbinom(IModerate_Get_Hosp_No_Ox_Survive2[i], p_IModerate_Get_Hosp_No_Ox_Survive2_R) # Number progressing through requiring hosp bed and oxygen, receiving hosp bed only -> Recovery

n_IModerate_No_Hosp_No_Ox_Die1[] <- rbinom(number_not_hosp_bed[i], prob_moderate_death_no_hosp_no_ox)
n_IModerate_No_Hosp_No_Ox_Die1_IModerate_No_Hosp_No_Ox_Die2[] <- rbinom(IModerate_No_Hosp_No_Ox_Die1[i], p_IModerate_No_Hosp_No_Ox_Die1_IModerate_No_Hosp_No_Ox_Die2) # Number progressing through requiring hosp bed and oxygen, receiving neither -> Dying
n_IModerate_No_Hosp_No_Ox_Die2_D_Community[] <- rbinom(IModerate_No_Hosp_No_Ox_Die2[i], p_IModerate_No_Hosp_No_Ox_Die2_D_Community) # Number progressing through requiring hosp bed and oxygen, receiving neither -> Dying
n_IModerate_No_Hosp_No_Ox_Survive1[] <- number_not_hosp_bed[i] - n_IModerate_No_Hosp_No_Ox_Die1[i]
n_IModerate_No_Hosp_No_Ox_Survive1_IModerate_No_Hosp_No_Ox_Survive2[] <- rbinom(IModerate_No_Hosp_No_Ox_Survive1[i], p_IModerate_No_Hosp_No_Ox_Survive1_IModerate_No_Hosp_No_Ox_Survive2) # Number progressing through requiring hosp bed and oxygen, receiving neither -> Recovery
n_IModerate_No_Hosp_No_Ox_Survive2_R[] <- rbinom(IModerate_No_Hosp_No_Ox_Survive2[i], p_IModerate_No_Hosp_No_Ox_Survive2_R) # Number progressing through requiring hosp bed and oxygen, receiving neither -> Recovery

# Numbers changing between ICU bed/non-mechanical ventilation related compartments
###n_ISevere_Get_ICU_Get_Ox_Die1[] <- rbinom(number_get_ICU_Ox[i], prob_severe_death_get_ICU_get_ox)
n_ISevere_Get_ICU_Get_Ox_Die1_ISevere_Get_ICU_Get_Ox_Die2[] <- rbinom(ISevere_Get_ICU_Get_Ox_Die1[i], p_ISevere_Get_ICU_Get_Ox_Die1_ISevere_Get_ICU_Get_Ox_Die2) # Number progressing through requiring ICU bed and oxygen and receiving both -> Dying
n_ISevere_Get_ICU_Get_Ox_Die2_D_Hospital[] <- rbinom(ISevere_Get_ICU_Get_Ox_Die2[i], p_ISevere_Get_ICU_Get_Ox_Die2_D_Hospital) # Number progressing through requiring ICU bed and oxygen and receiving both -> Dying

###n_ISevere_Get_ICU_Get_Ox_Survive1[] <- number_get_ICU_Ox[i] - n_ISevere_Get_ICU_Get_Ox_Die1[i]
n_ISevere_Get_ICU_Get_Ox_Survive1_ISevere_Get_ICU_Get_Ox_Survive2[] <- rbinom(ISevere_Get_ICU_Get_Ox_Survive1[i], p_ISevere_Get_ICU_Get_Ox_Survive1_ISevere_Get_ICU_Get_Ox_Survive2) # Number progressing through requiring ICU bed and oxygen and receiving both -> Recovery
n_ISevere_Get_ICU_Get_Ox_Survive2_R[] <- rbinom(ISevere_Get_ICU_Get_Ox_Survive2[i], p_ISevere_Get_ICU_Get_Ox_Survive2_R) # Number progressing through requiring ICU bed and oxygen and receiving both -> Recovery

###n_ISevere_Get_ICU_No_Ox_Survive1[] <- rbinom( , prob_severe_death_get_ICU_no_ox)
n_ISevere_Get_ICU_No_Ox_Survive1_ISevere_Get_ICU_No_Ox_Survive2[] <- rbinom(ISevere_Get_ICU_No_Ox_Survive1[i], p_ISevere_Get_ICU_No_Ox_Survive1_ISevere_Get_ICU_No_Ox_Survive2) # Number progressing through requiring ICU bed and oxygen, receiving ICU bed only -> Recovery
n_ISevere_Get_ICU_No_Ox_Survive2_R[] <- rbinom(ISevere_Get_ICU_No_Ox_Survive2[i], p_ISevere_Get_ICU_No_Ox_Survive2_R) # Number progressing through requiring ICU bed and oxygen, receiving ICU bed only -> Recovery


n_ISevere_Get_ICU_No_Ox_Die1_ISevere_Get_ICU_No_Ox_Die2[] <- rbinom(ISevere_Get_ICU_No_Ox_Die1[i], p_ISevere_Get_ICU_No_Ox_Die1_ISevere_Get_ICU_No_Ox_Die2) # Number progressing through requiring hosp bed and oxygen, receiving ICU bed only -> Dying
n_ISevere_Get_ICU_No_Ox_Die2_D_Hospital[] <- rbinom(ISevere_Get_ICU_No_Ox_Die2[i], p_ISevere_Get_ICU_No_Ox_Die2_D_Hospital) # Number progressing through requiring hosp bed and oxygen, receiving ICU bed only -> Dying


n_ISevere_No_ICU_No_Ox_Survive1_ISevere_No_ICU_No_Ox_Survive2[] <- rbinom(ISevere_No_ICU_No_Ox_Survive1[i], p_ISevere_No_ICU_No_Ox_Survive1_ISevere_No_ICU_No_Ox_Survive2) # Number progressing through requiring ICU bed and oxygen, receiving neither -> Recovery
n_ISevere_No_ICU_No_Ox_Survive2_R[] <- rbinom(ISevere_No_ICU_No_Ox_Survive2[i], p_ISevere_No_ICU_No_Ox_Survive2_R) # Number progressing through requiring ICU bed and oxygen, receiving neither -> Recovery


n_ISevere_No_ICU_No_Ox_Die1_ISevere_No_ICU_No_Ox_Die2[] <- rbinom(ISevere_No_ICU_No_Ox_Die1[i], p_ISevere_No_ICU_No_Ox_Die1_ISevere_No_ICU_No_Ox_Die2) # Number progressing through requiring ICU bed and oxygen, receiving neither -> Dying
n_ISevere_No_ICU_No_Ox_Die2_D_Community[] <- rbinom(ISevere_No_ICU_No_Ox_Die2[i], p_ISevere_No_ICU_No_Ox_Die2_D_Community) # Number progressing through requiring ICU bed and oxygen, receiving neither -> Dying

n_ICritical_Get_ICU_Get_Ox_Get_MV_Survive1_ICritical_Get_ICU_Get_Ox_Get_MV_Survive2[] <- rbinom(ICritical_Get_ICU_Get_Ox_Get_MV_Survive1[i], p_ICritical_Get_ICU_Get_Ox_Get_MV_Survive1_ICritical_Get_ICU_Get_Ox_Get_MV_Survive2) # Number progressing through requiring ICU bed, oxygen and MV, and receiving all -> Recovery
n_ICritical_Get_ICU_Get_Ox_Get_MV_Survive2_R[] <- rbinom(ICritical_Get_ICU_Get_Ox_Get_MV_Survive2[i], p_ICritical_Get_ICU_Get_Ox_Get_MV_Survive2_R) # Number progressing through requiring ICU bed, oxygen and MV, and receiving all -> Recovery
n_ICritical_Get_ICU_Get_Ox_Get_MV_Die1_ICritical_Get_ICU_Get_Ox_Get_MV_Die2[] <- rbinom(ICritical_Get_ICU_Get_Ox_Get_MV_Die1[i], p_ICritical_Get_ICU_Get_Ox_Get_MV_Die1_ICritical_Get_ICU_Get_Ox_Get_MV_Die2) # Number progressing through requiring ICU bed, oxygen and MV, and receiving all -> Dying
n_ICritical_Get_ICU_Get_Ox_Get_MV_Die2_D_Hospital[] <- rbinom(ICritical_Get_ICU_Get_Ox_Get_MV_Die2[i], p_ICritical_Get_ICU_Get_Ox_Get_MV_Die2_D_Hospital) # Number progressing through requiring ICU bed, oxygen and MV, and receiving all -> Recovery
n_ICritical_Get_ICU_Get_Ox_No_MV_Survive1_ICritical_Get_ICU_Get_Ox_No_MV_Survive2[] <- rbinom(ICritical_Get_ICU_Get_Ox_No_MV_Survive1[i], p_ICritical_Get_ICU_Get_Ox_No_MV_Survive1_ICritical_Get_ICU_Get_Ox_No_MV_Survive2) # Number progressing through requiring ICU bed, oxygen and MV, and receiving ICU bed and oxygen only -> Recovery
n_ICritical_Get_ICU_Get_Ox_No_MV_Survive2_R[] <- rbinom(ICritical_Get_ICU_Get_Ox_No_MV_Survive2[i], p_ICritical_Get_ICU_Get_Ox_No_MV_Survive2_R) # Number progressing through requiring ICU bed, oxygen and MV, and receiving ICU bed and oxygen only -> Recovery
n_ICritical_Get_ICU_Get_Ox_No_MV_Die1_ICritical_Get_ICU_Get_Ox_No_MV_Die2[] <- rbinom(ICritical_Get_ICU_Get_Ox_No_MV_Die1[i], p_ICritical_Get_ICU_Get_Ox_No_MV_Die1_ICritical_Get_ICU_Get_Ox_No_MV_Die2) # Number progressing through requiring ICU bed, oxygen and MV, and receiving ICU bed and oxygen only -> Dying
n_ICritical_Get_ICU_Get_Ox_No_MV_Die2_D_Hospital[] <- rbinom(ICritical_Get_ICU_Get_Ox_No_MV_Die2[i], p_ICritical_Get_ICU_Get_Ox_No_MV_Die2_D_Hospital) # Number progressing through requiring ICU bed, oxygen and MV, and receiving ICU bed and oxygen only -> Dying
n_ICritical_Get_ICU_No_Ox_No_MV_Survive1_ICritical_Get_ICU_No_Ox_No_MV_Survive2[] <- rbinom(ICritical_Get_ICU_No_Ox_No_MV_Survive1[i], ICritical_Get_ICU_No_Ox_No_MV_Survive1_ICritical_Get_ICU_No_Ox_No_MV_Survive2) # Number progressing through requiring ICU bed, oxygen and MV, receiving ICU bed only -> Recovery
n_ICritical_Get_ICU_No_Ox_No_MV_Survive2_R[] <- rbinom(ICritical_Get_ICU_No_Ox_No_MV_Survive2[i], p_ICritical_Get_ICU_No_Ox_No_MV_Survive2_R) # Number progressing through requiring ICU bed, oxygen and MV, receiving ICU bed only -> Recovery
n_ICritical_Get_ICU_No_Ox_No_MV_Die1_ICritical_Get_ICU_No_Ox_No_MV_Die2[] <- rbinom(ICritical_Get_ICU_No_Ox_No_MV_Die1[i], p_ICritical_Get_ICU_No_Ox_No_MV_Die1_ICritical_Get_ICU_No_Ox_No_MV_Die2) # Number progressing through requiring ICU bed, oxygen and MV, receiving ICU bed only -> Dying
n_ICritical_Get_ICU_No_Ox_No_MV_Die2_D_Hospital[] <- rbinom(ICritical_Get_ICU_No_Ox_No_MV_Die2[i], p_ICritical_Get_ICU_No_Ox_No_MV_Die2_D_Hospital) # Number progressing through requiring ICU bed, oxygen and MV, receiving ICU bed only -> Dying
n_ICritical_No_ICU_No_Ox_No_MV_Survive1_ICritical_No_ICU_No_Ox_No_MV_Survive2[] <- rbinom(ICritical_No_ICU_No_Ox_No_MV_Survive1[i], p_ICritical_No_ICU_No_Ox_No_MV_Survive1_ICritical_No_ICU_No_Ox_No_MV_Survive2) # Number progressing through requiring ICU bed, oxygen and MV, receiving nothing -> Recovery
n_ICritical_No_ICU_No_Ox_No_MV_Survive2_R[] <- rbinom(ICritical_No_ICU_No_Ox_No_MV_Survive2[i], p_ICritical_No_ICU_No_Ox_No_MV_Survive2_R) # Number progressing through requiring ICU bed, oxygen and MV, receiving nothing -> Recovery
n_ICritical_No_ICU_No_Ox_No_MV_Die1_ICritical_No_ICU_No_Ox_No_MV_Die2[] <- rbinom(ICritical_No_ICU_No_Ox_No_MV_Die1[i], p_ICritical_No_ICU_No_Ox_No_MV_Die1_ICritical_No_ICU_No_Ox_No_MV_Die2) # Number progressing through requiring ICU bed, oxygen and MV, receiving nothing -> Dying
n_ICritical_No_ICU_No_Ox_No_MV_Die2_D_Community[] <- rbinom(ICritical_No_ICU_No_Ox_No_MV_Die2[i], p_ICritical_No_ICU_No_Ox_No_MV_Die2_D_Community) # Number progressing through requiring ICU bed, oxygen and MV, receiving nothing -> Dying

# Working Out Number of ICU Beds Available and How Many Individuals Receive Them
ICU_occ <- sum(ISevere_Get_ICU_Get_Ox_Survive1) + sum(ISevere_Get_ICU_Get_Ox_Survive2) + sum(ISevere_Get_ICU_Get_Ox_Die1) + sum(ISevere_Get_ICU_Get_Ox_Die2) +
           sum(ISevere_Get_ICU_No_Ox_Survive1) + sum(ISevere_Get_ICU_No_Ox_Survive2) + sum(ISevere_Get_ICU_No_Ox_Die1) + sum(ISevere_Get_ICU_No_Ox_Die2) +
           sum(ICritical_Get_ICU_Get_Ox_Get_MV_Survive1) + sum(ICritical_Get_ICU_Get_Ox_Get_MV_Survive2) + sum(ICritical_Get_ICU_Get_Ox_Get_MV_Die1) +
           sum(ICritical_Get_ICU_Get_Ox_Get_MV_Die2) + sum(ICritical_Get_ICU_Get_Ox_No_MV_Survive1) + sum(ICritical_Get_ICU_Get_Ox_No_MV_Survive2) +
           sum(ICritical_Get_ICU_Get_Ox_No_MV_Die1) + sum(ICritical_Get_ICU_Get_Ox_No_MV_Die2) + sum(ICritical_Get_ICU_No_Ox_No_MV_Survive1) +
           sum(ICritical_Get_ICU_No_Ox_No_MV_Survive2) + sum(ICritical_Get_ICU_No_Ox_No_MV_Die1) + sum(ICritical_Get_ICU_No_Ox_No_MV_Die2)
number_requiring_ICU[] <- rbinom(n_ICase2_Hosp[i], prob_severe[i]) # Number of new hospitalisations that are going to require an ICU bed (either with or w/o mechanical ventilation)
total_number_requiring_ICU <- sum(number_requiring_ICU) # Totalling number newly requiring an ICU bed over age groups
current_free_ICU <- ICU_bed_capacity + sum(n_ISevere_Get_ICU_Get_Ox_Survive2_R) + sum(n_ISevere_Get_ICU_Get_Ox_Die2_D_Hospital) +
                    sum(n_ISevere_Get_ICU_No_Ox_Survive2_R) + sum(n_ISevere_Get_ICU_No_Ox_Die2_D_Hospital) + sum(n_ICritical_Get_ICU_Get_Ox_Get_MV_Survive2_R) +
                    sum(n_ICritical_Get_ICU_Get_Ox_Get_MV_Die2_D_Hospital) + sum(n_ICritical_Get_ICU_Get_Ox_No_MV_Survive2_R) + sum(n_ICritical_Get_ICU_Get_Ox_No_MV_Die2_D_Hospital) +
                    sum(n_ICritical_Get_ICU_No_Ox_No_MV_Survive2_R) + sum(n_ICritical_Get_ICU_No_Ox_No_MV_Die2_D_Hospital) - ICU_occ # Number of ICU beds that are currently free
total_number_get_ICU <- if(current_free_ICU <= 0) 0 else(if(current_free_ICU - total_number_requiring_ICU >= 0) total_number_requiring_ICU else(current_free_ICU)) # Working out the number of new ICU requiring infections that get a bed
number_get_ICU[] <- rmhyper(total_number_get_ICU, number_requiring_ICU)

# Working Out Number of Hospital Beds Available and How Many Individuals Receive Them
hosp_bed_occ <- sum(IModerate_Get_Hosp_Get_Ox_Survive1) + sum(IModerate_Get_Hosp_Get_Ox_Survive2) + sum(IModerate_Get_Hosp_Get_Ox_Die1) +
                sum(IModerate_Get_Hosp_Get_Ox_Die2) + sum(IModerate_Get_Hosp_No_Ox_Survive1) + sum(IModerate_Get_Hosp_No_Ox_Survive2) +
                sum(IModerate_Get_Hosp_No_Ox_Die1) + sum(IModerate_Get_Hosp_No_Ox_Die2) + sum(IRec1) + sum(IRec2)
number_requiring_hosp_bed[] <- n_ICase2_Hosp[i] - number_requiring_ICU[i]  # Number of new hospitalisations that are going to require a hospital bed
total_number_requiring_hosp_bed <- sum(number_requiring_hosp_bed) # Totalling number newly requiring a hospital bed over age groups
current_free_hosp_beds <- hosp_bed_capacity + sum(n_IModerate_Get_Hosp_Get_Ox_Survive2_R) + sum(n_IModerate_Get_Hosp_Get_Ox_Die2_D_Hospital) +
                          sum(n_IModerate_Get_Hosp_No_Ox_Survive2_R) + sum(n_IModerate_Get_Hosp_No_Ox_Die2_D_Hospital) + sum(n_IRec2_R) -
                          sum(n_ISevere_Get_ICU_Get_Ox_Survive2_R) - sum(n_ISevere_Get_ICU_No_Ox_Survive2_R) - hosp_occ # Number of hospital beds that are currently free
total_number_get_hosp_beds <- if (current_free_hosp_beds <= 0) 0 else (if(current_free_hosp_beds - total_number_requiring_hosp_bed >= 0) total_number_requiring_hosp_bed else(current_free_hosp_beds)) # Working out the number of new hospital bed requiring infections that get a bed
number_get_hosp_bed[] <- rmhyper(total_number_get_hosp_beds, number_requiring_hosp_bed)
number_not_hosp_bed[] <- number_requiring_hosp_bed[i] - number_get_hosp_bed[i]

# Working Out How Much Oxygen There Is Available and How Many Individuals Requiring Hospital/ICU Bed Receive Them
oxygen_availability <- oxygen_supply + previous_reserves
prop_ox_hosp_beds <- total_number_get_hosp_beds/(total_number_get_hosp_beds + total_number_get_ICU * severe_critical_case_oxygen_consumption_multiplier)
available_oxygen_for_hosp_beds <- floor(prop_ox_hosp_beds * oxygen_availability)
available_oxygen_for_ICU_beds <- floor((oxygen_availability - available_oxygen_for_hosp_beds)/severe_critical_case_oxygen_consumption_multiplier)
previous_reserves <- oxygen_availability - available_oxygen_for_hosp_beds - available_oxygen_for_ICU_beds # check this doesn't go below 0
number_get_hosp_Ox[] <- rmhyper(available_oxygen_for_hosp_beds, number_get_hosp_bed)
number_get_hosp_no_Ox[] <- number_get_hosp_bed[i] - number_get_hosp_Ox[i]

# Working Out Number of Mechanical Ventilators Available and How Many Individuals Requiring ICU Bed and MV Receive Them
MV_occ <- sum(ICritical_Get_ICU_Get_Ox_Get_MV_Survive1) + sum(ICritical_Get_ICU_Get_Ox_Get_MV_Survive2) + sum(ICritical_Get_ICU_Get_Ox_Get_MV_Die1) + sum(ICritical_Get_ICU_Get_Ox_Get_MV_Die2) # Summing over compartments that use mechanical ventilators
current_free_ICU_MV <- MV_capacity + sum(n_ICritical_Get_ICU_Get_Ox_Get_MV_Survive2_R) + sum(n_ICritical_Get_ICU_Get_Ox_Get_MV_Die2_D_Hospital) - MV_occ # Number of mechanical ventilators that are currently free

number_requiring_ICU_MV[] <- rbinom(number_requiring_ICU[i], prob_critical[i]) # Number of new ICU admissions that are going to require mechanical ventilation
total_number_requiring_ICU_MV <- sum(number_requiring_ICU_MV) # Totalling number newly requiring an ICU bed and mechanical ventilation over age groups
available_oxygen_for_ICU_MV <- available_oxygen_for_ICU_beds * round(total_number_requiring_ICU_MV/(total_number_requiring_ICU_MV + total_number_requiring_ICU_Ox))
initial_initial_number_get_ICU_Ox_MV[] <- rmhyper(available_oxygen_for_ICU_MV, number_requiring_ICU_MV)
initial_number_get_ICU_Ox_MV[] <- rmhyper(current_free_ICU_MV, initial_initial_number_get_ICU_Ox_MV)  # NEED TO CHECK, NOT SURE RIGHT, POSSIBLY LARGER NUMBER THAN SAMPLED #
initial_number_not_get_ICU_Ox_MV[] <- number_requiring_ICU_MV[i] - initial_number_get_ICU_Ox_MV[i]
spare_MV <- current_free_ICU_MV - sum(initial_number_get_ICU_Ox_MV[i])
extra_ICU_Ox_MV[] <- rmhyper(min(remainder_ICU_Ox, spare_MV), initial_number_not_get_ICU_Ox_MV)
number_get_ICU_Ox_MV[] <- initial_number_get_ICU_Ox_MV[i] + extra_ICU_Ox_MV[i]
number_not_get_ICU_Ox_MV[] <- initial_number_not_get_ICU_Ox_MV[i] - extra_ICU_Ox_MV[i]
remainder_ICU_MV <- available_oxygen_for_ICU_MV - sum(initial_number_get_ICU_Ox_MV[i]) # this means there is spare oxygen available - CHECK STRICLTY = OR GREATER THAN 0

number_requiring_ICU_Ox[] <- number_requiring_ICU[i] - number_requiring_ICU_MV[i]
total_number_requiring_ICU_Ox <- sum(number_requiring_ICU_Ox) # Totalling number newly requiring an ICU bed and oxygen over age groups
available_oxygen_for_ICU_Ox <- available_oxygen_for_ICU_beds - available_oxygen_for_ICU_MV
initial_number_get_ICU_Ox[] <- rmhyper(available_oxygen_for_ICU_Ox, number_requiring_ICU_Ox)
initial_number_not_get_ICU_Ox[] <- number_requiring_ICU_Ox[i] - number_get_ICU_Ox[i]
extra_get_ICU_Ox[] <- rmhyper(remainder_ICU_MV, initial_number_not_get_ICU_Ox)
number_get_ICU_Ox <- initial_number_get_ICU_Ox[i] + extra_get_ICU_Ox[i]
number_not_get_ICU_Ox <- initial_number_not_get_ICU_Ox[i] - extra_get_ICU_Ox[i]
remainder_ICU_Ox <- available_oxygen_for_ICU_Ox - sum(initial_number_get_ICU_Ox[i]) # this means there is spare oxygen available - CHECK STRICLTY = OR GREATER THAN 0

## TOTALLING UP THE FLOWS IN AND OUT OF EACH COMPARTMENT
##------------------------------------------------------------------------------
delta_E1[] <- n_S_E1[i] - n_E1_E2[i]
delta_E2[] <- n_E1_E2[i] - n_E2_I[i]
delta_IMild[] <- n_E2_IMild[i] - n_IMild_R[i]
delta_ICase1[] <- n_E2_ICase1[i] - n_ICase1_ICase2[i]
delta_ICase2[] <- n_ICase1_ICase2[i] - n_ICase2_Hosp[i]
delta_IRec1[] <- n_IMVGetLive2_Rec[i] - n_IRec1_IRec2[i]
delta_IRec2[] <- n_IRec1_IRec2[i] - n_IRec2_R[i]
delta_R[] <- n_IOxGetLive2_R[i] + n_IOxNotGetLive2_R[i] + n_IRec2_R[i] + n_IMVNotGetLive2_R[i] + n_IMild_R[i]
delta_D_Community[] <- n_IOxNotGetDie2_D[i] + n_IMVNotGetDie2_D[i]
delta_D_Hospital[] <- n_IOxGetDie2_D[i] + n_IMVGetDie2_D[i]

delta_IModerate_Get_Hosp_Get_Ox_Survive1[] <-
delta_IModerate_Get_Hosp_Get_Ox_Survive2[] <-
delta_IModerate_Get_Hosp_Get_Ox_Die1[] <-
delta_IModerate_Get_Hosp_Get_Ox_Die2[] <-
delta_IModerate_Get_Hosp_No_Ox_Survive1[] <-
delta_IModerate_Get_Hosp_No_Ox_Survive2[] <-
delta_IModerate_Get_Hosp_No_Ox_Die1[] <-
delta_IModerate_Get_Hosp_No_Ox_Die2[] <-
delta_IModerate_No_Hosp_No_Ox_Survive1[] <-
delta_IModerate_No_Hosp_No_Ox_Survive2[] <-
delta_IModerate_No_Hosp_No_Ox_Die1[] <-
delta_IModerate_No_Hosp_No_Ox_Die2[] <-

delta_ISevere_Get_ICU_Get_Ox_Survive1[] <-
delta_ISevere_Get_ICU_Get_Ox_Survive2[] <-
delta_ISevere_Get_ICU_Get_Ox_Die1[] <-
delta_ISevere_Get_ICU_Get_Ox_Die2[] <-
delta_ISevere_Get_ICU_No_Ox_Survive1[] <-
delta_ISevere_Get_ICU_No_Ox_Survive2[] <-
delta_ISevere_Get_ICU_No_Ox_Die1[] <-
delta_ISevere_Get_ICU_No_Ox_Die2[] <-
delta_ISevere_No_ICU_No_Ox_Survive1[] <-
delta_ISevere_No_ICU_No_Ox_Survive2[] <-
delta_ISevere_No_ICU_No_Ox_Die1[] <-
delta_ISevere_No_ICU_No_Ox_Die2[] <-

delta_ICritical_Get_ICU_Get_Ox_Get_MV_Survive1[] <-
delta_ICritical_Get_ICU_Get_Ox_Get_MV_Survive2[] <-
delta_ICritical_Get_ICU_Get_Ox_Get_MV_Die1[] <-
delta_ICritical_Get_ICU_Get_Ox_Get_MV_Die2[] <-
delta_ICritical_Get_ICU_Get_Ox_No_MV_Survive1[] <-
delta_ICritical_Get_ICU_Get_Ox_No_MV_Survive2[] <-
delta_ICritical_Get_ICU_Get_Ox_No_MV_Die1[] <-
delta_ICritical_Get_ICU_Get_Ox_No_MV_Die2[] <-
delta_ICritical_Get_ICU_No_Ox_No_MV_Survive1[] <-
delta_ICritical_Get_ICU_No_Ox_No_MV_Survive2[] <-
delta_ICritical_Get_ICU_No_Ox_No_MV_Die1[] <-
delta_ICritical_Get_ICU_No_Ox_No_MV_Die2[] <-
delta_ICritical_No_ICU_No_Ox_No_MV_Survive1[] <-
delta_ICritical_No_ICU_No_Ox_No_MV_Survive2[] <-
delta_ICritical_No_ICU_No_Ox_No_MV_Die1[] <-
delta_ICritical_No_ICU_No_Ox_No_MV_Die2[] <-


## COMPUTING THE FORCE OF INFECTION
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

## PARAMETERS DESCRIBING THE AVAILABILITY OF HEALTHCARE MATERIALS
##------------------------------------------------------------------------------
supply_oxygen <- user() # rate of resupply of oxygen
total_number_mv <- user() # number of mechanical ventilators available
severe_critical_case_oxygen_consumption_multiplier <- user() # consumption of oxygen for severe/critical covid-19 cases compared to moderate cases

## DEFINING INITIAL STATES
##------------------------------------------------------------------------------
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
initial(IModerate_Get_Hosp_Get_Ox_Survive1[]) <- IModerate_Get_Hosp_Get_Ox_Survive1_0[i]
initial(IModerate_Get_Hosp_Get_Ox_Survive2[]) <- IModerate_Get_Hosp_Get_Ox_Survive2_0[i]
initial(IModerate_Get_Hosp_Get_Ox_Die1[]) <- IModerate_Get_Hosp_Get_Ox_Die1_0[i]
initial(IModerate_Get_Hosp_Get_Ox_Die2[]) <- IModerate_Get_Hosp_Get_Ox_Die2_0[i]
initial(IModerate_Get_Hosp_No_Ox_Survive1[]) <- IModerate_Get_Hosp_No_Ox_Survive1_0[i]
initial(IModerate_Get_Hosp_No_Ox_Survive2[]) <- IModerate_Get_Hosp_No_Ox_Survive2_0[i]
initial(IModerate_Get_Hosp_No_Ox_Die1[]) <- IModerate_Get_Hosp_No_Ox_Die1_0[i]
initial(IModerate_Get_Hosp_No_Ox_Die2[]) <- IModerate_Get_Hosp_No_Ox_Die2_0[i]
initial(IModerate_No_Hosp_No_Ox_Survive1[]) <- IModerate_No_Hosp_No_Ox_Survive1_0[i]
initial(IModerate_No_Hosp_No_Ox_Survive2[]) <- IModerate_No_Hosp_No_Ox_Survive2_0[i]
initial(IModerate_No_Hosp_No_Ox_Die1[]) <- IModerate_No_Hosp_No_Ox_Die1_0[i]
initial(IModerate_No_Hosp_No_Ox_Die2[]) <- IModerate_No_Hosp_No_Ox_Die2_0[i]
initial(ISevere_Get_ICU_Get_Ox_Survive1[]) <- ISevere_Get_ICU_Get_Ox_Survive1_0[i]
initial(ISevere_Get_ICU_Get_Ox_Survive2[]) <- ISevere_Get_ICU_Get_Ox_Survive2_0[i]
initial(ISevere_Get_ICU_Get_Ox_Die1[]) <- ISevere_Get_ICU_Get_Ox_Die1_0[i]
initial(ISevere_Get_ICU_Get_Ox_Die2[]) <- ISevere_Get_ICU_Get_Ox_Die2_0[i]
initial(ISevere_Get_ICU_No_Ox_Survive1[]) <- ISevere_Get_ICU_No_Ox_Survive1_0[i]
initial(ISevere_Get_ICU_No_Ox_Survive2[]) <- ISevere_Get_ICU_No_Ox_Survive2_0[i]
initial(ISevere_Get_ICU_No_Ox_Die1[]) <- ISevere_Get_ICU_No_Ox_Die1_0[i]
initial(ISevere_Get_ICU_No_Ox_Die2[]) <- ISevere_Get_ICU_No_Ox_Die2_0[i]
initial(ISevere_No_ICU_No_Ox_Survive1[]) <- ISevere_No_ICU_No_Ox_Survive1_0[i]
initial(ISevere_No_ICU_No_Ox_Survive2[]) <- ISevere_No_ICU_No_Ox_Survive2_0[i]
initial(ISevere_No_ICU_No_Ox_Die1[]) <- ISevere_No_ICU_No_Ox_Die1_0[i]
initial(ISevere_No_ICU_No_Ox_Die2[]) <- ISevere_No_ICU_No_Ox_Die2_0[i]
initial(ICritical_Get_ICU_Get_Ox_Get_MV_Survive1[]) <- ICritical_Get_ICU_Get_Ox_Get_MV_Survive1_0[i]
initial(ICritical_Get_ICU_Get_Ox_Get_MV_Survive2[]) <- ICritical_Get_ICU_Get_Ox_Get_MV_Survive2_0[i]
initial(ICritical_Get_ICU_Get_Ox_Get_MV_Die1[]) <- ICritical_Get_ICU_Get_Ox_Get_MV_Die1_0[i]
initial(ICritical_Get_ICU_Get_Ox_Get_MV_Die2[]) <- ICritical_Get_ICU_Get_Ox_Get_MV_Die2_0[i]
initial(ICritical_Get_ICU_Get_Ox_No_MV_Survive1[]) <- ICritical_Get_ICU_Get_Ox_No_MV_Survive1_0[i]
initial(ICritical_Get_ICU_Get_Ox_No_MV_Survive2[]) <- ICritical_Get_ICU_Get_Ox_No_MV_Survive2_0[i]
initial(ICritical_Get_ICU_Get_Ox_No_MV_Die1[]) <- ICritical_Get_ICU_Get_Ox_No_MV_Die1_0[i]
initial(ICritical_Get_ICU_Get_Ox_No_MV_Die2[]) <- ICritical_Get_ICU_Get_Ox_No_MV_Die2_0[i]
initial(ICritical_Get_ICU_No_Ox_No_MV_Survive1[]) <- ICritical_Get_ICU_No_Ox_No_MV_Survive1_0[i]
initial(ICritical_Get_ICU_No_Ox_No_MV_Survive2[]) <- ICritical_Get_ICU_No_Ox_No_MV_Survive2_0[i]
initial(ICritical_Get_ICU_No_Ox_No_MV_Die1[]) <- ICritical_Get_ICU_No_Ox_No_MV_Die1_0[i]
initial(ICritical_Get_ICU_No_Ox_No_MV_Die2[]) <- ICritical_Get_ICU_No_Ox_No_MV_Die2_0[i]
initial(ICritical_No_ICU_No_Ox_No_MV_Survive1[]) <- ICritical_No_ICU_No_Ox_No_MV_Survive1_0[i]
initial(ICritical_No_ICU_No_Ox_No_MV_Survive2[]) <- ICritical_No_ICU_No_Ox_No_MV_Survive2_0[i]
initial(ICritical_No_ICU_No_Ox_No_MV_Die1[]) <- ICritical_No_ICU_No_Ox_No_MV_Die1_0[i]
initial(ICritical_No_ICU_No_Ox_No_MV_Die2[]) <- ICritical_No_ICU_No_Ox_No_MV_Die2_0[i]

##Initial vectors
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
IModerate_Get_Hosp_Get_Ox_Survive1_0[] <- user()
IModerate_Get_Hosp_Get_Ox_Survive2_0[] <- user()
IModerate_Get_Hosp_Get_Ox_Die1_0[] <- user()
IModerate_Get_Hosp_Get_Ox_Die2_0[] <- user()
IModerate_Get_Hosp_No_Ox_Survive1_0[] <- user()
IModerate_Get_Hosp_No_Ox_Survive2_0[] <- user()
IModerate_Get_Hosp_No_Ox_Die1_0[] <- user()
IModerate_Get_Hosp_No_Ox_Die2_0[] <- user()
IModerate_No_Hosp_No_Ox_Survive1_0[] <- user()
IModerate_No_Hosp_No_Ox_Survive2_0[] <- user()
IModerate_No_Hosp_No_Ox_Die1_0[] <- user()
IModerate_No_Hosp_No_Ox_Die2_0[] <- user()
ISevere_Get_ICU_Get_Ox_Survive1_0[] <- user()
ISevere_Get_ICU_Get_Ox_Survive2_0[] <- user()
ISevere_Get_ICU_Get_Ox_Die1_0[] <- user()
ISevere_Get_ICU_Get_Ox_Die2_0[] <- user()
ISevere_Get_ICU_No_Ox_Survive1_0[] <- user()
ISevere_Get_ICU_No_Ox_Survive2_0[] <- user()
ISevere_Get_ICU_No_Ox_Die1_0[] <- user()
ISevere_Get_ICU_No_Ox_Die2_0[] <- user()
ISevere_No_ICU_No_Ox_Survive1_0[] <- user()
ISevere_No_ICU_No_Ox_Survive2_0[] <- user()
ISevere_No_ICU_No_Ox_Die1_0[] <- user()
ISevere_No_ICU_No_Ox_Die2_0[] <- user()
ICritical_Get_ICU_Get_Ox_Get_MV_Survive1_0[] <- user()
ICritical_Get_ICU_Get_Ox_Get_MV_Survive2_0[] <- user()
ICritical_Get_ICU_Get_Ox_Get_MV_Die1_0[] <- user()
ICritical_Get_ICU_Get_Ox_Get_MV_Die2_0[] <- user()
ICritical_Get_ICU_Get_Ox_No_MV_Survive1_0[] <- user()
ICritical_Get_ICU_Get_Ox_No_MV_Survive2_0[] <- user()
ICritical_Get_ICU_Get_Ox_No_MV_Die1_0[] <- user()
ICritical_Get_ICU_Get_Ox_No_MV_Die2_0[] <- user()
ICritical_Get_ICU_No_Ox_No_MV_Survive1_0[] <- user()
ICritical_Get_ICU_No_Ox_No_MV_Survive2_0[] <- user()
ICritical_Get_ICU_No_Ox_No_MV_Die1_0[] <- user()
ICritical_Get_ICU_No_Ox_No_MV_Die2_0[] <- user()
ICritical_No_ICU_No_Ox_No_MV_Survive1_0[] <- user()
ICritical_No_ICU_No_Ox_No_MV_Survive2_0[] <- user()
ICritical_No_ICU_No_Ox_No_MV_Die1_0[] <- user()
ICritical_No_ICU_No_Ox_No_MV_Die2_0[] <- user()

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
dim(IModerate_Get_Hosp_Get_Ox_Survive1) <- N_age
dim(IModerate_Get_Hosp_Get_Ox_Survive2) <- N_age
dim(IModerate_Get_Hosp_Get_Ox_Die1) <- N_age
dim(IModerate_Get_Hosp_Get_Ox_Die2) <- N_age
dim(IModerate_Get_Hosp_No_Ox_Survive1) <- N_age
dim(IModerate_Get_Hosp_No_Ox_Survive2) <- N_age
dim(IModerate_Get_Hosp_No_Ox_Die1) <- N_age
dim(IModerate_Get_Hosp_No_Ox_Die2) <- N_age
dim(IModerate_No_Hosp_No_Ox_Survive1) <- N_age
dim(IModerate_No_Hosp_No_Ox_Survive2) <- N_age
dim(IModerate_No_Hosp_No_Ox_Die1) <- N_age
dim(IModerate_No_Hosp_No_Ox_Die2) <- N_age
dim(ISevere_Get_ICU_Get_Ox_Survive1) <- N_age
dim(ISevere_Get_ICU_Get_Ox_Survive2) <- N_age
dim(ISevere_Get_ICU_Get_Ox_Die1) <- N_age
dim(ISevere_Get_ICU_Get_Ox_Die2) <- N_age
dim(ISevere_Get_ICU_No_Ox_Survive1) <- N_age
dim(ISevere_Get_ICU_No_Ox_Survive2) <- N_age
dim(ISevere_Get_ICU_No_Ox_Die1) <- N_age
dim(ISevere_Get_ICU_No_Ox_Die2) <- N_age
dim(ISevere_No_ICU_No_Ox_Survive1) <- N_age
dim(ISevere_No_ICU_No_Ox_Survive2) <- N_age
dim(ISevere_No_ICU_No_Ox_Die1) <- N_age
dim(ISevere_No_ICU_No_Ox_Die2) <- N_age
dim(ICritical_Get_ICU_Get_Ox_Get_MV_Survive1) <- N_age
dim(ICritical_Get_ICU_Get_Ox_Get_MV_Survive2) <- N_age
dim(ICritical_Get_ICU_Get_Ox_Get_MV_Die1) <- N_age
dim(ICritical_Get_ICU_Get_Ox_Get_MV_Die2) <- N_age
dim(ICritical_Get_ICU_Get_Ox_No_MV_Survive1) <- N_age
dim(ICritical_Get_ICU_Get_Ox_No_MV_Survive2) <- N_age
dim(ICritical_Get_ICU_Get_Ox_No_MV_Die1) <- N_age
dim(ICritical_Get_ICU_Get_Ox_No_MV_Die2) <- N_age
dim(ICritical_Get_ICU_No_Ox_No_MV_Survive1) <- N_age
dim(ICritical_Get_ICU_No_Ox_No_MV_Survive2) <- N_age
dim(ICritical_Get_ICU_No_Ox_No_MV_Die1) <- N_age
dim(ICritical_Get_ICU_No_Ox_No_MV_Die2) <- N_age
dim(ICritical_No_ICU_No_Ox_No_MV_Survive1) <- N_age
dim(ICritical_No_ICU_No_Ox_No_MV_Survive2) <- N_age
dim(ICritical_No_ICU_No_Ox_No_MV_Die1) <- N_age
dim(ICritical_No_ICU_No_Ox_No_MV_Die2) <- N_age

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
dim(IModerate_Get_Hosp_Get_Ox_Survive1_0) <- N_age
dim(IModerate_Get_Hosp_Get_Ox_Survive2_0) <- N_age
dim(IModerate_Get_Hosp_Get_Ox_Die1_0) <- N_age
dim(IModerate_Get_Hosp_Get_Ox_Die2_0) <- N_age
dim(IModerate_Get_Hosp_No_Ox_Survive1_0) <- N_age
dim(IModerate_Get_Hosp_No_Ox_Survive2_0) <- N_age
dim(IModerate_Get_Hosp_No_Ox_Die1_0) <- N_age
dim(IModerate_Get_Hosp_No_Ox_Die2_0) <- N_age
dim(IModerate_No_Hosp_No_Ox_Survive1_0) <- N_age
dim(IModerate_No_Hosp_No_Ox_Survive2_0) <- N_age
dim(IModerate_No_Hosp_No_Ox_Die1_0) <- N_age
dim(IModerate_No_Hosp_No_Ox_Die2_0) <- N_age
dim(ISevere_Get_ICU_Get_Ox_Survive1_0) <- N_age
dim(ISevere_Get_ICU_Get_Ox_Survive2_0) <- N_age
dim(ISevere_Get_ICU_Get_Ox_Die1_0) <- N_age
dim(ISevere_Get_ICU_Get_Ox_Die2_0) <- N_age
dim(ISevere_Get_ICU_No_Ox_Survive1_0) <- N_age
dim(ISevere_Get_ICU_No_Ox_Survive2_0) <- N_age
dim(ISevere_Get_ICU_No_Ox_Die1_0) <- N_age
dim(ISevere_Get_ICU_No_Ox_Die2_0) <- N_age
dim(ISevere_No_ICU_No_Ox_Survive1_0) <- N_age
dim(ISevere_No_ICU_No_Ox_Survive2_0) <- N_age
dim(ISevere_No_ICU_No_Ox_Die1_0) <- N_age
dim(ISevere_No_ICU_No_Ox_Die2_0) <- N_age
dim(ICritical_Get_ICU_Get_Ox_Get_MV_Survive1_0) <- N_age
dim(ICritical_Get_ICU_Get_Ox_Get_MV_Survive2_0) <- N_age
dim(ICritical_Get_ICU_Get_Ox_Get_MV_Die1_0) <- N_age
dim(ICritical_Get_ICU_Get_Ox_Get_MV_Die2_0) <- N_age
dim(ICritical_Get_ICU_Get_Ox_No_MV_Survive1_0) <- N_age
dim(ICritical_Get_ICU_Get_Ox_No_MV_Survive2_0) <- N_age
dim(ICritical_Get_ICU_Get_Ox_No_MV_Die1_0) <- N_age
dim(ICritical_Get_ICU_Get_Ox_No_MV_Die2_0) <- N_age
dim(ICritical_Get_ICU_No_Ox_No_MV_Survive1_0) <- N_age
dim(ICritical_Get_ICU_No_Ox_No_MV_Survive2_0) <- N_age
dim(ICritical_Get_ICU_No_Ox_No_MV_Die1_0) <- N_age
dim(ICritical_Get_ICU_No_Ox_No_MV_Die2_0) <- N_age
dim(ICritical_No_ICU_No_Ox_No_MV_Survive1_0) <- N_age
dim(ICritical_No_ICU_No_Ox_No_MV_Survive2_0) <- N_age
dim(ICritical_No_ICU_No_Ox_No_MV_Die1_0) <- N_age
dim(ICritical_No_ICU_No_Ox_No_MV_Die2_0) <- N_age

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
dim(delta_IModerate_Get_Hosp_Get_Ox_Survive1) <- N_age
dim(delta_IModerate_Get_Hosp_Get_Ox_Survive2) <- N_age
dim(delta_IModerate_Get_Hosp_Get_Ox_Die1) <- N_age
dim(delta_IModerate_Get_Hosp_Get_Ox_Die2) <- N_age
dim(delta_IModerate_Get_Hosp_No_Ox_Survive1) <- N_age
dim(delta_IModerate_Get_Hosp_No_Ox_Survive2) <- N_age
dim(delta_IModerate_Get_Hosp_No_Ox_Die1) <- N_age
dim(delta_IModerate_Get_Hosp_No_Ox_Die2) <- N_age
dim(delta_IModerate_No_Hosp_No_Ox_Survive1) <- N_age
dim(delta_IModerate_No_Hosp_No_Ox_Survive2) <- N_age
dim(delta_IModerate_No_Hosp_No_Ox_Die1) <- N_age
dim(delta_IModerate_No_Hosp_No_Ox_Die2) <- N_age
dim(delta_ISevere_Get_ICU_Get_Ox_Survive1) <- N_age
dim(delta_ISevere_Get_ICU_Get_Ox_Survive2) <- N_age
dim(delta_ISevere_Get_ICU_Get_Ox_Die1) <- N_age
dim(delta_ISevere_Get_ICU_Get_Ox_Die2) <- N_age
dim(delta_ISevere_Get_ICU_No_Ox_Survive1) <- N_age
dim(delta_ISevere_Get_ICU_No_Ox_Survive2) <- N_age
dim(delta_ISevere_Get_ICU_No_Ox_Die1) <- N_age
dim(delta_ISevere_Get_ICU_No_Ox_Die2) <- N_age
dim(delta_ISevere_No_ICU_No_Ox_Survive1) <- N_age
dim(delta_ISevere_No_ICU_No_Ox_Survive2) <- N_age
dim(delta_ISevere_No_ICU_No_Ox_Die1) <- N_age
dim(delta_ISevere_No_ICU_No_Ox_Die2) <- N_age
dim(delta_ICritical_Get_ICU_Get_Ox_Get_MV_Survive1) <- N_age
dim(delta_ICritical_Get_ICU_Get_Ox_Get_MV_Survive2) <- N_age
dim(delta_ICritical_Get_ICU_Get_Ox_Get_MV_Die1) <- N_age
dim(delta_ICritical_Get_ICU_Get_Ox_Get_MV_Die2) <- N_age
dim(delta_ICritical_Get_ICU_Get_Ox_No_MV_Survive1) <- N_age
dim(delta_ICritical_Get_ICU_Get_Ox_No_MV_Survive2) <- N_age
dim(delta_ICritical_Get_ICU_Get_Ox_No_MV_Die1) <- N_age
dim(delta_ICritical_Get_ICU_Get_Ox_No_MV_Die2) <- N_age
dim(delta_ICritical_Get_ICU_No_Ox_No_MV_Survive1) <- N_age
dim(delta_ICritical_Get_ICU_No_Ox_No_MV_Survive2) <- N_age
dim(delta_ICritical_Get_ICU_No_Ox_No_MV_Die1) <- N_age
dim(delta_ICritical_Get_ICU_No_Ox_No_MV_Die2) <- N_age
dim(delta_ICritical_No_ICU_No_Ox_No_MV_Survive1) <- N_age
dim(delta_ICritical_No_ICU_No_Ox_No_MV_Survive2) <- N_age
dim(delta_ICritical_No_ICU_No_Ox_No_MV_Die1) <- N_age
dim(delta_ICritical_No_ICU_No_Ox_No_MV_Die2) <- N_age

##############################################
########################## For the Number of People Moving In and Out of Compartments STILL TO DO!!!!!!!!
dim(n_E1_E2) <- N_age
dim(n_E2_I) <- N_age
dim(n_E2_ICase1) <- N_age
dim(n_E2_IMild) <- N_age
dim(n_IMild_R) <- N_age
dim(n_ICase1_ICase2) <- N_age
dim(n_ICase2_Hosp) <- N_age
dim(n_IRec1_IRec2) <- N_age
dim(n_IRec2_R) <- N_age

dim(n_IMVGetDie1) <- N_age
dim(n_IMVGetDie1_IMVGetDie2) <- N_age
dim(n_IMVGetDie2_D) <- N_age
dim(n_IMVGetLive1) <- N_age
dim(n_IMVGetLive1_IMVGetLive2) <- N_age
dim(n_IMVGetLive2_Rec) <- N_age
dim(n_IMVNotGetDie1) <- N_age
dim(n_IMVNotGetDie1_IMVNotGetDie2) <- N_age
dim(n_IMVNotGetDie2_D) <- N_age
dim(n_IMVNotGetLive1) <- N_age
dim(n_IMVNotGetLive1_IMVNotGetLive2) <- N_age
dim(n_IMVNotGetLive2_R) <- N_age

dim(n_IOxGetDie1) <- N_age
dim(n_IOxGetDie1_IOxGetDie2) <- N_age
dim(n_IOxGetDie2_D) <- N_age
dim(n_IOxGetLive1) <- N_age
dim(n_IOxGetLive1_IOxGetLive2) <- N_age
dim(n_IOxGetLive2_R) <- N_age
dim(n_IOxNotGetDie1) <- N_age
dim(n_IOxNotGetDie1_IOxNotGetDie2) <- N_age
dim(n_IOxNotGetDie2_D) <- N_age
dim(n_IOxNotGetLive1) <- N_age
dim(n_IOxNotGetLive1_IOxNotGetLive2) <- N_age
dim(n_IOxNotGetLive2_R) <- N_age

dim(number_requiring_IMV) <- N_age
dim(number_get_IMV) <- N_age
dim(number_notget_IMV) <- N_age
dim(number_requiring_Ox) <- N_age
dim(number_get_Ox) <- N_age
dim(number_notget_Ox) <- N_age
###############################################################
################################################################

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

