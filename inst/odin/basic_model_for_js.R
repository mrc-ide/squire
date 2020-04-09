## Initial states:
initial(S) <- S0 # will be user-defined
initial(E) <- E0
initial(I_mild) <- I_mild0 # will be user-defined
initial(I_hosp) <- I_hosp0 # will be user-defined
initial(I_ICU) <- I_ICU0 # will be user-defined
initial(R) <- R0
initial(D) <- D0

## Core equations for transitions between compartments:
deriv(S) <- -beta * S * (I_mild + I_hosp + I_ICU) / N
deriv(E) <- beta * contact * S * (I_mild + I_hosp + I_ICU) / N - gamma * E
deriv(I_mild) <- p_mild * (gamma * E) - sigma * I_mild - mu * I_mild
deriv(I_hosp) <- p_hosp * (gamma * E) - sigma * I_hosp - mu * I_hosp
deriv(I_ICU) <- p_ICU * (gamma * E) - sigma * I_ICU - mu * I_ICU
deriv(R) <- sigma * I_mild + sigma * I_hosp + sigma * I_ICU
deriv(D) <- mu * I_mild + mu * I_hosp + mu * I_ICU

## Total population size (odin will recompute this at each timestep:
## automatically)
N <- S + E + I_mild + I_hosp + I_ICU + R + D

## User defined parameters - default in parentheses:
S0 <- user()
E0 <- user()
I_mild0 <- user()
I_hosp0 <- user()
I_ICU0 <- user()
R0 <- user()
D0 <- user()
gamma <- user()
sigma <- user()
mu <- user()
p_mild <- user()
p_hosp <- user()
p_ICU <- user()
beta_1 <- user()
beta_2 <- user()
contact_1 <- user()
contact_2 <- user()

# Changing Values for beta
beta <- if (t < 50) beta_1 else beta_2
contact <- if (t < 50) contact_1 else contact_2