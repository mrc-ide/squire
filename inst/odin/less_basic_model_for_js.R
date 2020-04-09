## Initial States
initial(S[]) <- S0[i] 
initial(E[]) <- E0[i]
initial(I_mild[]) <- I_mild0[i] 
initial(I_hosp[]) <- I_hosp0[i] 
initial(I_ICU[]) <- I_ICU0[i] 
initial(R[]) <- R0[i]
initial(D[]) <- D0[i]

# Calculating Force of Infection
temp[] <- I_mild[i] + I_hosp[i] + I_ICU[i]
s_ij[, ] <- m[i, j] * temp[j]
lambda[] <- beta * sum(s_ij[i, ])

# Changing Values for Beta and Contact
beta <- if (t < 50) beta_1 else beta_2

## Core equations for transitions between compartments:
deriv(S[]) <- -beta * S[i] * lambda[i]
deriv(E[]) <- beta * S[i] * lambda[i] - gamma * E[i]
deriv(I_mild[]) <- p_mild[i] * (gamma * E[i]) - sigma * I_mild[i] - mu * I_mild[i]
deriv(I_hosp[]) <- p_hosp[i] * (gamma * E[i]) - sigma * I_hosp[i] - mu * I_hosp[i]
deriv(I_ICU[]) <- p_ICU[i] * (gamma * E[i]) - sigma * I_ICU[i] - mu * I_ICU[i]
deriv(R[]) <- sigma * I_mild[i] + sigma * I_hosp[i] + sigma * I_ICU[i]
deriv(D[]) <- mu * I_mild[i] + mu * I_hosp[i] + mu * I_ICU[i]


## User defined parameters - default in parentheses:
S0[] <- user()
dim(S0) <- 2
dim(S) <- 2

E0[] <- user()
dim(E0) <- 2
dim(E) <- 2

I_mild0[] <- user()
dim(I_mild0) <- 2
dim(I_mild) <- 2

I_hosp0[] <- user()
dim(I_hosp0) <- 2
dim(I_hosp) <- 2

I_ICU0[] <- user()
dim(I_ICU0) <- 2
dim(I_ICU) <- 2

R0[] <- user()
dim(R0) <- 2
dim(R) <- 2

D0[] <- user()
dim(D0) <- 2
dim(D) <- 2

# Parameters
gamma <- user()
sigma <- user()
mu <- user()
beta_1 <- user()
beta_2 <- user()

p_mild[] <- user()
dim(p_mild) <- 2
p_hosp[] <- user()
dim(p_hosp) <- 2
p_ICU[] <- user()
dim(p_ICU) <- 2

m[, ] <- user()
dim(m) <- c(2, 2)
dim(temp) <- 2
dim(s_ij) <- c(2, 2)
dim(lambda) <- 2
