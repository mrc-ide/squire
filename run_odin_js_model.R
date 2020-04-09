# Setting Up Model Parameters
S0 <- c(100000, 100000)
mixing_matrix <- matrix(c(5, 2, 2, 5), nrow = 2, byrow = TRUE)
m <- t(t(mixing_matrix) / S0)
pars <- list(S0 = c(100000, 1000000),
             E0 = c(0, 0),
             I_mild0 = c(100, 100),
             I_hosp0 = c(100, 100),
             I_ICU0 = c(100, 100),
             R0 = c(0, 0),
             D0 = c(0, 0),
             gamma = 0.3,
             sigma = 0.3,
             mu = 0.01,
             p_mild = c(0.33, 0.33),
             p_hosp = c(0.33, 0.33), 
             p_ICU = c(0.34, 0.34),
             beta_1 = 0.1, 
             beta_2 = 0.1,
             m = m)

# Compile and Run Model Using odin.js
path <- system.file("odin/less_basic_model_for_js.R",
                    package = "squire", mustWork = TRUE)
x <- odin.js::odin_js("inst/odin/less_basic_model_for_js.R")
mod <- x(user = pars)
t <- seq(from = 1, to = 200)
output <- mod$run(t)
t <- output[, "t"]
S <- output[, "S[1]"] + output[, "S[2]"]
I <- apply(output[, c(4:11)], 1, sum)
R <- output[, "R[1]"] + output[, "R[2]"]
plot(t, S, ylim = c(0, max(S)), type = "l")
lines(t, I, col = "red")
lines(t, R, col = "green")

# Compile and Run Model Using odin
mod <- squire:::less_basic_model_for_js(user = pars)
t <- seq(from = 1, to = 200)
output <- mod$run(t)
t <- output[, "t"]
S <- output[, "S[1]"] + output[, "S[2]"]
I <- apply(output[, c(4:11)], 1, sum)
R <- output[, "R[1]"] + output[, "R[2]"]
plot(t, S, ylim = c(0, max(S)), type = "l")
lines(t, I, col = "red")
lines(t, R, col = "green")
