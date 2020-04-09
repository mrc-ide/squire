library(odin.js)
x <- odin::odin("inst/odin/basic_model_for_js.R")
pars <- list(S0 = 100000,
             E0 = 0,
             I_mild0 = 100,
             I_hosp0 = 100,
             I_ICU0 = 100,
             R0 = 0,
             D0 = 0,
             gamma = 0.3,
             sigma = 0.3,
             mu = 0.01,
             p_mild = 0.33,
             p_hosp = 0.33, 
             p_ICU = 0.34,
             beta_1 = 0.5, 
             beta_2 = 0.5,
             contact_1 = 1, 
             contact_2 = 1)

mod <- x(user = pars)
t <- seq(from = 1, to = 1000)
m <- mod$run(t)
results <- mod$transform_variables(m)
plot(m[, "t"], m[, "S"], ylim = c(0, max(m[, "S"])))
lines(m[, "t"], m[, "R"], lwd = 2)