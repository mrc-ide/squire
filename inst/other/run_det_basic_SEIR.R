devtools::install_github("https://github.com/mrc-ide/odin.js")
remotes::install_github("mrc-ide/odin.js@mrc-1492", upgrade = FALSE)
library(odin.js)

x <- odin.js::odin_js("inst/other/det_basic_SEIR.R")
path <- odin.js::odin_js_bundle("inst/other/det_basic_SEIR.R", dest = "inst/other/test_out", include_dopri = TRUE)

x <- odin::odin("inst/other/det_basic_SEIR.R")
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
plot(results$t, results$S, ylim = c(0, max(results$S)))
lines(results$t, results$R)

