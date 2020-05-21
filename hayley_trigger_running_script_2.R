# Loading Rrequired Libraries
#library(tidyverse); library(zoo); library(patchwork); library(devtools)

#devtools::document()


# Loading in ICU Capacity
#load("data/income_strata_healthcare_capacity.rda")

# Sourcing Required Functions
#source("hayley_trigger_running_functions.R")

set.seed(5)
# Run Invariant Parameters
suppression_reduction <- 0.25
suppression_duration <- 30
mitigation_reduction <-  1*0.66
max_lockdowns <- 16
R0 <- c(2.12, 1.42461, 0.8498, 1.42461)
tt_R0 <- c(0,   29,      83,     162)
replicates <- 10
r <- run_explicit_SEEIR_model("United Kingdom")
index <- squire:::odin_index(r$model)            # get the indices for each of the model outputs

# Running for LIC
income_strata <- "LMIC"
#trigger_threshold <- 6
country <- "Senegal"
pop <- get_population(country)
pop <- (50000000/sum(pop$n)) * pop$n
contact_matrix <- squire::get_mixing_matrix("Senegal")
income_strata_healthcare_capacity <- squire::income_strata_healthcare_capacity
time_period <- 730
dt <- 0.1

LIC <- run_trigger_threshold(country = country,
                             population = pop,
                             replicates = replicates,
                             income_strata = income_strata,
                             trigger_threshold = trigger_threshold,
                             suppression_reduction = suppression_reduction,
                             suppression_duration = suppression_duration,
                             mitigation_reduction = mitigation_reduction,
                             R0 = R0,
                             tt_R0 = tt_R0,
                             max_lockdowns = 20,
                             hospital_bed_capacity = 16000000, #hospital_bed_capacity,
                             ICU_bed_capacity = c(35,16000000), #ICU_bed_capacity,
                             income_strata_healthcare_capacity = income_strata_healthcare_capacity,
                             poorer_outcomes = poorer_outcomes,
                             time_period = time_period,
                             dt = dt)

LIC_time <- LIC$model_output[, index$time, 1]
LIC_icu <- 35
max_LIC <- get_max_ICU_req(LIC)
LIC_y <- lapply(seq_along(1:dim(LIC$model_output)[3]), function(p) {
  LIC$model_output[, index$ICU_occ, p]
})

LIC_y <- do.call(cbind, LIC_y)

#LIC_lockdown <- round(apply(LIC$time_in_lockdown[1:(time_period/dt), ], 1, median))

#LIC_z_test <- data.frame(time = LIC_time, y = LIC_y) %>%
#  gather(replicate, incidence, -time)
#ggplot(LIC_z_test, aes(x = time, y = incidence, col = replicate)) +
#  geom_line()

#LIC_z <- data.frame(time = LIC_time, y = LIC_y) %>%
#  gather(replicate, incidence, -time) %>%
#  group_by(time) %>%
#  summarise(median = mean(incidence),
#            lower = quantile(incidence, 0.025),
#            upper = quantile(incidence, 0.975))

#LIC_max <- LIC_icu
#d <- ggplot(LIC_z, aes(x = time, y = median)) +
#  geom_ribbon(aes(ymin = 0, ymax = LIC_lockdown * 50), alpha = 0.1) +
#  geom_line(col = "#7067CF", size = 1) +
#  labs(x= " Time ", y=" N ", title="ICU incidence trigger of 3") +
#  geom_line(aes(y = LIC_icu), linetype = "dashed", size = 0.5) +
#    theme_bw(10) +
#  scale_y_continuous(limits=c(0,50))+
#    theme(panel.grid.minor = element_blank(),
#          panel.grid.major.x = element_blank(),
#          panel.grid.major.y = element_blank()) +
#  scale_x_continuous(labels=c("Jan\n20", "Jul\n20", "Jan\n21", "Jul\n21", "Jan\n22"))
#+
 # geom_line(aes(x = tt_R0[1]), linetype = "dashed", size = 0.5) +
#  geom_line(aes(x = tt_R0[2]), linetype = "dashed", size = 0.5) +
 # geom_line(aes(x = tt_R0[3]), linetype = "dashed", size = 0.5) +
#  geom_line(aes(x = tt_R0[4]), linetype = "dashed", size = 0.5)
#d

#sum(LIC_lockdown)


#get_total_deaths <- function(x) {
#  index <- x$index
#  out <- x$model_output
#  replicates <- dim(out)[3]
#  total_daily_deaths <- c()
#  for (i in 1:replicates) {
#    total_daily_deaths[i] <- sum(x$model_output[, index$delta_D, i])#

#  }
#  print(mean(total_daily_deaths))
#  print(quantile(total_daily_deaths, probs = c(0.025, 0.975)))

#}

#get_total_infs <- function(x) {
#  index <- x$index
#  out <- x$model_output
#  replicates <- dim(out)[3]
#  total_daily_infs <- c()
#  for (i in 1:replicates) {
#    total_daily_infs[i] <- sum(x$model_output[, index$n_E2_I, i])
#
#  }
#  print(mean(total_daily_infs))
#  print(quantile(total_daily_infs, probs = c(0.025, 0.975)))


#}

#get_total_infs(LIC)
#get_total_deaths(LIC)

#get_max_ICU_req <- function(x) {
# index <- x$index
# out <- x$model_output
#  replicates <- dim(out)[3]
#  max_ICU_occupancy <- c()
#  for (i in 1:replicates) {
#    daily_ICU_occupancy <- rollapply(out[, index$total_ICU_req, i], 10,
#                                     mean, partial = TRUE, align = "right")
#    daily_ICU_occupancy <- daily_ICU_occupancy[seq(1, length(daily_ICU_occupancy), 10)]
#    max_ICU_occupancy[i] <- max(daily_ICU_occupancy)
#  }
#  print(mean(max_ICU_occupancy))
#  print(quantile(max_ICU_occupancy, probs=c(0.025,0.975)))


#}

#get_max_ICU_req(LIC)



#-------------------------------
#LIC_z <- data.frame(time = LIC_time, y = LIC_y) %>%
#  gather(replicate, incidence, -time) %>%
#  group_by(time) %>%
#  summarise(mean = mean(incidence),
#            lower = quantile(incidence, 0.025),
#            upper = quantile(incidence, 0.975),
#            compartment = "ICU bed demand") %>% ungroup()

#icu_occ = LIC_z[seq(0, nrow(LIC_z), 10), ]

#LIC_lockdown <- LIC$time_in_lockdown[1:(time_period/dt),2]
#LIC_lockdown <- LIC_lockdown[seq(1, length(LIC_lockdown), 10)]

#sum(LIC_lockdown)

#ggplot(icu_occ) +
#  geom_path(mapping=aes(x=time, y=mean), size=1.1, col="navy") +
#  theme_bw(10) +
#  scale_y_continuous(labels=comma) +
#  theme(panel.grid.minor = element_blank(),
#        panel.grid.major.x = element_blank(),
#        panel.grid.major.y = element_blank(),
#        legend.position = "bottom",
#        strip.background = element_blank()) +
#  labs(x=" ", y="N", title="ICU bed demand", col=" ")+
#  geom_hline(aes(yintercept = 35),linetype=2) +
#  scale_x_continuous(breaks=c(100,300,500,700),labels=c("Jul\n20", "Jan\n21", "Jul\n21", "Jan\n22"))+
#  geom_ribbon(aes(x=time, ymin = 0, ymax = LIC_lockdown * 40), alpha = 0.1)

#ggsave("C:/Users/ht1212/Imperial College London/ncov - Documents/2019-nCoV/LMIC/Senegal/supp_figure_4.pdf", height=6, width=9, dpi=600)
