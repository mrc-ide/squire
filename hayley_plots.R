


# to all the replicates - 10 replicates
bloop <- lapply(seq_along(1:dim(LIC$model_output)[3]), function(p) {
  temp <- apply(LIC$model_output[, index$delta_D, p], 1, sum) #to all the row - sum the age groups for that replicate
  daily_deaths <- rollapply(temp, 1/dt, sum, partial = TRUE, align = "right") #take time points to days
  daily_deaths_new <- daily_deaths[seq(1, length(daily_deaths), 10)] # 1/dt so that is the 10th elements = daily
  # list of 10 elements over each p value
})

LIC_deaths <- do.call(cbind, bloop) # turns list into matrix



# ne2 I for infections
# temp call
# aggregate each comp (age) and add all the hosp comps together


# to all the replicates - 10 replicates # Infections
shoop <- lapply(seq_along(1:dim(LIC$model_output)[3]), function(p) {
  temp <- apply(LIC$model_output[, index$n_E2_I, p], 1, sum) #to all the row - sum the age groups for that replicate
  daily_deaths <- rollapply(temp, 1/dt, sum, partial = TRUE, align = "right") #take time points to days
  daily_deaths_new <- daily_deaths[seq(1, length(daily_deaths), 10)] # 1/dt so that is the 10th elements = daily
  # list of 10 elements over each p value
})

LIC_infs <- do.call(cbind, shoop) # turns list into matrix


# total hospital ---------------------------------------------
#c("IOxGetLive1","IOxGetLive2","IOxGetDie1","IOxGetDie2",
#  "IRec1", "IRec2","IOxNotGetLive1","IOxNotGetLive2","IOxNotGetDie1",
#  "IOxNotGetDie2")

# to all the replicates - 10 replicates # Infections
ploop <- lapply(seq_along(1:dim(LIC$model_output)[3]), function(p) {
  temp_1 <- apply(LIC$model_output[, index$IOxGetLive1, p], 1, sum) #to all the row - sum the age groups for that replicate
  daily_1 <- rollapply(temp_1, 1/dt, sum, partial = TRUE, align = "right") #take time points to days
  daily_1 <- daily_1[seq(1, length(daily_1), 10)] # 1/dt so that is the 10th elements = daily
  # list of 10 elements over each p value
})

LIC_hosps_1 <- do.call(cbind, ploop) # turns list into matrix

# ----------------------------------------------------
ploop_2 <- lapply(seq_along(1:dim(LIC$model_output)[3]), function(p) {
  temp_1 <- apply(LIC$model_output[, index$IOxGetLive2, p], 1, sum) #to all the row - sum the age groups for that replicate
  daily_1 <- rollapply(temp_1, 1/dt, sum, partial = TRUE, align = "right") #take time points to days
  daily_1 <- daily_1[seq(1, length(daily_1), 10)] # 1/dt so that is the 10th elements = daily
  # list of 10 elements over each p value
})
LIC_hosps_2 <- do.call(cbind, ploop_2) # turns list into matrix

#----------------------------------------------------------
ploop_3 <- lapply(seq_along(1:dim(LIC$model_output)[3]), function(p) {
  temp_1 <- apply(LIC$model_output[, index$IOxGetDie1, p], 1, sum) #to all the row - sum the age groups for that replicate
  daily_1 <- rollapply(temp_1, 1/dt, sum, partial = TRUE, align = "right") #take time points to days
  daily_1 <- daily_1[seq(1, length(daily_1), 10)] # 1/dt so that is the 10th elements = daily
  # list of 10 elements over each p value
})
LIC_hosps_3 <- do.call(cbind, ploop_3) # turns list into matrix

#----------------------------------------------------------
ploop_4 <- lapply(seq_along(1:dim(LIC$model_output)[3]), function(p) {
  temp_1 <- apply(LIC$model_output[, index$IOxGetDie2, p], 1, sum) #to all the row - sum the age groups for that replicate
  daily_1 <- rollapply(temp_1, 1/dt, sum, partial = TRUE, align = "right") #take time points to days
  daily_1 <- daily_1[seq(1, length(daily_1), 10)] # 1/dt so that is the 10th elements = daily
  # list of 10 elements over each p value
})
LIC_hosps_4 <- do.call(cbind, ploop_4) # turns list into matrix

#----------------------------------------------------------
ploop_5 <- lapply(seq_along(1:dim(LIC$model_output)[3]), function(p) {
  temp_1 <- apply(LIC$model_output[, index$IRec1, p], 1, sum) #to all the row - sum the age groups for that replicate
  daily_1 <- rollapply(temp_1, 1/dt, sum, partial = TRUE, align = "right") #take time points to days
  daily_1 <- daily_1[seq(1, length(daily_1), 10)] # 1/dt so that is the 10th elements = daily
  # list of 10 elements over each p value
})
LIC_hosps_5 <- do.call(cbind, ploop_5) # turns list into matrix

#----------------------------------------------------------
ploop_6 <- lapply(seq_along(1:dim(LIC$model_output)[3]), function(p) {
  temp_1 <- apply(LIC$model_output[, index$IRec2, p], 1, sum) #to all the row - sum the age groups for that replicate
  daily_1 <- rollapply(temp_1, 1/dt, sum, partial = TRUE, align = "right") #take time points to days
  daily_1 <- daily_1[seq(1, length(daily_1), 10)] # 1/dt so that is the 10th elements = daily
  # list of 10 elements over each p value
})
LIC_hosps_6 <- do.call(cbind, ploop_6) # turns list into matrix

#----------------------------------------------------------
ploop_7 <- lapply(seq_along(1:dim(LIC$model_output)[3]), function(p) {
  temp_1 <- apply(LIC$model_output[, index$IOxNotGetLive1, p], 1, sum) #to all the row - sum the age groups for that replicate
  daily_1 <- rollapply(temp_1, 1/dt, sum, partial = TRUE, align = "right") #take time points to days
  daily_1 <- daily_1[seq(1, length(daily_1), 10)] # 1/dt so that is the 10th elements = daily
  # list of 10 elements over each p value
})
LIC_hosps_7 <- do.call(cbind, ploop_7) # turns list into matrix

#----------------------------------------------------------
ploop_8 <- lapply(seq_along(1:dim(LIC$model_output)[3]), function(p) {
  temp_1 <- apply(LIC$model_output[, index$IOxNotGetLive2, p], 1, sum) #to all the row - sum the age groups for that replicate
  daily_1 <- rollapply(temp_1, 1/dt, sum, partial = TRUE, align = "right") #take time points to days
  daily_1 <- daily_1[seq(1, length(daily_1), 10)] # 1/dt so that is the 10th elements = daily
  # list of 10 elements over each p value
})
LIC_hosps_8 <- do.call(cbind, ploop_8) # turns list into matrix


#----------------------------------------------------------
ploop_9 <- lapply(seq_along(1:dim(LIC$model_output)[3]), function(p) {
  temp_1 <- apply(LIC$model_output[, index$IOxNotGetDie1, p], 1, sum) #to all the row - sum the age groups for that replicate
  daily_1 <- rollapply(temp_1, 1/dt, sum, partial = TRUE, align = "right") #take time points to days
  daily_1 <- daily_1[seq(1, length(daily_1), 10)] # 1/dt so that is the 10th elements = daily
  # list of 10 elements over each p value
})
LIC_hosps_9 <- do.call(cbind, ploop_9) # turns list into matrix

#----------------------------------------------------------
ploop_10 <- lapply(seq_along(1:dim(LIC$model_output)[3]), function(p) {
  temp_1 <- apply(LIC$model_output[, index$IOxNotGetDie2, p], 1, sum) #to all the row - sum the age groups for that replicate
  daily_1 <- rollapply(temp_1, 1/dt, sum, partial = TRUE, align = "right") #take time points to days
  daily_1 <- daily_1[seq(1, length(daily_1), 10)] # 1/dt so that is the 10th elements = daily
  # list of 10 elements over each p value
})
LIC_hosps_10 <- do.call(cbind, ploop_10) # turns list into matrix

#----------------------------------------------------------

# sum all the hospital compartments together
LIC_hosp_dem <- LIC_hosps_1 + LIC_hosps_2 + LIC_hosps_3 + LIC_hosps_4 + LIC_hosps_5 + LIC_hosps_6 + LIC_hosps_7 + LIC_hosps_7 + LIC_hosps_8 + LIC_hosps_9 + LIC_hosps_10

#---------------------------------------------------------
# take the mean of all the replicates
par_median = apply(LIC_hosp_dem, MARGIN=1, FUN=mean)
par_quant_1 = apply(LIC_hosp_dem, MARGIN=1, quantile, probs=c(0.025))
par_quant_2 = apply(LIC_hosp_dem, MARGIN=1, quantile, probs=c(0.975))

hosp <- data.frame(mean=par_median,
                   lower = par_quant_1,
                   upper = par_quant_2,
                   compartment = "Hospital bed demand")

par_median = apply(LIC_deaths, MARGIN=1, FUN=mean)
par_quant_1 = apply(LIC_deaths, MARGIN=1, quantile, probs=c(0.025))
par_quant_2 = apply(LIC_deaths, MARGIN=1, quantile, probs=c(0.975))

deaths <- data.frame(mean=par_median,
                     lower = par_quant_1,
                     upper = par_quant_2,
                     compartment = "Deaths")

par_median = apply(LIC_infs, MARGIN = 1, FUN=mean)
par_quant_1 = apply(LIC_infs, MARGIN=1, quantile, probs=c(0.025))
par_quant_2 = apply(LIC_infs, MARGIN=1, quantile, probs=c(0.975))

infs <- data.frame(mean=par_median,
                     lower = par_quant_1,
                     upper = par_quant_2,
                     compartment = "Infections")

#--------------------------------------------------------
# ICU occupancy
LIC_z <- data.frame(time = LIC_time, y = LIC_y) %>%
  gather(replicate, incidence, -time) %>%
  group_by(time) %>%
  summarise(mean = mean(incidence),
            lower = quantile(incidence, 0.025),
            upper = quantile(incidence, 0.975),
            compartment = "ICU bed demand") %>% ungroup()

icu_occ = LIC_z[seq(0, nrow(LIC_z), 10), ]

trigger <- bind_rows(infs, deaths, hosp, icu_occ)
time = icu_occ$time

trigger$time[is.na(trigger$time)] <- time
trigger$compartment <- factor(trigger$compartment, levels=c("Infections", "Deaths", "Hospital bed demand", "ICU bed demand"))
trigger$scenario <- "Triggered monthly suppression"

LIC_lockdown <- LIC$time_in_lockdown[1:(time_period/dt),5]

LIC_lockdown <- LIC_lockdown[seq(1, length(LIC_lockdown), 10)]

sum(LIC_lockdown)

p1<- ggplot(trigger) +
  geom_path(mapping=aes(x=time, y=mean, col=scenario), size=1.1) +
  scale_color_manual(values=c("#CCCCFF"))+
  theme_bw(10) +
  scale_y_continuous(labels=comma) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        strip.background = element_blank()) +
  labs(x=" ", y="N", title="A", col=" ")+
  geom_hline(aes(yintercept = 35),
                 data = subset(trigger, compartment== "ICU bed demand"),
                 linetype=2) +
  geom_hline(aes(yintercept = 407),
             data = subset(trigger, compartment== "Hospital bed demand"),
             linetype=2) +
  facet_wrap(.~compartment, scale="free_y", nrow=1) +
  scale_x_continuous(labels=c("Jul\n20", "Jan\n21", "Jul\n21", "Jan\n22", "Jan\n22"))+
  geom_ribbon( data = subset(trigger, compartment== "ICU bed demand"),
               aes(x=time, ymin = 0, ymax = LIC_lockdown * 60), alpha = 0.1)

p2 <- ggplot(trigger) +
  geom_path(mapping=aes(x=time, y=mean, col=scenario), size=1.1) +
  scale_color_manual(values=c("#CCCCFF"))+
  theme_bw(10) +
  scale_y_continuous(labels=comma) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        strip.background = element_blank()) +
  labs(x=" ", y="N", title="A", col=" ") +  geom_hline(aes(yintercept = 35),
                 data = subset(trigger, compartment== "ICU bed demand"),
                 linetype=2) +
  geom_hline(aes(yintercept = 407),
             data = subset(trigger, compartment== "Hospital bed demand"),
             linetype=2) +
  facet_wrap(.~compartment, scale="free_y", nrow=1) +
  scale_x_continuous(breaks=c(100,300,500,700),labels=c("Jul\n20", "Jan\n21", "Jul\n21", "Jan\n22"))+
  geom_ribbon( data = subset(trigger, compartment== "ICU bed demand"),
               aes(x=time, ymin = 0, ymax = LIC_lockdown * 60), alpha = 0.1)

p1/
  p2 + plot_layout(guides = 'collect')

ggsave("15_trigger.pdf", width=12, height=6, dpi=600)

max(trigger$mean[which(trigger$compartment=="Hospital bed demand")])
max(trigger$lower[which(trigger$compartment=="Hospital bed demand")])
max(trigger$upper[which(trigger$compartment=="Hospital bed demand")])

max(trigger$mean[which(trigger$compartment=="ICU bed demand")])
max(trigger$lower[which(trigger$compartment=="ICU bed demand")])
max(trigger$upper[which(trigger$compartment=="ICU bed demand")])

#------------------------------------------------------------------------------------------------------------
# Age distribution of deaths

toot <- lapply(seq_along(1:dim(LIC$model_output)[3]), function(p) {
  temp <- rollapply(LIC$model_output[, index$delta_D, 1], 1/dt, sum, partial = TRUE, align = "right") #take time points to days
  daily_deaths_new <- temp[seq(1, length(temp[,11]), 10), ]
  daily_deaths_new <-  apply(daily_deaths_new, 2, sum) #to all the row - sum the age groups for that replicate
   # 1/dt so that is the 10th elements = daily
  # list of 10 elements over each p value
})

age_deaths <- do.call(cbind, toot) # turns list into matrix

par_median = apply(age_deaths, MARGIN=1, FUN=median)
par_quant_1 = apply(age_deaths, MARGIN=1, quantile, probs=c(0.025))
par_quant_2 = apply(age_deaths, MARGIN=1, quantile, probs=c(0.975))

deaths_age <- data.frame(mean=par_median,
                   lower = par_quant_1,
                   upper = par_quant_2
                   )

age_groups <- c("0-4","5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
                "40-44", "45-49","50-54", "55-59", "60-64", "65-69", "70-74",
                "75-79", "80+")

age_groups <- factor(age_groups, levels=c("0-4","5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
                                          "40-44", "45-49","50-54", "55-59", "60-64", "65-69", "70-74",
                                          "75-79", "80+"))
pop <- get_population("Senegal")
population <- pop$n

total_population <- sum(population)

deaths_age$age_groups = age_groups
deaths_age$population_size <- population
deaths_age$total_deaths <- sum(deaths_age$mean)
deaths_age$prop_deaths = (deaths_age$mean/deaths_age$total_deaths)

ggplot(deaths_age) +
  geom_col(mapping=aes(x=age_groups, y=prop_deaths), fill="#CCCCFF", show.legend = FALSE) +
  labs(x="Age Groupings", y="Proportion", title="Age distirbution of deaths")+
  labs(x="Age Groupings", y="Proportion", title="Age distirbution of deaths")+
  theme_bw(10) +
  scale_y_continuous(limits=c(0,0.25))+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        strip.background = element_blank(),
        axis.text.x=element_text(angle=90))

ggsave("C:/Users/ht1212/Imperial College London/ncov - Documents/2019-nCoV/LMIC/Senegal/deaths_age_dist_new_scen_15.pdf", width=4, height=4, dpi=600)
