




time_period = 500
hosp_bed_capacity = 10000000
ICU_bed_capacity = 10000000
population = as.numeric(region_demographics[which(region_demographics$region==region & region_demographics$country==country),c(4:20)])
tt_contact_matrix = c(0,10)
contact_matrix <- get_mixing_matrix("Argentina")
contact_matrix_set = list(contact_matrix,
                          contact_matrix*suppression_contact,
                          contact_matrix*mitigation_contact)
hosp_bed_capacity = 10000000
ICU_bed_capacity = 10000000
time_period = 500
number_of_deaths =100

r0 <- run_explicit_SEEIR_model(population = population, contact_matrix_set = list(contact_matrix),
                               tt_contact_matrix =  c(0), replicate=1,
                               hosp_bed_capacity = hosp_bed_capacity,
                               ICU_bed_capacity = ICU_bed_capacity,
                               time_period = time_period)

infections <- format_output(r0, var_select = "infections", reduce_age = TRUE)
ggplot(infections, aes(x = t, y = y)) +
  geom_line()

df0 <- format_output(r0, var_select = "D")
df0 <- df0 %>%
  filter(replicate == 1)
time <- df0$t[tail(which(df0$y < 100), 1)]/r0$parameters$dt

init0 <- r0$output[time, c(2:443), 1]
init <- as.data.frame(matrix(init0,nrow=17,byrow=F))
colnames(init) <- c("S", "E1", "E2", "IMild", "ICase1", "ICase2", "IOxGetLive1", "IOxGetLive2",
                    "IOxGetDie1", "IOxGetDie2", "IOxNotGetLive1", "IOxNotGetLive2",
                    "IOxNotGetDie1", "IOxNotGetDie2", "IMVGetLive1", "IMVGetLive2",
                    "IMVGetDie1", "IMVGetDie2", "IMVNotGetLive1", "IMVNotGetLive2",
                    "IMVNotGetDie1", "IMVNotGetDie2", "IRec1", "IRec2", "R", "D")

contact_matrix_set = list(contact_matrix*suppression_contact)
ri <- run_explicit_SEEIR_model(population = population,
                               contact_matrix_set = contact_matrix_set,
                               R0 = 0.5,
                               tt_R0 = 0,
                               tt_contact_matrix = c(0), replicate = 1,
                               hosp_bed_capacity = hosp_bed_capacity,
                               ICU_bed_capacity = ICU_bed_capacity,
                               time_period = time_period,
                               init=init)

ri$parameters$init

infections <- format_output(ri, var_select = "infections", reduce_age = TRUE)
ggplot(infections, aes(x = t, y = y)) +
  geom_line()

squire:::init_check_explicit(init, population) - init

dfi1 <- format_output(ri, var_select = "D")

plotting_function(ri)

rum <- r0
rum$output <- array(rbind(r0$output[c(1:(time*10)),,],ri$output[c(1:nrow(ri$output)),,]),dim=c(time*10+nrow(ri$output), 512, 1))
rum$output[,1,] <- c(1:length(rum$output[,1,]))

plotting_function(rum)

#xum <- c(total_output(rum,"infections"),total_output(rum,"deaths"),max_output(rum,"hospital_occupancy"),max_output(rum,"ICU_occupancy"))

return(rum)

