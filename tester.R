# Load Required Libraries
library(patchwork); library(dplyr); library(ggplot2)

# Run an Instance of the Model
set.seed(2)
x <- run_explicit_SEEIR_model(country = "Afghanistan", hosp_bed_capacity = 500, ICU_bed_capacity = 600)
index <- squire:::odin_index(x$model)

# Checking Hosp Bed Capacity
check_hosp_capacity <- x$output[, index$hosp_bed_capacity, 1]

# Checking Oxygen Assignments
number_get_ox <- apply(x$output[, index$number_get_Ox, 1], 1, sum)
number_get_ox_live <- apply(x$output[, index$n_IOxGetLive1, 1], 1, sum)
number_get_ox_die <- apply(x$output[, index$n_IOxGetDie1, 1], 1, sum)
number_get_ox_comb <- number_get_ox_live + number_get_ox_die
which(number_get_ox_comb != number_get_ox)
number_not_get_ox <- apply(x$output[, index$number_notget_Ox, 1], 1, sum)
number_requiring_ox <- apply(x$output[, index$number_requiring_Ox, 1], 1, sum)
number_get_ox <- apply(x$output[, index$number_get_Ox, 1], 1, sum)
number_ox <- number_not_get_ox + number_get_Ox
which(number_ox != number_requiring_ox)

# Checking That All Newly Hospitalised Individuals Are Correctly Assigned to ICU or Hosp Beds
newly_hospitalised <- apply(x$output[, index$n_ICase2_Hosp, 1], 1, sum)
total_number_requiring_ICU <- x$output[, index$total_number_requiring_IMV, 1]
total_number_requiring_ox <- x$output[, index$total_number_requiring_Ox, 1]
tot <- total_number_requiring_ICU + total_number_requiring_ox
which(newly_hospitalised != tot)

# Spotting for Bugs in Hospital Occupancy
hosp_occ <- x$output[, index$hosp_occ, 1]
total_number_requiring_ox <- x$output[, index$total_number_requiring_Ox, 1]
current_free_hosp <- x$output[, index$current_free_hosp, 1]
total_number_get_hosp <- x$output[, index$total_number_get_hosp, 1]
number_get_Ox <- apply(x$output[, index$number_get_Ox, 1], 1, sum)

max(hosp_occ)
min(current_free_hosp)
max_index <- which(hosp_occ == max(hosp_occ))
start <- max_index - 10
end <- max_index + 9

bloop <- x$output[, index$temp5, 1]
plot(bloop, type = "l")

which(bloop < 0)
which(hosp_occ > 500)

hosp_occ[start:end]
current_free_hosp[start:end]

cbind(hosp_occ[start:end], current_free_hosp[start:end], total_number_get_hosp[start:end])
total_number_get_hosp[start:end]
total_number_requiring_ox[start:end]

ioxgetlive1 <- apply(x$output[, index$IOxGetLive1, 1], 1, sum)
ioxgetlive2 <- apply(x$output[, index$IOxGetLive2, 1], 1, sum)
ioxgetdie1 <- apply(x$output[, index$IOxGetDie1, 1], 1, sum)
ioxgetdie2 <- apply(x$output[, index$IOxGetDie2, 1], 1, sum)
irec1 <- apply(x$output[, index$IRec1, 1], 1, sum)
irec2 <- apply(x$output[, index$IRec2, 1], 1, sum)
overall <- ioxgetlive1 + ioxgetlive2 + ioxgetdie1 + ioxgetdie2 + irec1 + irec2
max(overall)
plot(overall, type = "l")






# ioxgetlive1[(max_index-2):max_index]
# ioxgetlive2[(max_index-2):max_index]
# ioxgetdie1[(max_index-2):max_index]
# ioxgetdie2[(max_index-2):max_index]
# irec1[(max_index-2):max_index]
# irec2[(max_index-2):max_index]
# overall[(max_index-2):max_index]
#
# plot(ioxgetlive1, type = "l")
# plot(ioxgetlive2, type = "l")
# plot(ioxgetdie1, type = "l")
# plot(ioxgetdie2, type = "l")
# plot(irec1, type = "l")
# plot(irec2, type = "l")
#
#

#### scrap for now ####

# which(total_number_get_hosp != number_get_Ox)
# plot(hosp_occ, type = "l")
# max(hosp_occ)
# plot(current_free_hosp, type = "l")
# min(current_free_hosp)
# plot(total_number_get_hosp, type = "l")
#
# x <- c()
# for (i in 1:length(current_free_hosp)) {
#   x[i] <- if (current_free_hosp[i] <= 0) 0 else (if(current_free_hosp[i] - total_number_requiring_ox[i] >= 0) total_number_requiring_ox[i] else(current_free_hosp[i]))
# }
# table(x - total_number_get_hosp)
#
# min(total_number_get_hosp)
# which(number_get_Ox != total_number_requiring_ox)
# a <- current_free_hosp[which(number_get_Ox != total_number_requiring_ox)]
# b <- total_number_requiring_ox[which(number_get_Ox != total_number_requiring_ox)]
# which(a > b)
#
# plot(total_number_requiring_ox)
#
# total_number_get_hosp[total_number_requiring_ox > 0]
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# #
# ICU_occ <- x$output[, index$ICU_occ, 1]
# total_number_requiring_ICU <- x$output[, index$total_number_requiring_IMV, 1]
# current_free_ICU <- x$output[, index$current_free_ICUs, 1]
# total_number_get_IMV <- x$output[, index$total_number_get_IMV, 1]
# number_get_ICU <- apply(x$output[, index$number_get_IMV, 1], 1, sum)
#
# which(total_number_get_IMV != number_get_ICU)
# plot(ICU_occ, type = "l")
# max(ICU_occ)
# plot(current_free_ICU, type = "l")
# min(current_free_ICU)
# plot(total_number_get_IMV, type = "l")
# which(number_get_ICU != total_number_requiring_ICU)
# a <- current_free_ICU[which(number_get_ICU != total_number_requiring_ICU)]
# b <- total_number_requiring_ICU[which(number_get_ICU != total_number_requiring_ICU)]
# which(a > b)
#
#
#
#
#
#
#
# plot(hosp_occ, type = "l")
# max(hosp_occ)
# plot(current_free_hosp, type = "l")
# min(current_free_hosp)
#
# total_number_get_hosp[which(current_free_hosp <= 0)]
# total_number_get_hosp[which(current_free_hosp - total_number_requiring_ox >= 0)]
#
# total_number_get_hosp[which(!(current_free_hosp <= 0) | !(current_free_hosp - total_number_requiring_ox >= 0))]
#
# total_number_get_hosp[950:1000] - current_free_hosp[950:1000]
#
# total_number_get_hosp[which(hosp_occ >= 500)]
# current_free_hosp[which(hosp_occ >=500)]
#
#
# current_free_hosp
# min(current_free_hosp)
#
#
# plot(free, type = "l")
#
# number_get_hosp[which(free < 0)]
# number_get_hosp[which(free - req_ox >= 0)]
# number_get_hosp[which(free - req_ox < 0)]
#
#
#
# unique(number_get_hosp[which(free - req_ox > 0)])
#
# plot(free, type = "l")
# plot(number_get_hosp, type = "l")
#
# plot(free + number_get_hosp, type = "l")
#
# max(free + number_get_hosp)
# min(free + number_get_hosp)
#
# max(free)
# min(free)
#
# free[free < 0] <- 0
# a <- free - get_ox
#
#
# plot(hosp_occ, type = "l")
# max(hosp_occ)
# which(hosp_occ == 514)
#
# plot(hosp_occ, type = "l")
# plot(a, type = "l")
# unique(hosp_occ + a)
#
# total_get_ox <- x$output[, index$total_number_get_hosp, 1]
# get_ox <- apply(x$output[, index$number_get_Ox, 1], 1, sum)
# unique(total_get_ox - get_ox)
#
# total_get_ox[715]
# get_ox[715]
# free[715]
#
# rec <- x$output
#
#
# unique(a)
# plot(a, type = "l")
# min(a)
#
# require_ox <- apply(x$output[, index$number_requiring_Ox, 1], 1, sum)
#
#
# rec <- max(apply(x$output[, c(index$IRec1, index$IRec2), 1], 1, sum))
#
# hosp_bed <- format_output(x = x, var_select = "hospital_occupancy") %>%
#   mutate(replicate = factor(replicate)) %>%
#   filter(replicate == 1)
# max(hosp_bed$y)
#
# colnames(x$output[, , 1])
#
# deaths <- format_output(x = x, var_select = "deaths") %>%
#   mutate(replicate = factor(replicate))
# a <- ggplot(deaths, aes(x = t, y = y, col = replicate)) +
#   geom_line() + ylab("Daily Deaths")
#
# infections <- format_output(x = x, var_select = "infections") %>%
#   mutate(replicate = factor(replicate))
# b <- ggplot(infections, aes(x = t, y = y, col = replicate)) +
#   geom_line() + ylab("Daily Infections")
#
# hosp_bed <- format_output(x = x, var_select = "hospital_occupancy") %>%
#   mutate(replicate = factor(replicate))
# c <- ggplot(hosp_bed, aes(x = t, y = y, col = replicate)) +
#   geom_line() + ylab("Hospital Bed Occupancy")
#
# ICU_bed <- format_output(x = x, var_select = "ICU_occupancy") %>%
#   mutate(replicate = factor(replicate))
# d <- ggplot(ICU_bed, aes(x = t, y = y, col = replicate)) +
#   geom_line() + ylab("ICU Bed Occupancy")
#
# z <- a + b + c + d +
#   plot_layout(guides = 'collect')
# z
