source("trigger_running_function.R")
trigger_thresholds <- c(1, 2, 5, 10, 50, 100, 200, 350, 500, 750, 1000, 1500, 2000, 3000, 5000, 10000, 20000, 30000, 50000, 75000, 100000)
max_ICU_req <- matrix(nrow = 4, ncol = length(trigger_thresholds))
adjusted_max_ICU_req <- matrix(nrow = 4, ncol = length(trigger_thresholds))
time_in_lockdown <- matrix(nrow = 4, ncol = length(trigger_thresholds))
countries <- c("Malawi", "Nicaragua", "Grenada", "Malta")
for (i in 1:length(countries)) {
  pop <- get_population(countries[i])
  pop <- (50000000/sum(pop$n)) * pop$n
  for (j in 1:length(trigger_thresholds)) {
    x <- run_trigger_threshold(country = countries[i],
                               population = pop,
                               replicates = 6,
                               trigger_threshold = trigger_thresholds[j],
                               suppression_reduction = 0.15,
                               suppression_duration = 30,
                               mitigation_reduction = 1,
                               R0 = c(3, 3),
                               tt_R0 = c(0, 50),
                               max_lockdowns = 15)
    max_ICU_req[i, j] <- get_max_ICU_req(x)
    time_in_lockdown[i, j] <- get_time_in_lockdown(x)
    print(j)
  }
  adjusted_max_ICU_req[i, ] <- 1000 * max_ICU_req[i, ]/sum(pop)
}

plot(time_in_lockdown[4, ], adjusted_max_ICU_req[4, ], pch = 20, xlim = c(0.1, 0.7))
points(time_in_lockdown[3, ], adjusted_max_ICU_req[3, ], pch = 20, col = "red")
points(time_in_lockdown[2, ], adjusted_max_ICU_req[2, ], pch = 20, col = "blue")
points(time_in_lockdown[1, ], adjusted_max_ICU_req[1, ], pch = 20, col = "purple")

plot(time_in_lockdown[4, ], max_ICU_req[4, ], pch = 20, xlim = c(0.1, 0.7), ylim = c(0, 50000))
points(time_in_lockdown[3, ], max_ICU_req[3, ], pch = 20, col = "red")
points(time_in_lockdown[2, ], max_ICU_req[2, ], pch = 20, col = "blue")
points(time_in_lockdown[1, ], max_ICU_req[1, ], pch = 20, col = "purple")

plot(time_in_lockdown[4, ], adjusted_max_ICU_req[4, ], pch = 20, xlim = c(0.1, 0.7), ylim = c(0, 1.5), type = "l")
lines(time_in_lockdown[3, ], adjusted_max_ICU_req[3, ], pch = 20, col = "red")
lines(time_in_lockdown[2, ], adjusted_max_ICU_req[2, ], pch = 20, col = "blue")
lines(time_in_lockdown[1, ], adjusted_max_ICU_req[1, ], pch = 20, col = "purple")

plot(time_in_lockdown[4, ], max_ICU_req[4, ], pch = 20, xlim = c(0.1, 0.7), ylim = c(0, 10000), type = "l")
lines(time_in_lockdown[3, ], max_ICU_req[3, ], pch = 20, col = "red")
lines(time_in_lockdown[2, ], max_ICU_req[2, ], pch = 20, col = "blue")
lines(time_in_lockdown[1, ], max_ICU_req[1, ], pch = 20, col = "purple")

lines(time_in_lockdown[3, ], rep(LIC_icu, length(time_in_lockdown[3, ])), col = "purple", lty = 2)
lines(time_in_lockdown[3, ], rep(LMIC_icu, length(time_in_lockdown[3, ])), col = "blue", lty = 2)
lines(time_in_lockdown[3, ], rep(UMIC_icu, length(time_in_lockdown[3, ])), col = "red", lty = 2)
lines(time_in_lockdown[3, ], rep(HIC_icu, length(time_in_lockdown[3, ])), col = "black", lty = 2)


LIC_icu <- (1.5 * 50000000 * 1.25/1000)/100
LMIC_icu <- (2 * 50000000 * 2/1000)/100
UMIC_icu <- (3 * 50000000 * 2.5/1000)/100
HIC_icu <- (3.5 * 50000000 * 4.5/1000)/100


plot(time_in_lockdown[1, ], max_ICU_req[1, ], pch = 20)
points(time_in_lockdown[2, ], max_ICU_req[2, ], pch = 20, col = "red")
points(time_in_lockdown[3, ], max_ICU_req[3, ], pch = 20, col = "blue")
points(time_in_lockdown[4, ], max_ICU_req[4, ], pch = 20, col = "purple")


get_time_in_lockdown(x)
get_max_ICU_req(x)



out <- x$model_output
index <- x$index
time_in_lockdown <- x$time_in_lockdown

daily_time_in_lockdown <- rollapply(time_in_lockdown[, 1], 10, median, partial = TRUE, align = "right")
daily_time_in_lockdown <- daily_time_in_lockdown[seq(1, length(daily_time_in_lockdown), 10)]

daily_ICU_incidence <- rollapply(out[, index$total_number_requiring_IMV, 1], 10, sum, partial = TRUE, align = "right")
daily_ICU_incidence <- daily_ICU_incidence[seq(1, length(daily_ICU_incidence), 10)]

plot(daily_ICU_incidence, type = "l")
lines(daily_time_in_lockdown * max(daily_ICU_incidence) * 0.85)
lines(rep(100, length(daily_ICU_incidence)), col = "red", lwd = 2)

daily_ICU_occupancy <- rollapply(out[, index$total_ICU_req, 1], 10, mean, partial = TRUE, align = "right")
daily_ICU_occupancy <- daily_ICU_occupancy[seq(1, length(daily_ICU_occupancy), 10)]

plot(daily_ICU_occupancy, type = "l")
lines(daily_time_in_lockdown * max(daily_ICU_occupancy) * 0.85)
max(daily_ICU_occupancy)
sum(daily_time_in_lockdown)/length(daily_time_in_lockdown)
