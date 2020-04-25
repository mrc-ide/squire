library(tidyverse)
x <- run_explicit_SEEIR_model(country = "United Kingdom",
                              R0 = 2.4, dt = 0.001,
                              hosp_bed_capacity = 100000000,
                              ICU_bed_capacity = 100000000,
                              replicates = 1)

index <- squire:::odin_index(x$model)

number_not_get_IMV <- x$output[, index$number_notget_IMV, 1]
number_not_get <- apply(number_not_get_IMV, 1, sum)
plot(number_not_get, type = "l")

number_get_IMV <- x$output[, index$number_get_IMV, 1]
number_get <- apply(number_get_IMV, 1, sum)
plot(number_get, type = "l")

number_req_IMV <- x$output[, index$number_requiring_IMV, 1]
number_req <- apply(number_req_IMV, 1, sum)
plot(number_req, type = "l")

sum(number_req - number_get)

imv <- format_output(x, var_select = "IMVNotGetDie", reduce_age = FALSE)
imv$age_group <- factor(imv$age_group)
ggplot(imv, aes(x = t, y = y, col = age_group)) +
  geom_line() +
  guides(colour = "none")

imv_get <- format_output(x, var_select = "IMVGetDie", reduce_age = FALSE)
imv_get$age_group <- factor(imv_get$age_group)
ggplot(imv_get, aes(x = t, y = y, col = age_group)) +
  geom_line() +
  guides(colour = "none")

x <- rbind(imv, imv_get) %>%
  spread(compartment, y) %>%
  mutate(overall = IMVGetDie + IMVNotGetDie) %>%
  mutate(prop = ifelse(overall == 0, 0, IMVNotGetDie/overall))
ggplot(x, aes(x = t, y = prop, col = age_group)) +
  geom_line()



imv <- imv %>%
  group_by(replicate) %>%
  summarise(imv = sum(y))




deaths <- format_output(x, var_select = "deaths")
deaths <- deaths %>%
  group_by(replicate) %>%
  summarise(death = sum(y))
iox <- format_output(x, var_select = "IOxNotGetLive")
ggplot(iox, aes(x = t, y = y, col = replicate)) +
  geom_line()
iox <- iox %>%
  group_by(replicate) %>%
  summarise(iox = sum(y))
