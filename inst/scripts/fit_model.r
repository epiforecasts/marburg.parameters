library("marburg")
library("googlesheets4")
library("dplyr")
library("janitor")
library("cmdstanr")

linelist <- googlesheets4::read_sheet(
  paste0("https://docs.google.com/spreadsheets/d/1iviEt9eZCPDNjMSEqIfLXl0W2LlHXrt93Z6FI_1seAM/edit#gid=0"), ## nolint
  n_max = 31) |>
  janitor::clean_names()

wiw <- googlesheets4::read_sheet(
  paste0("https://docs.google.com/spreadsheets/d/1iviEt9eZCPDNjMSEqIfLXl0W2LlHXrt93Z6FI_1seAM/edit#gid=0"), ## nolint
  sheet = "Who_Infected_Who",
  n_max = 14) |>
  janitor::clean_names()

linelist <- linelist |>
  dplyr::rowwise() |>
  dplyr::mutate(
    date_of_onset_of_symptoms =
      lubridate::ymd(as.character(unlist(date_of_onset_of_symptoms))),
    date_of_outcome = lubridate::ymd(as.character(unlist(date_of_outcome))),
  ) |>
  dplyr::ungroup()

wiw <- wiw |>
  mutate(
    from = as.integer(sub("^ID", "", from)),
    to = as.integer(sub("^ID", "", to))
  )

model <- cmdstanr::cmdstan_model("inst/stan/model.stan")

data <- marburg::prepare_data(linelist, wiw)

init <- marburg::prepare_init(data)

fit <- model$sample(
  data = data,
  init = init,
  chains = 2,
  parallel_chains = 2,
  adapt_delta = 0.999
)

init_values <- init()
for (n in names(init_values)) assign(n, init_values[[n]])
for (n in names(data)) assign(n, data[[n]])

target <- 0

target <- target + log(dnorm(gi_mean, gi_mean_mean, gi_mean_sd) / 0.5)
target <- target + log(dnorm(gi_sd, gi_sd_mean, gi_sd_sd) / 0.5)
target <- target + dnorm(od_mu, od_mu_mean, od_mu_sd, log = TRUE)
target <- target + log(dnorm(od_sigma, od_sigma_mean, od_sigma_sd) / 0.5)
target <- target + dnorm(ip_mu, ip_mu_mean, ip_mu_sd, log = TRUE)
target <- target + log(dnorm(ip_sigma, ip_sigma_mean, ip_sigma_sd) / 0.5)

target <- target + sum(dlnorm(incubation_period, ip_mu, ip_sigma, log = TRUE))
target <- target + sum(dlnorm(onset_to_death, od_mu, od_sigma, log = TRUE))

known_onsets <- which(as.logical(known_onset))
known_outcomes <- which(as.logical(known_outcome))

target <- target + sum(dunif(
  time_infection[known_onsets] + incubation_period[known_onsets],
  day_onset[known_onsets],
  max_onset[known_onsets],
  log = TRUE
))

target <- target + sum(dunif(
  time_infection[known_outcomes] +
    incubation_period[known_outcomes] +
    onset_to_death[known_outcomes],
  day_outcome[known_outcomes],
  max_outcome[known_outcomes],
  log = TRUE
))

target <- target + sum(dgamma(
  abs(time_infection[pairs[, 1]] - time_infection[pairs[, 2]]),
  gi_mean,
  gi_sd,
  log = TRUE
))
