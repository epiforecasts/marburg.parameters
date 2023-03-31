data {
  int<lower = 1> N; // number of individuals
  int t; // number of time points
  int<lower = 0> n_pairs; // number of transmission pairs

  array[n_pairs, 2] int <lower = 0> pairs; // transmission pairs
  array[N] int<lower = 0> day_onset; // day of onset
  array[N] int<lower = 0> day_outcome; // day of outcome
  array[N] real <lower = 1> max_onset; // max onset
  array[N] real <lower = 1> max_outcome; // max outcome
  array[N] int<lower = 0, upper = 1> known_onset; // known onset
  array[N] int<lower = 0, upper = 1> known_outcome; // known onset

  real gi_mean_mean; // mean of mu for generation interval
  real<lower = 0> gi_mean_sd; // mean of sigma for generation interval
  real gi_sd_mean; // mean of sigma for generation interval
  real<lower = 0> gi_sd_sd; // mean of sigma for generation interval
  real ip_mu_mean; // mean of mu for incubation period
  real<lower = 0> ip_mu_sd; // mean of sigma for incubation period
  real ip_sigma_mean; // mean of sigma for incubation period
  real<lower = 0> ip_sigma_sd; // mean of sigma for incubation period
  real od_mu_mean; // mean of mu for onset-to-death
  real<lower = 0> od_mu_sd; // mean of sigma for onset-to-death
  real od_sigma_mean; // mean of sigma for onset-to-death
  real<lower = 0> od_sigma_sd; // mean of sigma for onset-to-death
}

parameters {
  array[N] real<lower = 0, upper = max_onset> time_infection; // exact time of infection
  array[N] real<lower = 0> incubation_period; // incubation perido
  array[N] real<lower = 0> onset_to_death; // onset to death

  real<lower = 0> gi_mean; // generation interval mu
  real<lower = 0> gi_sd; // generation interval sigma
  real od_mu; // onset-to-death mu
  real<lower = 0> od_sigma; // onset-to-death sigma
  real ip_mu; // incubation period mu
  real<lower = 0> ip_sigma; // incubation period sigma
}

transformed parameters {
  real gi_alpha = pow(gi_mean, 2) / pow(gi_sd, 2);
  real gi_beta = gi_mean / pow(gi_sd, 2);
}

model {
  gi_mean ~ normal(gi_mean_mean, gi_mean_sd) T[0, ];// GI mu prior from literature
  gi_sd ~ normal(gi_sd_mean, gi_sd_sd) T[0, ];// GI sigma prior from literature
  od_mu ~ normal(od_mu_mean, od_mu_sd);// OD mu prior from literature
  od_sigma ~ normal(od_sigma_mean, od_sigma_sd) T[0, ];// OD sigma prior from literature
  ip_mu ~ normal(ip_mu_mean, ip_mu_sd);// IP mu prior from literature
  ip_sigma ~ normal(ip_sigma_mean, ip_sigma_sd) T[0, ];// IP sigma prior from literature

  incubation_period ~ lognormal(ip_mu, ip_sigma);
  onset_to_death ~ lognormal(od_mu, od_sigma);

  for (i in 1:N) {
    if (known_onset[i]) {
      target += uniform_lpdf(time_infection[i] + incubation_period[i] | day_onset[i], max_onset[i]);
    }
  }
  for (i in 1:N) {
    if (known_outcome[i]) {
      target += uniform_lpdf(time_infection[i] + incubation_period[i] + onset_to_death[i] | day_outcome[i], max_outcome[i]);
    }
  }
   for (i in 1:n_pairs) {
    target += gamma_lpdf(abs(time_infection[pairs[i, 1]] - time_infection[pairs[i, 2]]) | gi_alpha, gi_beta);
  }
}
