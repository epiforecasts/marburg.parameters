data {
  int<lower = 1> N; // number of individuals
  int t; // number of time points
  int<lower = 0> n_pairs; // number of transmission pairs

  array[n_pairs, 2] int <lower = 0> pairs; // transmission pairs
  array[N] real<lower = 0> lower_onset; // day of onset
  array[N] real<lower = 0> lower_outcome; // day of outcome
  array[N] real <lower = 1> upper_onset; // max onset
  array[N] real <lower = 1> upper_outcome; // max outcome
  array[N] int<lower = 0, upper = 1> known_onset; // known onset
  array[N] int<lower = 0, upper = 1> known_outcome; // known onset

  real si_mean_mean; // mean of mu for generation interval
  real<lower = 0> si_mean_sd; // mean of sigma for generation interval
  real si_sd_mean; // mean of sigma for generation interval
  real<lower = 0> si_sd_sd; // mean of sigma for generation interval
  real od_mu_mean; // mean of mu for onset-to-death
  real<lower = 0> od_mu_sd; // mean of sigma for onset-to-death
  real od_sigma_mean; // mean of sigma for onset-to-death
  real<lower = 0> od_sigma_sd; // mean of sigma for onset-to-death
}

parameters {
  array[N] real<lower = lower_onset, upper = upper_onset> time_onset; // exact time of symptom onset
  array[N] real<lower = lower_outcome, upper = upper_outcome> time_outcome; // exact time of symptom onset

  real<lower = 0> si_mean; // generation interval mu
  real<lower = 0> si_sd; // generation interval sigma
  real od_mu; // onset-to-death mu
  real<lower = 0> od_sigma; // onset-to-death sigma
}

transformed parameters {
  real si_alpha = pow(si_mean, 2) / pow(si_sd, 2);
  real si_beta = si_mean / pow(si_sd, 2);
}

model {
  si_mean ~ normal(si_mean_mean, si_mean_sd) T[0, ];// SI mu prior from literature
  si_sd ~ normal(si_sd_mean, si_sd_sd) T[0, ];// SI sigma prior from literature
  od_mu ~ normal(od_mu_mean, od_mu_sd);// OD mu prior from literature
  od_sigma ~ normal(od_sigma_mean, od_sigma_sd) T[0, ];// OD sigma prior from literature

  time_onset ~ uniform(lower_onset, upper_onset);
  time_outcome ~ uniform(lower_outcome, upper_outcome);

  for (i in 1:N) {
    if (known_outcome[i]) {
      target += lognormal_lpdf(time_outcome[i] - time_onset[i] | od_mu, od_sigma);
    }
  }
  for (i in 1:n_pairs) {
    target += gamma_lpdf(abs(time_onset[pairs[i, 1]] - time_onset[pairs[i, 2]]) | si_alpha, si_beta);
  }
}
