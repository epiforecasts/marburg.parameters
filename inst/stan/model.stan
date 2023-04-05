data {
  int<lower = 1> N; // number of individuals
  int t; // number of time points
  int<lower = 0> n_pairs; // number of transmission pairs

  array[n_pairs, 2] int <lower = 0> pairs; // transmission pairs
  array[N] real<lower = 0> lower_onset; // lower bound on time of onset
  array[N] real<lower = 0> lower_death; // lower bound on time of death 
  array[N] real <lower = 1> upper_onset; // upper bound on time of onset
  array[N] real <lower = 1> upper_death; // upper bound on time of death
  array[N] int<lower = 0, upper = 1> known_onset; // known day of onset
  array[N] int<lower = 0, upper = 1> known_death; // known day of outcome

  real si_mean_mean; // mean of serial interval mean
  real<lower = 0> si_mean_sd; // sd of serial interval mean
  real si_sd_mean; // mean of serial interval sd
  real<lower = 0> si_sd_sd; // sd of serial interval sd
  real od_mu_mean; // mean of lognormal mu for onset-to-death
  real<lower = 0> od_mu_sd; // sd of lognormal mu for onset-to-death
  real od_sigma_mean; // mean of lognormal sigma for onset-to-death
  real<lower = 0> od_sigma_sd; // sd of lognormal sigma for onset-to-death
}

parameters {
  array[N] real<lower = lower_onset, upper = upper_onset> time_onset; // exact time of symptom onset
  array[N] real<lower = lower_death, upper = upper_death> time_death; // exact time of death

  real<lower = 0> si_mean; // serial interval mean
  real<lower = 0> si_sd; // serial interval sd
  real od_mu; // onset-to-death lognormal mu
  real<lower = 0> od_sigma; // onset-to-death lognormal sigma
}

transformed parameters {
  // parameters of the gamma distribution
  real si_alpha = pow(si_mean, 2) / pow(si_sd, 2);
  real si_beta = si_mean / pow(si_sd, 2);
}

model {
  si_mean ~ normal(si_mean_mean, si_mean_sd) T[0, ];// SI mean prior from literature
  si_sd ~ normal(si_sd_mean, si_sd_sd) T[0, ];// SI sigma prior from literature
  od_mu ~ normal(od_mu_mean, od_mu_sd);// OD mu prior from literature
  od_sigma ~ normal(od_sigma_mean, od_sigma_sd) T[0, ];// OD sigma prior from literature

  // uniform prior of onset/death timings given data
  time_onset ~ uniform(lower_onset, upper_onset);
  time_death ~ uniform(lower_death, upper_death);

  for (i in 1:N) {
    if (known_death[i]) {
      target += lognormal_lpdf(time_death[i] - time_onset[i] | od_mu, od_sigma);
    }
  }
  for (i in 1:n_pairs) {
    target += gamma_lpdf(abs(time_onset[pairs[i, 1]] - time_onset[pairs[i, 2]]) | si_alpha, si_beta);
  }
}

generated quantities {
  // derived quantities of onset-to-death distribution
  real od_median = exp(od_mu);
  real od_sd = sqrt((exp(pow(od_sigma, 2)) - 1) * exp(2 * od_mu + pow(od_sigma, 2)));
}
