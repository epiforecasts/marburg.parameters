##' Prepare initial conditions for the stan model
##'
##' @param data a list of stan data
##' @importFrom dplyr mutate
##' @return a function ready for stan
##' @export
##' @author Sebastian Funk
prepare_init <- function(data) {
   init_fn <- function() {
     out <- list()
     out$incubation_period <- array(
       rep(data$ip_mu_mean, data$N)
     )
     out$time_infection <- array(runif(
       data$N,
       pmax(data$day_onset - out$incubation_period, 0),
       data$max_onset - out$incubation_period
     ))
     out$onset_to_death <- array(runif(
       data$N,
       pmax(data$day_outcome - out$time_infection - out$incubation_period, 0),
       data$max_outcome - out$time_infection - out$incubation_period
     ))
     out$gi_mean <- data$gi_mean_mean
     out$gi_sd <- data$gi_sd_mean
     out$ip_mu <- data$ip_mu_mean
     out$ip_sigma <- data$ip_sigma_mean
     out$od_mu <- data$od_mu_mean
     out$od_sigma <- data$od_sigma_mean
     return(out)
   }
   return(init_fn)
}
