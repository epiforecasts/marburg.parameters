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
     out$time_onset <- array(runif(
       data$N,
       data$lower_onset,
       data$upper_onset
     ))
     out$time_death <- array(runif(
       data$N,
       data$lower_death,
       data$upper_death
     ))
     out$si_mean <- data$si_mean_mean
     out$si_sd <- data$si_sd_mean
     out$od_mu <- data$od_mu_mean
     out$od_sigma <- data$od_sigma_mean
     return(out)
   }
   return(init_fn)
}
