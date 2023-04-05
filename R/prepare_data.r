##' Prepare data for the stan model
##'
##' @param linelist a linelist data frame
##' @param pairs a pairs data frame
##' @param gi generation interval parameters
##' @param od onset-to-death parameters
##' @param ip incubation period parameters
##' @param max_ip maximum possible incubation period
##' @importFrom dplyr mutate
##' @return a list ready for stan
##' @export
##' @author Sebastian Funk
prepare_data <- function(linelist,
                         pairs,
                         si = list(mean_mean = 9.2,
                                   mean_sd = 1,
                                   sd_mean = 4.4,
                                   sd_sd = 1),
                         od = list(mu_mean = log(8.5),
                                   mu_sd = 1,
                                   sigma_mean = log(2.5),
                                   sigma_sd = 1),
                         ip = list(mu_mean = log(7),
                                   mu_sd = 1,
                                   sigma_mean = log(2.5),
                                   sigma_sd = 1),
                         init = 21) {

  names(si) <- paste0("si_", names(si))
  names(od) <- paste0("od_", names(od))
  names(ip) <- paste0("ip_", names(ip))

  t0 <- min(linelist$date_onset, na.rm = TRUE) - init

  linelist <- linelist |>
    dplyr::mutate(
      lower_onset = as.integer(date_onset - t0),
      lower_death = as.integer(date_death - t0),
      upper_onset = lower_onset + 1,
      upper_death = lower_death + 1
    )

  t <- max(
    c(linelist$lower_onset, linelist$lower_death), na.rm = TRUE
  ) + 1

  linelist <- linelist |>
    tidyr::replace_na(list(
      lower_onset = 0,
      lower_death = 0,
      upper_death = t
    ))

  linelist <- linelist |>
    dplyr::mutate(
      upper_onset = dplyr::if_else(
        is.na(upper_onset),
        upper_death,
        upper_onset
      ))

  pairs <- as.matrix(pairs[, c("from", "to")])

  n_pairs <- nrow(pairs)

  data <- list(
    N = nrow(linelist),
    t = t,
    n_pairs = n_pairs,
    pairs = pairs,
    lower_onset = linelist$lower_onset,
    upper_onset = linelist$upper_onset,
    lower_death = linelist$lower_death,
    upper_death = linelist$upper_death,
    known_onset = as.integer(!is.na(linelist$date_onset)),
    known_death = as.integer(!is.na(linelist$date_death))
  )

  data <- c(data, si, od, ip)

  return(data)
}
