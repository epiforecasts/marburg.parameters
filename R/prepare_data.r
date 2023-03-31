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
                         gi = list(mean_mean = 9.2,
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
                         max_ip = 26) {

  names(gi) <- paste0("gi_", names(gi))
  names(od) <- paste0("od_", names(od))
  names(ip) <- paste0("ip_", names(ip))

  t0 <- min(linelist$date_of_onset_of_symptoms, na.rm = TRUE) - max_ip

  linelist <- linelist |>
    dplyr::mutate(
      day_of_onset_of_symptoms = as.integer(date_of_onset_of_symptoms - t0),
      day_of_outcome = as.integer(date_of_outcome - t0),
      max_onset_of_symptoms = day_of_onset_of_symptoms + 1,
      max_outcome = day_of_outcome + 1
    )

  t <- max(
    c(linelist$day_of_onset_of_symptoms, linelist$day_of_outcome), na.rm = TRUE
  ) + 1

  linelist <- linelist |>
    tidyr::replace_na(list(
      day_of_onset_of_symptoms = 0,
      day_of_outcome = 0,
      max_outcome = t
    ))

  linelist <- linelist |>
    dplyr::mutate(
      max_onset_of_symptoms = dplyr::if_else(
        is.na(max_onset_of_symptoms),
        max_outcome,
        max_onset_of_symptoms
      ))

  pairs <- as.matrix(pairs[, c("from", "to")])

  n_pairs <- nrow(pairs)

  data <- list(
    N = nrow(linelist),
    t = t,
    n_pairs = n_pairs,
    pairs = pairs,
    day_onset = linelist$day_of_onset_of_symptoms,
    max_onset = linelist$max_onset_of_symptoms,
    day_outcome = linelist$day_of_outcome,
    max_outcome = linelist$max_outcome,
    known_onset = as.integer(!is.na(linelist$date_of_onset_of_symptoms)),
    known_outcome = as.integer(!is.na(linelist$date_of_outcome))
  )

  data <- c(data, gi, od, ip)

  return(data)
}
