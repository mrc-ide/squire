#' Project lockdowns based on triggering
#'
#' @param out Output of \code{\link{pmcmc}} or \code{\link{calibrate}}
#' @param trigger_metric Name of model output to tigger by. Must be one accepted
#'   by \code{format_output}
#' @param trigger_value Value of \code{trigger_metric} at which triggering occurs
#' @param R0_lockdowns Vector of R0 values to be used for each lockdown.
#'   \code{Default = c(0.5, 0.5, 0.5, 0.5)}
#' @param lockdown_lengths Vector of lengths of each lockdown in days.
#'   \code{Default = c(28, 42, 28, 42)}
#' @param max_lockdowns Maximum number of lockdowns. \code{Default = 4}
#' @param seed RNG seed to be used. \code{Default = 931L}
#' @export
trigger_projections <- function(out,
                                trigger_metric = "deaths",
                                trigger_value = 150,
                                # R0_lockdowns = c(0.6, 0.7, 0.8, 0.9) # idea here being that the R0 for each successive lockdown is larger
                                R0_lockdowns = c(0.5, 0.5, 0.5, 0.5),
                                lockdown_lengths = c(28, 42, 28, 42),
                                max_lockdowns = 4,
                                seed = 931L) {


  assert_length(R0_lockdowns, max_lockdowns)
  assert_length(lockdown_lengths, max_lockdowns)
  assert_pos(trigger_value)

  # Start with the baseline:
  reps <- dim(out$output)[3]
  to_be_run <- as.list(rep(TRUE, reps))

  if ("projection_args" %in% names(out)) {
    tts <- as.list(rep(out$projection_args$tt_R0, reps))
    R0s <- as.list(rep(out$projection_args$R0, reps))
    if(is.null(out$projection_args$R0)) {
      R0s <- lapply(t0_variables(out), "[[", "R0")
    }
  } else {
    tts <- as.list(rep(0, reps))
    R0s <- lapply(t0_variables(out), "[[", "R0")
  }

  lockdown_i <- 1
  while (lockdown_i <= max_lockdowns && any(unlist(to_be_run))) {

    message("Lockdown: ", lockdown_i)

    # Set the seed here to get same rng
    set.seed(seed)

    # starting projection
    if (lockdown_i == 1) {

      # conduct out projections
      p <- projections(out, tt_R0 = tts, R0 = R0s, to_be_run = to_be_run)

      # get our metric
      metric <- format_output(p, trigger_metric)

      # work out trigger times
      tts_new <- unlist(lapply(seq_len(reps), function(x) {
        metric$t[which(metric$y >= trigger_value & metric$t > 0 & metric$replicate == x)[1]]
        }))
      tts_new[tts_new<0] <- 0
      tts_new[is.na(tts_new)] <- 0

      # set what the new tt_r0s are
      tts_old <- tts
      tts <- lapply(seq_along(tts), function(x) {
        if(tts_new[[x]] == 0) {
          return(tts[[x]])
        } else {
          return(c(tts[[x]][tts[[x]] < tts_new[[x]]],
                   tts_new[[x]],
                   tts_new[[x]] + lockdown_lengths[lockdown_i])) # here you could some interpolation
        }
      })

      # and their corresponding R0s
      R0s <- lapply(seq_along(tts), function(x) {
        if(length(tts[[x]]) == 1) {
          return(R0s[[x]])
        } else {
          return(c(R0s[[x]][tts[[x]] < tts_new[[x]]],
                   R0_lockdowns[lockdown_i],
                   R0s[[x]][tts[[x]] < tts_new[[x]]]))  # here you could some interpolation
        }
      })

      # and do we need to do the rerun
      to_be_run <- as.list(as.logical(tts_new != 0))

    } else {

      p <- projections(p, tt_R0 = tts, R0 = R0s, to_be_run = to_be_run)

      # get our metric
      metric <- format_output(p, trigger_metric)

      # work out trigger times
      tts_new <- lapply(seq_along(tts), function(x) {
        ts <- stats::na.omit(metric$t[metric$replicate == x & metric$y >= trigger_value])
        if(length(ts) > 0) {
          if(max(ts) > max(tts[[x]])) {
            return(c(tts[[x]],
                     ts[ts>max(tts[[x]])][1],
                     ts[ts>max(tts[[x]])][1] + lockdown_lengths[lockdown_i])) # here you could some interpolation
          } else {
            return(tts[[x]])
          }
        } else {
          return(tts[[x]])
        }
      })

      # work out trigger R0s
      R0s <- lapply(seq_along(tts), function(x) {
        if(length(tts_new[[x]]) == length(tts[[x]])) {
          return(R0s[[x]])
        } else {
          return(c(R0s[[x]], R0_lockdowns[lockdown_i], R0s[[x]][1]))  # here you could some interpolation
        }
      })

      # and do we need to do the rerun
      to_be_run <- as.list(as.logical(lengths(tts_new) > lengths(tts)))

      # and move the correct tts
      tts <- tts_new

    }

    lockdown_i <- lockdown_i + 1

  }

  p$trigger_args <- list("R0s" = R0s, "tt_R0s" = tts)

  return(p)

}
