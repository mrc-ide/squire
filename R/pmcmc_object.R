
#' @noRd
plot_pmcmc_sample  <- function(x, what = "deaths") {

  idx <- odin_index(x$model)

  # what are we plotting
  if (what == "cases") {

    index <- unlist(
      idx[c("IMild", "ICase1", "ICase2", "IOxGetLive1", "IOxGetLive2",
            "IOxGetDie1", "IOxGetDie2", "IOxNotGetLive1", "IOxNotGetLive2",
            "IOxNotGetDie1", "IOxNotGetDie2", "IMVGetLive1", "IMVGetLive2",
            "IMVGetDie1", "IMVGetDie2", "IMVNotGetLive1", "IMVNotGetLive2",
            "IMVNotGetDie1", "IMVNotGetDie2", "IRec1", "IRec2", "R", "D")])
    ylab <- "Cumulative Cases"
    xlab <- "Date"
    particles <- vapply(seq_len(dim(x$output)[3]), function(y) {
      rowSums(x$output[,index,y], na.rm = TRUE)},
      FUN.VALUE = numeric(dim(x$output)[1]))
    quants <- as.data.frame(t(apply(particles, 1, quantile, c(0.025, 0.975))))
    quants$date <- rownames(quants)
    names(quants)[1:2] <- c("ymin","ymax")

    base_plot <- plot(x, "infections", ci = FALSE, replicates = TRUE, x_var = "date",
                      date_0 = max(x$pmcmc_results$inputs$data$date))
    base_plot <- base_plot +
      ggplot2::geom_line(ggplot2::aes(y=.data$ymin, x=as.Date(.data$date)), quants, linetype="dashed") +
      ggplot2::geom_line(ggplot2::aes(y=.data$ymax, x=as.Date(.data$date)), quants, linetype="dashed") +
      ggplot2::geom_point(ggplot2::aes(y=.data$cases/x$pmcmc_results$inputs$pars_obs$phi_cases,
                                       x=as.Date(.data$date)), x$pmcmc_results$inputs$data)

  }

  else if(what == "deaths") {

      index <- c(idx$D)

    ylab <- "Deaths"
    xlab <- "Date"
    particles <- vapply(seq_len(dim(x$output)[3]), function(y) {
      out <- c(0,diff(rowSums(x$output[,index,y], na.rm = TRUE)))
      names(out)[1] <- rownames(x$output)[1]
      out},
      FUN.VALUE = numeric(dim(x$output)[1]))
    quants <- as.data.frame(t(apply(particles, 1, quantile, c(0.025, 0.975))))
    quants$date <- rownames(quants)
    names(quants)[1:2] <- c("ymin","ymax")

    base_plot <- plot(x, "deaths", ci = FALSE, replicates = TRUE, x_var = "date",
                      date_0 = max(x$pmcmc_results$inputs$data$date))
    base_plot <- base_plot +
      ggplot2::geom_line(ggplot2::aes(y=.data$ymin, x=as.Date(.data$date)), quants, linetype="dashed") +
      ggplot2::geom_line(ggplot2::aes(y=.data$ymax, x=as.Date(.data$date)), quants, linetype="dashed") +
      ggplot2::geom_point(ggplot2::aes(y=.data$deaths/x$pmcmc_results$inputs$pars_obs$phi_death,
                                       x=as.Date(.data$date)), x$pmcmc_results$inputs$data) +
      ggplot2::theme(legend.position = "top")

    if ("treated_deaths_only" %in% names(x$pmcmc_results$inputs$pars_obs)) {
      if (x$pmcmc_results$inputs$pars_obs$treated_deaths_only) {

        d_get <- format_output(x, "deaths_treatment", date_0 = max(x$pmcmc_results$inputs$data$date))

        particles_get <- vapply(seq_len(dim(x$output)[3]), function(y) {
          out <- c(0,diff(rowSums(x$output[,idx$D_get,y], na.rm = TRUE)))
          names(out)[1] <- rownames(x$output)[1]
          out},
          FUN.VALUE = numeric(dim(x$output)[1]))
        quants_get <- as.data.frame(t(apply(particles_get, 1, quantile, c(0.025, 0.975))))
        quants_get$date <- rownames(quants_get)
        names(quants_get)[1:2] <- c("ymin","ymax")

        base_plot <- base_plot +
          ggplot2::geom_line(ggplot2::aes(
            y=.data$y, x=as.Date(.data$date), color = .data$compartment, group = .data$replicate),
          alpha = 0.2, d_get, show.legend = TRUE) +
          ggplot2::geom_line(ggplot2::aes(y=.data$ymin, x=as.Date(.data$date)), quants_get, linetype="dotted") +
          ggplot2::geom_line(ggplot2::aes(y=.data$ymax, x=as.Date(.data$date)), quants_get, linetype="dotted") +
          ggplot2::theme(legend.position = "top")

      }
    }



  } else {

    stop("Requested what must be one of 'cases', 'deaths'")

  }

  base_plot <-  base_plot +
    ggplot2::ylab(ylab) +
    ggplot2::xlab(xlab)
  return(base_plot)


}

#..............................................................
# Collect & Summarize MCMC Runs
#..............................................................

#' @title create a master chain from a pmcmc_list object
#' @param x a pmcmc_list object
#' @param burn_in an integer denoting the number of samples to discard from each chain
#' @export
#'
create_master_chain <- function(x, burn_in) {

  if(class(x) != 'squire_pmcmc_list') {
    stop('x must be a squire_pmcmc_list object')
  }
  if(!is.numeric(burn_in)) {
    stop('burn_in must be an integer')
  }
  if(burn_in < 0) {
    stop('burn_in must not be negative')
  }
  if(burn_in >= x$inputs$n_mcmc) {
    stop('burn_in is greater than chain length')
  }

  chains <- lapply(x$chains, function(z, burnin){
    if (burnin > 0) {
      z$results[-seq_len(burn_in), ]
    } else {
      z$results
    }
  }, burnin = burn_in)

  do.call(what = rbind, args = chains)
}


#' @export
#' @importFrom stats cor sd
summary.squire_pmcmc <- function(object, ...) {

  par_names <- names(object$inputs$pars$pars_init[[1]])

  ## convert start_date to numeric to calculate stats
  data_start_date <- as.Date(object$inputs$data$date[1])
  traces <- object$results[,par_names]

  # calculate correlation matrix
  corr_mat <- round(cor(traces),2)

  # compile summary
  summ <- rbind(mean = colMeans(traces),
                apply(traces, MARGIN = 2, quantile, c(0.025, 0.975)),
                min = apply(traces, MARGIN = 2, min),
                max =  apply(traces, MARGIN = 2, max)
  )
  summ <- as.data.frame(summ)
  summ <- round(summ, 3)

  sds <- round(apply(traces, 2, sd), 3)
  # convert start_date back into dates
  summ$start_date <- as.Date(summ$start_date, data_start_date)
  summ[c('2.5%', '97.5%', 'min', 'max'), 'start_date'] <- summ[c('97.5%', '2.5%', 'max', 'min'), 'start_date']

  out <- list('summary' = summ,
              'corr_mat' = corr_mat,
              'sd' = sds)
  out

}

#' @export
summary.squire_pmcmc_list <- function(object, ..., burn_in = 101) {

  master_chain <- create_master_chain(x = object,
                                      burn_in = burn_in)

  z <- list(inputs = object$inputs,
            results = master_chain)
  summary.squire_pmcmc(z)

}


#' @export
#' @importFrom viridis cividis
#' @importFrom graphics hist par plot.new text
plot.squire_pmcmc <- function(x, ...) {

  summ <- summary(x)
  par_names <- names(x$inputs$pars$pars_init[[1]])

  traces <- x$results[, par_names]
  cols <- viridis::cividis(nrow(traces))
  cols <- cols[order(order(x$results$log_likelihood))]

  print_summ <- function(par_name) {
    x <- summ$summary
    paste0(x['mean', par_name],
           '\n(',
           x['2.5%', par_name],
           ', ',
           x['97.5%', par_name], ')')
  }



  n_pars <- length(par_names)


  par( bty = 'n',
       mfcol = c(n_pars, n_pars + 1L),
       mar = c(3,3,2,1),
       mgp = c(1.5, 0.5, 0),
       oma = c(1,1,1,1))


  for (i in seq_len(n_pars)) {
    for(j in seq_len(n_pars)) {

      if (i == j) { # plot hists on diagonal
        par_name <- par_names[i]
        breaks = ifelse(par_name == 'start_date',
                        yes = seq(as.Date('2019-12-01'),
                                  as.Date(x$inputs$data$date[1]), 7),
                        no = 10)
        hist(traces[[i]],
             main = print_summ(par_name),
             xlab = par_name,
             breaks = breaks,
             cex.main = 1,
             font.main = 1,
             freq = FALSE)
      } else if (i < j) {  # plot correlations on lower triangle
        plot(x = traces[[i]],
             y = traces[[j]],
             xlab = par_names[i],
             ylab = par_names[j],
             col = cols,
             pch = 20)
      } else if (i > j) { # print rho on upper triangle
        plot.new()
        text(x = 0.5,
             y=0.5,
             labels = paste('r =',
                            summ$corr_mat[i, j]))
      }
    }
  }

  # print traces in final column
  mapply(FUN = plot, traces,
         type = 'l',
         ylab = par_names,
         xlab = "Iteration")


}

#' @export
#' @importFrom viridis cividis
#' @importFrom graphics hist par plot.new text lines legend
#'
plot.squire_pmcmc_list <- function(x, burn_in = 1, thin = 1, ...) {

  summ <- summary(x, burn_in = burn_in)
  par_names <- names(x$inputs$pars$pars_init[[1]])
  n_pars <- length(par_names)

  chains <- x$chains
  n_chains <- length(chains)
  cols_trace <- rev(viridis::viridis(n_chains))


  # compile master chain and order by log posterior for plotting
  master_chain <- create_master_chain(x, burn_in = burn_in)

  master_chain <- master_chain[order(master_chain$log_posterior), ]
  cols <- viridis::cividis(nrow(master_chain))
  cols <- cols[order(master_chain$log_posterior)]

  traces <- lapply(par_names, FUN = function(par_name) {
    lapply(X = chains,
           FUN = function(z) z$results[-seq_len(burn_in), par_name])
  })
  names(traces) <- par_names

  plot_traces <- function(trace, col) {
    lines(x = seq_along(trace),
          y = trace,
          col = col)
  }

  breaks <- lapply(par_names, function(par_name){
    seq(from = min(master_chain[, par_name]),
        to =  max(master_chain[, par_name]),
        length.out = 20)
  })
  names(breaks) <- par_names

  hists <- lapply(par_names, FUN = function(par_name) {
    lapply(X = traces[[par_name]],
           FUN = hist,
           plot = FALSE,
           breaks = breaks[[par_name]])
  })
  names(hists) <- par_names

  hist_ylim <- lapply(hists, function(h) {
    chain_max <- sapply(h, function(chain) max(chain$density) )
    c(0, max(chain_max))
  })

  plot_hists <- function(h, col, breaks) {
    with(h, lines(x =  breaks,
                  y = c(density,
                        density[length(density)]),
                  type = 's',
                  col = col))
  }


  print_summ <- function(par_name) {
    x <- summ$summary
    paste0(x['mean', par_name],
           '\n(',
           x['2.5%', par_name],
           ', ',
           x['97.5%', par_name], ')')
  }


  par( bty = 'n',
       mfcol = c(n_pars, n_pars + 1L),
       mar = c(3,3,2,1),
       mgp = c(1.5, 0.5, 0),
       oma = c(1,1,1,1))


  for (i in seq_len(n_pars)) {
    for(j in seq_len(n_pars)) {

      if (i == j) { # plot hists on diagonal
        par_name <- par_names[i]
        bs <- breaks[[par_name]]
        plot(x = bs[1] ,  # force date axis where needed
             y = 1,
             type = 'n',
             xlim = c(bs[1], bs[length(bs)]),
             ylim = hist_ylim[[par_name]],
             xlab = par_name,
             ylab = '',
             main = print_summ(par_name),
             cex.main = 1,
             font.main = 1
        )

        map_out <- mapply(FUN = plot_hists,
                          h = hists[[par_name]],
                          col = cols_trace,
                          MoreArgs = list(breaks = bs))


      } else if (i < j) {  # plot correlations on lower triangle
        if (thin == 1) {
          smp <- seq_len(length(master_chain[[i]]))
        } else {
          smp <- sample(length(master_chain[[i]]),size = round(length(master_chain[[i]])*thin), replace = FALSE)
        }
        plot(x = master_chain[[i]][smp],
             y = master_chain[[j]][smp],
             xlab = par_names[i],
             ylab = par_names[j],
             col = cols[smp],
             pch = 20)
      } else if (i > j) { # print rho on upper triangle
        plot.new()
        text(x = 0.5,
             y=0.5,
             labels = paste('r =',
                            summ$corr_mat[i, j]))
      }
    }
  }


  # print traces in final column
  n_iter <- nrow(master_chain) / n_chains

  map_out <- mapply(FUN = function(par_name, leg) {
    plot(x = 1,
         y = breaks[[par_name]][1],
         type = 'n',
         xlab = 'Iteration',
         ylab = par_name,
         xlim = c(0, n_iter),
         ylim <- range(master_chain[, par_name]))

    map_out <- mapply(FUN = plot_traces,
                      trace = traces[[par_name]],
                      col = cols_trace)

    if(leg) {
      legend('top',
             ncol = n_chains,
             legend = paste('Chain', seq_len(n_chains)),
             fill = cols_trace,
             bty = 'n')
    }
  },
  par_name = par_names,
  leg = rep(FALSE, length(traces)))

}

