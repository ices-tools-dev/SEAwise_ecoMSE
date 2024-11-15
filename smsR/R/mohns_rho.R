#' Calculate mohns rho.
#'
#' @param df.tmb list of tmb parameters
#' @param peels number of peels
#' @param parms parameters that are estimated
#' @param mps parameters that are mapped
#' @param useSSBprojection Use the projected SSB to calculate Mohns rho
#' @param lower lower boundary of parameters
#' @param upper upper boundary of parameters
#' @param plotfigure TRUE FALSE of whether to plot figure
#' @param limits limits on plot
#' @param dll which dll to use. Uses 'sms' as standard
#'
#' @return
#' returnsa list of Mohns rho values, input values, and a Mohns rho plot
#' @export
#'
#' @examples
#'
#' MR <- mohns_rho(df.tmb, peels = 5, parms)
#'
#' print(MR$p1()) # plot the results
#' @importFrom ggplot2 ggplot aes geom_bar coord_flip theme_classic geom_line geom_ribbon
#' @importFrom ggplot2 alpha scale_y_continuous coord_cartesian theme facet_wrap

mohns_rho <- function(df.tmb,
                      parms,
                      peels = 5,
                      mps = NULL,
                      useSSBprojection = TRUE,
                      lwr = list(NA),
                      upr = list(NA),
                      plotfigure = TRUE,
                      limits = NULL,
                      dll = "smsRdevel") {
  # Remove the dplyr summarise warning
  options(dplyr.summarise.inform = FALSE)

  # Run first the model as is
  # obj <-TMB::MakeADFun(df.tmb,parms,DLL=dll, map = mps, silent = TRUE)
  #
  # x <- obj$report()
  # system.time(opt<-stats::nlminb(obj$par,obj$fn,obj$gr,lower=lower,upper=upper,
  #                         control = list(iter.max = 1e6,
  #                                        eval.max = 1e6))) #
  # system.time(rep<-TMB::sdreport(obj))
  # rep

  # Save the results to a data frame
  asses1 <- runAssessment(df.tmb, parms = parms, mps = mps)

  SSB.base <- getSSB(df.tmb, asses1)
  recruit.base <- getR(df.tmb, asses1)


  F0base <- getFbar(df.tmb, asses1)

  if (useSSBprojection == TRUE) {
    ssb.index <- 0

    df.save <- data.frame(
      years = c(df.tmb$years, max(df.tmb$years) + 1),
      SSB = SSB.base$SSB[1:(nrow(SSB.base) - ssb.index)],
      R = c(recruit.base$R[1:(nrow(SSB.base) - 1)], NA),
      Fbar = c(F0base$Fbar, NA),
      peel = 0,
      convergence = asses1$reps$pdHess
    )

    df.var <- data.frame(
      years = df.save$years,
      SSBmin = SSB.base$low[1:(nrow(SSB.base) - ssb.index)],
      SSBmax = SSB.base$high[1:(nrow(SSB.base) - ssb.index)],
      Rmin = c(recruit.base$low[1:(nrow(SSB.base) - 1)], NA),
      Rmax = c(recruit.base$high[1:(nrow(SSB.base) - 1)], NA),
      Fmin = c(F0base$low, NA),
      Fmax = c(F0base$high, NA),
      peel = 0,
      convergence = 1
    )
  } else {
    ssb.index <- 1


    df.save <- data.frame(
      years = df.tmb$years,
      SSB = SSB.base$SSB[1:(nrow(SSB.base) - ssb.index)],
      R = recruit.base$R[1:(nrow(SSB.base) - 1)],
      Fbar = F0base$Fbar,
      peel = 0,
      convergence = asses1$reps$pdHess
    )

    df.var <- data.frame(
      years = df.save$years,
      SSBmin = SSB.base$low[1:(nrow(SSB.base) - ssb.index)],
      SSBmax = SSB.base$high[1:(nrow(SSB.base) - ssb.index)],
      Rmin = c(recruit.base$low[1:(nrow(SSB.base) - 1)]),
      Rmax = c(recruit.base$high[1:(nrow(SSB.base) - 1)]),
      Fmin = c(F0base$low),
      Fmax = c(F0base$high),
      peel = 0,
      convergence = 1
    )
  }




  xx <- data.frame(
    base = df.save$R[1:df.tmb$nyears],
    "-1" = NA,
    "-2" = NA,
    "-3" = NA,
    "-4" = NA,
    "-5" = NA,
    row.names = df.tmb$years
  )

  # colnames(xx) <- names(shake)

  for (i in 1:peels) {
    df.new <- df.tmb

    if (i == 1) {
      rmidx <- length(df.new$years)
    } else {
      rmidx <- ((length(df.new$years) - (i - 1)):length(df.new$years))
    }

    df.new$years <- df.new$years[-rmidx]

    df.new$nyears <- length(df.new$years)
    df.new$effort <- df.new$effort[1:df.new$nyears, , drop = F]
    df.new$bidx <- df.new$bidx[1:df.new$nyears]
    df.new$Surveyobs <- df.new$Surveyobs[, 1:df.new$nyears, , drop = F]
    df.new$Catchobs <- df.new$Catchobs[, 1:df.new$nyears, , drop = F]
    df.new$nocatch <- df.new$nocatch[1:df.new$nyears, , drop = F]
    df.new$env_matrix <- df.new$env_matrix[,1:df.new$nyears, drop = F]
    df.new$west <- df.new$west[,1:(df.new$nyears+1),, drop = F]
    df.new$weca <- df.new$weca[,1:(df.new$nyears+1),, drop = F]
    df.new$propM <- df.new$propM[,1:(df.new$nyears+1),, drop = F]
    df.new$propF <- df.new$propF[,1:(df.new$nyears+1),, drop = F]
    df.new$Mat <- df.new$Mat[,1:(df.new$nyears+1),, drop = F]
    df.new$M <- df.new$M[,1:(df.new$nyears+1),, drop = F]
    df.new$M_matrix <- df.new$M_matrix[,1:df.new$nyears, drop = F]
#
#
#     parms.new <- parms
#
#     parms.new$logRin <- parms.new$logRin[1:df.new$nyears]
#     parms.new$Fyear <- parms.new$Fyear[1:(df.new$nyears-1)]
#     parms.new$ext_M <- parms.new$ext_M[1:df.new$nyears,, drop =FALSE]

#
#     if(df.new$recmodel > 1){
#       parms.new$logRin <- parms.new$logRin[1:(length(parms.new$logRin)-1)]
#     }


    if (max(df.new$bidx) > 0) {
      if (max(df.new$years) < max(df.new$years[df.new$bidx])) {
        stop("peeling below the latest selectivity year")
      }
    }

    mps.new <- mps

    if ("Fyear" %in% names(mps.new)) {
      mps.new$Fyear <- factor(rep(NA, df.new$nyears))
    }

    if ("logRin" %in% names(mps.new)) {
      mps.new$logRin <- factor(rep(NA, df.new$nyears))

      if(recmodel > 1){
        mps.new$logRin <- factor(rep(NA, df.new$nyears-1))
      }
    }

    if ("ext_M" %in% names(mps.new)) {
      mps.new$ext_M <- factor(rep(NA, df.new$nyears))
    }

    parms.new <- getParms(df.new)

    assess.new <- runAssessment(df.new, parms = parms.new, mps = mps.new, silent = TRUE)


    SSB.tmb <- getSSB(df.new, assess.new)
    recruit.tmb <- getR(df.new, assess.new)


    F0.tmb <- getFbar(df.new, assess.new)


    if (useSSBprojection == TRUE) {
      tmp <- data.frame(
        years = SSB.tmb$years,
        SSB = SSB.tmb$SSB[1:(nrow(SSB.tmb) - ssb.index)],
        R = c(recruit.tmb$R[1:(nrow(SSB.tmb) - 1)], NA),
        Fbar = c(F0.tmb$Fbar, NA),
        peel = i,
        convergence = assess.new$reps$pdHess
      )
      index.new <- 1
    } else {
      tmp <- data.frame(
        years = df.new$years,
        SSB = SSB.tmb$SSB[1:(nrow(SSB.tmb) - ssb.index)],
        R = c(recruit.tmb$R[1:(nrow(SSB.tmb) - 1)]),
        Fbar = c(F0.tmb$Fbar),
        peel = i,
        convergence = assess.new$reps$pdHess
      )
      index.new <- 0
    }

    xx[1:(df.new$nyears), i + 1] <- tmp$R[1:df.new$nyears]

    df.save <- rbind(df.save, tmp)

    # Calculate mohns rho
    if (i == 1) {
      mohns <- data.frame(
        SSB = (tmp$SSB[nrow(tmp)] - SSB.base$SSB[df.tmb$nyears - i + index.new]) / SSB.base$SSB[df.tmb$nyears - i + index.new],
        R = (tmp$R[df.new$nyears] - recruit.base$R[df.tmb$nyears - i]) / recruit.base$R[df.tmb$nyears - i],
        F0 = (tmp$Fbar[df.new$nyears] - F0base$Fbar[df.tmb$nyears - i]) / F0base$Fbar[df.tmb$nyears - i],
        row.names = FALSE
      )
    } else {
      tmp.m <- data.frame(
        SSB = (tmp$SSB[nrow(tmp)] - SSB.base$SSB[df.tmb$nyears - i + index.new]) / SSB.base$SSB[df.tmb$nyears - i + index.new],
        R = (tmp$R[df.new$nyears] - recruit.base$R[df.tmb$nyears - i]) / recruit.base$R[df.tmb$nyears - i],
        F0 = (tmp$Fbar[df.new$nyears] - F0base$Fbar[df.tmb$nyears - i]) / F0base$Fbar[df.tmb$nyears - i],
        row.names = FALSE
      )

      mohns <- rbind(tmp.m, mohns)
    }
  }
  # Plot the figures and calculate Mohns rho

  mohns.tot <- mohns %>% dplyr::summarise(
    SSB = mean(SSB),
    R = mean(R),
    F0 = mean(F0)
  )

  df.save <- df.save %>% dplyr::arrange(years)


  df.plot <- df.save # %>% tidyr::pivot_longer(c(SSB,R,Fbar))

  SSBname <- paste("SSB, rho = ", round(mohns.tot$SSB, 3))
  Fbarname <- paste("Fbar, rho = ", round(mohns.tot$F0, 3))
  Rname <- paste("R, rho = ", round(mohns.tot$R, 3))
  #
  #
  if (is.null(limits)) {
    limits <- c(min(df.plot$years), max(df.plot$years))
  }

  Rlims <- c(min(df.var$Rmin), max(df.plot$R) * 1.5)

  p1 <- function() {
    x1 <- ggplot2::ggplot(df.plot, aes(x = years, y = SSB, color = factor(peel))) +
      geom_line() +
      theme_classic() +
      theme(legend.position = "none") +
      scale_y_continuous("") +
      coord_cartesian(xlim = limits) +
      geom_ribbon(
        data = df.var, aes(ymin = SSBmin, ymax = SSBmax, y = 0), fill = "red", alpha = .1,
        linetype = 0
      ) +
      scale_x_continuous("") +
      ggplot2::labs(title = SSBname)

    x2 <- ggplot2::ggplot(df.plot, aes(x = years, y = R, color = factor(peel))) +
      geom_line() +
      theme_classic() +
      theme(legend.position = "none") +
      scale_y_continuous("") +
      coord_cartesian(xlim = limits, ylim = Rlims) +
      geom_ribbon(
        data = df.var, aes(ymin = Rmin, ymax = Rmax, y = 0), fill = "red", alpha = .1,
        linetype = 0
      ) +
      scale_x_continuous("") +
      ggplot2::labs(title = Rname)

    x3 <- ggplot2::ggplot(df.plot, aes(x = years, y = Fbar, color = factor(peel))) +
      geom_line() +
      theme_classic() +
      theme(legend.position = "none") +
      scale_y_continuous("") +
      coord_cartesian(xlim = limits) +
      geom_ribbon(
        data = df.var, aes(ymin = Fmin, ymax = Fmax, y = 0), fill = "red", alpha = .1,
        linetype = 0
      ) +
      ggplot2::labs(title = Fbarname)


    p1 <- x1 / x2 / x3

    return(p1)
  }

  if (plotfigure == TRUE) {
    print(p1())
  }


  return(list(
    df.save = df.save,
    p1 = p1,
    mohns = mohns.tot
  ))
}
