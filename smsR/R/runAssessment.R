#' run the assessment with the data and the estimated parameters
#'
#' @param df.tmb list of input data
#' @param parms parameters
#' @param lwr optional list of user evaluated lower bounds
#' @param upr optional list of user evaluated lower bounds
#' @param mps optional list of parameteres to be mapped
#' @param silent silent or verbose
#' @param dll optional file in case of custom stuff
#'
#' @return
#' @export
#'
#' @examples
runAssessment <- function(df.tmb,
                           parms,
                           lwr = list(NA),
                           upr = list(NA),
                           mps = NULL,
                           silent = TRUE,
                           dll = "smsRdevel") {

  # Start timer
  tmm <- Sys.time()

  if (is.null(mps)) {
    # map the parameters that are not estimated in this model
    mps <- getMPS(df.tmb, parms)
  }

  if (is.null(df.tmb$betaSR)) {
    df.tmb$betaSR <- NA
  }

  if (length(df.tmb$Qidx) > length(parms$logQ)) {
    stop("wrong length of survey ages - redo input parameters")
  }

  rlist <- c()


  if (df.tmb$randomF == 1) {
    rlist <- c("logFyear")
  }

  if (df.tmb$randomR == 1) {
    rlist <- c(rlist,"logRin")
  }


  if(df.tmb$recmodel == 3){
    rlist <- c(rlist, 'logNinit')
  }

  if(df.tmb$randomM == 1){
    rlist <- c(rlist,'ext_M')
  }

  #  rlist <- c()

  # Test some lengths

  if(dim(df.tmb$env_matrix)[2] != df.tmb$nyears){stop('watch size of env matrix')}
  if(dim(df.tmb$weca)[2] < df.tmb$nyears){stop('watch size of weca')}
  if(dim(df.tmb$west)[2] < df.tmb$nyears){stop('watch size of west')}
  if(dim(df.tmb$Mat)[2] < df.tmb$nyears){stop('watch size of mat')}
  if(dim(df.tmb$M)[2] < df.tmb$nyears){stop('watch size of M')}
  if(dim(df.tmb$M_matrix)[2] < df.tmb$nyears){stop('Check size of M_matrix')}
  obj <- TMB::MakeADFun(df.tmb, parms, DLL = dll, map = mps, silent = silent, random = rlist)
  x <- obj$report()

  # Set boundaries
  lower <- obj$par - Inf
  upper <- obj$par + Inf

  # Permanent bounds for realism
  lower[names(lower) == "logFyear"] <- log(0.001)
  lower[names(lower) == "Fseason"] <- 0.0001
  lower[names(lower) == "SDsurvey"] <- 0.0001
  lower[names(lower) == "logSDrec"] <- log(0.1)
  lower[names(lower) == "SDcatch"] <- 0.01
  lower[names(lower) == "creep"] <- -0.1
  lower[names(lower) == 'logh'] <- log(0.21)
  lower[names(lower) == 'logSDM'] <- log(0.01)
  lower[names(lower) == 'ext_M'] <- -.5
  lower[names(lower) == 'alphaM'] <- 0.00001
  #lower[names(lower) == 'logalpha'] <- log(max(df.tmb$Catchobs))


  upper[names(upper) == "SDsurvey"] <- 2
  upper[names(upper) == "logSDrec"] <- log(4)
  upper[names(upper) == "logFyear"] <- log(10)
  upper[names(upper) == "SDcatch"] <- sqrt(2)
  upper[names(upper) == "creep"] <- 0.2
  upper[names(upper) == 'logh'] <- log(0.99)
  upper[names(upper) == 'logSDM'] <- log(2)
  upper[names(upper) == 'ext_M'] <- .5
  # Add custom boundaries to parameters
  for (i in 1:length(lwr)) {
    if (is.na(lwr[[1]][1]) == 0) {
      idx <- (names(lower) %in% names(lwr)[i])

      if (sum(idx) != length(lwr[[i]])) {
        if (length(lwr[[i]]) != 1) {
          stop("Error in custom boundaries")
        }
      }

      lower[idx] <- lwr[[i]]
    }
  }
  #
  for (i in 1:length(upr)) {
    if (is.na(upr[[1]][1]) == 0) {
      idx2 <- (names(upper) %in% names(upr)[i])
      if (sum(idx2) != length(upr[[i]])) {
        if (length(lwr[[i]]) != 1) {
          stop("Error in custom boundaries")
        }
      }

      upper[idx2] <- upr[[i]]
    }
  }

  system.time(opt <- stats::nlminb(obj$par, obj$fn, obj$gr,
                                   lower = lower, upper = upper,
                                   control = list(
                                     iter.max = 1e5,
                                     eval.max = 1e5
                                   )
  )) #


  system.time(reps <- TMB::sdreport(obj))


  # Add congrats
  if (reps$pdHess == TRUE & max(abs(reps$gradient.fixed)) < 0.1) {
    message("Congrats, your model converged")
  } else {
    message("Check model convergence")
    message(paste("hessian =", reps$pdHess))
    message(max(abs(reps$gradient.fixed)))
  }


  # Throw a warning if something is on the boundary
  if (any(reps$par.fixed == lower)) {
    parms.boundary <- reps$par.fixed[reps$par.fixed == lower]

    for (i in 1:length(parms.boundary)) {
      warning(paste(names(parms.boundary[i]), "estimated on the lower boundary"))
    }
  }

  if (any(reps$par.fixed == upper)) {
    parms.boundary <- reps$par.fixed[reps$par.fixed == upper]

    for (i in 1:length(parms.boundary)) {
      warning(paste(names(parms.boundary[i]), "estimated on the upper boundary"))
    }
  }

  opt$date <- tmm
  opt$time_to_eval <- as.numeric(Sys.time() - tmm)

  return(structure(list(
    x = x,
    opt = opt,
    reps = reps,
    obj = obj, dat = df.tmb
  ), class = "sms"))
}
