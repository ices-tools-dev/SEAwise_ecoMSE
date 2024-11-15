# Output utilities


#' Get a data frame of the spawning stock biomass
#'
#' `getSSB()` retrieves the spawning stock biomass per year, and it's associated uncertainty in the 95th confidence region
#'
#' @param df.tmb list of input parameters
#' @param sas fitted smsR model
#'
#' @return
#' data frame of spawning stock biomass and 95\% confidence intervals.
#' SE is standard error of log SSB
#' @seealso [getR()] why are the brackets here
#' @examples
#' SSB <- getSSB(df.tmb, sas)
#' print(SSB)
#' @export
getSSB <- function(df.tmb, sas) {
  reps <- sas$reps

  sdrep <- summary(reps)
  rep.values <- rownames(sdrep)
  years <- df.tmb$years

  SSB <- data.frame(SSB = sdrep[rep.values == "logSSB", 1])
  SSB$SE <- (sdrep[rep.values == "logSSB", 2])
  SSB$low <- SSB$SSB - 2 * SSB$SE
  SSB$high <- SSB$SSB + 2 * SSB$SE
  SSB$years <- c(years, max(years) + 1)

  SSB$SSB <- exp(SSB$SSB)
  SSB$low <- exp(SSB$low)
  SSB$high <- exp(SSB$high)


  return(SSB)
}

#' Get a data frame of the biomass of each age group
#'
#' @param df.tmb list of input parameters
#' @param sas fitted smsR model
#'
#' @seealso [getR()],[getYield()],[getF()],[getCatch()], [getSSB()], [getN()], [getFbar()], [getCatchN()], [getCatchCV()],
#' [getSurvey()]
#' @return
#' data frame containing the biomass of each age group.
#' low and high are the 95\% confidence intervals.
#' SE is standard error of log Biomass
#' @export
#'
getBiomass <- function(df.tmb, sas) {
  reps <- sas$reps

  sdrep <- summary(reps)
  rep.values <- rownames(sdrep)
  years <- df.tmb$years

  N <- array(sdrep[rep.values == "logBiomass", 1],
    dim = c(df.tmb$nage, df.tmb$nyears, df.tmb$nseason),
    dimnames = list(df.tmb$age, df.tmb$years, 1:df.tmb$nseason)
  )
  Nse <- array(sdrep[rep.values == "logBiomass", 2],
    dim = c(df.tmb$nage, df.tmb$nyears, df.tmb$nseason),
    dimnames = list(df.tmb$age, df.tmb$years, 1:df.tmb$nseason)
  )

  Biomass <- N
  BiomassSE <- as.data.frame.table(Nse)

  Biomass.df <- as.data.frame.table(Biomass)
  names(Biomass.df) <- c("ages", "years", "season", "Biomass")
  Biomass.df$SE <- BiomassSE$Freq
  Biomass.df$low <- Biomass.df$Biomass - 2 * Biomass.df$SE
  Biomass.df$high <- Biomass.df$Biomass + 2 * Biomass.df$SE
  Biomass.df$ages <- as.numeric(as.character(Biomass.df$ages))
  Biomass.df$years <- as.numeric(as.character(Biomass.df$years))

  Biomass.df <- Biomass.df %>% dplyr::select(Biomass, SE, low, high, ages, season, years)
  Biomass.df <- Biomass.df[-which(Biomass.df$Biomass == 0), ]
  Biomass.df$Biomass <- exp(Biomass.df$Biomass)
  Biomass.df$low <- exp(Biomass.df$low)
  Biomass.df$high <- exp(Biomass.df$high)

  return(Biomass.df)
}

#' Get the total stock biomass
#'
#' @param df.tmb list of input parameters
#' @param sas fitted smsR model
#'
#' @return
#' data frame containing the biomass of each age group.
#' low and high are the 95\% confidence intervals.
#' SE is standard error of log TSB
#' @seealso [getR()],[getSSB()],[getYield()],[getF()],[getCatch()], [getBiomass()], [getN()], [getFbar()], [getCatchN()], [getCatchCV()],
#' [getSurvey()]
#' @export
#'
#' @examples
#'
#' dat <- getTSB(df.tmb, sas)
#'
getTSB <- function(df.tmb, sas) {
  reps <- sas$reps

  sdrep <- summary(reps)
  rep.values <- rownames(sdrep)
  years <- df.tmb$years

  TSB <- data.frame(TSB = sdrep[rep.values == "logTSB", 1])
  TSB$SE <- (sdrep[rep.values == "logTSB", 2])
  TSB$low <- TSB$TSB - 2 * TSB$SE
  TSB$high <- TSB$TSB + 2 * TSB$SE
  TSB$years <- c(years)

  TSB$TSB <- exp(TSB$TSB)
  TSB$low <- exp(TSB$low)
  TSB$high <- exp(TSB$high)

  return(TSB)
}


#' Retrieve the total catch from a fitted smsR object
#'
#' @param df.tmb input parameters
#' @param sas fitted smsR object
#'
#' @return
#' data frame containing the biomass of catch each year.
#' low and high are the 95\% confidence intervals.
#' SE is standard error of log Catch
#' @export
#'
#' @examples
#' catch <- getCatch(df.tmb, sas)
#' print(catch)
getCatch <- function(df.tmb, sas) {
  reps <- sas$reps

  sdrep <- summary(reps)
  rep.values <- rownames(sdrep)
  years <- df.tmb$years

  tmp <- data.frame(Catch = sdrep[rep.values == "logCatchtot", 1])
  tmp$SE <- sdrep[rep.values == "logCatchtot", 2]
  tmp$low <- tmp$Catch - 2 * tmp$SE
  tmp$high <- tmp$Catch + 2 * tmp$SE
  tmp$years <- years

  tmp$Catch <- exp(tmp$Catch)
  tmp$low <- exp(tmp$low)
  tmp$high <- exp(tmp$high)

  Catch <- tmp


  return(Catch)
}


#' Get the survey catchability
#'
#' @param df.tmb sms input parameters
#' @param sas fitted model object
#'
#' @return
#' returns a table of survey catchability
#' @export
#'
#' @examples
#'
#' print(getCatchability(df.tmb, sas))
#'
getCatchability <- function(df.tmb, sas) {
  reps <- sas$reps

  sdrep <- summary(reps)
  rep.values <- rownames(sdrep)
  years <- df.tmb$years

  tmp <- data.frame(Q = sdrep[rep.values == "logQsurv", 1])
  tmp$Q[tmp$Q == 0] <- NA
  tmp <- exp(tmp)
  tmp$age <- rep(df.tmb$age, df.tmb$nsurvey)
  tmp$survey <- rep(1:df.tmb$nsurvey, each = df.tmb$nage)

  return(tmp)
}

#' Retrieve the fishing selectivity
#'
#' @param df.tmb sms input parameters
#' @param sas fitted sms model
#'
#' @return
#' returns a table of estimated fisheries selectivity
#' @export
#'
#' @examples
#' ## ' Not Run
#' sel <- getSel(df.tmb, sas)
#'
#' ## End
getSel <- function(df.tmb, sas) {
  reps <- sas$reps

  sdrep <- summary(reps)
  rep.values <- rownames(sdrep)
  years <- df.tmb$years
  blocks <- unique(df.tmb$bidx)
  Fage <- df.tmb$Fminage:df.tmb$Fmaxage

  tmp <- data.frame(
    Fsel = 0, age = rep(df.tmb$age, max(df.tmb$bidx) + 1),
    block = rep(unique(df.tmb$bidx), each = df.tmb$nage)
  )

  Fsel <- array(as.numeric(exp(sdrep[rep.values == "logFage", 1])), dim = c(length(Fage), length(blocks)))

  for (i in 1:length(blocks)) {
    tmp$Fsel[tmp$age %in% Fage & tmp$block == blocks[i]] <- Fsel[, i] / max(Fsel[, i])
    tmp$Fsel[tmp$age > max(Fage) & tmp$block == blocks[i]] <- Fsel[nrow(Fsel), i] / max(Fsel[, i])
  }


  return(tmp)
}


#' Get a data frame of recruitment from a fitted model
#'
#' @param df.tmb list of input parameters
#' @param sas fitted smsR model
#'
#' @return
#' data frame of recruitment.
#' Last year is based on the Stock recruitment relationship.
#' low and high are the 95\% confidence intervals.
#' SE is standard error of log R

#' @export
#'
#' @examples
#' R <- getR(df.tmb, sas)
#' print(R)
getR <- function(df.tmb, sas) {
  reps <- sas$reps

  sdrep <- summary(reps)
  rep.values <- rownames(sdrep)
  years <- df.tmb$years

  R <- data.frame(R = sdrep[rep.values == "logRec", 1])
  R$SE <- sdrep[rep.values == "logRec", 2]
  R$low <- R$R - 2 * R$SE
  R$high <- R$R + 2 * R$SE
  R$years <- years


  R$R <- exp(R$R)
  R$low <- exp(R$low)
  R$high <- exp(R$high)



  return(R)
}


#' Get numbers at age from a fitted model
#'
#' @param df.tmb list of input parameters
#' @param sas fitted smsR model
#'
#' @return
#' data frame of Numbers at age.
#' Last year is based on the expected survival from terminal year.
#' low and high are the 95\% confidence intervals.
#' SE is standard error of log N

#' @export
#'
#' @examples
#' N <- getN(df.tmb, sas)
getN <- function(df.tmb, sas) {
  reps <- sas$reps

  sdrep <- summary(reps)
  rep.values <- rownames(sdrep)
  years <- df.tmb$years

  N <- data.frame(N = sdrep[rep.values == "logN", 1])
  N$SE <- sdrep[rep.values == "logN", 2]
  N$low <- N$N - 2 * N$SE
  N$high <- N$N + 2 * N$SE
  N$ages <- df.tmb$age
  N$season <- rep(1:df.tmb$nseason, each = df.tmb$nage * (df.tmb$nyears))
  N$years <- rep(years, each = df.tmb$nage)

  if (min(N$N) == 0) {
    N <- N[-which(N$N == 0), ]
  }


  N$N <- exp(N$N)
  N$low <- exp(N$low)
  N$high <- exp(N$high)


  return(N)
}



#' Get the number of catch individuals per age
#'
#' @param df.tmb input parameters
#' @param sas fitted smsR model
#'
#' @return
#' data frame containing the numbers of individuals by age in the catch each year.
#' low and high are the 95\% confidence intervals.
#' SE is standard error of log N
#' @export
#'
#' @examples
#' CatchN <- getCatchN(df.tmb, sas)
#' print(CatchN)
getCatchN <- function(df.tmb, sas) {
  reps <- sas$reps

  sdrep <- summary(reps)
  rep.values <- rownames(sdrep)
  years <- df.tmb$years

  tmp <- data.frame(CatchN = sdrep[rep.values == "logCatchN", 1])
  tmp$SE <- sdrep[rep.values == "logCatchN", 2]
  tmp$low <- tmp$CatchN - 2 * tmp$SE
  tmp$high <- tmp$CatchN + 2 * tmp$SE
  tmp$ages <- df.tmb$age
  tmp$season <- rep(1:df.tmb$nseason, each = df.tmb$nage * (df.tmb$nyears))
  tmp$years <- rep(years, each = df.tmb$nage)
  tmp <- tmp[-which(tmp$CatchN == 0), ]


  tmp$CatchN <- exp(tmp$CatchN)
  tmp$low <- exp(tmp$low)
  tmp$high <- exp(tmp$high)

  return(tmp)
}

#' Get the observed yield from the fishery
#'
#' @param df.tmb a list of parameters for smsR model
#'
#' @return
#' returns the observed catch at age
#' @export
#'
#' @examples
#'
#' Yield <- getYield(df.tmb) # Get observed catch at age as a data frame
#'
getYield <- function(df.tmb) {
  Yield <- apply(df.tmb$Catchobs * df.tmb$weca[, 1:df.tmb$nyears, , drop = FALSE], MARGIN = c(2), FUN = sum)
  tmp <- data.frame(years = df.tmb$years, Yield = Yield)


  return(tmp)
}



#' Get fishing mortality at age from fitted model
#'
#' @param df.tmb list of input parameters
#' @param sas fitted smsR model
#'
#' @return
#' data frame of fishing mortality at age.
#' Last year is based on the expected survival from terminal year.
#' low and high are the 95\% confidence intervals.
#' SE is standard error of log F0

#' @export
#'
#' @examples
#'
#' Ftest <- getF(df.tmb, sas)
#'
getF <- function(df.tmb, sas) {
  reps <- sas$reps

  sdrep <- summary(reps)
  rep.values <- rownames(sdrep)
  years <- df.tmb$years

  tmp <- data.frame(F0 = sdrep[rep.values == "logF0", 1])
  tmp$SE <- sdrep[rep.values == "logF0", 2]
  tmp$low <- tmp$F0 - 2 * tmp$SE
  tmp$high <- tmp$F0 + 2 * tmp$SE
  tmp$ages <- df.tmb$age
  tmp$season <- rep(1:df.tmb$nseason, each = df.tmb$nage * (df.tmb$nyears))
  tmp$years <- rep(years, each = df.tmb$nage)
  tmp$F0[tmp$F0 == 0] <- -Inf
  tmp$low[tmp$F0 == -Inf] <- -Inf
  tmp$high[tmp$F0 == -Inf] <- -Inf

  tmp$F0 <- exp(tmp$F0)
  tmp$low <- exp(tmp$low)
  tmp$high <- exp(tmp$high)

  F0 <- tmp

  return(F0)
}

#' Get Fbar from a fitted smsR model
#'
#' @param df.tmb list of input parameters
#' @param sas smsR fitted model
#'
#' @return
#' data frame containing the average fishing mortality each year.
#' low and high are the 95\% confidence intervals.
#' SE is standard error of log Fbar
#' @export
#'
#' @examples
#'
#' Fbar <- getFbar(df.tmb, sas)
#'
getFbar <- function(df.tmb, sas) {
  reps <- sas$reps

  sdrep <- summary(reps)
  rep.values <- rownames(sdrep)

  tmp <- data.frame(Fbar = sdrep[rep.values == "logFavg", 1])
  tmp$SE <- sdrep[rep.values == "logFavg", 2]
  tmp$low <- tmp$Fbar - 2 * tmp$SE
  tmp$high <- tmp$Fbar + 2 * tmp$SE
  tmp$years <- df.tmb$years
  tmp$Fbar[tmp$Fbar == 0] <- -Inf
  tmp$low[tmp$Fbar == -Inf] <- -Inf
  tmp$high[tmp$Fbar == -Inf] <- -Inf

  tmp$Fbar <- exp(tmp$Fbar)
  tmp$low <- exp(tmp$low)
  tmp$high <- exp(tmp$high)

  return(tmp)
}

#' Get the catch residuals
#'
#' @param df.tmb list of input data for smsR object
#' @param sas fitted stock assessment model for smsR
#'
#' @export
#'
#' @return
#' Returns a data frame of catch residuals
#' @examples
#'
#' getResidCatch(df.tmb, sas)
#' print(getResidCatch)
#'
getResidCatch <- function(df.tmb, sas) {
  reps <- sas$reps

  sdrep <- summary(reps)
  rep.values <- rownames(sdrep)
  years <- df.tmb$years

  tmp <- data.frame(ResidCatch = sdrep[rep.values == "resid_catch", 1])
  tmp$ResidCatch[tmp$ResidCatch == -99] <- NA

  # tmp$SE <- sdrep[rep.values == 'resid_catch',2]
  # tmp$SE[is.na(tmp$ResidCatch)] <- NA
  # tmp$low <- tmp$ResidCatch-2*tmp$SE
  # tmp$high <- tmp$ResidCatch+2*tmp$SE
  tmp$ages <- df.tmb$age
  tmp$season <- rep(1:df.tmb$nseason, each = df.tmb$nage * (df.tmb$nyears))
  tmp$years <- rep(years, each = df.tmb$nage)

  tmp <- tmp[-which(is.na(tmp$ResidCatch)), ]

  Resid_Catch <- tmp

  return(Resid_Catch)
}


#' Get survey residuals from fitted model
#'
#' @param df.tmb list of input parameters
#' @param sas fitted smsR model
#'
#' @return
#' data frame of fishing mortality at age. Last year is based on the expected survival from terminal year. low and high are the 95\% confidence intervals
#' @export
#'
#' @examples
#' res <- getResidSurvey(df.tmb, sas)
#' print(res)
#'
getResidSurvey <- function(df.tmb, sas) {
  reps <- sas$reps

  sdrep <- summary(reps)
  rep.values <- rownames(sdrep)
  years <- df.tmb$years

  tmp <- data.frame(ResidSurvey = sdrep[rep.values == "resid_survey", 1])
  tmp$ResidSurvey[tmp$ResidSurvey == -99] <- NA

  tmp$SE <- sdrep[rep.values == "resid_survey", 2]
  # tmp$SE[is.na(tmp$ResidSurvey)] <- NA
  # tmp$low <- tmp$ResidSurvey-2*tmp$SE
  # tmp$high <- tmp$ResidSurvey+2*tmp$SE
  tmp$ages <- df.tmb$age
  tmp$years <- rep(years, each = df.tmb$nage)
  tmp$survey <- rep(dimnames(df.tmb$Surveyobs)$survey, each = df.tmb$nage * (df.tmb$nyears))
  tmp <- tmp[-which(is.na(tmp$ResidSurvey)), ]

  Resid_Survey <- tmp

  return(Resid_Survey)
}


#' Retrieve estimated parameters
#'
#' @param sas stock assessment output from smsR
#'
#' @return
#' data frame of estimated parameters and their SE
#' @export
#'
#' @examples
#'
#' print(getEstimatedParms(sas))
#'
getEstimatedParms <- function(sas) {
  parms.estimated <- data.frame(
    value = sas$opt$par,
    se = sqrt(diag(sas$reps$cov.fixed)),
    parameter = names(sas$opt$par)
  )


  return(parms.estimated)
}



#' Retrieve estimated catch CV
#' @param df.tmb Input list for smsR data
#' @param sas stock assessment output from smsR
#'
#' @return
#' data frame of estimated parameters and their SE
#' @export
#'
getCatchCV <- function(df.tmb, sas) {
  reps <- sas$reps

  sdrep <- summary(reps)
  rep.values <- rownames(sdrep)

  tmp <- data.frame(catchCV = sdrep[rep.values == "SD_catch2", 1])

  tmp$SE <- sdrep[rep.values == "SD_catch2", 2]
  tmp$low <- tmp$catchCV - 2 * tmp$SE
  tmp$high <- tmp$catchCV + 2 * tmp$SE

  tmp$ages <- df.tmb$age

  tmp$season <- rep(1:df.tmb$nseason, each = df.tmb$nage)

  # tmp <- tmp[-which(tmp$catchCV == 0),] # Remove the ones that are not caught


  CatchCV <- tmp
  return(CatchCV)
}


#' Retrieve estimated parameters
#'
#' @param sas stock assessment output from smsR
#'
#' @return
#' data frame of estimated parameters and their SE
#' @export
#'
getSurveyCV <- function(df.tmb, sas) {
  reps <- sas$reps

  sdrep <- summary(reps)
  rep.values <- rownames(sdrep)
  years <- df.tmb$years

  tmp <- data.frame(surveyCV = sdrep[rep.values == "SDS", 1])

  tmp$SE <- sdrep[rep.values == "SDS", 2]
  tmp$low <- tmp$surveyCV - 2 * tmp$SE
  tmp$high <- tmp$surveyCV + 2 * tmp$SE

  tmp$ages <- df.tmb$age

  tmp$survey <- rep(dimnames(df.tmb$Surveyobs)$survey, each = df.tmb$nage)

  # tmp <- tmp[-which(tmp$surveyCV == 0),] # Remove the ones that are not caught

  surveyCV <- tmp
  return(surveyCV)
}

#' Calculate AIC from smsR stock assessment object
#'
#' @param object Stock assesment object from smsR
#' @param p penalty on number of parameters, default = 2
#' @param ... etc
#' @param n AICc sample size, default = Inf
#'
#' @aliases AIC AIC.sms
#'
#' @return
#' return the AIC value of the fitted assessment
#' @export
#'
#' @examples
#'
#' AIC(sas)
AIC.sms <- function(object, p = 2, n = Inf, ...) {
  opt <- object$opt

  k <- length(opt[["par"]])
  nll <- opt[["objective"]]
  aic.sms <- p * k + 2 * nll + 2 * k * (k + 1) / (n - k - 1)
  return(aic.sms)
}

# Extract the log likelihood of a sms model
#
# @return object of class \code{logLik} with attributes
# \item{val}{log likelihood}
# \item{nobs,nall}{number of non NA observations initially supplied to TMB}
# \item{df}{number of parameters}
#' @importFrom stats logLik
#' @export
logLik.sms <- function(object, ...) {
  if (object$opt$convergence != 0) {
    val <- NA
  } else {
    val <- -object$opt$objective
  }

  nobs <- nobs.sms(object)
  df <- length(object$opt[["par"]])
  structure(val,
    nobs = nobs, nall = nobs, df = df,
    class = "logLik"
  )
}

#' @importFrom stats nobs
#' @export
nobs.sms <- function(object, ...) {
  sum(object$x$resid_survey != -99) + sum(object$x$resid_catch != -99)
}


#' Get summary of derived variables from fitted smsR object
#'
#' @param df.tmb list of input parameters for smsR object
#' @param sas fitted smsR model
#'
#' @return
#' returns a table of SSB, R, Yield and Fbar with uncertainty
#' @export
#'
#' @examples
#'
#' dat <- getSummary(df.tmb, sas)
#'
getSummary <- function(df.tmb, sas) {
  SSB <- getSSB(df.tmb, sas)
  R <- getR(df.tmb, sas)
  Yield <- getYield(df.tmb)
  Fbar <- getFbar(df.tmb, sas)

  Rgeo <- exp(mean(log(R$R)))

  df.out <- data.frame(
    years = SSB$years,
    R = c(R$R, Rgeo),
    Rhigh = c(R$high, NA),
    Rlow = c(R$low, NA),
    SSB = SSB$SSB,
    SSBhigh = SSB$high,
    SSBlow = SSB$low,
    Catch = c(Yield$Yield, NA),
    Fbar = c(Fbar$Fbar, NA),
    Fbarhigh = c(Fbar$high, NA),
    Fbarlow = c(Fbar$low, NA)
  )


  return(df.out)
}

#' Get the stock recruitment relationship
#'
#' @param df.tmb data frame with input data
#' @param sas fitted smsR model
#'
#' @return
#' returns a data frame with a stock recruitment relationship
#' @export
#'
#' @examples
#'
#' SR <- getSR(df.tmb, sas)
#'
getSR <- function(df.tmb, sas) {
  reps <- sas$reps

  sdrep <- summary(reps)
  rep.values <- rownames(sdrep)

  years <- df.tmb$years
  SSB <- getSSB(df.tmb, sas)

  alpha <- sdrep[rep.values == "logalpha", 1]
  SE <- (sdrep[rep.values == "logalpha", 2])
  low <- alpha - 2 * SE
  high <- alpha + 2 * SE
  SSBrange <- seq(1, max(SSB$SSB), length.out = 100)

  SR <- exp(alpha + log(SSBrange))
  SR[SSBrange > df.tmb$betaSR] <- exp(alpha + log(df.tmb$betaSR))

  mins <- exp(low + log(SSBrange))
  mins[SSBrange > df.tmb$betaSR] <- exp(low + log(df.tmb$betaSR))
  maxs <- exp(high + log(SSBrange))
  maxs[SSBrange > df.tmb$betaSR] <- exp(high + log(df.tmb$betaSR))


  SR.out <- data.frame(SR = SR, minSR = mins, maxSR = maxs, SSB = SSBrange)

  return(SR.out)
}



#' Get the estimated survey
#'
#' @param df.tmb list of input data for smsR
#' @param sas fitted smsR assessment model
#'
#' @return
#' returns the estimated survey as a data frame
#' @export
#'
#' @examples
#'
#' survest <- getSurvey(df.tmb, sas)
#'
getSurvey <- function(df.tmb, sas) {
  # Get the estimated survey
  reps <- sas$reps
  sdrep <- summary(reps)
  rep.values <- rownames(sdrep)
  years <- df.tmb$years

  tmp <- data.frame(surveyest = sdrep[rep.values == "Surveyout", 1])
  tmp$SE <- sdrep[rep.values == "Surveyout", 2]
  tmp$low <- tmp$surveyest - 2 * tmp$SE
  tmp$high <- tmp$surveyest + 2 * tmp$SE
  tmp$ages <- df.tmb$age
  tmp$survey <- rep(dimnames(df.tmb$Surveyobs)$survey, each = df.tmb$nage * (df.tmb$nyears))
  tmp$years <- rep(years, each = df.tmb$nage)

  tmp <- tmp %>% dplyr::filter(surveyest > 0)

  tmp$surveyest <- exp(tmp$surveyest)
  tmp$low <- exp(tmp$low)
  tmp$high <- exp(tmp$high)


  return(tmp)
}


#' Get the selectivity in the last year
#'
#' @param df.tmb list of input data for an smsR model
#' @param sas fitted smsR model
#'
#' @return
#' returns a table of most recent selectivity with uncertainty
#' @export
#'
#' @examples
#'
#' sel <- getSelectivity(df.tmb, sas)
#'
getSelectivity <- function(df.tmb, sas) {
  # Get the estimated survey
  reps <- sas$reps
  sdrep <- summary(reps)
  rep.values <- rownames(sdrep)
  years <- df.tmb$years

  tmp <- data.frame(selec = sdrep[rep.values == "log_exp_pattern", 1])
  tmp$SE <- sdrep[rep.values == "log_exp_pattern", 2]
  tmp$low <- tmp$selec - 2 * tmp$SE
  tmp$high <- tmp$selec + 2 * tmp$SE
  tmp$ages <- df.tmb$age
  tmp$season <- rep(1:df.tmb$nseason, each = df.tmb$nage)

  tmp$selec[tmp$selec < -100] <- -Inf

  tmp$selec <- exp(tmp$selec)
  tmp$low <- exp(tmp$low)
  tmp$high <- exp(tmp$high)


  return(tmp)
}

#' Get the CVs of the summary information
#'
#' @param df.tmb list of smsR input data
#' @param sas fitted smsR model
#' @param verbose print the output
#'
#' @return
#' returns a table of annual CVs for SSB, R and Fbar
#' @export
#'
#' @examples
#'
#' cvs <- getSummaryCVs(df.tmb, sas, verbose = TRUE) # Don't print it
#'
getSummaryCVs <- function(df.tmb, sas, verbose = TRUE) {
  ssb <- sas$reps$sd[names(sas$reps$value) == "logSSB"][1:df.tmb$nyears]
  Fbar <- sas$reps$sd[names(sas$reps$value) == "logFavg"][1:df.tmb$nyears]
  Rec <- sas$reps$sd[names(sas$reps$value) == "logRec"][1:df.tmb$nyears]



  df.out <- data.frame(
    years = df.tmb$years,
    CV_SSB = ssb,
    CV_Fbar = Fbar,
    CV_R = Rec
  )


  if (verbose) {
    tbl <- c(
      mean(ssb[df.tmb$nyears - 2:df.tmb$nyears]),
      mean(Fbar[df.tmb$nyears - 2:df.tmb$nyears]),
      mean(Rec[df.tmb$nyears - 2:df.tmb$nyears])
    )

    message(paste(
      paste("mean(CV(SSB)) = ", round(tbl[1], digits = 2)),
      paste("mean(CV(Fbar)) = ", round(tbl[2], digits = 2)),
      paste("mean(CV(R)) = ", round(tbl[3], digits = 2))
    ))
  }


  return(df.out)
}


#' get the weight at age from the input data and plot it
#'
#' @param df.tmb smsR list of input parameters
#' @param WW weca or west
#' @param plotFig plot the data (TRUE OR FALSE)
#'
#' @return returns a data frame of weight at aeg
#'
#' @export
#'
#' @examples
#' getWeight(df.tmb, WW = "west", plotFig = TRUE) # Plots the weight in the stock
#' @importFrom reshape2 melt
getWeight <- function(df.tmb, WW = "weca", plotFig = FALSE) {
  # Get weight per season
  weca <- df.tmb[WW][[1]]
  dimnames(weca) <- list(age = df.tmb$age, years = c(df.tmb$years, max(df.tmb$years) + 1), season = 1:df.tmb$nseason)

  weca <- reshape2::melt(weca)

  if (WW == "weca") {
    wlab <- "Weight at age\nin catch (g)"
  }

  if (WW == "west") {
    wlab <- "Weight at age\nin stock (g)"
  }


  if (plotFig == TRUE) {
    p1 <- ggplot(weca, aes(x = years, y = value * 1000, color = factor(age), group = factor(age))) +
      geom_line() +
      facet_wrap(~season, nrow = df.tmb$nseason) +
      theme_classic() +
      scale_y_continuous(wlab) +
      theme(legend.position = "top", legend.title = element_blank())

    print(p1)
  }
  return(weca)
}


#' get the natural mortality at age from the input data and plot it
#'
#' @param df.tmb smsR list of input parameters
#' @param plotFig plot the data (TRUE OR FALSE)
#'
#' @return returns a data frame of weight at aeg
#'
#' @export
#'
#' @examples
#'
#' M <- getM(df.tmb, plotFig = TRUE) # Plot the natural mortality by age
#'
#' @importFrom reshape2 melt
getM <- function(df.tmb, plotFig = FALSE) {
  # Get weight per season
  M <- df.tmb$M
  dimnames(M) <- list(age = df.tmb$age, years = c(df.tmb$years, max(df.tmb$years) + 1), season = 1:df.tmb$nseason)

  M <- reshape2::melt(M)

  if (plotFig == TRUE) {
    p1 <- ggplot(M %>% dplyr::filter(value > 0), aes(x = years, y = value, color = factor(age), group = factor(age))) +
      geom_line() +
      facet_wrap(~season, nrow = df.tmb$nseason) +
      theme_classic() +
      scale_y_continuous("natural mortality \n(per season)") +
      theme(legend.position = "top", legend.title = element_blank())

    print(p1)
  }
  return(M)
}

#' get the natural mortality at age from the assesment model
#'
#' @param df.tmb smsR list of input parameters
#' @param plotFig plot the data (TRUE OR FALSE)
#'
#' @return returns a data frame of weight at aeg
#'
#' @export
#'
#' @examples
#'
#' M <- getM(df.tmb, plotFig = TRUE) # Plot the natural mortality by age
#'
#' @importFrom reshape2 melt
#'
getM_var <- function(df.tmb, sas, plotFig = FALSE) {

  reps <- sas$reps
  sdrep <- summary(reps)
  rep.values <- rownames(sdrep)
  years <- df.tmb$years

  tmp <- data.frame(M_var = sdrep[rep.values == "M_new", 1])
  tmp$SE <- sdrep[rep.values == "M_new", 2]
  tmp$low <- tmp$M_var - 2 * tmp$SE
  tmp$high <- tmp$M_var + 2 * tmp$SE
  tmp$ages <- df.tmb$age
  tmp$season <- rep(1:df.tmb$nseason, each = df.tmb$nage)
  tmp$years <- rep(df.tmb$years, each = df.tmb$nage * df.tmb$nseason)


return(tmp)
}



#' get the maturity at age from the input data and plot it
#'
#' @param df.tmb smsR list of input parameters
#' @param plotFig plot the data (TRUE OR FALSE)
#'
#' @return returns a data frame of weight at aeg
#'
#' @export
#'
#' @examples
#' getMat(df.tmb, plotFig = TRUE) # Plot maturity
#'
#' @importFrom reshape2 melt
getMat <- function(df.tmb, plotFig = FALSE) {
  # Get weight per season
  Mat <- df.tmb$Mat
  dimnames(Mat) <- list(age = df.tmb$age, years = c(df.tmb$years, max(df.tmb$years) + 1), season = 1:df.tmb$nseason)

  Mat <- reshape2::melt(Mat)

  if (plotFig == TRUE) {
    p1 <- ggplot(Mat, aes(x = years, y = value, color = factor(age), group = factor(age))) +
      geom_line() +
      facet_wrap(~season, nrow = df.tmb$nseason) +
      theme_classic() +
      scale_y_continuous("Maturity ogive \n(per season)") +
      theme(legend.position = "top", legend.title = element_blank())

    print(p1)
  }
  return(Mat)
}
