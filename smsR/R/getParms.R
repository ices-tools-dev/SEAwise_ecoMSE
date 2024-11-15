#' Title
#'
#' @param df.tmb input values for TMB file
#' @param parms.true data frame of true parameters
#'
#' @return
#' returns a set of parameters to estimate for an smsR model
#'
#' @export
#'
#' @examples
#'
#' parms <- getParms(df.tmb)
#'
getParms <- function(df.tmb, parms.true = NULL) {
  # Try initial parameters in weird spots
  SDCatch <- rep(.6, max(df.tmb$Cidx_CV) + 1)
  SDsurvey <- rep(.1, length(unique(df.tmb$Qidx_CV[df.tmb$Qidx_CV > -1])))
  logQ <- sum(df.tmb$Qlastage - df.tmb$Qminage) + length(df.tmb$Qidx)

  if (is.null(df.tmb$betaSR)) {
    betaSR <- 50000
  } else {
    betaSR <- df.tmb$betaSR
  }




  if (df.tmb$useEffort == 1) {
    Fseason <- matrix(1, nrow = df.tmb$nseason - 1, ncol = length(unique(df.tmb$bidx)))
  } else {
    srows <- sum(df.tmb$isFseason)
    scol <- length(df.tmb$CminageSeason[1]:max(df.tmb$age))
    Fseason <- matrix(1, nrow = srows, ncol = scol)
  }

  if (df.tmb$useEffort == 0 & df.tmb$useBlocks == 1) {
    Fseason <- matrix(1, nrow = df.tmb$nseason, ncol = length(unique(df.tmb$bidx)))
  }

  if(df.tmb$nenv == 0){
    nenv = 1
  }

  if(df.tmb$nalphaM == 0){
    nalphaM <- 1
  }else{
    nalphaM <- df.tmb$nalphaM
  }


  parms <- list(
    logRin = rep(log(max(df.tmb$Catchobs)), df.tmb$nyears),
    logNinit = rep(log(max(df.tmb$Catchobs)), df.tmb$nage - 1),
    logFyear = rep(log(1), df.tmb$nyears - 1), # Mapped out
    Fseason = Fseason,
    logFage = matrix(log(1), nrow = length(df.tmb$Fminage:df.tmb$Fmaxage), ncol = length(unique(df.tmb$bidx))),
    SDsurvey = SDsurvey,
    SDcatch = as.matrix(SDCatch),
    creep = 0,
    logQ = rep(log(1), logQ), # length(df.tmb$surveyCV)
    pin = 1,
    logalpha = 2,
    logbeta = log(betaSR),
    logSDrec = log(1),
    logSDF = log(0.2),
    logSDM = log(rep(0.2, nalphaM)),
    env = rep(0, nenv),
    ext_M = matrix(0, ncol = nalphaM, nrow = df.tmb$nyears),
    alphaM = rep(0, nalphaM),
    logR0 = log(sum(df.tmb$Catchobs[,1,])*5),
    logh = log(0.5)
  )

  if (df.tmb$nseason == 1) {
    parms$Fseason <- matrix(1, nrow = 1, ncol = max(df.tmb$bidx) + 1)
  }


  if(df.tmb$recmodel == 3){

    parms$logRin <- 0 * parms$logRin # R is estimates as deviates from the mean
    parms$logRin <- parms$logRin[1:(df.tmb$nyears-1)] # R is estimates as deviates from the mean
    parms$logNinit <- 0 * parms$logNinit

  }

  if(df.tmb$recmodel == 2){

    parms$logRin <- 0 * parms$logRin # R is estimates as deviates from the mean
    parms$logRin <- parms$logRin[1:(df.tmb$nyears-1)] # R is estimates as deviates from the mean

  }


  if (!is.null(parms.true)) {
    pnames <- unique(parms.true$parameter)


    for (i in 1:length(pnames)) {
      if (is.null(dim(parms[[i]]))) {
        parms[names(parms) == pnames[i]][[1]] <- parms.true$value[parms.true$parameter == pnames[i]]
      } else {
        if (length(dim(parms[[i]])) == 2) {
          parms[names(parms) == pnames[i]] <- array(parms.true$value[parms.true$parameter == pnames[i]],
            dim = c(dim(parms[[i]])[1], dim(parms[[i]])[2])
          )
        }

        if (length(dim(parms[[i]])) == 3) {
          parms[names(parms) == pnames[i]] <- array(parms.true$value[parms.true$parameter == pnames[i]],
            dim = c(
              dim(parms[[i]])[1], dim(parms[[i]])[2],
              dim(parms[[i]])[3]
            )
          )
        }
      }
    }
  }


  return(parms)
}



#' Get a list of mapped parameters (not estimated)
#'
#' @param df.tmb list of tmb input data
#' @param parms list of estimated parameters
#' @param mapExtra vector of parameters to map
#'
#' @return
#' returns a list of parameters to be mapped for smsR
#' @export
#'
#' @examples
#'
#' # Map the  CV of surveys to the parms value
#' MPS <- getMPS(df.tmb, parms, mapExtra = "SDsurvey")
#'
getMPS <- function(df.tmb, parms, mapExtra = NA) {
  # Not estimated parameters
  mps <- list()

  if (sum(df.tmb$power) == 0) {
    mps$pin <- factor(parms$pin * NA)
  }


  if (df.tmb$estCV[2] == 2) {
    mps$SDcatch <- factor(parms$SDcatch * NA)
  }

  if (df.tmb$estCV[3] == 2) {
    mps$logSDrec <- factor(parms$logSDrec * NA)
  }



  if (df.tmb$useEffort == 1) {
    mps$logFyear <- factor(parms$logFyear * NA)
  }

  if (is.numeric(df.tmb$betaSR)) {
    mps$logbeta <- factor(parms$logbeta * NA)
  }

  if (df.tmb$nseason == 1) {
    mps$Fseason <- factor(parms$Fseason * NA)
  }

  if (df.tmb$estimateCreep == 0) {
    mps$creep <- factor(parms$creep * NA)
  }

  if (df.tmb$randomF == 0) {
    mps$logSDF <- factor(parms$logSDF * NA)
  }

  if(df.tmb$recmodel == 3){
    mps$logalpha <- factor(parms$logalpha * NA)
    mps$logbeta <- factor(parms$logalpha * NA)
  }

  if(df.tmb$recmodel != 3){
    mps$logR0 <- factor(NA)
  }



  if(df.tmb$randomM == 0){
    mps$ext_M <- factor(parms$ext_M * NA)
    mps$logSDM <- factor(NA)
    mps$alphaM <- factor(NA * parms$alphaM)

  }


  if(all(is.na(df.tmb$Pred_in))){
    mps$alphaM <- factor(NA * parms$alphaM)
  }


  mps$logh <- factor(parms$logh * NA)


  if(df.tmb$nenv == 0){
    mps$env <- rep(factor(NA), 1) # Ignore the environmental input in

  }


  if(df.tmb$nalphaM == 0){
    mps$alphaM <- factor(rep(factor(NA),1))
  }



  for (i in 1:length(mapExtra)) {
    if (is.na(mapExtra[i]) == 0) {
      mps[[mapExtra[i]]] <- factor(parms[[mapExtra[i]]] * NA)
    }
  }


  return(mps)
}
