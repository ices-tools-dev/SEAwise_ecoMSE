
#' Forecast an sms model
#'
#' @param df.tmb sms dataframe
#' @param sas fitted sms model
#' @param recruitment hockey, mean
#' @param HCR Fmsy or Bescape
#' @param avg_R vector of years to average R (e.g. 2010:2022)
#' @param Btarget SSB value for Btarget
#' @param Fcap Maximum possible fishing mortality
#'
#' @return
#' returns the TAC based on the applied HCR
#' @export
#'
#' @examples
#'getTAC(df.tmb, sas, HCR = 'Bescape')
#'
getTAC <- function(df.tmb,
                     sas,
                     recruitment = 'mean',
                     HCR = 'Bescape',
#                     avg_years = rep(3, 4),
                     avg_R = NULL,
                     Btarget = NULL,
                     Fcap = 2
                     ){

  # Extract N in the beginning of the coming year

  # Get the estimated survey
  reps <- sas$reps
  sdrep <- summary(reps)
  rep.values<-rownames(sdrep)

  N_temp <- as.numeric(sdrep[rep.values == 'term_logN_next',1])

  Rold <- getR(df.tmb, sas)[-df.tmb$nyears+1,]

  if(is.null(avg_R)){
    R.index <- df.tmb$years
  }else{
    R.index <- df.tmb$years[(df.tmb$nyears-avg_R+1):df.tmb$nyears]
  }

  if(recruitment == 'mean'){
    N_temp[1] <- log(exp(mean(log(Rold$R[Rold$years %in% R.index]), na.rm = TRUE)))
  }

  N_temp <- exp(N_temp)


  if(df.tmb$nseason == 1){
    N_current <- matrix(N_temp, nrow = df.tmb$nage, ncol = df.tmb$nseason)

    Fsel <- getF(df.tmb, sas)
    Fsel <- Fsel$F0[Fsel$years == max(df.tmb$years)]
    Fsel[df.tmb$age < df.tmb$Fminage & df.tmb$age > df.tmb$Fmaxage] <- 0

    Fsel <- (Fsel/mean(Fsel[(df.tmb$Fbarage[1]:df.tmb$Fbarage[2]+1)])) # Scale selectivity to Fbar
  }else{

    N_current <- matrix(0 , nrow = df.tmb$nage, ncol = df.tmb$nseason)
    N_current[2:df.tmb$nage,1] <- N_temp[2:df.tmb$nage]
    N_current[1, df.tmb$recseason] <- N_temp[1]

    Fsel <- getF(df.tmb, sas)
    Fsel <- matrix(Fsel$F0[Fsel$years == max(df.tmb$years)], nrow = df.tmb$nage, ncol = df.tmb$nseason)
    Fsel[df.tmb$age < df.tmb$Fminage & df.tmb$age > df.tmb$Fmaxage] <- 0


    Fsel <- (Fsel/mean(rowSums(Fsel)[(df.tmb$Fbarage[1]:df.tmb$Fbarage[2])+1])) # Scale selectivity to Fbar
  }
  # Now do a forecast with fishing mortality

  if(HCR == 'Fmsy'){ # Calc Fmsy from assessment model

    Fmsy <- getFmsy(df.tmb, sas, recruitment = 'hockey')
    F0 <- matrix(Fsel*Fmsy$Fmsy, nrow = df.tmb$nage, ncol = df.tmb$nseason)
    fc <- forecast.sms(df.tmb, N_current, F0)

  }

  if(HCR == 'Bescape'){

    if(is.null(Fcap)){warning('assuming Fcap = 2yr-1')}

    if(is.null(Btarget)){
      stop('Give a target Btarget to calc Bescape')}

    # Find the fishing mortality leading to Bescape
    F0 <- calcBescape(Btarget, df.tmb, N_current, Fsel, Fcap)*Fsel
    # Do forecast
    fc <- forecast.sms(df.tmb , N_current , F0)
    Fmsy <- mean(rowSums(F0)[(df.tmb$Fbarage[1]:df.tmb$Fbarage[2])+1])


    Fmsy <- list(Fmsy = Fmsy, MSY = NA)


  }

  if(HCR == 'Fmean'){

    Fsel <- getF(df.tmb, sas)
    Fsel <- matrix(Fsel$F0[Fsel$years == max(df.tmb$years)], nrow = df.tmb$nage, ncol = df.tmb$nseason)
    Fsel[df.tmb$age < df.tmb$Fminage & df.tmb$age > df.tmb$Fmaxage] <- 0

    fc <- forecast.sms(df.tmb, N_current, Fsel)
    Fmsy  <- list(Fmsy = NA,
                  MSY = NA)

    F0 <- Fsel
  }



ls.out <- list(TAC = fc$Catch,
               SSB = fc$SSB,
               Fmsy = Fmsy$Fmsy,
               Fnext = F0,
               MSY = Fmsy$MSY)

  return(ls.out)
    }



#' Simple one year forecast
#'
#' @param df.tmb smsR input data list
#' @param N_current Latest estimate of numbers at age. Can be extracted from 'sas' object
#' @param F0 Fishing mortality and selectivity to project
#'
#' @return
#' returns a list with forecasted SSB and catch
#' @export
#'
#' @examples
#'
#' N_current <-  N_current <- matrix(0 , nrow = df.tmb$nage, ncol = df.tmb$nseason)
#' N_current[2:df.tmb$nage,1] <- N_temp[2:df.tmb$nage]
#' N_current[1, df.tmb$recseason] <- N_temp[1]
#'
#' fcast <- forecast.sms(df.tmb, N_current, rep(0, df.tmb$nage))
#'
forecast.sms <- function(df.tmb , N_current, F0 ){

  N_future <- matrix(NA, df.tmb$nage) # For the following year SSB
  C_new <- matrix(0, df.tmb$nage, df.tmb$nseason)

  #
  M <- matrix(df.tmb$M[,df.tmb$nyears+1,], nrow = df.tmb$nage, ncol = df.tmb$nseason)
  weca <- matrix(df.tmb$weca[,df.tmb$nyears+1,], nrow = df.tmb$nage, ncol = df.tmb$nseason)
  west <- matrix(df.tmb$west[,df.tmb$nyears+1,], nrow = df.tmb$nage, ncol = df.tmb$nseason)
  mat <- matrix(df.tmb$Mat[,df.tmb$nyears+1,], nrow = df.tmb$nage, ncol = df.tmb$nseason)



  Z <- F0 + M


  for (qrts in 1:df.tmb$nseason) {
    for (i in 1:df.tmb$nage){

      if(Z[i,qrts]>0){
        C_new[i,qrts]=(F0[i,qrts]/Z[i,qrts])*N_current[i,qrts]*weca[i,qrts]*(1-exp(-(Z[i,qrts])))
      }

      if(qrts < df.tmb$nseason){

        if(i == 1 & qrts < df.tmb$recseason){
         # Dont do anything
        }else{
         N_current[i, qrts+1] <- N_current[i,qrts]*exp(-Z[i, qrts])
        }
      }else{

        N_future[1] <- 0
        if(i < df.tmb$nage){
          N_future[i+1] <- N_current[i,qrts]*exp(-Z[i,qrts])
        }else{
          N_future[df.tmb$nage] <- N_future[df.tmb$nage]+N_current[i,qrts]*exp(-Z[i,qrts])
        }
      }
        if(mat[1,1]>0){
          warning('new recruits not included in SSB for forecasted year')
        }
         SSB_next <- sum(N_future*weca[,1]*mat[,1])
    }
  }

  N_out <- data.frame(cbind(N_current, N_future))
  N_out$age <- df.tmb$age
  names(N_out)[1:df.tmb$nseason] <- paste(df.tmb$years[df.tmb$nyear]+1,1:df.tmb$nseason, sep = '-')
  names(N_out)[df.tmb$nseason+1] <- paste(df.tmb$years[df.tmb$nyear]+2,1, sep = '-')

  return(list(Catch = sum(C_new),
            SSB = sum(SSB_next),
            N_out = N_out,
            C_out = C_new
            )


       )
}


#' Calculate the fishing mortality required to get the assigned TAC
#'
#' @param TAC TAC were are trying to achieve
#' @param df.tmb SMS input list
#' @param N_current numbers at age at the beginning of the year
#' @param Fsel Selectivity
#' @param Fcap maximum fishing mortality allowed

#'
#' @return
#' Returns the fishing mortality required to reach the input TAC
#' @export
#'
#' @examples
#'
#' calcFTAC(10000, df.tmb,
#'
calcFTAC <- function(TAC ,
                        df.tmb,
                        N_current,
                        Fsel,
                        Fcap = 2){

  optFTAC <- function(data, par ){
    df.tmb <- data$df.tmb
    Fsel <- data$Fsel
    Fcalc <- as.numeric(par[1])
    TAC <- data$TAC

    F0 <- Fsel*Fcalc
    ls <- forecast.sms(df.tmb, N_current,F0)

    ans <- (log(TAC) - log(ls$Catch))^2
  }


  data.in <- list(TAC = TAC,
                  N_current = N_current,
                  Fsel = Fsel,
                  df.tmb = df.tmb)

  parms.in <- list(0.5)

  Fnew <- stats::optim(parms.in, lower = 0.001, upper = Fcap, fn = optFTAC, data= data.in, method = 'L-BFGS-B', control = list(ndeps = 1e-6))

  return(Fnew$par)
}


#' calc f required in OM to get TAC
#'
#' @param TAC TAC to optimize for
#' @param df.OM list of OM parameters
#' @param Fcap Maximum F to test
#'
#' @export
#'
getOM_FTAC <- function(TAC ,
                     df.OM,
                     Fcap = 10){

  optFTAC <- function(data, par ){
    df <- data[[2]]
    TAC <- data[[1]]
    # Fsel <- data$Fsel
    Fcalc <- as.numeric(par[1])
    # N_current <- df.OM$N_current

    df$F0[,length(df$years),] <- df$F0[,length(df$years),]*Fcalc
    #ls <- forecast.sms(df.tmb, N_current,F0)
    #ls <- forecast.sms(df.tmb)
    tmp <- run.agebased.sms.op(df)

    ans <- (log(TAC) - log(tmp$Catch[length(df$years)]))^2
  }



  # data.in <- list(TAC = TAC,
  #                 N_current = df.OM$N_current,
  #                 Fsel = Fsel,
  #                 df.OM = df.OM)
  data.in <- list(TAC,
                  df.OM)



  parms.in <- list(1)

  Fnew <- stats::optim(parms.in, lower = 0.0001, upper = Fcap, fn = optFTAC, data= data.in, method = 'L-BFGS-B')

  Fsel <- df.OM$F0[,length(df.OM$years),]

  return(Fnew$par*Fsel)
}


#' Calculate the fishing mortality required to get the assigned TAC
#'
#' @param Btarget Target spawning biomass to reach
#' @param df.tmb SMS input list
#' @param N_current numbers at age at the beginning of the year
#' @param Fsel Current selectivity
#' @param Fcap maximum fishing mortality
#'
#' @export
#'
calcBescape <- function(Btarget ,
                        df.tmb,
                        N_current,
                        Fsel,
                        Fcap = 2){

  optBtarget <- function(data, par ){
    df.tmb <- data$df.tmb
    Fsel <- data$Fsel
    Fcalc <- as.numeric(par[1])
    Btarget <- data$Btarget

    F0 <- Fsel*Fcalc
    ls <- forecast.sms(df.tmb, N_current,F0)

    ans <- (log(Btarget) - log(ls$SSB))^2
    return(ans)
  }


  data.in <- list(Btarget = Btarget,
                  N_current = N_current,
                  Fsel = Fsel,
                  df.tmb = df.tmb)

  parms.in <- list(0.5)

  Fnew <- stats::optim(parms.in, lower = 0, upper = Fcap, fn = optBtarget, data= data.in, method = 'L-BFGS-B')

  return(Fnew$par)
}




#' Produce an ICES forecast table
#'
#' @param df.tmb sms model input data
#' @param sas fitted sms model
#' @param TACold last years tac
#' @param HCR Harvest control rule to use - options Bescape or Fmsy
#' @param avg_R years to average recruitment
#' @param Btarget SSB at Btarget
#' @param Fcap Maximum possible F value
#' @param recruitment projected recruitment option ('mean)
#'
#' @return
#' Returns a table with different catch options
#' @export
#'
#' @examples
#' getForecastTable(df.tmb, sas, TACold = 1e4, HCR = 'Bescape')
#'
getForecastTable <- function(df.tmb,
                                sas,
                                TACold,
                                HCR = 'Bescape',
                                avg_R = NULL,
                                recruitment = 'mean',
                                Btarget = NULL,
                                TACtarget = NULL,
                                Fcap = 2){

  # Get numbers at age
  # Get the estimated survey
  reps <- sas$reps
  sdrep <- summary(reps)
  rep.values<-rownames(sdrep)

  N_temp <- as.numeric(sdrep[rep.values == 'term_logN_next',1])

  Rold <- getR(df.tmb, sas)


  if(is.null(avg_R)){
    avg_R <- df.tmb$years
  }

  if(recruitment == 'mean'){
    N_temp[1] <- log(exp(mean(log(Rold$R[Rold$years %in% avg_R]), na.rm = TRUE)))
  }

  N_temp <- exp(N_temp)

  if(df.tmb$nseason == 1){
    Fsel <- getF(df.tmb, sas)
    Fsel <- Fsel$Fsel[Fsel$years == max(df.tmb$years)]
    Fsel[df.tmb$age < df.tmb$Fminage & df.tmb$age > df.tmb$Fmaxage] <- 0

    Fsel <- (Fsel/mean(Fsel[(df.tmb$Fbarage[1]:df.tmb$Fbarage[2])+1])) # Scale selectivity to Fbar
  }else{

    N_current <- matrix(0 , nrow = df.tmb$nage, ncol = df.tmb$nseason)
    N_current[2:df.tmb$nage,1] <- N_temp[2:df.tmb$nage]
    N_current[1, df.tmb$recseason] <- N_temp[1]

    Fsel <- getF(df.tmb, sas)
    Fsel <- matrix(Fsel$F0[Fsel$years == max(df.tmb$years)], nrow = df.tmb$nage, ncol = df.tmb$nseason)
    Fsel[df.tmb$age < df.tmb$Fminage] <- 0


    Fsel <- (Fsel/mean(rowSums(Fsel)[(df.tmb$Fbarage[1]:df.tmb$Fbarage[2])+1])) # Scale selectivity to Fbar
  }


  F0 <- matrix(0, df.tmb$nage, df.tmb$nseason)
  f0 <- forecast.sms(df.tmb , N_current , F0)

  # Forecast with last years fishing
  Flast <- getF(df.tmb, sas) %>% dplyr::filter(years == max(df.tmb$years))
  F0 <- matrix(Flast$F0, nrow = df.tmb$nage, ncol = df.tmb$nseason)

  flast <- forecast.sms(df.tmb, N_current, F0)
  Fbar <- getFbar(df.tmb, sas)

  # Btarget = Blim
  Flim <- calcBescape(Btarget = df.tmb$betaSR, df.tmb, N_current, Fsel, Fcap = 2)
  flim <- forecast.sms(df.tmb , N_current , Flim*Fsel)


  # Regular Btarget
  if(is.null(Btarget) != 1){
   Fpa <- calcBescape(Btarget = Btarget, df.tmb, N_current, Fsel, Fcap)
   Fpa_sel <- Fpa * Fsel
   fpa <- forecast.sms(df.tmb , N_current , Fpa_sel)

   Fpa_nocap <- calcBescape(Btarget = Btarget, df.tmb, N_current, Fsel, Fcap  = 2)
   Fpa_sel <- Fpa_nocap * Fsel
   fpa_nocap <- forecast.sms(df.tmb , N_current , Fpa_sel)


  }

  if(is.null(TACtarget) != 1){
    F_tac <- calcFTAC(TACtarget, df.tmb, N_current,Fsel, Fcap)
    F_tac_f <- forecast.sms(df.tmb, N_current, F_tac*Fsel)
    F_tac_name <- 'Obs TAC'
  }else{
    F_tac <- NULL
    F_tac_f <- NULL
    F_tac_name <- NULL
  }


  HCRnames = c('Bescapement (Fcap)',
            'F = 0',
            'Bescapement (no cap)',
            'Blim',
            paste('F = F',df.tmb$years[df.tmb$nyears]),
            F_tac_name)

  TACs <- round(c(fpa$Catch,
            f0$Catch,
            fpa_nocap$Catch,
            flim$Catch,
            flast$Catch,
            F_tac_f$Catch),2)

  Fs <- round(c(Fpa,
          0,
          Fpa_nocap,
          Flim,
          Fbar$Fbar[Fbar$years == max(Fbar$years)],
          F_tac),2)

  SSBout <- round(c(fpa$SSB,
              f0$SSB,
              fpa_nocap$SSB,
              flim$SSB,
              flast$SSB,
              F_tac_f$SSB),2)

  SSBold <- getSSB(df.tmb, sas) %>% dplyr::filter(years == max(df.tmb$years+1)) %>% dplyr::select(SSB)


  if(TACold == 0){
    TACold = 0.001
  }



  SSBrel <- round((SSBout-as.numeric(SSBold))/as.numeric(SSBold), 2)*100
  TACrel <- round((TACs-as.numeric(TACold))/as.numeric(TACold), 2)*100


 # Create a nice table
  df.out <- data.frame(
    HCRnames,
    TACs,
    Fs,
    SSBout,
    SSBrel,
    TACrel
    )

names(df.out) <- c('Basis',
                   paste('Total Catch (', max(df.tmb$years)+1,')', sep = ''),
                   paste('F (', max(df.tmb$years)+1,')', sep = ''),
                   paste('SSB (', max(df.tmb$years)+2,')', sep = ''),
                   paste('SSB change %'),
                   paste('TAC change %')
  )


 # Do the list of input parameters
R_current <- round(c(Rold$R[nrow(Rold)],
               N_temp[1],
               as.numeric(SSBold)))

vars <- c(paste('Recruitment (', max(df.tmb$years),')', sep = ''),
          paste('Recruitment (', max(df.tmb$years)+1,')', sep = ''),
          paste('SSB (', max(df.tmb$years)+1,')', sep = ''))


df.fut <- data.frame(
  Variable = vars,
  Value = R_current)


return(list(Forecastval = df.fut,
            Forecast = df.out,
            N_current = N_current))
}





