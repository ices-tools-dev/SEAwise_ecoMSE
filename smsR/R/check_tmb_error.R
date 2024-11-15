#' Check if parameter inputs has the correct format and dimensions to make sure TMB does not crash R
#'
#' @param df.tmb
#'
#' @export
#'
#' @examples
#' check_tmb_error(sandeel1r$df.tmb)
#'
check_tmb_error <- function(df.tmb){


nyears <- df.tmb$nyears
nage <- df.tmb$nage
nseason <- df.tmb$nseason


cstring <- 'Input data has wrong dimensions. Check this input: '

if(all(dim(df.tmb$weca) != c(nage, nyears+1, nseason))){
  stop(paste(cstring, 'weca'))
}

if(all(dim(df.tmb$west) != c(nage, nyears+1, nseason))){
  stop(paste(cstring, 'west'))
}

if(all(dim(df.tmb$Catchobs) != c(nage, nyears, nseason))){
  stop(paste(cstring, 'Catchobs'))
}

if(all(dim(df.tmb$Surveyobs) != c(nage, nyears, df.tmb$nsurvey))){
  stop(paste(cstring, 'Surveyobs'))
}

if(all(dim(df.tmb$Mat) != c(nage, nyears+1, nseason))){
  stop(paste(cstring, 'Mat'))
}

if(all(dim(df.tmb$M) != c(nage, nyears+1, nseason))){
  stop(paste(cstring, 'M'))
}

if(all(dim(df.tmb$propM) != c(nage, nyears+1, nseason))){
  stop(paste(cstring, 'propM'))
}

if(all(dim(df.tmb$propF) != c(nage, nyears+1, nseason))){
  stop(paste(cstring, 'propF'))
}




if(length(df.tmb$surveySeason) != df.tmb$nsurvey){
  stop('Add correct number of survey seasons')
}



}
