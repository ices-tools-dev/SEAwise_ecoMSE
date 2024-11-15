#' read data from a SAM stock assesment for comparison
#'
#' @param wd working directory of sam data
#'
#' @return returns a list of life history, catch, and survey data from SAM
#' @export
#'
#' @examples
#' wd <- 'C:/testwd'
#' read.sam.data('C:/testwd') # Get the list
#'
read.sam.data <- function(wd){


  # Get catch numbers at age
  cn.yr <- scan(file.path(wd,'cn.dat'), skip = 2, nlines=1, quiet = TRUE)
  cn.yr <- cn.yr[1]:cn.yr[2]
  cn.age <- scan(file.path(wd,'cn.dat'), skip = 3, nlines=1, quiet = TRUE)
  cn.age <- cn.age[1]:cn.age[2]

  cn <- read.table(file.path(wd, 'cn.dat'), skip = 5)
  colnames(cn) <- cn.age

  if(cn.age[1] >0){
    age.idx = 0
    while(age.idx < min(cn.age)){
    cn <- cbind(data.frame(age.idx = rep(0, length(cn.yr))),cn)
    age.idx <- age.idx + 1
    }
  }
  # Turn into matrix
  cn <- array(t(as.matrix(cn)), dim = c(length(0:max(cn.age)), length(cn.yr),1))

  # weca

  cw.yr <- scan(file.path(wd,'cw.dat'), skip = 2, nlines=1, quiet = TRUE)
  cw.yr <- cw.yr[1]:cw.yr[2]
  cw.age <- scan(file.path(wd,'cw.dat'), skip = 3, nlines=1, quiet = TRUE)
  cw.age <- cw.age[1]:cw.age[2]

  cw <- read.table(file.path(wd, 'cw.dat'), skip = 5)
  colnames(cw) <- cw.age

  if(cw.age[1] >0){
    age.idx = 0
    while(age.idx < min(cw.age)){
      cw <- cbind(data.frame(age.idx = rep(0, length(cw.yr))),cw)
      age.idx <- age.idx + 1
    }
  }
  # Turn into matrix
  weca <- array(t(as.matrix(cw)), dim = c(length(0:max(cw.age)), length(cw.yr),1))


  sw.yr <- scan(file.path(wd,'sw.dat'), skip = 2, nlines=1, quiet = TRUE)
  sw.yr <- sw.yr[1]:sw.yr[2]
  sw.age <- scan(file.path(wd,'sw.dat'), skip = 3, nlines=1, quiet = TRUE)
  sw.age <- sw.age[1]:sw.age[2]

  sw <- read.table(file.path(wd, 'sw.dat'), skip = 5)
  colnames(sw) <- sw.age

  # Turn into matrix
  if(sw.age[1] >0){
    age.idx = 0
    while(age.idx < min(sw.age)){
      sw <- cbind(data.frame(age.idx = rep(0, length(sw.yr))),sw)
      age.idx <- age.idx + 1
    }
  }

    west <- array(t(as.matrix(sw)), dim = c(length(0:max(sw.age)), length(sw.yr),1))


  # Maturity

  mat.yr <- scan(file.path(wd,'mo.dat'), skip = 2, nlines=1, quiet = TRUE)
  mat.yr <- mat.yr[1]:mat.yr[2]
  mat.age <- scan(file.path(wd,'mo.dat'), skip = 3, nlines=1, quiet = TRUE)
  mat.age <- mat.age[1]:mat.age[2]

  mat <- read.table(file.path(wd, 'mo.dat'), skip = 5)
  colnames(mat) <- mat.age

  if(mat.age[1] >0){
    age.idx = 0
    while(age.idx < min(mat.age)){
      mat <- cbind(data.frame(age.idx = rep(0, length(mat.yr))),mat)
      age.idx <- age.idx + 1
    }
  }


  # Turn into matrix
  mat <- array(t(as.matrix(mat)), dim = c(length(0:max(mat.age)), length(mat.yr),1))


  # PropF
  propF.yr <- scan(file.path(wd,'pf.dat'), skip = 2, nlines=1, quiet = TRUE)
  propF.yr <- propF.yr[1]:propF.yr[2]
  propF.age <- scan(file.path(wd,'pf.dat'), skip = 3, nlines=1, quiet = TRUE)
  propF.age <- propF.age[1]:propF.age[2]

  propF <- read.table(file.path(wd, 'pf.dat'), skip = 5)
  colnames(propF) <- propF.age


  if(propF.age[1] >0){
    age.idx = 0
    while(age.idx < min(propF.age)){
      propF <- cbind(data.frame(age.idx = rep(0, length(propF.yr))),propF)
      age.idx <- age.idx + 1
    }
  }



  # Turn into propFrix
  propF <- array(t(as.matrix(propF)), dim = c(length(0:max(propF.age)), length(propF.yr),1))

  # PropM
  propM.yr <- scan(file.path(wd,'pm.dat'), skip = 2, nlines=1, quiet = TRUE)
  propM.yr <- propM.yr[1]:propM.yr[2]
  propM.age <- scan(file.path(wd,'pm.dat'), skip = 3, nlines=1, quiet = TRUE)
  propM.age <- propM.age[1]:propM.age[2]

  propM <- read.table(file.path(wd, 'pm.dat'), skip = 5)
  colnames(propM) <- propM.age

   if(propM.age[1] >0){
    age.idx = 0
    while(age.idx < min(propM.age)){
      propM <- cbind(data.frame(age.idx = rep(0, length(propM.yr))),propM)
      age.idx <- age.idx + 1
    }
  }

  # Turn into propMrix
  propM <- array(t(as.matrix(propM)), dim = c(length(0:max(propM.age)), length(propM.yr),1))

  # M2
  M2.yr <- scan(file.path(wd,'nm.dat'), skip = 2, nlines=1, quiet = TRUE)
  M2.yr <- M2.yr[1]:M2.yr[2]
  M2.age <- scan(file.path(wd,'nm.dat'), skip = 3, nlines=1, quiet = TRUE)
  M2.age <- M2.age[1]:M2.age[2]

  M2 <- read.table(file.path(wd, 'nm.dat'), skip = 5)
  colnames(M2) <- M2.age

  if(M2.age[1] >0){
    age.idx = 0
    while(age.idx < min(M2.age)){
      M2 <- cbind(data.frame(age.idx = rep(as.numeric(M2[1]), length(M2.yr))),M2)
      age.idx <- age.idx + 1
    }
  }

  # Turn into M2rix
  M2 <- array(t(as.matrix(M2)), dim = c(length(0:max(M2.age)), length(M2.yr),1))

  # Surveys (might be several)
  n_surv_lines <- nrow(read.table(file.path(wd, 'survey.dat'), fill = TRUE, flush = TRUE))

  surv.yr <- scan(file.path(wd,'survey.dat'), skip = 3, nlines=1, quiet = TRUE)
  surv.yr <- surv.yr[1]:surv.yr[2]

  surv.age <- scan(file.path(wd,'survey.dat'), skip = 5, nlines=1, quiet = TRUE)
  surv.age <- surv.age[1]:surv.age[2]

  surv <- read.table(file.path(wd, 'survey.dat'), skip = 6, nrows = length(surv.yr))
  surv <- as.data.frame(surv[,-1])
  colnames(surv) <- surv.age

  ntot <- nrow(surv)+6

  slist <- list()
  slist[[1]] <- list(surv = surv, age = surv.age, yr = surv.yr)


  i <- 2

  while(ntot < n_surv_lines){


    surv.yr <- scan(file.path(wd,'survey.dat'), skip = ntot+1, nlines=1, quiet = TRUE)
    surv.yr <- surv.yr[1]:surv.yr[2]

    surv.age <- scan(file.path(wd,'survey.dat'), skip = ntot+3, nlines=1, quiet = TRUE)
    surv.age <- surv.age[1]:surv.age[2]

    surv <- read.table(file.path(wd, 'survey.dat'), skip = ntot+4, nrows = length(surv.yr))
    colnames(surv)[2:ncol(surv)] <- surv.age
    surv <- as.data.frame(surv[,-1])

    slist[[i]] <- list(surv = surv, age = surv.age, yr = surv.yr)
    i <- i + 1
    ntot <- nrow(surv) + ntot + 4

  }

  nsurvey <- i-1
  ages <- 0:max(sw.age)
  years <- sw.yr

  surv.out <- array(-1, dim = c(length(ages), length(years), nsurvey))

  for(i in 1:nsurvey){
    s.ages <- slist[[i]]$age
    s.years <- slist[[i]]$yr
    for(k in 1:length(s.ages)){
    surv.out[ages == s.ages[k], years %in% s.years, i] <- as.numeric(slist[[i]]$surv[,k])
    }
  }


  surv.out[is.na(surv.out)] <- -1

  # Turn into matrix same size as catch data
#
#   # Get indices
#   yr.idx <- which(cw.yr %in% surv.yr)
#   age.idx <- which(cw.age %in% surv.age)
#
#
#   surv.out[age.idx, yr.idx,] <- t(as.matrix(surv))
#
#   # message('removing first row of survey, check input data')




 # Save it all in a list

  mtrx <- list(weca = weca, west = west, mat = mat, M = M2,propF = propF, propM = propM)

  ls.out <- list(mtrx = mtrx,
                 Catchobs = cn,
                 Surveyobs = surv.out)

return(ls.out)
}
