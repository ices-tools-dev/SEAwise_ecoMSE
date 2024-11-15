require(testthat)
require(smsR)

df.tmb <- get_TMB_parameters(
  mtrx = sandeel_1r$lhs, # List that contains M, mat, west, weca
  Surveyobs = sandeel_1r$survey, # Survey observations
  Catchobs = sandeel_1r$Catch, # Catch observations
  years = 1983:2021, # Years to include in model
  nseason = 2, # Number of seasons
  useEffort = TRUE, # Use effort to calculate F
  ages = 0:4, # Ages of the population
  recseason = 2, # Season when recruitment occurs
  CminageSeason = c(1,1), # Minimum catch age by season
  Fmaxage = 3, # Age with maximum fishing mortality
  Qminage = c(0,1),# Minimum age by survey
  Qmaxage = c(1,3), # Max age by survey
  Fbarage = c(1,2), # Min and max age used to calculate Fbar
  effort = sandeel_1r$effort, # Effort input
  blocks = c(1983,1999), # Breakpoints of blocks for selectivity
  nocatch = sandeel_1r$nocatch, # (Binary) non-zero catch by year and season
  surveyStart = c(0.75,0), # Portion of season elapsed at start of survey
  surveyEnd =  c(1,0), # Portion of season elapsed at end of survey
  surveySeason = c(2,1), # Season in which each survey occurs
  surveyCV =  list(c(0,1),c(1,2)), # Survey age groupings for CV estimates
  catchCV = list(c(1,3), c(1,3)), # Catch age groupings for CV estimates
  estCV = c(0,2,0), # Flags for how to handle CVs for 1) survey, 2) catch, 3) Stock recruitment relationship
  beta = 105809, # Assumed hockey stick break point (SSB)
  nllfactor = c(1,1,0.05) # Weights of 3 log-likelihood components

)

parms <- getParms(df.tmb)
sas <- runAssessment(df.tmb, parms)


test_that('getSSB works', {
  SSB <- getSSB(df.tmb, sas)

  expect_equal(SSB$SSB[nrow(SSB)], 142251.7, tolerance = 1) # Throw an error if NA

})
#
test_that('geBiomass works', {
  B <- getBiomass(df.tmb, sas)
  expect_equal(sum(B$Biomass), 65856316) # Throw an error if NA
})
#
test_that('getTSB works', {
  TSB <- getTSB(df.tmb, sas)
  expect_equal(sum(TSB$TSB), 31813946) # Throw an error if NA
})

#
test_that('getCatch works', {
  Catch <- getCatch(df.tmb, sas)
  expect_equal(is.numeric(Catch$Catch), TRUE) # Throw an error if NA
})
#
test_that('getCatchability works', {
  q <- getCatchability(df.tmb, sas)
  expect_equal(q$Q[1], 6.641422, tolerance = 1e-5) # Throw an error if NA
})

test_that('getSel works', {
  sel <- getSel(df.tmb, sas)
  expect_equal(sum(sel$Fsel), 6.281508, tolerance = 1e-5) # Throw an error if NA

})

test_that('getR works', {
  R <- getR(df.tmb, sas)
  expect_equal(R$R[39], 51128441, tolerance = 1e-5) # Throw an error if NA

})

test_that('getN works', {
  N <- getN(df.tmb, sas)
  expect_equal(sum(N$N), 12871366163, tolerance = 1) # Throw an error if NA

})


test_that('getCatchN works', {
  CN <- getCatchN(df.tmb, sas)
  expect_equal(sum(CN$CatchN), 1339063760, tolerance = 1) # Throw an error if NA

})

test_that('getYield works', {
  Yield <- getYield(df.tmb)
  expect_equal(sum(Yield$Yield), 11160267, tolerance = 1) # Throw an error if NA

})

test_that('getF works', {
  F0 <- getF(df.tmb,sas)
  expect_equal(mean(F0$F0), 0.2272798, tolerance = .01) # Throw an error if NA

})

test_that('getFbar works', {
  F0 <- getFbar(df.tmb,sas)
  expect_equal(mean(F0$Fbar), 0.5192592, tolerance = .01) # Throw an error if NA

})

test_that('getResidCatch works', {
  RC <- getResidCatch(df.tmb,sas)
  expect_equal(mean(RC$ResidCatch),-0.01431434, tolerance = .01) # Throw an error if NA

})

test_that('getResidSurvey works', {
  RC <- getResidSurvey(df.tmb,sas)
  expect_equal(mean(RC$ResidSurvey),2.658219e-08, tolerance = .001) # Throw an error if NA

})


test_that('getEstimatedparms works', { # Check to see if parameter estimation changes
  parms <- getEstimatedParms(sas)
  expect_equal(sum(parms$value),775.4538, tolerance = .01)

})

test_that('getCatchCV works', { # Check to see if parameter estimation changes
  ccv <- getCatchCV(df.tmb,sas)
  expect_equal(sum(ccv$catchCV),6.200394, tolerance = .001)

})

test_that('getsurveyCV works', { # Check to see if parameter estimation changes
  scv <- getSurveyCV(df.tmb,sas)
  expect_equal(sum(scv$surveyCV),2.66516, tolerance = .001)

})

test_that('AIC.sms works', { # Check to see if parameter estimation changes
  aic_test <- AIC(sas)
  expect_equal(AIC(sas),864.8487, tolerance = .001)

})

test_that('logLik.sms works', { # Check to see if parameter estimation changes
  loglik_test <- logLik(sas)
  expect_equal(as.numeric(loglik_test),-370.4244, tolerance = .001)

})

test_that('nobs.sms works', { # Check to see if parameter estimation changes
  nobs_test <- nobs(sas)
  expect_equal(nobs_test,352, tolerance = .1)

})

test_that('getSummary works', { # Check to see if parameter estimation changes
  sums <- getSummary(df.tmb,sas)
  expect_equal(sum(sums[,1]),80100, tolerance = .001)

})


test_that('getSR works', { # Check to see if parameter estimation changes
  SR <- getSR(df.tmb,sas)
  expect_equal(mean(SR$SR),54566200, tolerance = .001)

})

test_that('getSurvey works', { # Check to see if parameter estimation changes
  surv <- getSurvey(df.tmb,sas)
  expect_equal(mean(surv$surveyest),155196594, tolerance = 1)

})


test_that('getSelectivity works', { # Check to see if parameter estimation changes
  sel <- getSelectivity(df.tmb,sas)
  expect_equal(mean(sel$selec),0.2068351, tolerance = .001)

})


test_that('getSummaryCVs works', { # Check to see if parameter estimation changes
  sel <- getSummaryCVs(df.tmb,sas)
  expect_equal(as.numeric(colMeans(sel[,2:4])),c(0.17900121, 0.08852804, 0.23261650) , tolerance = .001)

})


test_that('getWeight works', { # Check to see if parameter estimation changes
  ww <- getWeight(df.tmb)
  expect_equal(mean(ww$value),0.01006359 , tolerance = .001)

})


test_that('getM works', { # Check to see if parameter estimation changes
  M2<- getM(df.tmb)
  expect_equal(mean(M2$value),0.3725904 , tolerance = .001)

})

test_that('getMat works', { # Check to see if parameter estimation changes
  Mat<- getMat(df.tmb)
  expect_equal(mean(Mat$value),0.5027047 , tolerance = .001)

})




