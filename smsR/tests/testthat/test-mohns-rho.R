## Test Mohns
require(testthat)

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


test_that('Mohns rho works', {

  mr <- mohns_rho(df.tmb,parms, peels = 3, plotfigure = FALSE)

  expect_equal(mr$df.save$SSB[mr$df.save$years == 2022 & mr$df.save$peel == 0],142251.71, tolerance = 1e-4 )
  })



