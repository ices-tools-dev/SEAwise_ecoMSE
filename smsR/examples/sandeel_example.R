# Run a the 2022 stock assessment of sandeel in Area 1a
library(smsR)

years <- 1983:2022
nseason <- 2
ages <- 0:4

Qminage = c(1,0) # Qminage = c(0,1) minimum age in surveys
Qmaxage = c(3,1) #Qmaxage = c(1,3)
surveyStart = c(0,0.75) #c(0.75,0)
surveyEnd =  c(0,1) # c(1,0) Does the survey last throughout the season it's conducted?
surveySeason = c(1,2) # c(2,1)Which seasons do the surveys occur in
surveyCV =  list(c(1,2),
                 c(0,1)) #c(1,2)),
powers = list(NA,
              NA)




df.tmb <- get_TMB_parameters(
  mtrx = sandeel_1r[['mtrx']], # List that contains M, mat, west, weca
  Surveyobs = sandeel_1r$survey, # Survey observations (dimensions age, year, quarter, number of surveys)
  Catchobs = sandeel_1r$Catch, # Catch observations  (dimensions age, year, quarter)
  years = years, # Years to run
  nseason = nseason, # Number of seasons
  useEffort = 1,
  ages = ages, # Ages of the species
  recseason = 2, # Season where recruitment occurs
  CminageSeason = c(1,0),
  Fmaxage = 3, # Fully selected fishing mortality age
  Qminage = Qminage, # Qminage = c(0,1) minimum age in surveys
  Qmaxage = Qmaxage, #Qmaxage = c(1,3)
  Fbarage = c(1,2),
  isFseason = c(1,0), # Seasons to calculate fishing in
  effort = sandeel_1r$effort,
  powers = powers,
  blocks = c(1989, 1999 ), # Blocks with unique selectivity
  endFseason = 2, # which season does fishing stop in the final year of data
  nocatch = as.matrix(sandeel_1r$nocatch),
  surveyStart = surveyStart, #c(0.75,0)
  surveyEnd =  surveyEnd, # c(1,0) Does the survey last throughout the season it's conducted?
  surveySeason = surveySeason, # c(2,1)Which seasons do the surveys occur in
  surveyCV =  surveyCV, #c(1,2)),
  catchCV = list(c(0,1,3),
                 c(0,1,3)),
  recmodel = 2, # Chose recruitment model (2 = estimated, hockey)
  estCV = c(0,2,0), # Estimate
  beta = 110000, # Hockey stick plateau
  nllfactor = c(1,1,.05) # Factor for relative strength of log-likelihood (survey , catch, SR )

)



# Get initial parameter structure
parms <- getParms(df.tmb)

# Get non-estimated parameters, based on info in df.tmb
mps <- getMPS(df.tmb, parms)

# Set boundaries
# This model works best if SDsurvey is mapped
#parms[names(parms) == 'SDsurvey'][[1]] <- c(0.5, 0.3, 0.3,.3)

# Try initial parameters in weird spots
sas <- runAssessment(df.tmb, parms = parms, mps = mps)
#
sas$reps


p1 <- smsPlots(df.tmb = df.tmb,sas)

# See Mohns rho
mr <- mohns_rho(df.tmb,parms, peels = 5, plotfigure = TRUE)
print(mr$mohns)
# Show some other functions
p.diag <- plotDiagnostics(df.tmb, sas, mr = mr)


require(ggplot2)

# Get SSB
SSB <- getSSB(df.tmb, sas)

# Recruitment
R <- getR(df.tmb, sas)

# N
N <- getN(df.tmb, sas)

ggplot(N, aes(x = years, y = N, color = factor(season), fill = factor(season)))+
  geom_line()+facet_wrap(~ages, scales = 'free_y')+
  geom_ribbon(aes(ymin = low, ymax = high), alpha = .2, linetype = 0)+
  theme_classic()
#

# Catch
C <- getCatch(df.tmb, sas)

# Catch numbers
CN <- getCatchN(df.tmb, sas)

# Get all derived variables
sms.summary <- getSummary(df.tmb, sas)



# Get Fishing mortality
F0 <- getF(df.tmb, sas)
Fbar <- getFbar(df.tmb, sas)


# Get the CVs
surveyCV <- getSurveyCV(df.tmb,sas)

catchCV <- getCatchCV(df.tmb, sas)


# Residuals

residualCatch <- getResidCatch(df.tmb, sas)

ggplot(residualCatch, aes(x = years, y = ResidCatch, color = factor(ages)))+
  geom_col()+facet_grid(season~ages, scales = 'free_y')+
  theme_classic()+theme(legend.position = 'none')+
  geom_hline(aes(yintercept = 0), linetype = 2)

residualSurvey <- getResidSurvey(df.tmb, sas)

ggplot(residualSurvey, aes(x = years, y = ResidSurvey, color = factor(ages)))+
  geom_col()+facet_grid(survey~ages, scales = 'free_y')+
  theme_classic()+theme(legend.position = 'none')+
  geom_hline(aes(yintercept = 0), linetype = 2)


# Print the AIC of the model
AIC(sas)

# Print the negative log likelihood
sas$opt[['objective']]

# Get estimated parameters
p.est <- getEstimatedParms(sas)
p.est




