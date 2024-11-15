# Run the Wester baltic cod as an sms assessment using the sam input data
library(smsRdevel) # Development version of smsR
library(TMB)
library(tidyverse)

wd <- "C:/Users/nsja/Documents/Github/SOS-FISK/"
wd.dat <- "C:/Users/nsja/Documents/Github/SOS-FISK/data/user3-WBCod2024/WBCod2024/data/"


#TMB::compile(file.path(wd,'src/smsRdevel.cpp')) # I am using a custom function based on smsR


dat.sam <- read.sam.data(wd.dat)


# Remove 2024 here (has no catch data )
dat.sam$Surveyobs <- dat.sam$Surveyobs[,1:39,]
sam.dat <- load(file.path(wd, 'data/user3-WBCod2024/WBCod2024/run/data.Rdata'))
maxage <- max(as.numeric(colnames(dat$natMor)))
years <- 1985:2023
#dat <- GoR_data(file.path(wd,'data'), maxage, years) # Read the input data
# Some basic data to run the model
nyear <- length(years)
ages <- 0:maxage
nseason <- 1 # Number of seasons

# get_TMB_parameters prepares a list for the assessment model.

Blim <- 16710.16 # From SMS

df.tmb <- get_TMB_parameters(
  mtrx = dat.sam$mtrx, # List that contains M, mat, west, weca
  Surveyobs = dat.sam$Surveyobs, # Survey observations (dimensions age, year, quarter, number of surveys)
  Catchobs = dat.sam$Catchobs, # Catch observations  (dimensions age, year, quarter)
  propM = dat.sam$mtrx$propM,
  propF = dat.sam$mtrx$propF,
  years = years, # Years to run
  #endYear = 2020,
  nseason = nseason, # Number of seasons
  nsurvey = 3,
  ages = ages, # Ages of the species
  Fbarage = c(3,5), # Ages to calculate average fishing mortality (used for Fmsy calc)
  Fminage = 1,
  Fmaxage = 4, # Fully selected fishing mortality age
  Qminage = c(0,1,0), # minimum age in surveys,
  Qlastage = c(4,4,0),
  Qmaxage = c(4,4,0),
  leavesurveyout = c(1,1,1),
  randomF = 0, # We run Fishing as a random effect
  #effort = Feffort,
  endFseason = 1, # which season does fishing stop in the final year of data
  surveyStart =dat$sampleTimes[2:length(dat$sampleTimes)],
  surveyEnd =  dat$sampleTimes[2:length(dat$sampleTimes)], # Does the survey last throughout the season it's conducted?
  surveyCV =  list(c(0,1,2,3,4), c(1,2,3,4), c(0)),
  catchCV = list(c(0,1,3)),
  beta = Blim,
  recmodel = 1, # Chose recruitment model (2 = estimated, hockey stick nll addition)
  estCV = c(0,0,0),
  nllfactor = c(1,1,.1) # Factor for relative strength of log-likelihood

)

parms <- getParms(df.tmb) # Standard smsR call to get the estimated parameters
mps <- getMPS(df.tmb,parms) # parameters in 'mps' are mapped, i.e., not estimated. Input value is used.

sas <- runAssessment(df.tmb, parms, mps = mps, silent = TRUE)

plot(sas)
sas$reps


#MR <- mohns_rho(df.tmb, parms = parms, useSSBprojection = FALSE)


ssb <- getSSB(df.tmb, sas)
R <- getR(df.tmb,sas)
M <- getM(df.tmb) %>% group_by(years) %>% summarise(M = mean(value))
# Now try with a random walk on M, R and F


df.tmb <- get_TMB_parameters(
  mtrx = dat.sam$mtrx, # List that contains M, mat, west, weca
  Surveyobs = dat.sam$Surveyobs, # Survey observations (dimensions age, year, quarter, number of surveys)
  Catchobs = dat.sam$Catchobs, # Catch observations  (dimensions age, year, quarter)
  propM = dat.sam$mtrx$propM,
  propF = dat.sam$mtrx$propF,
  years = years, # Years to run
  nseason = nseason, # Number of seasons
  nsurvey = 3,
  ages = ages, # Ages of the species
  Fbarage = c(3,5), # Ages to calculate average fishing mortality (used for Fmsy calc)
  Fminage = 1,
  Fmaxage = 4, # Fully selected fishing mortality age
  Qminage = c(0,1,0), # minimum age in surveys,
  Qlastage = c(4,4,0),
  Qmaxage = c(4,4,0),
  leavesurveyout = c(1,1,1),
  randomF = 1, # We run Fishing as a random effect
  randomM = 1, # Estimate M as a random effect,
  randomR = 1,
  Mprior = 10,
  #effort = Feffort,
  endFseason =  1, # which season does fishing stop in the final year of data
  surveyStart =dat$sampleTimes[2:length(dat$sampleTimes)],
  surveyEnd =  dat$sampleTimes[2:length(dat$sampleTimes)], # Does the survey last throughout the season it's conducted?
  surveyCV =  list(c(0,1,2,3,4), c(1,2,3,4), c(0)),
  catchCV = list(c(0,1,3)),
  MCV = list(c(0,2,4)),
  #Mprior = 0.01,
  M_max = 4,
  beta = Blim,
  recmodel = 2, # Chose recruitment model (1 = estimated, 2 = deviations from mean, 3 Beverton holt with steepness
  estCV = c(0,0,0),
  nllfactor = c(1,1,1) # Factor for relative strength of log-likelihood

)

# Parameters needed to be estimated
parms <- getParms(df.tmb) # Standard smsR call to get the estimated parameters
mps <- getMPS(df.tmb, parms)

mps$logSDM <- factor(parms$logSDM*NA) # Need to fix this. Maybe revisit?


sas_Mrand <- runAssessment(df.tmb, parms, mps = mps, silent =TRUE)
#MR <- mohns_rho(df.tmb, parms = parms, mps = mps)


plot(sas_Mrand)
ssb_Mrand <- getSSB(df.tmb, sas_Mrand)
R_Mrand <- getR(df.tmb,sas_Mrand)
M_Mrand <- getM_var(df.tmb, sas_Mrand) %>% group_by(years) %>% summarise(M = mean(M_var))

M_age <- getM_var(df.tmb, sas_Mrand)
# See d'del
Fage <- getF(df.tmb, sas_Mrand)
M_age$Fest <- Fage$F0

ggplot(M_age, aes(x = years, y = M_var, color = factor(ages)))+geom_line()+
  geom_ribbon(aes(ymin = low, ymax = high), alpha = .2)+
  facet_wrap(~ages, scales = )+geom_line(aes(y  = Fest), color = 'black')
# Now try with a dummy variable for some unknown predator
preds <- read.csv(file.path(wd,'data/Predators.csv'), sep = ';')

pred.matrix <- matrix(NA, nrow = 2, ncol = df.tmb$nyears)

corm <- preds %>% filter(Species == 'Denmark', year %in% df.tmb$year)
seal <- preds %>% filter(Species == 'Harbour seal', year %in% df.tmb$year) %>% group_by(year) %>% summarise(Abundance = sum(Abundance))

pred.matrix[1,] <- corm$Abundance

pred.matrix[2,1:nrow(seal)] <- seal$Abundance
pred.matrix[2,(nrow(seal)+1):df.tmb$nyears] <- seal$Abundance[nrow(seal)]



pred.matrix[1,] <- pred.matrix[1,]/mean(pred.matrix[1,])-1
pred.matrix[2,] <- pred.matrix[2,]/mean(pred.matrix[2,])-1
# Now try with a random walk on M, R and F
df.tmb <- get_TMB_parameters(
  mtrx = dat.sam$mtrx, # List that contains M, mat, west, weca
  Surveyobs = dat.sam$Surveyobs, # Survey observations (dimensions age, year, quarter, number of surveys)
  Catchobs = dat.sam$Catchobs, # Catch observations  (dimensions age, year, quarter)
  propM = dat.sam$mtrx$propM,
  propF = dat.sam$mtrx$propF,
  years = years, # Years to run
  nseason = nseason, # Number of seasons
  nsurvey = 3,
  ages = ages, # Ages of the species
  Fbarage = c(3,5), # Ages to calculate average fishing mortality (used for Fmsy calc)
  Fminage = 1,
  Fmaxage = 4, # Fully selected fishing mortality age
  Qminage = c(0,1,0), # minimum age in surveys,
  Qlastage = c(4,4,0),
  Qmaxage = c(4,4,0),
  leavesurveyout = c(1,1,1),
  randomF = 1, # We run Fishing as a random effect
  randomM = 1, # Estimate M as a random effect,
  randomR = 1, # R as random effect
  Mprior = 10,
  Pred_in = pred.matrix,#pred.matrix,
  #effort = Feffort,
  endFseason = 1, # which season does fishing stop in the final year of data
  surveyStart =dat$sampleTimes[2:length(dat$sampleTimes)],
  surveyEnd =  dat$sampleTimes[2:length(dat$sampleTimes)], # Does the survey last throughout the season it's conducted?
  surveyCV =  list(c(0,1,2,3,4), c(1,2,3,4), c(0)),
  catchCV = list(c(0,1,3)),
  MCV = list(c(0, 2, 4), c(0,2,4)),
  beta = Blim,
  recmodel = 2, # Chose recruitment model (1 = estimated, 2 = deviations from mean, 3 Beverton holt with steepness
  estCV = c(0,0,0),
  nllfactor = c(1,1,1) # Factor for relative strength of log-likelihood

)

df.tmb$M_matrix <- pred.matrix
#  mean(matrix(seq(minpred, maxpred, length.out = df.tmb$nyears), nrow = df.tmb$nalphaM))
parms <- getParms(df.tmb)
mps <- getMPS(df.tmb, parms)
mps$logSDM <- factor(parms$logSDM * NA)
mps$ext_M <- factor(parms$ext_M * NA) # Fix stochastic part of M
#mps$alphaM <- factor(parms$alphaM * NA) # Fixate strength of predator contribution to M
#parms$alphaM <- .25 # Set the predator contribution to M if mapped

sas_mice <- runAssessment(df.tmb, parms, mps = mps, silent =TRUE)
ssb_mice <- getSSB(df.tmb, sas_mice)
R_mice <- getR(df.tmb,sas_mice)
M_mice <- getM_var(df.tmb, sas_mice) %>% group_by(years) %>% summarise(M = mean(M_var))


head(M_mice)
ggplot(M_mice, aes(x = years, y = M))+geom_line()

Mcorm <- exp(sas_mice$reps$value[names(sas_mice$reps$value) == 'M_tot']) # Extract the predator contribution from the stock assesment


AIC(sas)
AIC(sas_Mrand)
AIC(sas_mice)

# In this case the base model seems to have the lowest AIC

# Compare the SSB, R, Catch and M2

df.out <- data.frame(SSB_all = c(ssb$SSB[1:df.tmb$nyears], ssb_Mrand$SSB[1:df.tmb$nyears], ssb_mice$SSB[1:df.tmb$nyears]),
                     R_all = c(R$R, R_Mrand$R, R_mice$R),
                     M_all = c(M$M[1:df.tmb$nyears], M_Mrand$M, M_mice$M),
                     model = rep(c('const','rand','mice'), each = df.tmb$nyears),
                     years = rep(df.tmb$years, 3)) %>%
  pivot_longer(1:3, values_to = 'value', names_to = 'state')




ggplot(df.out %>% filter(state == 'M_all'), aes(x = years, y = value, color = model))+geom_line()+facet_wrap(~state, scales= 'free_y', nrow =3)


# New mice model




