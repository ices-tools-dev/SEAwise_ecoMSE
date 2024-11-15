# Compare wb cod with sam
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
  randomF = 0, # We run Fishing as a random effect
  #effort = Feffort,
  endFseason = 1, # which season does fishing stop in the final year of data
  surveyStart =dat$sampleTimes[2:length(dat$sampleTimes)],
  surveyEnd =  dat$sampleTimes[2:length(dat$sampleTimes)]*1.001, # Does the survey last throughout the season it's conducted?
  surveyCV =  list(c(0,1,2,3,4), c(1,2,3,4), c(0)),
  catchCV = list(c(0,1,3)),
  beta = 16698.53,
  recmodel = 1, # Chose recruitment model (2 = estimated, hockey stick nll addition)
  estCV = c(0,0,0),
  nllfactor = c(1,1,.1) # Factor for relative strength of log-likelihood

)

parms <- getParms(df.tmb) # Standard smsR call to get the estimated parameters
mps <- getMPS(df.tmb,parms) # parameters in 'mps' are mapped, i.e., not estimated. Input value is used.

sas <- runAssessment(df.tmb, parms, mps = mps, silent = TRUE)

R1 <- getR(df.tmb, sas)
plot(sas)
sas$reps

reps <- sas$reps
load(file.path(wd,'data/user3-WBCod2024/WBCod2024/baserun/model.Rdata'))

R <- as.data.frame(stockassessment::rectable(fit))
# Compare the two
R$model <- 'SAM'
R$years <- as.numeric(as.character(rownames(R)))
# R sms

Rx <- getR(df.tmb, sas)
idx <- which(R$years < 2024)

R.sms <- data.frame(Estimate = Rx$R, model = 'SMS', Low = R$Low[idx], High = Rx$high[idx],
                    years = df.tmb$years)
R.tot <- bind_rows(R, R.sms) %>% mutate(unit = 'R')

ssb <- as.data.frame(stockassessment::ssbtable(fit))
# Compare the two
ssb$model <- 'SAM'
ssb$years <- as.numeric(as.character(rownames(R)))
# R sms
ssbx <- exp(reps$value[names(reps$value) == 'logSSB'])
ssblow <- exp(reps$value[names(reps$value) == 'logSSB']-2* reps$sd[names(reps$value) == 'logSSB'])
ssbhigh <- exp(reps$value[names(reps$value) == 'logSSB']+2* reps$sd[names(reps$value) == 'logSSB'])

ssb.sms <- data.frame(Estimate = ssbx[1:df.tmb$nyears],
                      Low = ssblow[1:df.tmb$nyears],
                      High = ssbhigh[1:df.tmb$nyears],
                      model = 'SMS',
                      years = df.tmb$years)

ssb.tot <- bind_rows(ssb, ssb.sms) %>% mutate(unit = 'SSB')

Fbar <- as.data.frame(stockassessment::fbartable(fit)) %>% mutate(model = 'SAM')
Fbar$years <- as.numeric(as.character(rownames(Fbar)))
Fbar_sms <- exp(reps$value[names(reps$value) == "logFavg"])
Fbarlow <- exp(reps$value[names(reps$value) == 'logFavg']-2* reps$sd[names(reps$value) == 'logFavg'])
Fbarhigh <- exp(reps$value[names(reps$value) == 'logFavg']+2* reps$sd[names(reps$value) == 'logFavg'])

Fbar.plot <- data.frame(Estimate = Fbar_sms, Low = Fbarlow, High = Fbarhigh,
                        model = 'SMS', years = df.tmb$years)
Fbar_tot <- bind_rows(Fbar, Fbar.plot ) %>% mutate(unit = 'Fbar')


