# Run the Wester baltic cod as an sms assessment using the sam input data
library(smsRdevel) # Development version of smsR
library(TMB)
library(tidyverse)

wd <- "C:/Users/nsja/Documents/Github/SOS-FISK/"
wd.dat <- "C:/Users/nsja/Documents/Github/SOS-FISK/data/user3-ple.27/ple.27.21-23_WGBFAS_2024_SPALY_v1/data/"


#TMB::compile(file.path(wd,'src/smsRdevel.cpp')) # I am using a custom function based on smsR


dat.sam <- read.sam.data(wd.dat)

# Remove 2024 here (has no catch data )
# dat.sam$Surveyobs <- dat.sam$Surveyobs[,1:39,]
load(file.path(wd, 'data/user3-ple.27/ple.27.21-23_WGBFAS_2024_SPALY_v1/run/data.Rdata'))

maxage <- max(as.numeric(colnames(dat$natMor)))
years <- dat$years
#dat <- GoR_data(file.path(wd,'data'), maxage, years) # Read the input data
# Some basic data to run the model
nyear <- length(years)
ages <- 0:maxage
nseason <- 1 # Number of seasons
dat.sam <- reduce_max_age(dat.sam, maxage = maxage)

# Add extra M to 0 years
dat.sam$mtrx$M[1,,] <- 0.5

# get_TMB_parameters prepares a list for the assessment model.

Blim <- 10000 # From SMS




df.tmb <- get_TMB_parameters(
  mtrx = dat.sam$mtrx, # List that contains M, mat, west, weca
  Surveyobs = dat.sam$Surveyobs, # Survey observations (dimensions age, year, quarter, number of surveys)
  Catchobs = dat.sam$Catchobs, # Catch observations  (dimensions age, year, quarter)
  propM = dat.sam$mtrx$propM,
  propF = dat.sam$mtrx$propF,
  years = years, # Years to run
  nseason = nseason, # Number of seasons
  nsurvey = 2,
  ages = ages, # Ages of the species
  Fbarage = c(3,5), # Ages to calculate average fishing mortality (used for Fmsy calc)
  Fminage = 1,
  Fmaxage = 5, # Fully selected fishing mortality age
  Qminage = c(1,1), # minimum age in surveys,
  Qlastage = c(6,6),
  Qmaxage = c(6,6),
#  leavesurveyout = c(1,1,1),
  randomF = 0, # We run Fishing as a random effect
  #effort = Feffort,
  endFseason = 1, # which season does fishing stop in the final year of data
  surveyStart =dat$sampleTimes[2:length(dat$sampleTimes)],
  surveyEnd =  dat$sampleTimes[2:length(dat$sampleTimes)], # Does the survey last throughout the season it's conducted?
  surveyCV =  list(c(1,2,3), c(1,2,3)),
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

ssb <- getSSB(df.tmb, sas)
R <- getR(df.tmb,sas)
M <- getM(df.tmb) %>% filter(age > 0) %>% group_by(years) %>% summarise(M = mean(value))
Fbar <- getFbar(df.tmb, sas)
# Now try with a random walk on M, R and F


df.tmb <- get_TMB_parameters(
  mtrx = dat.sam$mtrx, # List that contains M, mat, west, weca
  Surveyobs = dat.sam$Surveyobs, # Survey observations (dimensions age, year, quarter, number of surveys)
  Catchobs = dat.sam$Catchobs, # Catch observations  (dimensions age, year, quarter)
  propM = dat.sam$mtrx$propM,
  propF = dat.sam$mtrx$propF,
  years = years, # Years to run
  nseason = nseason, # Number of seasons
  nsurvey = 2,
  ages = ages, # Ages of the species
  Fbarage = c(3,5), # Ages to calculate average fishing mortality (used for Fmsy calc)
  Fminage = 1,
  Fmaxage = 5, # Fully selected fishing mortality age
  Qminage = c(1,1), # minimum age in surveys,
  Qlastage = c(6,6),
  Qmaxage = c(6,6),
  randomM = 1,
  randomF = 1,
  randomR = 1,
  #  leavesurveyout = c(1,1,1),
  #effort = Feffort,
  endFseason = 1, # which season does fishing stop in the final year of data
  surveyStart =dat$sampleTimes[2:length(dat$sampleTimes)],
  surveyEnd =  dat$sampleTimes[2:length(dat$sampleTimes)], # Does the survey last throughout the season it's conducted?
  surveyCV =  list(c(1,2,3), c(1,2,3)),
  catchCV = list(c(0,1,3)),
  MCV = list(c(0,2)),
  M_max= 4,
  beta = Blim,
  recmodel = 2, # Chose recruitment model (2 = estimated, hockey stick nll addition)
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
M_Mrand <- getM_var(df.tmb, sas_Mrand) %>% filter(ages > 0) %>% group_by(years) %>% summarise(M = mean(M_var))

M_age <- getM_var(df.tmb, sas_Mrand)
F_Mrand <- getFbar(df.tmb, sas_Mrand)
# See d'del
Fage <- getF(df.tmb, sas_Mrand)
M_age$Fest <- Fage$F0

ggplot(M_age, aes(x = years, y = M_var, color = factor(ages)))+geom_line(show.legend = FALSE)+
  geom_ribbon(aes(ymin = low, ymax = high), alpha = .2, linetype = 0, show.legend = FALSE)+
  facet_wrap(~ages, scales = 'free_y')+geom_line(aes(y  = Fest), color = 'black')+
  theme_classic()


# Compare the outputs with sam
load(file.path(wd, 'data/user3-ple.27/ple.27.21-23_WGBFAS_2024_SPALY_v1/run/model.Rdata'))
SSB.sam <-ssbtable(fit)
R.sam <- rectable(fit)
M.sam <- rowMeans(dat$natMor) # Take from the other model
F.sam <- fbartable(fit)



df.out <- data.frame(SSB_all = c(ssb$SSB[1:df.tmb$nyears], ssb_Mrand$SSB[1:df.tmb$nyears], SSB.sam[,1]*1000),
                     R_all = c(R$R/mean(R$R), R_Mrand$R/mean(R_Mrand$R), R.sam[,1]/mean(R.sam[,1])),
                     M_all = c(M$M[1:df.tmb$nyears], M_Mrand$M, M.sam),
                     F_all = c(Fbar$Fbar, F_Mrand$Fbar, F.sam[,1]),
                     model = rep(c('const','rand','SAM'), each = df.tmb$nyears),
                     years = rep(df.tmb$years, 3)) %>%
  pivot_longer(1:4, values_to = 'value', names_to = 'state')


p1 <- ggplot(df.out, aes(x = years, y= value, color = model))+geom_line()+
  facet_wrap(~state, scales = 'free_y')+theme_classic()

p2 <-ggplot(M_age %>% filter(ages > 0), aes(x = years, y = M_var, color = factor(ages)))+geom_line(show.legend = FALSE)+
  geom_ribbon(aes(ymin = low, ymax = high), alpha = .2, linetype = 0, show.legend = FALSE)+
  facet_wrap(~ages, scales = 'free_y')+geom_line(aes(y  = Fest), color = 'black')+
  theme_classic()


p2

ggsave(file.path(wd,'data/user3-ple.27/compare_SSB.png'), p1)
ggsave(file.path(wd,'data/user3-ple.27/compare_M.png'), p2)
# Load the SAM model and compare





