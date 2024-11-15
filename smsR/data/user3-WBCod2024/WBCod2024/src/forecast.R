library(stockassessment)
load("run/model.RData")
FC<-list()
set.seed(12345)


library(stockassessment)
load("run/model.RData")
FC<-list()
#set.seed(12345)
#FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,1,1,1), ave.years=max(fit$data$years) + ((-3):(-1)), lagR=TRUE, label="Fsq")


#set.seed(12345)
#FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,NA,NA,NA), catchval=c(NA,NA,NA,NA), fval=c(NA,0.896 ,0.896,0.896), 
 #                                   ave.years=max(fit$data$years) + ((-3):(-1)), lagR=TRUE, label="Fsq", year.base=2021, rec.years = max(fit$data$years) + ((-10):(-1)), savesim=TRUE)
                                    
                                    set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,NA, NA, NA), catchval=c(NA,NA,NA,NA), fval=c(NA,0.930,0.930,0.930), 
                                    ave.years=max(fit$data$years) + ((-3):(-1)), lagR=TRUE, label="Fsq - IM year", year.base=2022, rec.years = max(fit$data$years) + ((-10):(-1)), savesim=TRUE)
                                    
#                                                                        set.seed(12345)
#FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,NA, NA, NA), catchval=c(NA,NA,NA,NA), fval=c(NA,0.896, 0.896,0.896), 
#                                    ave.years=max(fit$data$years) + ((-3):(-1)), lagR=TRUE, label="Fsq - IM year-5 years R", year.base=2021, rec.years = max(fit$data$years) + ((-5):(-1)), savesim=TRUE)
                                    
 
#set.seed(12345)
#FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,NA,NA,NA), catchval=c(NA,4953,NA,NA), fval=c(NA, NA,0.537 ,0.537), 
#                                    ave.years=max(fit$data$years) + ((-3):(-1)), lagR=TRUE, label="Fsq base2022", year.base=2022, rec.years = max(fit$data$years) + ((-10):(-1)), savesim=TRUE)
                                     
   
set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,NA,NA,NA), catchval=c(NA,NA,NA,NA), fval=c(NA,0.930, 0.26,0.26), 
                                    ave.years=max(fit$data$years) + ((-3):(-1)), lagR=TRUE, label="Fmsy", year.base=2022, rec.years = max(fit$data$years) + ((-10):(-1)), savesim=TRUE)

set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,NA,NA,NA), catchval=c(NA,NA,NA,NA), fval=c(NA,0.930, 0.17,0.17), 
                                    ave.years=max(fit$data$years) + ((-3):(-1)), lagR=TRUE, label="Lower", year.base=2022, rec.years = max(fit$data$years) + ((-10):(-1)), savesim=TRUE)

set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,NA,NA,NA), catchval=c(NA,NA,NA,NA), fval=c(NA,0.930,0.44,0.44), 
                                    ave.years=max(fit$data$years) + ((-3):(-1)), lagR=TRUE, label="Upper", year.base=2022, rec.years = max(fit$data$years) + ((-10):(-1)), savesim=TRUE)
                                    
                                    
#set.seed(12345)
#FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,NA,NA,NA), catchval=c(NA,NA,NA,NA), fval=c(NA,0.930, 0.064,0.064), 
#                                    ave.years=max(fit$data$years) + ((-3):(-1)), lagR=TRUE, label="Fmsy Ratio SSB23", year.base=2022, rec.years = max(fit$data$years) + ((-5):(-1)), savesim=TRUE)
                                    
                                    set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,NA,NA,NA), catchval=c(NA,NA,NA,NA), fval=c(NA,0.930, 0.162,0.162), 
                                    ave.years=max(fit$data$years) + ((-3):(-1)), lagR=TRUE, label="Fmsy Ratio SSB24", year.base=2022, rec.years = max(fit$data$years) + ((-5):(-1)), savesim=TRUE)
                                    
#set.seed(12345)
#FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,NA,NA,NA), catchval=c(NA,NA,NA,NA), fval=c(NA,0.930, 0.042,0.042), 
#                                    ave.years=max(fit$data$years) + ((-3):(-1)), lagR=TRUE, label="Lower Fmsy Ratio SSB23", year.base=2022, rec.years = max(fit$data$years) + ((-10):(-1)), savesim=TRUE)
                                    
                                    set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,NA,NA,NA), catchval=c(NA,NA,NA,NA), fval=c(NA,0.930, 0.106,0.106), 
                                    ave.years=max(fit$data$years) + ((-3):(-1)), lagR=TRUE, label="Lower Fmsy Ratio SSB24", year.base=2022, rec.years = max(fit$data$years) + ((-10):(-1)), savesim=TRUE)
                                    
#                                    set.seed(12345)
#FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,NA,NA,NA), catchval=c(NA,NA,NA,NA), fval=c(NA,0.896, 0.103,0.103), 
#                                    ave.years=max(fit$data$years) + ((-3):(-1)), lagR=TRUE, label="Fmsy Ratio- R-5yr", year.base=2021, rec.years = max(fit$data$years) + ((-5):(-1)), savesim=TRUE)
                                    
#                                    set.seed(12345)
#FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,NA,NA,NA), catchval=c(NA,NA,NA,NA), fval=c(NA,0.896, 0.103,0.103), 
#                                    ave.years=max(fit$data$years) + ((-3):(-1)), lagR=TRUE, label="Fmsy Ratio- R-3yr", year.base=2021, rec.years = max(fit$data$years) + ((-3):(-1)), savesim=TRUE)
                                    
#set.seed(12345)
#FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,NA,NA,NA), catchval=c(NA,NA,NA,NA), fval=c(NA,0.896,0.067,0.067), 
#                                    ave.years=max(fit$data$years) + ((-3):(-1)), lagR=TRUE, label="Lower Ratio", year.base=2021, rec.years = max(fit$data$years) + ((-10):(-1)), savesim=TRUE)
                                    
set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,NA,NA,NA), catchval=c(NA,NA,NA,NA), fval=c(NA,0.930,0.274,0.274), 
                                    ave.years=max(fit$data$years) + ((-3):(-1)), lagR=TRUE, label="Upper Ratio", year.base=2022, rec.years = max(fit$data$years) + ((-10):(-1)), savesim=TRUE)

set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,NA,NA,NA), catchval=c(NA,NA,NA,NA), fval=c(NA,0.930,0.689,0.26), 
                                    ave.years=max(fit$data$years) + ((-3):(-1)), lagR=TRUE, label="F=Fpa", year.base=2022, rec.years = max(fit$data$years) + ((-10):(-1)), savesim=TRUE)

set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,NA,NA,NA), catchval=c(NA,NA,NA,NA), fval=c(NA,0.93, 1.23,0.26), 
                                    ave.years=max(fit$data$years) + ((-3):(-1)), lagR=TRUE, label="F=Flim", year.base=2022, rec.years = max(fit$data$years) + ((-10):(-1)), savesim=TRUE)

set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,NA,NA,NA), catchval=c(NA,NA,NA,NA), fval=c(NA,0.930,0.000001,0.000001), 
                                    ave.years=max(fit$data$years) + ((-3):(-1)), lagR=TRUE, label="Fzero", year.base=2022, rec.years = max(fit$data$years) + ((-10):(-1)), savesim=TRUE)

                 
set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,NA,NA,NA), catchval=c(NA,NA,NA,NA), fval=c(NA,0.93,0.7176,0.26), 
                                   ave.years=max(fit$data$years) + ((-3):(-1)), lagR=TRUE, label="SSB=Bpa", year.base=2022, rec.years = max(fit$data$years) + ((-10):(-1)), savesim=TRUE)
                                    
set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,NA,NA,NA), catchval=c(NA,NA,NA,NA), fval=c(NA,0.93,1.8437,0.26), 
                                    ave.years=max(fit$data$years) + ((-3):(-1)), lagR=TRUE, label="SSB=Blim", year.base=2022, rec.years = max(fit$data$years) + ((-10):(-1)), savesim=TRUE)
                                    
                                    
#set.seed(12345)
#FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,NA,NA,NA), catchval=c(NA, NA,494, NA), fval=c(NA,0.896, NA,0.26),
#                                    ave.years=max(fit$data$years) + ((-3):(-1)), lagR=TRUE, label="F=F_R", year.base=2021, rec.years = max(fit$data$years) + ((-10):(-1)), savesim=TRUE)
                                    
                                    set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,NA,NA,NA), catchval=c(NA, NA,788, NA), fval=c(NA,0.930, NA,0.26),
                                    ave.years=max(fit$data$years) + ((-3):(-1)), lagR=TRUE, label="TAC=2023+R", year.base=2022, rec.years = max(fit$data$years) + ((-10):(-1)), savesim=TRUE)
                                    
                                                                        set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,NA,NA,NA), catchval=c(NA, NA,788, NA), fval=c(NA,0.930, NA,0.26),
                                    ave.years=max(fit$data$years) + ((-3):(-1)), lagR=TRUE, label="TAC consterain", year.base=2022, rec.years = max(fit$data$years) + ((-10):(-1)), savesim=TRUE)
                                    
#                                                                        set.seed(12345)
#FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,NA,NA,NA), catchval=c(NA, NA,698, NA), fval=c(NA,0.896, NA,0.26),
#                                    ave.years=max(fit$data$years) + ((-3):(-1)), lagR=TRUE, label="advice=2022", year.base=2021, rec.years = max(fit$data$years) + ((-10):(-1)), savesim=TRUE)
                                    
#set.seed(12345)
#FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,NA,NA,NA), catchval=c(NA,NA,NA,NA), fval=c(NA,0.896, 0.26,0.26), 
#                                    ave.years=max(fit$data$years) + ((-3):(-1)), lagR=TRUE, label="Fmsy", year.base=2021, rec.years = max(fit$data$years) + ((-10):(-1)), savesim=TRUE)
                                    
#       set.seed(12345)
#FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,NA,NA,NA), catchval=c(NA,NA,NA,NA), fval=c(NA,0.896, 0.103,0.103), 
#                                    ave.years=max(fit$data$years) + ((-3):(-1)), lagR=TRUE, label="Fmsy Ratio", year.base=2021, rec.years = max(fit$data$years) + ((-10):(-1)), savesim=TRUE)



Blim=15067
above<-function(x){xx<-mean(x[[4]]$ssb>Blim);yr<-x[[4]]$year; round(xx*100,1)}
Pabove<-lapply(FC, above)
names(Pabove)<-lapply(FC, function(x){attr(x,"label")})
Pabove<-as.matrix(unlist(Pabove))
colnames(Pabove)<-paste0("P(SSB",FC[[1]][[4]]$year, " > ",Blim,") in %")


save(FC, Pabove, file="run/forecast.RData")



