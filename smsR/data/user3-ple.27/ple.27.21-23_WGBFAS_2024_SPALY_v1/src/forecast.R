library(stockassessment)
load("run/model.RData")
FC<-list()
DataYear <- as.numeric(format(Sys.time(), "%Y"))-1
nruns <- 50000

#for exploratory runs, reduce the nbr of simulations. For final results, increase the nbr to hit closest to the biomass and Fbar)

set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,1,1,1), label="SQ all years", ave.years=DataYear, rec.years=1999:DataYear, year.base=DataYear, splitLD=TRUE, nosim=nruns)  

set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,1,NA,NA), fval=c(NA,NA,0.31,0.31), label="Fmsy", ave.years=DataYear, rec.years=1999:DataYear, year.base=DataYear, splitLD=TRUE, nosim=50000)

set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,1,NA,NA), fval=c(NA,NA,0.000001,0.000001), label="F=0", ave.years=DataYear, rec.years=1999:DataYear, year.base=DataYear, splitLD=TRUE,nosim=nruns)

set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,1,1,1), fval=c(NA,NA,NA,NA), label=paste0("F=F",DataYear), ave.years=DataYear, rec.years=1999:DataYear, year.base=DataYear, splitLD=TRUE,nosim=nruns)

set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit,
                               fscale=c(1,1,NA,NA),
                               fval=c(NA,NA,0.8088,0.8088),
                               label="Fpa=Fp05",
                               ave.years=DataYear,
                               rec.years=1999:DataYear,
                               year.base=DataYear,
                               splitLD=TRUE,
                               nosim=nruns)
set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,1,NA,NA), fval=c(NA,NA,1.00,1.00), label="Flim", ave.years=DataYear, rec.years=1999:DataYear, year.base=DataYear, splitLD=TRUE,nosim=nruns)

set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,1,NA,NA), nextssb=c(NA,NA,4730,4730), label=paste0("SSB(",DataYear+3,")=MSYBtrigger"), ave.years=DataYear, rec.years=1999:DataYear, year.base=DataYear,nosim=nruns, splitLD=TRUE)

set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,1,NA,NA), nextssb=c(NA,NA,4730,4730), label=paste0("SSB(",DataYear+3,")=Bpa"), ave.years=DataYear,rec.years=1999:DataYear, year.base=DataYear,nosim=nruns, splitLD=TRUE)

set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,1,NA,NA), nextssb=c(NA,NA,3635,3635), label=paste0("SSB(",DataYear+3,")=Blim"), ave.years=DataYear, rec.years=1999:DataYear, year.base=DataYear,nosim=nruns, splitLD=TRUE)

set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit,
                               fscale=c(1,1,NA,NA),
                               nextssb=c(NA,NA,
                                         attr(FC[[2]], "tab")[rownames(attr(FC[[2]], "tab")) == (as.character(as.numeric(format(Sys.Date(), "%Y"))+1)), "ssb:median"],
                                         attr(FC[[2]], "tab")[rownames(attr(FC[[2]], "tab")) == (as.character(as.numeric(format(Sys.Date(), "%Y"))+1)), "ssb:median"]),
                               label=paste0("SSB(",DataYear+3,")= SSB(", DataYear+2,")"),
                               ave.years=DataYear,
                               rec.years=1999:DataYear,
                               year.base=DataYear,
                               nosim=nruns,
                               splitLD=TRUE)

set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,1,NA,NA), fval=c(NA,NA,0.18,0.18), label="Fmsy (lower)", ave.years=DataYear, rec.years=1999:DataYear, year.base=DataYear, splitLD=TRUE,nosim=nruns)

set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,1,NA,NA), fval=c(NA,NA,0.59,0.59), label="Fmsy (upper)", ave.years=DataYear, rec.years=1999:DataYear, year.base=DataYear, splitLD=TRUE,nosim=nruns)

set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit,
                               fscale=c(1,1,NA,NA),
                               catchval=c(NA,NA,(17254*1.2),(17254*1.2)),
                               label=paste0("Catch(", DataYear+2, ")<=20% Advice(", DataYear+1, ")"),
                               ave.years=DataYear,
                               rec.years=1999:DataYear,
                               year.base=DataYear,
                               splitLD=TRUE,
                               nosim=nruns)
                               

save(FC, file="run/forecast.RData")
