library(stockassessment)

oldwd<-setwd("data")

catch.no<-read.ices('cn.dat')/1000
catch.mean.weight<-read.ices('cw.dat')
dis.mean.weight<-read.ices('dw.dat')
land.mean.weight<-read.ices('lw.dat')
stock.mean.weight<-read.ices('sw.dat')
prop.mature<-read.ices('mo.dat')
natural.mortality<-read.ices('nm.dat')
surveys<-read.ices('survey.dat')
land.frac<-read.ices('lf.dat')
prop.f<-read.ices('pf.dat')
prop.m<-read.ices('pm.dat')

land.no<-land.frac*catch.no
dis.no<-(1-land.frac)*catch.no
setwd('..')

# Modify to 7+ data
cutage<-7
low<-1
GE<-which(as.numeric(colnames(catch.no))>=cutage)
E<-which(as.numeric(colnames(catch.no))==cutage)
w<-catch.no[,GE]/rowSums(catch.no[,GE])
wex<-w #rbind(w,w[nrow(w),])
wD<-dis.no[,GE]/rowSums(dis.no[,GE])
wL<-land.no[,GE]/rowSums(land.no[,GE])
catch.no[,E]<-rowSums(catch.no[,GE])
catch.no<-catch.no[,low:E]
prop.mature[,E]<-rowSums(prop.mature[,GE]*wex)
prop.mature<-prop.mature[,low:E]
stock.mean.weight[,E]<-rowSums(stock.mean.weight[,GE]*wex)
stock.mean.weight<-stock.mean.weight[,low:E]
catch.mean.weight[,E]<-rowSums(catch.mean.weight[,GE]*w)
catch.mean.weight<-catch.mean.weight[,low:E]
dis.mean.weight[,E]<-rowSums(dis.mean.weight[,GE]*w)
dis.mean.weight<-dis.mean.weight[,low:E]
land.mean.weight[,E]<-rowSums(land.mean.weight[,GE]*w)
land.mean.weight<-land.mean.weight[,low:E]
natural.mortality[,E]<-rowSums(natural.mortality[,GE]*wex)
natural.mortality<-natural.mortality[,low:E]
land.no[,E]<-rowSums(land.no[,GE])
land.no<-land.no[,low:E]
land.frac<-ifelse(catch.no>0,land.no/catch.no,1)
prop.f<-prop.f[,low:E]
prop.m<-prop.m[,low:E]

cutto<-function(x,to=7){
 g<-as.numeric(colnames(x))>to
 ret<-x[,!g]
 ret[,ncol(ret)]<-ret[,ncol(ret)]+rowSums(x[,g,drop=FALSE])
 attr(ret,"time")<-attr(x,"time")
 ret
}

surveys<-lapply(surveys,cutto)
surveys[[1]]<-surveys[[1]]/mean(surveys[[1]], na.rm=TRUE)
surveys[[2]]<-surveys[[2]]/mean(surveys[[2]], na.rm=TRUE)



  cn<-catch.no
  cw<-catch.mean.weight
  dw<-dis.mean.weight
  lw<-land.mean.weight
  mo<-prop.mature
  nm<-natural.mortality
  pf<-prop.f
  pm<-prop.m
  sw<-stock.mean.weight
  lf<-land.frac
  surveys<-surveys

setwd(oldwd)

dat<-setup.sam.data(surveys=surveys,
                    residual.fleet=cn, 
                    prop.mature=mo, 
                    stock.mean.weight=sw, 
                    catch.mean.weight=cw, 
                    dis.mean.weight=dw, 
                    land.mean.weight=lw,
                    prop.f=pf, 
                    prop.m=pm, 
                    natural.mortality=nm, 
                    land.frac=lf)


save(dat, file="run/data.RData")