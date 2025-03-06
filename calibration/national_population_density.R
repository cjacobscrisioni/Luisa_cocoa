library(plm)
setwd("E:/GeoDMS/ProjDir/Africa/trunk/calibration")
mdata=read.csv2("Africa_Country_PanelData_v4.csv")
plmdata<-pdata.frame(mdata, index=c("CountryId", "Year"))

plmdata$duration<-2
plmdata$duration[Year=2]<-3
plmdata$pdens<-plmdata$Pop / plmdata$Cop_urban
plmdata$GDPperCap<-plmdata$GDP / plmdata$Pop
plmdata$prevdens <- lag(plmdata$pdens, 1)
plmdata$prevPop <- lag(plmdata$Pop, 1)
plmdata$prevGDPperCap <- lag(plmdata$GDPperCap, 1)
plmdata$fdGDP<-(((plmdata$GDP / lag(plmdata$GDP, 1))-1)/plmdata$duration)+1
plmdata$fdGDPperCap<-(((plmdata$GDPperCap / lag(plmdata$GDPperCap, 1))-1)/plmdata$duration)+1
plmdata$fdGDPperCap[is.na(plmdata$fdGDPperCap)]<-1
plmdata$fdPop<-(((plmdata$Pop / lag(plmdata$Pop,1))-1)/plmdata$duration)+1
plmdata$fdDens<-(((plmdata$pdens / lag(plmdata$pdens,1))-1)/plmdata$duration)+1

plmdata$sq_fdPop<-plmdata$fdPop^2
plmdata$sq_fdGDPperCap<-plmdata$fdGDPperCap^2

##fixed effects model
# old model, causes problems with scaling because of prevdens inconsistencies
model<-plm(formula = log(fdDens) ~ log(fdPop) + log(fdGDPperCap) + log(prevdens), data = plmdata, model="within")
# model<-plm(formula = log(fdDens) ~ log(fdPop) + log(fdGDPperCap), data = plmdata, model="within")
summary(model)

##extract country-specific fixed effects
x<-fixef(model)
summary(x)

##separate approach for ERI and ESH (no GDP data available)
erieshmodel<-plm(formula = log(fdDens) ~ log(fdPop) + log(prevdens), data = plmdata, model="within")
summary(erieshmodel)
erieshx<-fixef(erieshmodel)
summary(erieshx)

##test for validity of fixed effects approach
randommodel<-plm(log(fdDens) ~ log(fdPop) + log(fdGDPperCap) + log(prevdens), data=plmdata, model="random")
phtest(model, randommodel)

##breusch-pagan test for heteroskedasticity (no significant heteroskedasticity ie homoskedastic!)
pooledmodel<-plm(log(fdDens) ~ log(fdPop) + log(fdGDPperCap) + log(prevdens), data=plmdata, model="pooling")
print(plmtest(pooledmodel, type = "bp"))

