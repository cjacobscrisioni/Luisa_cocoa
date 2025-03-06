##need broom methods to store cleaned results
##install broom from CRAN
library(broom)
library(prediction)
library(stargazer)

dirname<-"F:/LocalData/trunk/"
##dirname<-"Z:/LUISA_modeling/Factory/2018_LUISA_AFR_calibration/"
#pressdirname<-"F:/ProjDir/Africa/trunk/parameters/PopulationPressure/"
landusedirname<-"F:/ProjDir/Africa_cocoa/trunk/parameters/ModelAlloc/"
#redistribdirname<-"F:/ProjDir/Africa/trunk/parameters/PopulationRedistribution/"

##countries<-c("BEN", "RWA")
countries<-c("GHA")

#ghana<-c("GHA", "BEN")

#countries<-c("SDNSSD")


	## tests with population pressure and distribution functions
	mdata = read.csv("F:/LocalData/trunk/GHA/calibfile/calibfile.csv", sep=";")
    

	##tP90<- sum(mdata$GHS_Pop_90)
	##tP00<- sum(mdata$GHS_Pop_00)
	##tP15<- sum(mdata$GHS_Pop_15)

	mdata$rP90 <- mdata$GHS_Pop_90 / mean(mdata$GHS_Pop_90)
	mdata$rP00 <- mdata$GHS_Pop_00 / mean(mdata$GHS_Pop_00)
	mdata$rP15 <- mdata$GHS_Pop_15 / mean(mdata$GHS_Pop_15)
	mdata$relpotaccess <- mdata$relpotaccess_2000
	
	mdata$y<-log(mdata$GHS_Pop_15 + 0.001) - log(mdata$GHS_Pop_00 + 0.001)
	mdata$populated<-ifelse(mdata$GHS_Pop_15 > 0 & mdata$GHS_Pop_00 == 0, 1, 0)
	
	mdata$ln_Q<-log(mdata$GHS_Pop_00 + 0.01)

	mdata$rpa<-mdata$relpotaccess
	mdata$rpa2<-mdata$relpotaccess^2

	##mdata$rwP90 <- mdata$w_GHS_Pop_90 / mean(mdata$GHS_Pop_90)
	##mdata$dP <- mdata$rP15 - mdata$rP90
	mdata$dP <- mdata$GHS_Pop_15 - mdata$GHS_Pop_00
	mdata$dGHS_BU <- mdata$GHS_BU_14 - mdata$GHS_BU_00	
	
	mdata$Urban <- mdata$BU14_Disc
	
	##binning with absolute values
	mdata$Constant <- 1	
	mdata$w_bu_val <- mdata$w_GHS_BU_00	
  
	mdata$permanentcrop<-mdata$LandUse==2
	mdata$forest<-mdata$LandUse==3
	
	pcmod<-glm(permanentcrop ~ log_cocoa_climate_suit+rough+static_inv_TT_Cities, family=binomial(link='logit'), data=mdata)
	fmod<-glm(forest ~ log_cocoa_climate_suit+rough+static_inv_TT_Cities, family=binomial(link='logit'), data=mdata)
	
	##binned model

	##umod <- lm(dGHS_BU ~ s_hat + RelRuggedness + w_RelRuggedness + w_GHS_BU_00 +
	## inv_TT_Cities + w_inv_TT_Cities + relpotaccess  + GHS_Pop_00 + w_GHS_Pop_00, 
	##data=mdata)
	##summary(umod)

	#umod <- lm(dGHS_BU ~ ln_Q + rpa + rpa2 + w_bu_val + inv_TT_Cities, data=mdata)
	#summary(umod)
  
	
		
	##umod <- glm(Urban ~ s_hat + RelRuggedness + w_start_bu + GHS_dBU_90_14 +
	## inv_TT_Cities + relpotaccess_1990 + GHS_Pop_90, 
	## family=binomial(link='logit'), data=mdata, subset = BU90_Disc == 0)
	## print(umod)

	## rmod <- lm(dP ~ 0 + (Constant : cQi_) + (relpotaccess : cQi_) + (GHS_Pop_00 : cQi_) + (w_GHS_Pop_00 : cQi_) + (Urban : cQi_) + (inv_TT_Cities : cQi_), data = mdata)
	## rmod <- lm(dP ~ 0 + relpotaccess + GHS_Pop_00 + w_GHS_Pop_00 + Urban + inv_TT_Cities + RelRuggedness, data = mdata)

	

	
	stargazer(smod, umod, rmod, rnmod, popmod, type="html", star.cutoffs=c(0.05, 0.01), out=paste(dirname, countrycode, ".doc"), digits=3, single.row=T)




