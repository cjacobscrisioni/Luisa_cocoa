##need broom methods to store cleaned results
##install broom from CRAN
library(broom)
library(prediction)
library(stargazer)

dirname<-"F:/LocalData/trunk/"
##dirname<-"Z:/LUISA_modeling/Factory/2018_LUISA_AFR_calibration/"
pressdirname<-"F:/ProjDir/Africa/trunk/parameters/PopulationPressure/"
landusedirname<-"F:/ProjDir/Africa/trunk/parameters/ModelAlloc/"
redistribdirname<-"F:/ProjDir/Africa/trunk/parameters/PopulationRedistribution/"

##countries<-c("BEN", "RWA")
countries<-c("AGO","BEN","BFA","BWA","CAF","CIV","CMR","COD","COG",
"DJI","DZA","EGY","ERI","ESH","ETH","GAB","GHA","GIN","GNB","GNQ","KEN",
"LBR","LBY","LSO","MAR","MLI","MOZ","MRT","MWI","NAM","NER","NGA","RWA",
"SC1","SENGMB","SLE","SOM","TCD","TGO",
"UGA","ZAF","ZMB","ZWE","BDI","BWA","SWZ","TUN","TZA","SDNSSD") 
#"BDI","BWA","SWZ","TUN","TZA") ##latter: problematic countries
problemcountries<-c("BDI","BWA","SWZ","TUN","TZA","SSD","SDN")
##BDI, SWZ issue with reclassification cQi_ (nill values? more obs in return than in original)
##BWA issue with rbind parsing
##SC2 issue with discrete choice model

##countries<-c("UGA","ZAF","ZMB","ZWE")

#ghana<-c("GHA", "BEN")

#countries<-c("SDNSSD")

for (countrycode in countries) {
	print(countrycode)

	## tests with population pressure and distribution functions
	setwd(paste(dirname, countrycode, "/calibfile", sep=""))
	mdata = read.csv("calibfile.csv", sep=";")

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
	mdata$cQi_[mdata$GHS_Pop_00 < 50]<-0
	mdata$cQi_[mdata$GHS_Pop_00 >= 50 	& mdata$GHS_Pop_00 < 500]<-1
	mdata$cQi_[mdata$GHS_Pop_00 >= 500 	& mdata$GHS_Pop_00 < 2500]<-2
	mdata$cQi_[mdata$GHS_Pop_00 >= 2500 & mdata$GHS_Pop_00 < 5000]<-3
	mdata$cQi_[mdata$GHS_Pop_00 >= 5000 & mdata$GHS_Pop_00 < 15000]<-4
	mdata$cQi_[mdata$GHS_Pop_00 >= 15000]<-5
	mdata$cQi_ <- as.factor(mdata$cQi_)
	mdata$Constant <- 1	
	mdata$w_bu_val <- mdata$w_GHS_BU_00	

	##binned model
	smod <- lm(dP ~ 0 + Constant + rpa + rpa2 + GHS_Pop_00 + w_GHS_Pop_00 + inv_TT_Cities + rel_GHS_dBU_90_00_km120, data = mdata)
	summary(smod)
	mdata$s_hat <- predict(smod, na.action = na.exclude)
	
	##umod <- lm(dGHS_BU ~ s_hat + RelRuggedness + w_RelRuggedness + w_GHS_BU_00 +
	## inv_TT_Cities + w_inv_TT_Cities + relpotaccess  + GHS_Pop_00 + w_GHS_Pop_00, 
	##data=mdata)
	##summary(umod)

	umod <- lm(dGHS_BU ~ ln_Q + rpa + rpa2 + w_bu_val + inv_TT_Cities, data=mdata)
	summary(umod)

	
		
	##umod <- glm(Urban ~ s_hat + RelRuggedness + w_start_bu + GHS_dBU_90_14 +
	## inv_TT_Cities + relpotaccess_1990 + GHS_Pop_90, 
	## family=binomial(link='logit'), data=mdata, subset = BU90_Disc == 0)
	## print(umod)

	## rmod <- lm(dP ~ 0 + (Constant : cQi_) + (relpotaccess : cQi_) + (GHS_Pop_00 : cQi_) + (w_GHS_Pop_00 : cQi_) + (Urban : cQi_) + (inv_TT_Cities : cQi_), data = mdata)
	## rmod <- lm(dP ~ 0 + relpotaccess + GHS_Pop_00 + w_GHS_Pop_00 + Urban + inv_TT_Cities + RelRuggedness, data = mdata)

	
	popmod<-glm(populated~rpa+inv_TT_Cities+w_GHS_Pop_00, data=mdata, family=binomial, subset=(GHS_Pop_00 == 0))
	rmod <- lm(y ~ rpa + rpa2 + GHS_Pop_00 + w_GHS_Pop_00 + inv_TT_Cities + rel_GHS_dBU_90_00_km120, subset=(GHS_Pop_00 > 0 & GHS_Pop_15 > 0 & BU14_Disc), data = mdata)
	rnmod <- lm(y ~ rpa + rpa2 + GHS_Pop_00 + w_GHS_Pop_00 + inv_TT_Cities + rel_GHS_dBU_90_00_km120, subset=(GHS_Pop_00 > 0 & GHS_Pop_15 > 0 & !BU14_Disc), data = mdata)
	
	summary(rmod)
	summary(rnmod)
	summary(popmod)
	
	stargazer(smod, umod, rmod, rnmod, popmod, type="html", star.cutoffs=c(0.05, 0.01), out=paste(dirname, countrycode, ".doc"), digits=3, single.row=T)

	pop_press_frame = manageresults(smod)
	if (exists("press_output")) {
	   pop_press_frame<-alignframes(pop_press_frame, press_output)
	   press_output<-rbind(press_output, pop_press_frame)
	  } else {press_output<-pop_press_frame}
  	write.csv(press_output, paste(pressdirname, "press_coefs.csv", sep=""))
	
	urban_frame = manageresults(umod)
	if (exists("urban_output")) {
	   urban_frame<-alignframes(urban_frame, urban_output)
	   urban_output<-rbind(urban_output, urban_frame)
	  } else {urban_output<-urban_frame}
	write.csv(urban_output, paste(landusedirname, "urban.csv", sep=""))
	
	urb_redistr_frame = manageresults(rmod)
	if (exists("urb_redistr_output")) {
	    urb_redistr_frame <-alignframes(urb_redistr_frame , urb_redistr_output)
	    urb_redistr_output<-rbind(urb_redistr_output, urb_redistr_frame )
	   } else {urb_redistr_output<-urb_redistr_frame }
	write.csv(urb_redistr_output, paste(redistribdirname, "redist_coefs_urb.csv", sep=""))
	
	nurb_redistr_frame = manageresults(rnmod)
	if (exists("nurb_redistr_output")) {
	    nurb_redistr_frame <-alignframes(nurb_redistr_frame , nurb_redistr_output)
	    nurb_redistr_output<-rbind(nurb_redistr_output, nurb_redistr_frame )
	   } else {nurb_redistr_output<-nurb_redistr_frame}
	write.csv(nurb_redistr_output, paste(redistribdirname, "redist_coefs_nurb.csv", sep=""))

	populate_redistr_frame = manageresults(popmod)
	if (exists("populate_redistr_output")) {
	    populate_redistr_frame <-alignframes(populate_redistr_frame , populate_redistr_output)
	    populate_redistr_output<-rbind(populate_redistr_output, populate_redistr_frame )
	   } else {populate_redistr_output<-populate_redistr_frame}
	write.csv(populate_redistr_output, paste(redistribdirname, "redist_coefs_populate.csv", sep=""))
	

}

alignframes<-function(workFrame, refFrame) {
  for (var in 1:NCOL(refFrame)) {
    if (!(colnames(refFrame)[var] %in% colnames(workFrame))) {
      val<-0
      workFrame[[colnames(refFrame)[var]]]<-val
    }
  }
  return(workFrame)
}



manageresults<-function(inframe) {
  
  ##manage coefficients storage
  coefmod<-tidy(inframe)
  coefmod$std.error<-NULL
  coefmod$statistic<-NULL
  coefmod$p.value<-NULL
  
  elements<-strsplit(coefmod$term, ':')
  for (row in 1:NROW(elements)) {
    ##print(row)
    ##print(length(elements[row]))
    if (length(elements[[row]]) == 2) {
      if (grepl("cQi_", elements[[row]][2], fixed=TRUE) == TRUE) {
        print(row)
        tstring<-elements[[row]][2]
        elements[[row]][2]<-elements[[row]][1]
        elements[[row]][1]<-tstring
      }
    }
  }
  
  coefmod$term<-sapply(elements, paste, collapse="_x_")
  
  ##transpose
  outputframe <- as.data.frame(t(coefmod$estimate))
  colnames(outputframe) <- noquote(coefmod$term)
  rownames(outputframe) <- noquote(countrycode)
  outputframe  
}

##different tests

##general model, r2 = 0.682 
##smod <- lm(dP ~ relpotaccess_1990 + rP90 + rwP90 + inv_TT_Cities, data = mdata)
##summary(smod)

##binning with relative values - not so attractive
##mdata$cQi_[which(mdata$rP90 == 0)]<-0
##mdata$cQi_[which(mdata$rP90 > 0 & mdata$rP90 < 0.005)]<-1
##mdata$cQi_[which(mdata$rP90 >= 0.005 & mdata$rP90 < 0.05)]<-2
##mdata$cQi_[which(mdata$rP90 >= 0.05 & mdata$rP90 < 0.5)]<-3
##mdata$cQi_[which(mdata$rP90 >= 0.5)]<-4


##no useful results from factor population changes (in terms of r2 =~ 0.07)
##a$fP<-(mdata$rP15 + 0.01) / (mdata$rP90 + 0.01)
##mdata$ln_fP <- log(mdata$fP)
##smod <- lm(ln_fP ~ relpotaccess_1990 + rP90 + rwP90 + inv_TT_Cities + RelRuggedness, data = mdata)
##summary(smod)
