library(ggplot2)
library(dplyr)
library(tidyr)

drive<-"F:/"

ssg<-FALSE #FALSE # store subgraphs of all randomized variants yes/no
#RandomVersions<-"Reference"
RandomVersions<-c("Reference", paste0('Random_', 1:99))

  #runset<-"SSP2_Cocoa_uncons_bau"
  #runset<-"SSP2_EUDR_0"
runset<-"SSP2_Cocoa_cons_bau"
#runset<-"SSP2_EUDR_0_cons"
#runset<-"SSP2_Cocoa_cons_BAU_protected"
#runset<-"SSP2_BAU_cons_protected_no_nc"
#runset<-"SSP2_EUDR_0_cons_no_nc"

setwd(paste0(drive,"LocalData/luisa_cocoa/GHA/",runset))
#getwd()



counter<-0

# function to transform csv files into format digestable by ggplot()
graphable<-function(inframe, fieldname="km2") {
  outframe<-inframe %>% pivot_longer(!ZoneId, names_to = "Year", values_to = fieldname)
  outframe$ZoneId<-as.factor(outframe$ZoneId)
  outframe$Year<-as.numeric(gsub('y', '', outframe$Year))
  return(outframe)
}

# function to sum column values
col_summing<-function(inframe) {
  outframe<-NULL
  j<-0
  for(i in 1:ncol(inframe)) {
    if(is.numeric(inframe[,i])) {
      j<-j+1
      outframe[j]<-sum(inframe[,i])
      #colnames(outframe)[i]<-colnames(inframe)[i]
    } 
  }
  return(outframe)
}

for (randomval in RandomVersions) {

outputdir<-paste0(drive,"LocalData/luisa_cocoa/GHA/",runset,"/",randomval,"/graphs")

if(!dir.exists(outputdir)) {dir.create(outputdir)}

setwd(paste0(drive,"LocalData/luisa_cocoa/GHA/",runset,"/",randomval))

#--------- make cocoa totals graph
all_cocoa<-read.csv("_total_cocoa.csv")
compliant_cocoa<-read.csv("_total_compliant_cocoa.csv")
noncompliant_cocoa<-read.csv("_total_non_compliant_cocoa.csv")
allyield<-read.csv("_summed_cocoa_yield.csv")
compliantyield<-read.csv("_summed_compliant_cocoa_yield.csv")
noncompliantyield<-read.csv("_summed_noncompliant_cocoa_yield.csv")
forestlost<-read.csv("_summed_forest_loss_by_cocoa.csv")
crops<-read.csv("_total_crops.csv")
forest<-read.csv("_total_mature_forest.csv")

# ------- make overview graph: prepare data
cocoasummed<-col_summing(all_cocoa)
compliantcocoasummed<-col_summing(compliant_cocoa)
noncompliantcocoasummed<-col_summing(noncompliant_cocoa)
forestsummed<-col_summing(forest)
cropsummed<-col_summing(crops)
yieldsummed<-col_summing(allyield)
compliantyieldsummed<-col_summing(compliantyield)
noncompliantyieldsummed<-col_summing(noncompliantyield)
forestlostsummed<-col_summing(forestlost)
years<-c(2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050)
totals<-cbind(years, cocoasummed, compliantcocoasummed, noncompliantcocoasummed, cropsummed, forestsummed, forestlostsummed, yieldsummed, compliantyieldsummed, noncompliantyieldsummed)
totals<-data.frame(totals)

totals$rv<-randomval
if(counter==0) {
  aggset<-totals
} else {
  aggset<-rbind(aggset, totals)
}

counter<-counter+1

if(ssg) { # in case we want to graph all randomized results per region yes/no
  all_cocoalong<-graphable(all_cocoa)
  g0<-ggplot(all_cocoalong, aes(x=Year+5, y=km2, colour=ZoneId)) 
  g0 + geom_line() +
    labs(title = paste("Cocoa plantation area, ", gsub("_", " ", runset)))+xlab("Year")+ylab("Area (km2)")
  ggsave(paste(outputdir, "/all_cocoa_area_",runset,".png",sep=""), width=16, height=10)
  
  
  compliant_cocoalong<-graphable(compliant_cocoa)
  g0<-ggplot(compliant_cocoalong, aes(x=Year+5, y=km2, colour=ZoneId)) 
  g0 + geom_line() +
    labs(title = paste("Compliant cocoa plantation area, ", gsub("_", " ", runset)))+xlab("Year")+ylab("Area (km2)")
  ggsave(paste(outputdir, "/compliant_cocoa_area_",runset,".png",sep=""), width=16, height=10)
  
  noncompliant_cocoalong<-graphable(noncompliant_cocoa)
  g0<-ggplot(noncompliant_cocoalong, aes(x=Year+5, y=km2, colour=ZoneId)) 
  g0 + geom_line() +
    labs(title = paste("Non compliant cocoa plantation area, ", gsub("_", " ", runset)))+xlab("Year")+ylab("Area (km2)")
  ggsave(paste(outputdir, "/non_compliant_cocoa_area_",runset,".png",sep=""), width=16, height=10)
  
  #--------- make yield per ha graphs
  allyieldlong<-graphable(allyield, "Tons")
  allyieldlong$km2<-all_cocoalong$km2
  g1<-ggplot(allyieldlong, aes(x=Year+5, y=Tons / (km2 * 100), colour=ZoneId)) 
  g1 + geom_line() +
    labs(title = paste("Total yield per ha, ", gsub("_", " ", runset)))+xlab("Year")+ylab("Yield (tonnes per ha)")
  ggsave(paste(outputdir, "/all_cocoa_yield_",runset,".png",sep=""), width=16, height=10)
  
  compliantyieldlong<-graphable(compliantyield, "Tons")
  compliantyieldlong$km2<-compliant_cocoalong$km2
  g1<-ggplot(compliantyieldlong, aes(x=Year+5, y=Tons / (km2 * 100), colour=ZoneId)) 
  g1 + geom_line() +
    labs(title = paste("Total compliant yield per ha, ", gsub("_", " ", runset)))+xlab("Year")+ylab("Yield (tonnes per ha)")
  ggsave(paste(outputdir, "/compliant_cocoa_yield_",runset,".png",sep=""), width=16, height=10)
  
  noncompliantyieldlong<-graphable(noncompliantyield, "Tons")
  noncompliantyieldlong$km2<-noncompliant_cocoalong$km2
  g1<-ggplot(noncompliantyieldlong, aes(x=Year+5, y=Tons / (km2 * 100), colour=ZoneId)) 
  g1 + geom_line() +
    labs(title = paste("Total non-compliant yield per ha, ", gsub("_", " ", runset)))+xlab("Year")+ylab("Yield (tonnes per ha)")
  ggsave(paste(outputdir, "/noncompliant_cocoa_yield_",runset,".png",sep=""), width=16, height=10)
  
  # ------- make forest loss graph
  forestlostlong<-graphable(forestlost)
  g3<-ggplot(forestlostlong, aes(x=Year+5, y=km2, colour=ZoneId)) 
  g3 + geom_line() +
    labs(title = paste("Mature forest lost to cocoa, ", gsub("_", " ", runset)))+xlab("Year")+ylab("Forest lost (km2)")
  ggsave(paste(outputdir, "/forest_lost_to_cocoa",runset,".png",sep=""), width=16, height=10)
  
  cropslong<-graphable(crops)
  g3<-ggplot(cropslong, aes(x=Year+5, y=km2, colour=ZoneId)) 
  g3 + geom_line() +
    labs(title = paste("Other crops area, ", gsub("_", " ", runset)))+xlab("Year")+ylab("Crops area (km2)")
  ggsave(paste(outputdir, "/crops_area",runset,".png",sep=""), width=16, height=10)
  
  forestlong<-graphable(forest)
  g3<-ggplot(forestlong, aes(x=Year+5, y=km2, colour=ZoneId)) 
  g3 + geom_line() +
    labs(title = paste("Mature forest area, ", gsub("_", " ", runset)))+xlab("Year")+ylab("Forest area (km2)")
  ggsave(paste(outputdir, "/forest_area",runset,".png",sep=""), width=16, height=10)
  
  # ------ collate results into one csv file
  export<-all_cocoalong
  export$all_cocoa_area<-export$km2
  export$km2<-NULL
  export$compliant_cocoa_area<-compliant_cocoalong$km2
  export$non_compliant_cocoa_area<-noncompliant_cocoalong$km2
  export$all_yield<-allyieldlong$Tons
  export$compliant_yield<-compliantyieldlong$Tons
  export$noncompliant_yield<-noncompliantyieldlong$Tons
  export$crops_area<-cropslong$km2
  export$forest_area<-forestlong$km2
  export$forest_lost_area<-forestlostlong$km2

  write.csv(export, paste0(outputdir, "/all_outputs_", runset, ".csv"))

    # ------- create plot
  #totals_long<-totals %>% pivot_longer(!years, names_to = "Variable", values_to = "km2")
  #ggplot(totals_long, aes(x=years+5, y=km2, colour=Variable))+geom_line()+
  #  labs(title = paste("Total cocoa area Ghana, ", gsub("_", " ", runset)))+xlab("Year")+ylab("Cocoa plantations area (km2)")
  #ggsave(paste(outputdir, "/total_areas",runset,".png",sep=""), width=16, height=10)
}


}

outputdir<-paste0(drive,"LocalData/luisa_cocoa/GHA/graphs")
if(!dir.exists(outputdir)) {dir.create(outputdir)}

aggset$rvf<-factor(aggset$rv)
refvals<-subset(aggset, rv=="Reference")

fp<-ggplot(data=aggset, aes(x=years+5, y=forestsummed / (100*100), colour=rvf, size=rvf))
fp+geom_line(alpha=0.5)+
  xlab("Year") + ylab("Mha forest") + ggtitle(paste0(runset, " scenario"))+
  coord_cartesian(ylim=c(0,12))+
  theme_light() +
  theme(legend.position='none')+
  scale_colour_manual(values=c(rep("gray",99), "black"))+
  scale_size_manual(values=c(rep(0.5,99), 1.5))
ggsave(paste(outputdir, "/area_forest_",runset,".png",sep=""), width=16, height=10)

fp<-ggplot(data=aggset, aes(x=years+5, y=cocoasummed / (100*100), colour=rvf, size=rvf))
fp+geom_line(alpha=0.5)+
  xlab("Year") + ylab("Mha cocoa total") + ggtitle(paste0(runset, " scenario"))+
  coord_cartesian(ylim=c(0,12))+
  theme_light() +
  theme(legend.position='none')+
  scale_colour_manual(values=c(rep("gray",99), "black"))+
  scale_size_manual(values=c(rep(0.5,99), 1.5))
ggsave(paste(outputdir, "/area_cocoa_all_",runset,".png",sep=""), width=16, height=10)

fp<-ggplot(data=aggset, aes(x=years+5, y=compliantcocoasummed / (100*100), colour=rvf, size=rvf))
fp+geom_line(alpha=0.5)+
  xlab("Year") + ylab("Mha compliant cocoa total") + ggtitle(paste0(runset, " scenario"))+
  coord_cartesian(ylim=c(0,12))+
  theme_light() +
  theme(legend.position='none')+
  scale_colour_manual(values=c(rep("gray",99), "black"))+
  scale_size_manual(values=c(rep(0.5,99), 1.5))
ggsave(paste(outputdir, "/area_cocoa__cc_",runset,".png",sep=""), width=16, height=10)   

fp<-ggplot(data=aggset, aes(x=years+5, y=noncompliantcocoasummed / (100*100), colour=rvf, size=rvf))
fp+geom_line(alpha=0.5)+
  xlab("Year") + ylab("Mha non compliant cocoa total") + ggtitle(paste0(runset, " scenario"))+
  coord_cartesian(ylim=c(0,12))+
  theme_light() +
  theme(legend.position='none')+
  scale_colour_manual(values=c(rep("gray",99), "black"))+
  scale_size_manual(values=c(rep(0.5,99), 1.5))
ggsave(paste(outputdir, "/area_cocoa_nc_",runset,".png",sep=""), width=16, height=10)  

fp<-ggplot(data=aggset, aes(x=years+5, y=cropsummed / (100*100), colour=rvf, size=rvf))
fp+geom_line(alpha=0.5)+
  xlab("Year") + ylab("Mha crops total") + ggtitle(paste0(runset, " scenario"))+
  coord_cartesian(ylim=c(2,5))+
  theme_light() +
  theme(legend.position='none')+
  scale_colour_manual(values=c(rep("gray",99), "black"))+
  scale_size_manual(values=c(rep(0.5,99), 1.5))
ggsave(paste(outputdir, "/area_crops_",runset,".png",sep=""), width=16, height=10)

fp<-ggplot(data=aggset, aes(x=years+5, y=yieldsummed * 10/ (cocoasummed), colour=rvf, size=rvf))
# yield * 1000 (tonnes) / area * 100 (hectares) = 1000/100 = 10
fp+geom_line(alpha=0.5)+
  xlab("Year") + ylab("Total cocoa yield per ha") + ggtitle(paste0(runset, " scenario"))+
  coord_cartesian(ylim=c(0,600))+
  theme_light() +
  theme(legend.position='none')+
  scale_colour_manual(values=c(rep("gray",99), "black"))+
  scale_size_manual(values=c(rep(0.5,99), 1.5))
ggsave(paste(outputdir, "/yield_pha_all_",runset,".png",sep=""), width=16, height=10)

fp<-ggplot(data=aggset, aes(x=years+5, y=compliantyieldsummed * 10 / (compliantcocoasummed), colour=rvf, size=rvf))
# yield * 1000 (tonnes) / area * 100 (hectares) = 1000/100 = 10
fp+geom_line(alpha=0.5)+
  xlab("Year") + ylab("Total compliant cocoa yield per ha") + ggtitle(paste0(runset, " scenario"))+
  coord_cartesian(ylim=c(0,600))+
  theme_light() +
  theme(legend.position='none')+
  scale_colour_manual(values=c(rep("gray",99), "black"))+
  scale_size_manual(values=c(rep(0.5,99), 1.5))
ggsave(paste(outputdir, "/yield_pha_cc_",runset,".png",sep=""), width=16, height=10)

fp<-ggplot(data=aggset, aes(x=years+5, y=noncompliantyieldsummed * 10 / (noncompliantcocoasummed), colour=rvf, size=rvf))
# yield * 1000 (tonnes) / area * 100 (hectares) = 1000/100 = 10
fp+geom_line(alpha=0.5)+
  xlab("Year") + ylab("Total noncompliant cocoa yield per ha") + ggtitle(paste0(runset, " scenario"))+
  coord_cartesian(ylim=c(0,600))+
  theme_light() +
  theme(legend.position='none')+
  scale_colour_manual(values=c(rep("gray",99), "black"))+
  scale_size_manual(values=c(rep(0.5,99), 1.5))
ggsave(paste(outputdir, "/yield_pha_nc_",runset,".png",sep=""), width=16, height=10)

fp<-ggplot(data=aggset, aes(x=years+5, y=yieldsummed / (1000 * 1000), colour=rvf, size=rvf))
fp+geom_line(alpha=0.5)+
  xlab("Year") + ylab("Total cocoa output (M)") + ggtitle(paste0(runset, " scenario"))+
  #coord_cartesian(ylim=c(0,1000))+
  theme_light() +
  theme(legend.position='none')+
  scale_colour_manual(values=c(rep("gray",99), "black"))+
  scale_size_manual(values=c(rep(0.5,99), 1.5))
ggsave(paste(outputdir, "/output_all_",runset,".png",sep=""), width=16, height=10)

fp<-ggplot(data=aggset, aes(x=years+5, y=compliantyieldsummed / (1000 * 1000), colour=rvf, size=rvf))
fp+geom_line(alpha=0.5)+
  xlab("Year") + ylab("Total compliant cocoa output (M)") + ggtitle(paste0(runset, " scenario"))+
  #coord_cartesian(ylim=c(0,1000))+
  theme_light() +
  theme(legend.position='none')+
  scale_colour_manual(values=c(rep("gray",99), "black"))+
  scale_size_manual(values=c(rep(0.5,99), 1.5))
ggsave(paste(outputdir, "/output_cc_",runset,".png",sep=""), width=16, height=10)

fp<-ggplot(data=aggset, aes(x=years+5, y=noncompliantyieldsummed / (1000 * 1000), colour=rvf, size=rvf))
fp+geom_line(alpha=0.5)+
  xlab("Year") + ylab("Total non-compliant cocoa output (M)") + ggtitle(paste0(runset, " scenario"))+
  #coord_cartesian(ylim=c(0,1000))+
  theme_light() +
  theme(legend.position='none')+
  scale_colour_manual(values=c(rep("gray",99), "black"))+
  scale_size_manual(values=c(rep(0.5,99), 1.5))
ggsave(paste(outputdir, "/output_nc_",runset,".png",sep=""), width=16, height=10)

refvals$years<-refvals$years+5
write.csv(refvals, paste0(outputdir, "/all_outputs_", runset, ".csv"))
