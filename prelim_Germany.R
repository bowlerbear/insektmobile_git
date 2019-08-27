#inseckMobile:

#note: what i call route_no here is rather the sample no

#sort image data################################################################################

#read in dataset files
setwd("C:/Users/db40fysa/Nextcloud/mobileInsect/13_data/jenaData")
images<-read.delim("stats_images.txt",as.is=T)

#exclude ones with wrong label in them
images$Image_Name<-gsub("_wrong label","",images$Image_Name)

#extract metadata
#habitat
images$habitat <- sapply(images$Image_Name,function(x)strsplit(x,"_")[[1]][1])
images$habitat[which(images$habitat=="GrÃ¼n")] <- "Gruen"
images$habitat[which(images$habitat=="Freucht")] <- "Feucht"
unique(images$habitat)

images$route_no <- as.character(sapply(images$Image_Name,function(x)strsplit(x,"_")[[1]][2]))
images$route_no <- gsub("-17-20","",images$route_no)
images$route_no <- gsub("-12-15","",images$route_no)
images$route_no <- as.numeric(images$route_no)
unique(images$route_no)

images$time <- NA
images$time[grepl("12-15",images$Image_Name)] <- "12-15"
images$time[grepl("12_15",images$Image_Name)] <- "12-15"
images$time[grepl("17-20",images$Image_Name)] <- "17-20"
images$time[grepl("17_20",images$Image_Name)] <- "17-20"
unique(images$time)

#set habitat levels
images$habitat <- factor(images$habitat, levels=c("Urban","Agrar","Gruen","Feucht","Wald")) 

##check dataset file########################################################################################

#are any in the images files AND in the dataset file
dataset<-read.delim("stats_dataset.txt",as.is=T)
unique(dataset$habitat)
unique(dataset$route_no)
unique(dataset$time)
dataset$time[which(dataset$time=="12_15")] <- "12-15"

#make aggregate names
images$name <- paste(images$habitat,images$route_no,images$time,sep="_")
dataset$name <- paste(dataset$habitat,dataset$route_no,dataset$time,sep="_")

#are there dataset names not in images
dataset$name[dataset$name %in% images$name]
dataset$name[!dataset$name %in% images$name]#all good

#fix mistakes####################################################################

#from Suzanne:
#"As far as we can see, Agrar 09 should be Wald 09. 
#And the route Gr?n 4 had to be taken out because of quality issues of the route itself."

images <- subset(images, !(habitat=="Gruen" & route_no==4))
images$habitat[images$habitat=="Agrar" & images$route_no==9] <- "Wald"
table(images$habitat)

##summarise data####################################################################

#per habitat and route and time
summary(images)
library(plyr)
summaryData <- ddply(images,.(habitat,route_no,time),summarise,
                     nuObjects=length(unique(Object_ID)),
                     meanSize=median(Size_cm),
                     sumBiomass=sum(Biomass_Area_cm2),
                     sumBiomassP=sum(Biomass_Percentage))

summaryData$Urban <- ifelse(summaryData$habitat=="Urban",1,0)
summaryData$Agrar <- ifelse(summaryData$habitat=="Agrar",1,0)
summaryData$Gruen <- ifelse(summaryData$habitat=="Gruen",1,0)
summaryData$Feucht <- ifelse(summaryData$habitat=="Feucht",1,0)
summaryData$Wald <- ifelse(summaryData$habitat=="Wald",1,0)

nrow(summaryData)#129

##size analysis#######################################################################

hist(summaryData$meanSize)
lm1<-lm(meanSize~habitat+time,data=summaryData)
anova(lm1)
#Analysis of Variance Table

#Response: meanSize
#Df   Sum Sq   Mean Sq F value   Pr(>F)   
#habitat     4 0.010462 0.0026156  3.5046 0.009701 **
# time        1 0.007192 0.0071919  9.6365 0.002397 **
# Residuals 116 0.086573 0.0007463

#summaryData$habitat <- factor(summaryData$habitat, levels=c("Gruen","Urban","Agrar","Feucht","Wald"))
#effect of agricultural land
lm1<-lm(meanSize~time+Urban+Wald+Gruen+Feucht,data=summaryData)
summary(lm1)
library(lmSupport)
modelEffectSizes(lm1) 
#Coefficients
#                SSR df pEta-sqr dR-sqr
#(Intercept) 0.7132  1   0.8918     NA
#time        0.0072  1   0.0767 0.0690
#Urban       0.0050  1   0.0548 0.0481
#Wald        0.0009  1   0.0108 0.0090
#Gruen       0.0000  1   0.0002 0.0002
#Feucht      0.0001  1   0.0016 0.0013

# as lmer
library(lme4)
library(lmerTest)
hist(log(images$Size_cm))
lme1<-lmer(log(Size_cm)~habitat+time+(1|route_no),data=images)
summary(lme1)#sig

##biomass analysis############################################################

hist(summaryData$sumBiomass)
lm1<-lm(log(sumBiomass)~habitat+time,data=summaryData)
#Analysis of Variance Table
#Response: log(sumBiomass)
#Df Sum Sq Mean Sq F value    Pr(>F)    
#habitat    3 23.876  7.9588  9.6154 1.324e-05 ***
# time       1  5.579  5.5794  6.7407   0.01092 *  
# Residuals 95 78.633  0.8277                      
#less in urban, more in the evening

summary(lm(log(sumBiomass)~habitat+time,data=summaryData))

lm1<-lm(log(sumBiomass)~time+Urban+Wald+Gruen+Feucht,data=summaryData)
summary(lm1)
library(lmSupport)
modelEffectSizes(lm1) 
#Coefficients
#                  SSR df pEta-sqr dR-sqr
#(Intercept) 140.5315  1   0.6126     NA
#time          5.6522  1   0.0598 0.0473
#Urban        15.3537  1   0.1473 0.1284
#Wald          0.1516  1   0.0017 0.0013
#Gruen         0.0046  1   0.0001 0.0000
#Feucht        0.4568  1   0.0051 0.0038

###indiv nu analysis#############################################################

hist(log(summaryData$nuObjects))
anova(lm(log(nuObjects)~habitat+time,data=summaryData))
#Analysis of Variance Table
#Response: log(nuObjects)
#Df  Sum Sq Mean Sq F value    Pr(>F)    
#habitat    3  5.6304 1.87681  7.3338 0.0001788 ***
#  time       1  1.7805 1.78051  6.9575 0.0097527 ** 
#  Residuals 95 24.3116 0.25591 

summary(lm(log(nuObjects)~habitat+time,data=summaryData))

lm1<-lm(log(nuObjects)~time+Urban+Wald+Gruen+Feucht,data=summaryData)
summary(lm1)
library(lmSupport)
modelEffectSizes(lm1) 

##plots############################################################

#set order of levels of habitat
images$habitat <- factor(images$habitat,levels=c("Urban","Agrar","Gruen","Feucht","Wald"))

library(ggplot2)
library(plyr)

#histograms
ggplot(images,aes(Size_cm))+
  geom_histogram(binwidth=0.5)+
  #geom_freqpoly(aes(colour=time))+
  facet_grid(~habitat)+
  scale_y_log10()+
  theme_bw()

#geom_freqpoly by route
ggplot(images,aes(Size_cm,color=factor(route_no)))+
  geom_freqpoly()+
  facet_wrap(~habitat,scales = "free_y")+
  scale_y_log10()+
  theme_bw()+
  theme(legend.position="none")

#by night
ggplot(images,aes(Size_cm,color=factor(time)))+
  geom_freqpoly()+
  facet_wrap(~habitat,scales = "free_y",nrow=1)+
  scale_y_log10()+
  theme_bw()+
  theme(legend.position="top")

#change in biomass

#get route biomass per route
biomassSummary <- ddply(images,.(route_no,habitat,time),summarise,
                        totBiomass = sum(Biomass_Area_cm2))

ggplot(biomassSummary,aes(x=habitat, y=totBiomass))+
  geom_boxplot(aes(fill=time))+
  theme_bw()+
  scale_y_log10()+
  xlab("habitat")+ylab("total insect biomass")

#devtools::install_github("mikabr/ggpirate")
library(ggpirate)
ggplot(biomassSummary,aes(x=habitat, y=totBiomass))+
  geom_pirate(aes(colour=time,fill=time),show.legend=TRUE)+
  theme_bw()+
  scale_y_log10()+
  xlab("habitat")+ylab("total insect biomass")

#total number of insects
ggplot(summaryData,aes(x=habitat, y=nuObjects))+
  geom_pirate(aes(colour=time,fill=time),show.legend=TRUE)+
  theme_bw()+
  xlab("habitat")+ylab("total insect abundance")

###size classes###################################################################

#images$Species <- round(50*log(images$Size_cm))/50
images$Species <- round(10*log(images$Size_cm+0.001))/10
images$Species <- cut_number(images$Size_cm,50)
table(images$Species)

#get matrix of number of species in each class per route and time and habitat

library(reshape2)
#out <- acast(images,Species~habitat+route_no+time,fun=function(x)length(x))
out <- acast(images,habitat+route_no+time~Species,fun=function(x)length(x))
outDF <- dcast(images,habitat+route_no+time~Species,fun=function(x)length(x))
row.names(out)<-paste(outDF$habitat,outDF$route_no, outDF$time)
row.names(outDF)<-paste(outDF$habitat,outDF$route_no, outDF$time)

#plot "trait" space
outDFm <- melt(outDF,id=c("habitat","route_no","time")) 
#get median values across each route
outDFm <- ddply(outDFm,.(habitat,time,variable),summarise,value=median(value))
outDFm$variableS <- as.numeric(outDFm$variable)
ggplot(outDFm,aes(y=value,x=variableS))+
  geom_point(aes(colour=time),alpha=0.2)+
  geom_smooth(aes(colour=time),alpha=0.2)+
  facet_wrap(~habitat,nrow=1)+
  xlab("Increasing size class")+
  ylab("Number of individuals")+
  theme_classic()


#split size by 1cm
images$Size <- ifelse(images$Size_cm<1,"small","larger") 
biomassSummary <- ddply(images,.(route_no,habitat,time),summarise,
                        smallInsects_Biomass = sum(Biomass_Area_cm2[Size=="small"]),
                        largeInsects_Biomass = sum(Biomass_Area_cm2[Size=="larger"]))
#split by size
g1 <- ggplot(biomassSummary,aes(x=habitat, y=smallInsects_Biomass))+
  geom_boxplot(aes(colour=time,fill=time))+
  theme_bw()+
  scale_y_log10()+
  xlab("habitat")+ylab("Small insect biomass")
g2 <- ggplot(biomassSummary,aes(x=habitat, y=largeInsects_Biomass))+
  geom_boxplot(aes(colour=time,fill=time))+
  theme_bw()+
  scale_y_log10()+
  xlab("habitat")+ylab("Large insect biomass")
library(cowplot)
plot_grid(g1,g2,nrow=2)


###community analysis####################################################################

library(vegan)
library(MASS)
# data(varespec)
# vare.dis <- vegdist(out)
# vare.mds0 <- isoMDS(vare.dis)
# ordiplot(vare.mds0, type = "t")
# 
# vare.pca <- rda(out)
# plot(vare.pca)
# biplot(vare.pca, scaling = -1)

dune <- out
dune.env <- outDF[,c(1,3)]
dune.ca <- cca(dune)
ef <- envfit(dune.ca, dune.env, permutations = 999)
plot(dune.ca, display = "sites")
plot(ef)

plot(dune.ca, display = "sites", type = "p")
with(dune.env, ordiellipse(dune.ca, habitat, kind = "se", conf = 0.95))
with(dune.env, ordispider(dune.ca, habitat, col = "blue", label= TRUE))
#with(dune.env, ordihull(dune.ca, habitat, col="blue", lty=2))

plot(dune.ca, display = "sites", type = "p")
with(dune.env, ordiellipse(dune.ca, time, kind = "se", conf = 0.95))
with(dune.env, ordispider(dune.ca, time, col = "blue", label= TRUE))

###route length info############################################################################## 

routelength <- read.csv("C:/Users/db40fysa/Nextcloud/mobileInsect/13_data/metaData/2018_gefahreneRouten25832.csv")
unique(routelength$Codierung)
routelength$route_no <- sapply(as.character(routelength$Codierung),function(x)strsplit(x,"_")[[1]][2])
routelength$route_no <- as.numeric(routelength$route_no)
routelength$habitat <- sapply(as.character(routelength$Codierung),function(x)strsplit(x,"_")[[1]][1])
routelength$habitat <- gsub("Grün","Gruen",routelength$habitat)

#merge with data
summaryData <- merge(summaryData,routelength,by=c("habitat","route_no"),all.x=T)
subset(summaryData,is.na(length))

ggplot(summaryData,aes(x=length,y=sumBiomass))+
  geom_point(aes(x=length,y=sumBiomass))+
  facet_wrap(~habitat,scales="free")+
  geom_smooth(method="lm")

##effect of time and day of sampling#########################################################################

routelength <- read.csv("C:/Users/db40fysa/Nextcloud/mobileInsect/13_data/metaData/190503_volunteer sampling protocols 2018_d1-sh-for Diana.csv")
unique(routelength$Codierung)
routelength$route_no <- sapply(as.character(routelength$Codierung.coding),function(x)strsplit(x,"_")[[1]][2])
routelength$route_no <- as.numeric(routelength$route_no)
routelength$habitat <- sapply(as.character(routelength$Codierung.coding),function(x)strsplit(x,"_")[[1]][1])
routelength$habitat <- gsub("Grün","Gruen",routelength$habitat)
routelength$time <- sapply(as.character(routelength$Codierung.coding),function(x)strsplit(x,"_")[[1]][3])

#merge with data
summaryData <- merge(summaryData,routelength,by=c("habitat","route_no","time"),all.x=T)

#anlysis of time at a finer scale
#format time
summaryData$Time <- as.POSIXct(summaryData$Startzeit.starting.time,format="%H:%M")
ggplot(summaryData,aes(x=Time,y=sumBiomass))+
  geom_point(aes(colour=habitat))+
  geom_smooth(method="lm")+
  facet_wrap(~time,scales="free")+
  scale_y_log10()

#analysis of date at a finer scale
summaryData$Date <- as.Date(summaryData$Date,format="%d-%m-%Y")
library(lubridate)
summaryData$Julian_day <- yday(summaryData$Date)

ggplot(summaryData,aes(x=Julian_day,y=SamplesummaryData..mg.))+
  geom_point(aes(colour=Landuse))+
  geom_smooth(method="lm")+
  facet_wrap(~Time,scales="free")+
  scale_y_log10()

##dry weight###############################################################################

#compare biomass from fotos with dry weight
dryWeights <- read.csv("C:/Users/db40fysa/Nextcloud/mobileInsect/13_data/dryWeight/Dry weight_Insektenmobil_Julianas lab book_16_05_2019-she.csv",as.is=T)

#get habitat
dryWeights$habitat <- sapply(dryWeights$Sample.ID,function(x)strsplit(as.character(x),"_")[[1]][1])
dryWeights <- subset(dryWeights,!habitat %in% c("Blank","Blank filter"))
dryWeights <- subset(dryWeights,!is.na(habitat))
dryWeights$habitat[dryWeights$habitat=="Grün"]<- "Gruen"
unique(dryWeights$habitat)

#get route number
dryWeights$route_no <- sapply(dryWeights$Sample.ID,function(x)strsplit(as.character(x),"_")[[1]][2])
dryWeights$route_no <- as.numeric(dryWeights$route_no)
unique(dryWeights$route_no)

#get time
dryWeights$time <- sapply(dryWeights$Sample.ID,function(x)strsplit(as.character(x),"_")[[1]][3])
unique(dryWeights$time)

#get total biomass per habitat, route and time
summaryDry <- ddply(dryWeights,.(habitat,route_no,time),summarise,dryBiomass=sum(Dry.mass..mg.))

#merge with image based mass data
allBiomass <- merge(summaryData,summaryDry,by=c("habitat","time","route_no"),all=T)
subset(allBiomass,is.na(sumBiomass))
subset(allBiomass,is.na(dryBiomass))#none missing

#plot
library(ggplot2)
summary(allBiomass$sumBiomass)
summary(allBiomass$dryBiomass)
qplot(sumBiomass,dryBiomass,data=allBiomass,colour=habitat,shape=time)+
  scale_y_log10()+scale_x_log10()+
  theme_bw()+
  ylab("Biomass (weighted)")+xlab("Biomass (image-based)")
#missing 4 site combinations!

qplot(sumBiomass,dryBiomass,data=allBiomass)+
  facet_wrap(time~habitat,scales="free")+
  scale_y_log10()+scale_x_log10()

cor.test(allBiomass$sumBiomass,allBiomass$dryBiomass)#0.9050286


#compare small and large insects in terms of dry weight

###land use data##############################################################################

#get land use data
library(plyr)

#get 100m buffer
land100 <- read.csv("C:/Users/db40fysa/Nextcloud/mobileInsect/13_data/landData/insectmobile_schnittflaechengroessen_atkis2018+buffer100_conflict-20190529-181641.csv",
                    sep=",")
unique(land100$beschreibu)

#fix mistake
land100$name <- as.character(land100$name)
land100$name[which(land100$name=="Nienburg_Wald_1\n")]<-"Nienburg_Wald_1" 
land100$name[which(land100$name=="Leipzig_Anett_Alternative")]<-"Leipzig_Urban_17"
land100$name[which(land100$name=="Leipzig_Urban_SemmelweisstraÃŸe")]<-"Leipzig_Urban_16"
land100$name[which(land100$name=="Volkse-Didderse")]<-"Volkse_Agrar_15"
land100$name[which(land100$name=="Rosslau_Luko")]<-"Rosslau_Wald_14"
unique(land100$name)

#get forest area as Wald (Gehölz too)
#agriculture as Landwirtschaft
#get urban as 
#Wohnbaufläche, Industrie- und Gewerbefläche, Stra?Yenverkehr,Bahnverkehr

#overall see what is common
out <- ddply(land100,.(beschreibu),summarise,totArea=sum(area_sqm))
arrange(out,totArea)

#separate habitat and route
land100$habitat <- land100$mI_habitat 
land100$route_no <- land100$mI_route_no
#land100$route_no <- sapply(as.character(land100$name),function(x)strsplit(x,"_")[[1]][3]) 
land100$habitat[which(land100$habitat=="Feuchtland")] <- "Feucht"
land100$habitat[which(land100$habitat=="Gruenland")] <- "Gruen"
unique(land100$habitat)
unique(land100$route_no)

land100S <- ddply(land100,.(habitat,route_no),summarise,
                 forest=mean(area_sqm[beschreibu %in% c("Wald","GehÃ¶lz")]),
                 urban=mean(area_sqm[beschreibu %in% c("WohnbauflÃ¤che",
                                                      "Industrie- und GewerbeflÃ¤che",
                                                      "StraÃŸenverkehr",
                                                      "Bahnverkehr")]),
                 agri=mean(area_sqm[beschreibu %in% c("Landwirtschaft")]))
land100S[is.na(land100S)] <- 0

#merge with pop data
summaryData <- merge(summaryData,land100S,by=c("habitat","route_no"),all.x=T)
nrow(subset(summaryData,is.na(forest)))#4
nrow(summaryData)

##land use analyssi###########################################################################

library(ggplot2)
q1 <- qplot(forest,sumBiomass,data=summaryData)+stat_smooth(method="lm")
q2 <- qplot(agri,sumBiomass,data=summaryData)+stat_smooth(method="lm")
q3 <- qplot(urban,sumBiomass,data=summaryData)+stat_smooth(method="lm")
library(cowplot)
plot_grid(q1,q2,q3,nrow=1)

library(ggplot2)
q1 <- qplot(forest,nuObjects,data=summaryData)+stat_smooth(method="lm")+ylab("total abundance")
q2 <- qplot(agri,nuObjects,data=summaryData)+stat_smooth(method="lm")+ylab("total abundance")
q3 <- qplot(urban,nuObjects,data=summaryData)+stat_smooth(method="lm")+ylab("total abundance")
library(cowplot)
plot_grid(q1,q2,q3,nrow=1)

library(ggplot2)
q1 <- qplot(forest,meanSize,data=summaryData)+stat_smooth(method="lm")
q2 <- qplot(agri,meanSize,data=summaryData)+stat_smooth(method="lm")
q3 <- qplot(urban,meanSize,data=summaryData)+stat_smooth(method="lm")
library(cowplot)
plot_grid(q1,q2,q3,nrow=1)

###sampling range##########################################################################################

#relationships
g1 <- ggplot(summaryData,aes(forest,agri))+
  geom_jitter(alpha=0.5)+theme_bw()
g2 <- ggplot(summaryData,aes(forest,urban))+
  geom_jitter(alpha=0.5)+theme_bw()
g3 <- ggplot(summaryData,aes(agri,urban))+
  geom_jitter(alpha=0.5)+theme_bw()

#histograms
g4 <- ggplot(summaryData,aes(forest))+
  geom_histogram()+theme_bw()
g5 <- ggplot(summaryData,aes(urban))+
  geom_histogram()+theme_bw()
g6 <- ggplot(summaryData,aes(agri))+
  geom_histogram()+theme_bw()

library(cowplot)
plot_grid(g4,g5,g6,g1,g2,g3,ncol=3,nrow=2)

####################################################################################################
