library(plyr)
library(tidyverse) # data manipulation
library(ggplot2) # graphics
library(digest)
library(forcats)
library(lubridate) # works with date and time formats

setwd("H:/Documents/Insektmobilen/Analysis/Biomass_test/August_2019")

# Prepare datasets for biomass data
biomass <-
  read.csv(
    "Biomass_august2019.csv",
    header = TRUE,
    row.names = NULL,
    sep = ";"
  ) # Biomass data
head(biomass)

hist(log(biomass$SampleBiomass_mg)) # Biomass data is normally distributed when it is log transformed (many zeros)

biomass_nozeros <-
  filter(biomass, SampleBiomass_mg > 0) # remove zero from total sample dry weight/biomass
head(biomass_nozeros)
summary(biomass_nozeros)
str(biomass_nozeros)

hist(log(biomass_nozeros$SampleBiomass_mg)) # Normally distributed but we still need to log transform due to a surplus of low weights

# Import metadata
samplingevent <-
  read.csv(
    "SamplingEvent.csv",
    header = TRUE,
    row.names = NULL,
    sep = ";",
    check.names = FALSE,
    na.strings = c("", " ", "NA")
)
meta <- samplingevent %>% filter(FullySampled == 'yes') # Filter function to only select the samples that are correctly sampled
meta_select <-  select(meta, 'SampleID', 'LandUSeType', 'Date', 'StartTime', 'EndTime', 'Wind', 'Temperature') %>% droplevels # We choose only the variables we're interested in

summary(meta_select$LandUSeType) # Look at the different types of land use categories and how many samples are present in each category
metadata2 <- subset(meta_select, !is.na(meta_select$LandUSeType)) # subset your data further and removes samples that does not have a landuse type

# Rename the values from Danish to English
metadata2$LandUSeType <- mapvalues(
  metadata2$LandUSeType,
  from = c(
    'mark',
    'skov',
    'skov, tør',
    'skov_tør',
    'tør',
    'tør, mark',
    'tør, våd',
    'urban',
    'urban, tør, skov',
    'våd',
    'våd, mark',
    'våd, tør'
  ),
  to = c(
    'agriculture',
    'forest',
    'forest_dry',
    'forest_dry',
    'dry',
    'dry_agriculture',
    'dry_wet',
    'urban',
    'urban_dry_forest',
    'wet',
    'wet_agriculture',
    'dry_wet'
  )
)

head(metadata2)

data <- inner_join(metadata2, biomass_nozeros, by = "SampleID")  # Join the two tables based on row matches in both tables
head(data)

data %>% group_by(LandUSeType) %>% summarize(count=n())

# choose only the clean land use types
data_landuse <-  data %>% filter(LandUSeType == 'agriculture' | LandUSeType == 'forest' | LandUSeType == 'dry' | LandUSeType == 'urban'| LandUSeType =='wet') %>% droplevels

data.frame(table(data_landuse$Wind))
data.frame(table(data_landuse$Temperature))
data.frame(table(data_landuse$Date))

str(data_landuse)
data_landuse$Date <- as.Date(data_landuse$Date, format = "%d-%m-%Y") # makes date a date class and specifies the format. Can be difficult to adjust, but help can be found here: https://www.r-bloggers.com/date-formats-in-r/ . You might need to change the formatting in excel for the column for it to work or at least be aware of the formatting setup
data_landuse$StartTime <- lubridate::hms(data_landuse$StartTime)
data_landuse$EndTime <- lubridate::hms(data_landuse$EndTime, roll = FALSE)

# create between function
#between <- function(x,lower,upper,incbounds=TRUE){if(incbounds) x>=lower & x<=upper else x>lower & x<upper}

#"%between%" <- function(x,y) between(x,y[1],y[2],incbounds=TRUE) # It can be used as between(x,lower,upper) or x %between% c(lower, upper)

df <- data.frame(data_landuse, stringsAsFactors = FALSE)

# on way to create intervals, even though it does not work for 17-20
df$seconds <- period_to_seconds(lubridate::hms(df$StartTime)) # from lubridate
myintervals <- c("12-15", "17-20")
df$interval <-
  myintervals[findInterval(df$seconds, c(12, 15, 17, 20) * 3600)] # somehow this does not work for late trips but perhaps NAs could be recoded

# Another try to make intervals of the times. We use starttime as time
df$datetime <- as.POSIXct(paste(df$Date, df$StartTime)) # if you define the date column to be dates and do not define the starttime column to time (?) you can make a datetime format column. 
start <- lubridate::hms("12:00:00")
end <- lubridate::hms("15:00:00")

# This plot visualises mean biomass caught by hour based on sampling start time
hour <-  df %>% 
  mutate(hour = hour(StartTime)) %>% 
  group_by(hour) %>% 
  summarise(
    avg_biomass = mean(SampleBiomass_mg, na.rm = TRUE),
    n = n())  
# trying to plot both start and end time for sampling in the same plot
hour2 <-  df %>% 
  mutate(hour = hour(EndTime)) %>% 
  group_by(hour) %>% 
  summarise(
    avg_biomass = mean(SampleBiomass_mg, na.rm = TRUE),
    n = n()) 
  
ggplot(hour, aes(x=hour, y=avg_biomass)) + 
  geom_line() + 
  geom_line(data = hour2, aes(x=hour, y=avg_biomass, colour="#000099"))

ggplot(data=hour) + 
  geom_point(aes(x=hour, y=avg_biomass), col = "Steelblue", size = 1.5) + 
  geom_point(data = hour2, aes(x=hour, y=avg_biomass), col="Forestgreen", size = 1.5) 

hour <-  df %>% 
  mutate(hour = hour(StartTime)) %>% 
  group_by(hour) %>% 
  summarise(
    avg_biomass = mean(SampleBiomass_mg, na.rm = TRUE),
    n = n())  

# playing with plots of biomass over time during the day
start <-  df %>% 
  mutate(hour = hour(StartTime)) %>% 
  group_by(hour) 

end <-  df %>% 
  mutate(hour = hour(EndTime)) %>% 
  group_by(hour)  

ggplot(start, aes(x=hour, y=log(SampleBiomass_mg))) + 
  geom_line() + 
  geom_line(data = end, aes(x=hour, y=log(SampleBiomass_mg), colour="#000099"))

ggplot(data=start) + 
  geom_point(aes(x=hour, y=log(SampleBiomass_mg)), col = "Pink", size = 1.5) + 
  geom_point(data = end, aes(x=hour, y=log(SampleBiomass_mg)), col="Forestgreen", size = 1.5) + geom_smooth(aes(x=hour, y = log(SampleBiomass_mg)), col ="Darkgrey")

ggplot(data=df) + 
  geom_point(aes(x=Date, y=log(SampleBiomass_mg)), col = "Purple", size = 1.5) + 
  geom_point(data = df, aes(x=Date, y=log(SampleBiomass_mg)), col="Forestgreen", size = 1.5) + geom_smooth(aes(x=Date, y = log(SampleBiomass_mg)), col ="Darkgrey")


# how would it look if we plotted biomass in terms of time it took to sample (length of sampling)? We expect more biomass the longer the trip was. 

ggplot(data = data_landuse) + geom_point(aes(x = Date, y = SampleBiomass_mg, color = LandUSeType)) + scale_y_log10() + geom_smooth(aes(x = Date, y = SampleBiomass_mg), col = "darkgrey") + scale_x_date()  # Plot the sample biomass in June 2018

bar <- data_landuse %>% group_by(LandUSeType) %>% summarise(avg=mean(SampleBiomass_mg)) # mean sample biomass per habitat


ggplot(bar, aes(fct_reorder(LandUSeType, avg, .desc = TRUE), avg, fill = LandUSeType)) + geom_col() + xlab("Land use type") + ylab("Mean sample biomass (mg)") + theme_classic(base_size = 20) + theme(legend.position = "none") +scale_fill_brewer(palette = "Set1")  # plot mean biomass (mg) per habitat in descending order

ggplot(data = data_landuse) + geom_point(aes(x = Date, y = SampleBiomass_mg, color = LandUSeType)) + scale_y_log10() + geom_smooth(aes(x = Date, y = SampleBiomass_mg), col = "darkgrey") + scale_x_date()  # Plot the sample biomass in June 2018

# Simple boxplot of all pure landuse types (descending)
plot <- ggplot(data = data_landuse) +
  geom_boxplot(lwd = 1, aes(
    x = fct_reorder(LandUSeType, SampleBiomass_mg, .desc = TRUE),
    y = SampleBiomass_mg,
    colour = factor(LandUSeType)
  )) + scale_y_log10(name = "Total Sample Biomass log (mg)") +
  theme_classic(base_size = 20) + theme(legend.position = "none")

plot + labs(x = "Land Use Type") + labs(legend = NULL) + scale_color_brewer(palette = "Set1")

require(lubridate)


plot <- ggplot(data = data_landuse) +
  geom_boxplot(lwd = 1, aes(
    x = fct_reorder(StartTime, SampleBiomass_mg, .desc = TRUE),
    y = SampleBiomass_mg,
    colour = factor(StartTime)
  )) + scale_y_log10(name = "Total Sample Biomass log (mg)") +
  theme_classic(base_size = 20) + theme(legend.position = "none")

plot + labs(x = "Land Use Type") + labs(legend = NULL) + scale_color_brewer(palette = "Set1")


#Diana's contribution:

#danish analysis
setwd("C:/Users/db40fysa/Nextcloud/mobileInsect/13_data/danishData")

#read in data
biomass <- read.csv("Biomass_2018_DK.csv",sep=";",as.is=T)
metadata <- read.csv("SamplingEvent_allsamples_2018.csv",sep=";",as.is=T)


#explore data
names(biomass)
names(metadata)
nrow(metadata)
unique(metadata$PilotNotes)
table(metadata$NotFullRoute)
table(metadata$NotFullySampledRoute)
table(metadata$FullySampled)
unique(metadata$PID)

#match sheets
biomass$SampleID[biomass$SampleID %in% metadata$PilotTripID]
biomass$SampleID[!biomass$SampleID %in% metadata$PilotTripID]#all in there
biomass <- merge(biomass,metadata,by.x="SampleID", by.y="PilotTripID")
head(biomass)

#subset to fully sampled route
biomass <- subset(biomass, FullySampled=="yes")


#explore land use characters
unique(biomass$LandUSeType)
unique(biomass$Landuse)
table(biomass$Landuse)


#subset to only agriculture, forest, urban, wet, dry other
biomass <- subset(biomass, Landuse %in% c("agriculture","dry","forest","urban","wet"))

#organise time
unique(biomass$StartTime)
biomass$hour <- sapply(biomass$StartTime,function(x)substr(x,1,2))
table(biomass$hour)
biomass$Time <- ifelse(biomass$hour %in% c(12,13,14),"12-14","17-19")

##################################################################################################

#remove missing values?
summary(biomass$SampleBiomass..mg.)
#zeros...

#plot biomass trend
library(ggpirate)
ggplot(biomass,aes(x=Landuse, y=SampleBiomass..mg.))+
  geom_pirate(aes(colour=Time,fill=Time),show.legend=TRUE)+
  theme_bw()+
  scale_y_log10()+
  xlab("habitat")+ylab("total insect biomass")

#split by size
g1 <- ggplot(biomass,aes(x=Landuse, y=DryMassSmall..mg.))+
  geom_boxplot(aes(colour=Time,fill=Time))+
  theme_bw()+
  scale_y_log10()+
  xlab("habitat")+ylab("Small insect biomass")
g2 <- ggplot(biomass,aes(x=Landuse, y=DryMassLarge..mg.))+
  geom_boxplot(aes(colour=Time,fill=Time))+
  theme_bw()+
  scale_y_log10()+
  xlab("habitat")+ylab("Large insect biomass")
library(cowplot)
plot_grid(g1,g2,nrow=2)

#study other predictors
unique(biomass$Wind)
unique(biomass$Temperature)
g1 <- ggplot(biomass,aes(x=Wind, y=SampleBiomass..mg.))+
  geom_boxplot(aes(colour=Time,fill=Time))+
  theme_bw()+
  scale_y_log10()
g2 <- ggplot(biomass,aes(x=Temperature, y=SampleBiomass..mg.))+
  geom_boxplot(aes(colour=Time,fill=Time))+
  theme_bw()+
  scale_y_log10()
library(cowplot)
plot_grid(g1,g2,nrow=2)


#build model
hist(biomass$SampleBiomass..mg.)
hist(log(biomass$SampleBiomass..mg.+1))

#urban forest
biomass$Landuse <- factor(biomass$Landuse,levels=c("urban","agriculture","wet","dry","forest"))

lm1 <- lm(log(SampleBiomass..mg.+1) ~ Landuse + Time + Wind + Temperature, data=biomass)
summary(lm1)

library(car)
avPlots(lm1)


#anlysis of time at a finer scale
#format time
biomass$time <- as.POSIXct(biomass$StartTime,format="%H:%M:%S")
ggplot(biomass,aes(x=time,y=SampleBiomass..mg.))+
  geom_point(aes(colour=Landuse))+
  geom_smooth(method="lm")+
  facet_wrap(~Time,scales="free")+
  scale_y_log10()

g1 <- ggplot(biomass,aes(x=time,y=DryMassSmall..mg.))+
  geom_point(aes(colour=Landuse))+
  geom_smooth(method="lm")+
  facet_wrap(~Time,scales="free")+
  scale_y_log10()
g2 <- ggplot(biomass,aes(x=time,y=DryMassLarge..mg.))+
  geom_point(aes(colour=Landuse))+
  geom_smooth(method="lm")+
  facet_wrap(~Time,scales="free")+
  scale_y_log10()
plot_grid(g1,g2,nrow=1)

#analysis of date at a finer scale
biomass$Date <- as.Date(biomass$Date,format="%d-%m-%Y")
library(lubridate)
biomass$Julian_day <- yday(biomass$Date)

ggplot(biomass,aes(x=Julian_day,y=SampleBiomass..mg.))+
  geom_point(aes(colour=Landuse))+
  geom_smooth(method="lm")+
  facet_wrap(~Time,scales="free")+
  scale_y_log10()

g1 <- ggplot(biomass,aes(x=Julian_day,y=DryMassSmall..mg.))+
  geom_point(aes(colour=Landuse))+
  geom_smooth(method="lm")+
  facet_wrap(~Time,scales="free")+
  scale_y_log10()
g2 <- ggplot(biomass,aes(x=Julian_day,y=DryMassLarge..mg.))+
  geom_point(aes(colour=Landuse))+
  geom_smooth(method="lm")+
  facet_wrap(~Time,scales="free")+
  scale_y_log10()
plot_grid(g1,g2,nrow=1)


