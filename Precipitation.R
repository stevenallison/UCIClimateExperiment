#September 14, 2017
#Reset R's Brain
rm(list=ls())
# load in required packages -----------------------------------------------

library(tidyverse)
#install.packages("reshape")
library(reshape)
library(lattice)
library(plyr)
library(stringr)
#install.packages("chron")
library(chron)
library(ggplot2)

######################################################
########Upload data
######################################################

setwd("~/Dropbox/StatsandProgramming/16SElevationGradient/precipdata/")
#desert data: campround weather station from boyd deep canyon http://deepcanyon.ucnrs.org/weather-data/
desert.precip <- read.csv("Desert_Campground-long-term-precip.csv", skip=1)
#scrubland data: pinyonc crest weather station from boyd deep canyon: http://deepcanyon.ucnrs.org/weather-data/
scrubland.precip <- read.csv("PC-long-term-precip_scrubland.csv", skip=1)
#subalpine data: remote weather station in Mt San Jacinto, 2626.157m elevation: https://raws.dri.edu/cgi-bin/rawMAIN.pl?caCMSJ
subalpine.precip <- read.csv("MtSanJacinto_precip_Oct2015toApril2017.csv", skip=1)

#pine oak data: weather station in james reserve:https://wrcc.dri.edu/cgi-bin/rawMAIN.pl?caucja
pineoak.precip <- read.csv("JamesReserve_precip_Oct2015toApril2017.csv", skip=1)

#grassland - these data sheets need some formatting and cleaning up to get the total precip values
grassland.precip.2015 <- read.csv("Loma water input 1516.csv", skip=20)
head(grassland.precip.2015)
grassland.precip.2016 <- read.csv("Lomawaterinput1617.csv", skip=20)
head(grassland.precip.2016)
###########################################################################
########clean data and get it all in same format (avg per month ): Grassland
##########################################################################
head(grassland.precip.2015)
head(grassland.precip.2016)

# Set the format of Date (origin = "1899-12-30" for Windows)
grassland.precip.2015$Date <- as.POSIXct(grassland.precip.2015$Rain.gauge.Date, format="%m/%d/%Y %H:%M")
grassland.precip.2016$Date <- as.POSIXct(grassland.precip.2016$Rain.gauge.Date, format="%m/%d/%Y %H:%M")

#subset only the relevant colummns
grassland.precip.2016 <- grassland.precip.2016[ ,c(4,5,6,7,18)]
head(grassland.precip.2016)
#subset only the relevant colummns
grassland.precip.2015 <- grassland.precip.2015[ ,c(4,5,6,7,18)]
head(grassland.precip.2015)
#need to remove first column that isn't in standard date format
toremove <- which(grassland.precip.2015$Rain.gauge.Date == "Loma tank")
grassland.precip.2015 <- grassland.precip.2015[-toremove, ]
#combine 2015 and 2016 and 2017 data
grassland.precip.all <- rbind(grassland.precip.2015,grassland.precip.2016)

#subset out the relevant dates: October 1 2015 through April 30 2017
#this doesn't work says, dates aren't all in same format
grassland.precip.all[ ,grassland.precip.all$Date > "10/1/15 0:00" & grassland.precip.all$Date < "4/30/17 23:40"]
#try something different - get all months
grassland.precip.all$month <- months(grassland.precip.all$Date, abbreviate=TRUE)
grassland.precip.all$month <- as.factor(grassland.precip.all$month)
levels(grassland.precip.all$month )
#get the years out 
grassland.precip.all$year <- years(grassland.precip.all$Date)
#add in 20 in front of year so it is 2015 instead of 15, etc
grassland.precip.all$year <-  paste(20, grassland.precip.all$year,sep ="")
#combine month and year to a single column
grassland.precip.all$monthyear <-  paste(grassland.precip.all$month,grassland.precip.all$year,sep =" ")

grassland.precip.all$monthyear <- as.factor(grassland.precip.all$monthyear)
levels(grassland.precip.all$monthyear )
#pick out which months I want to keep
#why is July 2016 missing? and august 2016? I'm missing July, August, Sept 2016 
monthstokeep <- c("Oct 2015","Nov 2015","Dec 2015", "Jan 2016","Feb 2016","Mar 2016", 
                  "Apr 2016","May 2016","Jun 2016","Jul 2016","Aug 2016","Sep 2016","Oct 2016","Nov 2016","Dec 2016",
                  "Jan 2017","Feb 2017","Mar 2017", "Apr 2017")
#subset the grassland precip for only the months I want to keep
grassland.precip.2 <- grassland.precip.all[grassland.precip.all$monthyear %in% monthstokeep,]

#convert to a factor and check the levels are right
grassland.precip.2$monthyear <- as.factor(grassland.precip.2$monthyear)
levels(grassland.precip.2$monthyear)

#get the total precipitation in mm per month
#need to sum the R-gauge value per  month
names(grassland.precip.2)
grassland.precip.bymonth <- ddply(grassland.precip.2, c("monthyear"), summarise,
                                          total = sum(R_gauge, na.rm=TRUE),
                                          sd = sd(R_gauge, na.rm=TRUE),
                                          n = sum(!is.na(R_gauge)),
                                          se = sd/sqrt(n))
#add in site name column
grassland.precip.bymonth$Site<- rep("Grassland", nrow(grassland.precip.bymonth))
#change total column to proper name
names(grassland.precip.bymonth)[2] <- "TotalPrecip_mm"
#get total amount of precip
TotalgrassPrecip_T0toT3 <- sum(grassland.precip.bymonth$TotalPrecip_mm)
TotalgrassPrecip_T0toT3 

grassland.precip.bymonth$month <- substr(grassland.precip.bymonth$monthyear,1,3)
grassland.precip.bymonth$Year <- substr(grassland.precip.bymonth$monthyear,5,8)
grassland.precip.bymonth2 <-grassland.precip.bymonth[ ,c(2,7,8,6)]
###########################################################################
########clean data and get it all in same format (avg per month ): Desert
##########################################################################

#subset relevant data for desert and transform it
dim(desert.precip)
#start in October 2015
desert.subset.2015 <- as.data.frame(t(desert.precip[34,11:13]))
names(desert.subset.2015) <- "TotalPrecip_mm"
desert.subset.2015$month <- row.names(desert.subset.2015)
desert.subset.2015$Year <- rep(2015,nrow(desert.subset.2015))
desert.subset.2015$Site <- rep("Desert",nrow(desert.subset.2015))

#keep everything because oct 2015: april 2016 was T0 to T1
desert.subset.2016 <- as.data.frame(t(desert.precip[35,2:13]))
names(desert.subset.2016) <- "TotalPrecip_mm"
desert.subset.2016$month <- row.names(desert.subset.2016)
desert.subset.2016$Year <- rep(2016,nrow(desert.subset.2016))
desert.subset.2016$Site <- rep("Desert",nrow(desert.subset.2016))

#only collect through April 2017
desert.subset.2017 <- as.data.frame(t(desert.precip[36,2:5]))
names(desert.subset.2017) <- "TotalPrecip_mm"
desert.subset.2017$month <- row.names(desert.subset.2017)
desert.subset.2017$Year <- rep(2017,nrow(desert.subset.2017))
desert.subset.2017$Site <- rep("Desert",nrow(desert.subset.2017))


desert.precip2 <- rbind(desert.subset.2015, desert.subset.2016,desert.subset.2017)

row.names(desert.precip2) <- seq(1,nrow(desert.precip2),by=1)
desert.precip2
TotalDesertPrecip_T0toT3 <- sum(desert.precip2$TotalPrecip_mm)
TotalDesertPrecip_T0toT3
###########################################################################
########clean data and get it all in same format (avg per month ): Scrubland
##########################################################################

#start in October 2015
scrubland.subset.2015 <- as.data.frame(t(scrubland.precip[34,11:13]))
names(scrubland.subset.2015) <- "TotalPrecip_mm"
scrubland.subset.2015$month <- row.names(scrubland.subset.2015)
scrubland.subset.2015$Year <- rep(2015,nrow(scrubland.subset.2015))
scrubland.subset.2015$Site <- rep("Scrubland",nrow(scrubland.subset.2015))

#keep everything because oct 2015: april 2016 was T0 to T1
scrubland.subset.2016 <- as.data.frame(t(scrubland.precip[35,2:13]))
names(scrubland.subset.2016) <- "TotalPrecip_mm"
scrubland.subset.2016$month <- row.names(scrubland.subset.2016)
scrubland.subset.2016$Year <- rep(2016,nrow(scrubland.subset.2016))
scrubland.subset.2016$Site <- rep("Scrubland",nrow(scrubland.subset.2016))

#only collect through April 2017
scrubland.subset.2017 <- as.data.frame(t(scrubland.precip[36,2:5]))
names(scrubland.subset.2017) <- "TotalPrecip_mm"
scrubland.subset.2017$month <- row.names(scrubland.subset.2017)
scrubland.subset.2017$Year <- rep(2017,nrow(scrubland.subset.2017))
scrubland.subset.2017$Site <- rep("Scrubland",nrow(scrubland.subset.2017))


scrubland.precip2 <- rbind(scrubland.subset.2015, scrubland.subset.2016,scrubland.subset.2017)

row.names(scrubland.precip2) <- seq(1,nrow(scrubland.precip2),by=1)
scrubland.precip2
TotalscrublandPrecip_T0toT3 <- sum(scrubland.precip2$TotalPrecip_mm)
TotalscrublandPrecip_T0toT3

#combine desert and scrubland and grassland
desert.scrub.grass.precip <- rbind(desert.precip2, scrubland.precip2,grassland.precip.bymonth2)
desert.scrub.grass.precip

######################################################################################
########clean data and get it all in same format (avg per month ): subalpine and pineoak
#####################################################################################
###convert to date
subalpine.precip$Date <- as.Date(subalpine.precip$Date, format="%m/%d/%Y")
tail(subalpine.precip$Date )
#get column of just months
subalpine.precip$month <- months(subalpine.precip$Date, abbreviate=TRUE)
#add column for site name
subalpine.precip$Site <- rep("Subalpine",nrow(subalpine.precip))
TotalsubalpinePrecip_T0toT3 <- sum(subalpine.precip$TotalPrecip_mm)
TotalsubalpinePrecip_T0toT3



###convert to date
pineoak.precip$Date <- as.Date(pineoak.precip$Date, format="%m/%d/%Y")
#get column of just months
pineoak.precip$month <- months(pineoak.precip$Date, abbreviate=TRUE)
#add column for site name
pineoak.precip$Site <-rep("Pine-oak",nrow(pineoak.precip))
TotalpineoakPrecip_T0toT3 <- sum(pineoak.precip$TotalPrecip_mm)
TotalpineoakPrecip_T0toT3 

#combine pineoak and subalpine
pinoaksub <- rbind(pineoak.precip,subalpine.precip)
##############get total precip by month by year to have same level as I have for desert and scrubland


pineoak.subalpine.precip.bymonth <- ddply(pinoaksub , c("month","Year","Site"), summarise,
                                    mean = mean(TotalPrecip_mm, na.rm=TRUE),
                                    total = sum(TotalPrecip_mm, na.rm=TRUE),
                                    sd = sd(TotalPrecip_mm, na.rm=TRUE),
                                    n = sum(!is.na(TotalPrecip_mm)),
                                    se = sd/sqrt(n))

pineoak.subalpine.precip.bymonth$Year <-as.factor(pineoak.subalpine.precip.bymonth$Year)

##############subset and put into same order as desert and scrubland
names(desert.scrub.precip)
#order: Total precip (column 5), month(column 1,), Year (column 2), Site (column 3)
pineoak.sub.precip <- pineoak.subalpine.precip.bymonth[ , c(5,1:3)]
#rename first column to same name as others
colnames(pineoak.sub.precip)[1] <- "TotalPrecip_mm"

#combine subalpine,pineoak, desert, scrubland
fivesites.precip <- rbind(desert.scrub.precip,pineoak.sub.precip)

fivesites.precip$monthyear <-  paste(fivesites.precip$month,fivesites.precip$Year ,sep =" ")
fivesites.precip$monthyear <- as.factor(fivesites.precip$monthyear )
length(levels(fivesites.precip$monthyear))

######################################################################################
########Addin snow fall data forsubalpine
#####################################################################################
#subalpine data with snow fall info: NOAA weather station: https://www.ncdc.noaa.gov/cdo-web/datasets#GHCND
subalpine.snow <- read.csv("NOAA_precipandsnow_subalpine.csv", header=TRUE)
subalpine.snow$PRCP_in
#according to NOAA guy this is in 100ths of an inch
#subalpine.snow$precip_in <- (subalpine.snow$PRCP_in)*100
#convert from inches to mm by multiplying by 25.4
subalpine.snow$precip_mm <- (subalpine.snow$PRCP_in)*25.4
###subalpine data from NOAA with snow info:https://www.ncdc.noaa.gov/cdo-web/datasets#GHCND

names(subalpine.snow)
head(subalpine.snow)
subalpine.snow$Date <- as.Date(subalpine.snow$DATE, format="%m/%d/%Y")
subalpine.snow$month <- months(subalpine.snow$Date, abbreviate = TRUE)

subalpine.snow$Year <- years(subalpine.snow$Date)
#add in 20 in front of year so it is 2015 instead of 15, etc
subalpine.snow$Year <-  paste(20, subalpine.snow$Year,sep ="")

#average monthly precip (snow + rain) fall by these 3 stations
subalpine.snow.month <- ddply(subalpine.snow, c("month","Year","STATION"), summarise,
                                      mean = mean(precip_mm, na.rm=TRUE),
                                      total = sum(precip_mm, na.rm=TRUE),
                                      sd = sd(precip_mm, na.rm=TRUE),
                                      n = sum(!is.na(precip_mm)),
                                      se = sd/sqrt(n))

subalpine.snow.month 
#make a figure 

p <- ggplot(data=subalpine.snow.month , aes(x=month, y=total, col=STATION, shape=Year)) + geom_point(size=2) +  
  labs(x=" ", y="Precipitation mm", col="Month") + #change y axis label to "Temp C" and remove "Date" for x axis and change legend title
  theme(axis.text.x=element_text(size=10,angle=70, hjust=1)) #change angles and size of x axis labels
p


#choose out station that I think is the same as the mt san jacinto one
which(subalpine.snow.month$STATION =="USC00045091")
subalpine.snow.month.idyll <- subalpine.snow.month [which(subalpine.snow.month$STATION =="USC00045091"), ]
#only 4 months bc it's a new station

#need to avg the 3 stations for each month
subalpine.snow.avg <- ddply(subalpine.snow.month, c("month","Year"), summarise,
                              mean = mean(total, na.rm=TRUE),
                              sd = sd(total, na.rm=TRUE),
                              n = sum(!is.na(total)),
                              se = sd/sqrt(n))

#make a column that combines month and year so I have unique IDs for all months
subalpine.snow.avg$monthyear <-  paste(subalpine.snow.avg$month,subalpine.snow.avg$Year ,sep =" ")

#make a vector of appropriate montyear levels in correct order
monthyearlevels <- c("Oct 2015","Nov 2015","Dec 2015", "Jan 2016","Feb 2016","Mar 2016", 
                     "Apr 2016","May 2016","Jun 2016","Jul 2016","Aug 2016","Sep 2016","Oct 2016","Nov 2016","Dec 2016",
                     "Jan 2017","Feb 2017","Mar 2017", "Apr 2017")

#order the factor appropriately
subalpine.snow.avg$monthyear <- factor(subalpine.snow.avg$monthyear, levels =monthyearlevels )     


#plot out the total precip by month for the 3 subalpine weather stations from Oct 2015 to April 2017
sp <- ggplot(data=subalpine.snow.avg , aes(x=monthyear, y=mean, shape=Year, col=monthyear)) + geom_point(size=2) +  
  labs(x=" ", y="Precipitation mm", col="Month") + #change y axis label to "Temp C" and remove "Date" for x axis and change legend title
  theme(axis.text.x=element_text(size=10,angle=70, hjust=1)) #change angles and size of x axis labels

sp

######################################################################################
########Make new datable for all 5 sites  with updated precip data for subalpine
#####################################################################################
##############include updated subalpine data
#add column for site name
subalpine.snow.avg$Site <- rep("Subalpine",nrow(subalpine.snow.avg))
#rename column for total precip
names(subalpine.snow.avg)[3] <- "TotalPrecip_mm"
TotalsubalpinePrecip_T0toT3 <- sum(subalpine.snow.avg$TotalPrecip_mm)
TotalsubalpinePrecip_T0toT3

#order: Total precip (column 3), month(column 1,), Year (column 2), Site (column 8)
subalpine.precip <- subalpine.snow.avg[ , c(3,1:2,8)]

#subset out just pine oak from previous data table
pineoak.precip2 <- pineoak.sub.precip[which(pineoak.sub.precip$Site =="Pine-oak"), ]
TotalpinePrecip_T0toT3 <- sum(pineoak.precip2$TotalPrecip_mm)
TotalpinePrecip_T0toT3
#combine subalpine,pineoak, desert, scrubland
fivesites.precip <- rbind(desert.scrub.grass.precip,pineoak.precip2,subalpine.precip)

#add in a column for monthyear
fivesites.precip$monthyear <-  paste(fivesites.precip$month,fivesites.precip$Year ,sep =" ")
#put months and years in correct order
fivesites.precip$monthyear <- factor(fivesites.precip$monthyear, levels=monthyearlevels)
length(levels(fivesites.precip$monthyear))
#19 months 
######################################################################################
########Make a figure of total precip by month by site
#####################################################################################

##############put months in proper order
#put the months into chronological instead of alphabetical order for ggplot
fivesites.precip$month <- factor(fivesites.precip$month, levels = c("Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar"))     
levels(fivesites.precip$monthyear )

class(fivesites.precip$TotalPrecip_mm)
names(fivesites.precip)
#put sites in the appropriate order
fivesites.precip$Site<- factor(fivesites.precip$Site, levels = c("Desert","Scrubland","Grassland","Pine-oak","Subalpine"))

#make plot
siteprecip <- ggplot(data=fivesites.precip, aes(x=monthyear, y=TotalPrecip_mm, col=month)) + geom_point(size=2) +  
  labs(x=" ", y="Total Precipitation mm", col="Month") + #change y axis label to "Temp C" and remove "Date" for x axis and change legend title
  theme(strip.text.x = element_text(size = 14, colour = "black"), #Site label names bigger
      axis.text.x=element_text(size=6, angle=70, hjust=1),  #change size angle and justification of x axis labels
      axis.text.y=element_text(size=12),  #make y axis tick sizes bigger
      axis.title=element_text(size=14)) #make y axis label larger  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1)+  #add in standard deviation error bars +
pdf("~/Dropbox/StatsandProgramming/16SElevationGradient/Figures/Precipfigures/fivesites_totalprecip_bymonth.pdf")
siteprecip+ facet_wrap(~Site, ncol=3)
dev.off()


###attempts to add in T0 T1 and T2 T3 points in appropriate locations
#install.packages("grid")
library(grid)
library(ggplot2)


#T0: October 19, 2015 (T0 survey samples and day bags were deployed)
#T1: April 5th 2016
#T2: October 23 and 24, 2016
#T3: April 18 and 19, 2017

 # Create the textGrobs
 text_T0 <- textGrob("T0", gp=gpar(fontsize=13, fontface="bold"))
 text_T1 <- textGrob("T1", gp=gpar(fontsize=13, fontface="bold"))
 text_T2 <- textGrob("T2", gp=gpar(fontsize=13, fontface="bold"))
 text_T3 <- textGrob("T3", gp=gpar(fontsize=13, fontface="bold"))
 
 
 #
 p1 = siteprecip+ facet_wrap(~Site, ncol=3) + 
   theme(plot.margin = unit(c(1,1,2,1), "lines")) +
   annotation_custom(text_T0, xmin=1,xmax=1,ymin=-3,ymax=-3) + 
   annotation_custom(text_T1, xmin=7,xmax=7,ymin=-3,ymax=-3)+
   annotation_custom(text_T2, xmin=13,xmax=13,ymin=-3,ymax=-3)+
   annotation_custom(text_T3, xmin=19,xmax=19,ymin=-3,ymax=-3)
 
 # Code to override clipping
 gt <- ggplotGrob(p1)
 gt$layout$clip[gt$layout$name=="panel"] <- "off"
 grid.draw(gt)
 
 #this didn't work 

 ######################################################################################
 #######Summary precip by total for sites
 #####################################################################################
library(plyr)

fivesites.precip.month<- ddply(fivesites.precip, c("Site", "monthyear"), summarise,
                             sum = sum(TotalPrecip_mm, na.rm=TRUE),
                             n = sum(!is.na(TotalPrecip_mm)))
            

fivesites.precip.month


fivesites.precip.year <- ddply(fivesites.precip, c("Site"), summarise,
                             sum = sum(TotalPrecip_mm, na.rm=TRUE),
                             sd = sd(TotalPrecip_mm, na.rm=TRUE),
                             n = sum(!is.na(TotalPrecip_mm)),
                             #se = sd/sqrt(TotalPrecip_mm),
                             max = max(TotalPrecip_mm, na.rm=TRUE),
                             min = min(TotalPrecip_mm, na.rm=TRUE),
                             diff = (max-min))
fivesites.precip.year

#PLOT JUST the total precip + SE for the entire sampling period  (19 months)
precip <- ggplot(fivesites.precip.year, aes(x=Site, y=sum, col=Site)) + geom_point(size=4) +labs(x="",y="Precipitation mm",col="Site", title ="Total Precipitation from T0 to T3")+
          geom_errorbar(aes(ymin=sum-sd, ymax=sum+sd), width=.1) + #add in standard deviation error bars +
          ylim(0,(TotalpineoakPrecip_T0toT3+100)) + #add in ylims from 0 to pineoak + 100
          theme(axis.text.x=element_text(size=13,angle=70, hjust=1), axis.text.y=element_text(size=13), text = element_text(size=15)) +#change angles and size of x axis labels, y axis, labels
        scale_color_manual(labels=c("Desert","Scrubland","Grassland","Pine-Oak","Subalpine"), #manual labels for legend
                   values=c("red", "orange", "green","blue","purple"))#add in manual colors for points/lines
precip

#pdf("~/Dropbox/StatsandProgramming/16SElevationGradient/Figures/Precipfigures/fivesites_totalprecip.pdf")
precip 
#dev.off()


#write.csv(fivesites.precip.year,"~/Dropbox/StatsandProgramming/16SElevationGradient/results/fivesitestotalprecip_mm.csv")

