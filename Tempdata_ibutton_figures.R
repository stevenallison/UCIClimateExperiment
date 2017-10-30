#September 11, 2017
######WARNING: huge issues with PC/Mac conversions and 1900 vs 1904 times
#####this won't work unless you copy and paste csv files into new mac excel spreadsheet and let it convert
#####I haven't figure out how to code this in R
#figure out how to use i Button data
#Reset R's Brain
rm(list=ls())
#try out ibuttonr: https://github.com/aammd/ibuttonr
### ibuttonr: Functions for cleaning and loading ibutton data into 
par(mfrow=c(1,1))
## Installing ibuttonr
#You can install ibuttonr directly from github using `devtools`:
#install.packages("devtools")
#devtools::install_github("aammd/ibuttonr")
library(ibuttonr)
## Using ibuttonr

# load in required packages -----------------------------------------------

library(tidyverse)
#install.packages("reshape")
library(reshape)
library(lattice)
library(plyr)
library(stringr)
library(chron)
######################################################
########1. Desert Data
######################################################

#upload all of the desert data plot 1, rbind, remove duplicate rows, then average with plot 4
desert.data.plot1.3 <- read.csv("~/Dropbox/elevation temperature data/updatednames/desert_plot1_04-20-17-2.csv", skip=14)
desert.data.plot1.2 <- read.csv("~/Dropbox/elevation temperature data/updatednames/desert_plot1_10-23-16-2.csv", skip=14)
desert.data.plot1.1 <- read.csv("~/Dropbox/elevation temperature data/updatednames/desert_plot1_06-16-2.csv", skip=14)

desert.data.plot4.3 <- read.csv("~/Dropbox/elevation temperature data/updatednames/desert_plot4_04-20-17-2.csv", skip=14)
desert.data.plot4.2 <- read.csv("~/Dropbox/elevation temperature data/updatednames/desert_plot4_10-23-16-2.csv", skip=14)
desert.data.plot4.1 <- read.csv("~/Dropbox/elevation temperature data/updatednames/desert_plot4_06_16-2.csv", skip=14)

#bind together the 3 time points from desert plot 1
desertplot1 <- rbind(desert.data.plot1.1,desert.data.plot1.2,desert.data.plot1.3 )
nrow(desertplot1)
#3696

#find unique rows by a certaim column "date.time"
desertplot1.unique <- unique(desertplot1, by = "Date.Time")
nrow(desertplot1.unique) #2286

#how much data do I have?
#number of rows dividided by 6 samples per day times 30 days a month
nrow(desertplot1.unique)/(6*30) #I have 12.7 months of data
#bind together the 3 time points from desert plot 4
desertplot4 <- rbind(desert.data.plot4.1,desert.data.plot4.2,desert.data.plot4.3 )
nrow(desertplot4)
#3696
plot4.unique <- unique(desertplot4, by = "Date.Time")
nrow(plot4.unique)
#2286

#####################################################################
########Make a function to make a data frame for temperatures
#####################################################################
maketempdataframe <- function(plot1,plot4,sitename){
  #find unique values/remove duplicate dates from plot 1 and plot4
  plot1.unique <- unique(plot1, by = "Date.Time")
  plot4.unique <- unique(plot4, by = "Date.Time")
  #do the rows equal each other?
  nrow(plot1.unique) == nrow(plot4.unique)
  #add in a if else conditional statement to do nothing if the rows are the same but to add the last row of plot 4 to the plot1 if they aren't
  #because what happened in these plots is that there was a sampling time point taken after noon in plot 4 but not in plot1 so there is one less sample
  if(nrow(plot1.unique) != nrow(plot4.unique)) plot1.unique <- rbind(plot1.unique,tail(plot4.unique, n=1)) else plot1.unique <- plot1.unique
  
  #bind together columns for plots 1 and plot 4
  combo.temp <- cbind(plot1.unique,plot4.unique)
  #rename columns
  names(combo.temp) <- c("Date.Time","Unit","Value.Plot1","Date.Time.Plot4","Unit.Plot4","Value.Plot4")
  ###realized that plot 1 is on the :01 minute and plot 4 is on the :02 minute so need to remove the minutes so they can match
  combo.temp.times <- ldply(str_split(string = combo.temp$Date.Time, pattern=":"), rbind) # Divide a column using ";"and convert list to data frame
  combo.temp$Date.Month.Year.Hour <- combo.temp.times[,1 ]
  #now get a column of just the hours
  combo.hours <- ldply(str_split(string = combo.temp$Date.Month.Year.Hour, pattern=" "), rbind) 
  combo.temp$hour <- combo.hours[,2]  
  #get a column of just the dates
  combo.temp$Date <- as.Date(combo.temp$Date.Time, format="%m/%d/%Y")
  #get column of just months
  combo.temp$month <- months(combo.temp$Date, abbreviate=TRUE)
  #get column of just years
  combo.temp$year <- years(combo.temp$Date)
  #get a column for the day of the month
  combo.temp$days <- days(combo.temp$Date)
  #get date and time in a nice format R likes
  combo.temp$DateandTime <- as.POSIXct(combo.temp$Date.Time, format="%m/%d/%Y %H:%M")
  #calculate mean site temperature
  combo.temp$Value.Mean <- (combo.temp$Value.Plot1+combo.temp$Value.Plot4)/2
  #make a day night column
  #make a column that assigns to day or night based on the hour
  #day times are 11am, noon, 3pm, 4pm, night times are everything else
  combo.temp$daynight <- "night"
  combo.temp$daynight[combo.temp$hour %in% c("11","12","15","16")] <- "day"
  #make it a factor instead of a character
  combo.temp$daynight<- as.factor(combo.temp$daynight)
  #get column of just the sites 
  combo.temp$site <- rep(sitename,nrow(combo.temp))
  #make it a factor instead of a character
  combo.temp$site <- as.factor(combo.temp$site)
  #return the dataframe
  return(combo.temp)
}

##apply function to get desert data frame
desert.temp <- maketempdataframe(desertplot1, desertplot4,"Desert")

#figure out classes of everything in desert.temp
sapply(desert.temp, class)

#check if plot 1 and plot 4 are correlated
cor.test(desert.temp$Value.Plot1, desert.temp$Value.Plot4) #99% correlated
plot(desert.temp$Value.Plot1 ~ desert.temp$Value.Plot4)
#check if correlated with mean
cor.test(desert.temp$Value.Plot1, desert.temp$Value.Mean)
#ok plot 1 and plot 2 ae highly correlated
levels(desert.temp$Date.Month.Year.Hour) #switched to 3,7,11,15,19,23 in november and switched back in march
#check plot of mean temp over time
plot(Value.Mean ~ Date, data=desert.temp)
######################################################
########2. scrubland Data
######################################################
#something about the dates being different systems, R doesn't like and doesn't recognize them as being the same - have to figure this out!
#was having a devil of a time with the scrubland data so i finally copied and pasted them in to new excel spread sheets and it converted the dates and did something so they would all be hte same
#i think it's a pc/mac issue with date conversions, no idea how to elegantly deal with this in R
#upload all of the scrubland data plot 1, rbind, remove duplicate rows, then average with plot 4
scrubland.data.plot1.3 <- read.csv("~/Dropbox/elevation temperature data/updatednames/scrubland_plot1_04-20-17-2.csv", skip=14)
scrubland.data.plot1.2 <- read.csv("~/Dropbox/elevation temperature data/updatednames/scrubland_plot1_10-23_16-2.csv", skip=14)
scrubland.data.plot1.1 <- read.csv("~/Dropbox/elevation temperature data/updatednames/scrubland_plot1_06_16-2.csv", skip=14)

scrubland.data.plot4.3 <- read.csv("~/Dropbox/elevation temperature data/updatednames/scrubland_plot4_04-20-17-2.csv", skip=14)
scrubland.data.plot4.2 <- read.csv("~/Dropbox/elevation temperature data/updatednames/scrubland_plot4_10-23-16-2.csv", skip=14)
scrubland.data.plot4.1 <- read.csv("~/Dropbox/elevation temperature data/updatednames/scrubland_plot4_06-16-2.csv", skip=14)
nrow(scrubland.data.plot1.1 ) #427
nrow(scrubland.data.plot4.1) #427
nrow(scrubland.data.plot1.2 ) #1220
nrow(scrubland.data.plot4.2) #1220
nrow(scrubland.data.plot1.3 ) #2048
nrow(scrubland.data.plot4.3) #2048
levels(scrubland.data.plot1.1$Date.Time) #these are on the :02 min 
levels(scrubland.data.plot4.1$Date.Time) #these are on :01 min

#bind together the 3 time points from scrubland plot 1
scrublandplot1 <- rbind(scrubland.data.plot1.1,scrubland.data.plot1.2,scrubland.data.plot1.3 )
#they collected data every 4 hours, so in a day there will be data at 1. 0 (midnight), 2. 4 (am), 3. 8 (am), 4. 12 (noon), 5. 16 (4pm), 6. 20 (8pm)
#so there should be 6 data points per day

#find unique rows by a certaim column "date.time"
scrublandplot1.unique <- unique(scrublandplot1, by = "Date.Time")
nrow(scrublandplot1.unique)
tail(scrublandplot1.unique)
#2286

#bind together the 3 time points from scrubland plot 4
scrublandplot4 <- rbind(scrubland.data.plot4.1,scrubland.data.plot4.2,scrubland.data.plot4.3 )
nrow(scrublandplot4)

#get the unique dates and hours in scrubland plot 4
scrublandplot4.unique <- unique(scrublandplot4, by = "Date.Time")
nrow(scrublandplot4.unique) #there are 2287 here
tail(scrublandplot4.unique) #it;s because a noon sample was collected in plot4 but not in plot 1 on the last sampling day 4/20/1


########apply function to get scrubland dataframe
##apply function to get scrublanddata frame
scrubland.temp <- maketempdataframe(scrublandplot1, scrublandplot4,"Scrubland")

#figure out classes of everything in scrubland.temp
sapply(scrubland.temp, class)

#check if plot 1 and plot 4 are correlated
cor.test(scrubland.temp$Value.Plot1, scrubland.temp$Value.Plot4) #97.5% correlated
plot(scrubland.temp$Value.Plot1 ~ scrubland.temp$Value.Plot4)
#check if correlated with mean
cor.test(scrubland.temp$Value.Plot1, scrubland.temp$Value.Mean) #99% correlated with mean

levels(scrubland.temp$Date.Month.Year.Hour) #switched to 3,7,11,15,19,23 in november and switched back in march between 3/11/17 and 3/12/17

length(levels(scrubland.temp$Date.Month.Year.Hour)) #2287 - addded the noon sampling from 4/20/17 in plot 4 to plot1

tail(scrubland.temp)

plot(Value.Mean ~ Date, data=scrubland.temp)


######################################################
########3. grassland Data
######################################################

#upload all of the grassland data plot 1, rbind, remove duplicate rows, then average with plot 4
grassland.data.plot1.3 <- read.csv("~/Dropbox/elevation temperature data/updatednames/grassland_plot1_04-20-17-2.csv", skip=14)
grassland.data.plot1.2 <- read.csv("~/Dropbox/elevation temperature data/updatednames/grassland_plot1_10-23-16-2.csv", skip=14)
#grassland.data.plot1.1 <- read.csv("~/Dropbox/elevation temperature data/updatednames/grassland_plot1_06_16.csv", skip=14)
#this file doesn't exist for grassland

grassland.data.plot4.3 <- read.csv("~/Dropbox/elevation temperature data/updatednames/grassland_plot4_4-20-17-2.csv", skip=14)
grassland.data.plot4.2 <- read.csv("~/Dropbox/elevation temperature data/updatednames/grassland_plot4_10-23-16-2.csv", skip=14)
#grassland.data.plot4.1 <- read.csv("~/Dropbox/elevation temperature data/updatednames/grassland_plot4_06_16.csv", skip=14)

#bind together the 3 time points from grassland plot 1
grasslandplot1 <- rbind(grassland.data.plot1.2,grassland.data.plot1.3 )

#bind together the 3 time points from grassland plot 4
grasslandplot4 <- rbind(grassland.data.plot4.2,grassland.data.plot4.3 )

########apply function to get grassland dataframe
##apply function to get grassland data frame
grassland.temp <- maketempdataframe(grasslandplot1, grasslandplot4,"Grassland")

#figure out classes of everything in grassland temp
sapply(grassland.temp, class)

#check if plot 1 and plot 4 are correlated
cor.test(grassland.temp$Value.Plot1, grassland.temp$Value.Plot4) #97.6% correlated
plot(grassland.temp$Value.Plot1 ~ grassland.temp$Value.Plot4)
#check if correlated with mean
cor.test(grassland.temp$Value.Plot1, grassland.temp$Value.Mean) #99% correlated with mean


levels(grassland.temp$Date.Month.Year.Hour) #switched to 3,7,11,15,19,23 in november and switched back in march between 3/11/17 and 3/12/17

tail(grassland.temp) #no noon sampling on 4/20/17

plot(Value.Mean ~ Date, data=grassland.temp)


######################################################
########4. Pine-oak Data
######################################################

#upload all of the pineoak data plot 1, rbind, remove duplicate rows, then average with plot 4
pineoak.data.plot1.3 <- read.csv("~/Dropbox/elevation temperature data/updatednames/pineoak_plot1_04-20-17-2.csv", skip=14)
pineoak.data.plot1.2 <- read.csv("~/Dropbox/elevation temperature data/updatednames/pineoak_plot1_10-23-16-2.csv", skip=14)
pineoak.data.plot1.1 <- read.csv("~/Dropbox/elevation temperature data/updatednames/pineoak_plot1_06_16-2.csv", skip=14)

pineoak.data.plot4.3 <- read.csv("~/Dropbox/elevation temperature data/updatednames/pineoak_plot4_04-20-17-2.csv", skip=14)
pineoak.data.plot4.2 <- read.csv("~/Dropbox/elevation temperature data/updatednames/pineoak_plot4_10-23-16-2.csv", skip=14)
pineoak.data.plot4.1 <- read.csv("~/Dropbox/elevation temperature data/updatednames/pineoak_plot4_06-16-2.csv", skip=14)

#bind together the 3 time points from pineoak plot 1
pineoakplot1 <- rbind(pineoak.data.plot1.1,pineoak.data.plot1.2,pineoak.data.plot1.3 )

#bind together the 3 time points from pineoak plot 4
pineoakplot4 <- rbind(pineoak.data.plot4.1,pineoak.data.plot4.2,pineoak.data.plot4.3 )

tail(pineoakplot4) #difference is plot 4 has 4 values for 4/20
tail(pineoakplot1) #difference is plot one only has 3 values for 4/20 


########apply function to get pineoak dataframe
##apply function to get pineoak data frame
pineoak.temp <- maketempdataframe(pineoakplot1, pineoakplot4,"Pine-oak")

#figure out classes of everything in pineoak .temp
sapply(pineoak.temp, class)

#check if plot 1 and plot 4 are correlated
cor.test(pineoak.temp$Value.Plot1, pineoak.temp$Value.Plot4) #93% correlated
plot(pineoak.temp$Value.Plot1 ~ pineoak.temp$Value.Plot4) #plots vary quite widely at higher tempers
#check if correlated with mean
cor.test(pineoak.temp$Value.Plot1, pineoak.temp$Value.Mean) #98.5% correlated with mean


levels(pineoak.temp$Date.Month.Year.Hour) #switched to 3,7,11,15,19,23 in november and switched back in march between 3/11/17 and 3/12/17

tail(pineoak.temp) #no noon sampling on 4/20/17

plot(Value.Mean ~ Date, data=pineoak.temp)

######################################################
########5. subalpine Data
######################################################

#upload all of the subalpine data plot 1, rbind, remove duplicate rows, then average with plot 4
subalpine.data.plot1.3 <- read.csv("~/Dropbox/elevation temperature data/updatednames/subalpine_plot1_04-20-17-2.csv", skip=14)
subalpine.data.plot1.2 <- read.csv("~/Dropbox/elevation temperature data/updatednames/subalpine_plot1_10-23-16-2.csv", skip=14)
subalpine.data.plot1.1 <- read.csv("~/Dropbox/elevation temperature data/updatednames/subalpine_plot1_06_16-2.csv", skip=14)

#had to go in and open up a new excel sheet, copy and paste to a new spread sheet, it converted, then saved
subalpine.data.plot4.3 <- read.csv("~/Dropbox/elevation temperature data/updatednames/subalpine_plot4_04-20-17-2.csv", skip=14)
subalpine.data.plot4.2 <- read.csv("~/Dropbox/elevation temperature data/updatednames/subalpine_plot4_10-23-16-2.csv", skip=14)
subalpine.data.plot4.1 <- read.csv("~/Dropbox/elevation temperature data/updatednames/subalpine_plot4_06_16-2.csv", skip=14)

#bind together the 3 time points from subalpine plot 1
subalpineplot1 <- rbind(subalpine.data.plot1.1,subalpine.data.plot1.2,subalpine.data.plot1.3 )

#bind together the 3 time points from subalpine plot 4
subalpineplot4 <- rbind(subalpine.data.plot4.1,subalpine.data.plot4.2,subalpine.data.plot4.3 )

########apply function to get subalpine dataframe
##apply function to get subalpine data frame
subalpine.temp <- maketempdataframe(subalpineplot1, subalpineplot4,"Subalpine")

#figure out classes of everything in subalpine .temp
sapply(subalpine.temp, class)

#check if plot 1 and plot 4 are correlated
cor.test(subalpine.temp$Value.Plot1, subalpine.temp$Value.Plot4) #93.6% correlated
plot(subalpine.temp$Value.Plot1 ~ subalpine.temp$Value.Plot4) #plots vary a lot - in plot1 there was a long time where it was 0 while plot 4 was higher
#check if correlated with mean
cor.test(subalpine.temp$Value.Plot1, subalpine.temp$Value.Mean) #98.7% correlated with mean
plot(subalpine.temp$Value.Plot1 ~ subalpine.temp$Value.Mean) #looks better

tail(subalpine.temp) #both had noon sampling on 4/20/17 

plot(Value.Mean ~ Date, data=subalpine.temp)


########################################################################################################################################################################
###########combine data frames into one dataframe and make a facet plot of all 5 in one - this is a million times easier than anything that I tried below: https://plot.ly/ggplot2/facet/
########################################################################################################################################################################
#check that names are the same
names(subalpine.temp)==names(desert.temp)
names(grassland.temp)==names(pineoak.temp)
names(desert.temp)==names(scrubland.temp)

#write these as csv files
#write.csv(fivesites.temp, "~/Dropbox/StatsandProgramming/16SElevationGradient/data/5sitestemperature.csv")
#write.csv(desert.temp, "~/Dropbox/StatsandProgramming/16SElevationGradient/data/deserttemperature.csv")
#write.csv(scrubland.temp, "~/Dropbox/StatsandProgramming/16SElevationGradient/data/scrublandtemperature.csv")
#write.csv(grassland.temp, "~/Dropbox/StatsandProgramming/16SElevationGradient/data/grasslandtemperature.csv")
#write.csv(pineoak.temp, "~/Dropbox/StatsandProgramming/16SElevationGradient/data/pineoaktemperature.csv")
#write.csv(subalpine.temp, "~/Dropbox/StatsandProgramming/16SElevationGradient/data/subalpinetemperature.csv")

fivesites.temp <- rbind(desert.temp,scrubland.temp,grassland.temp,pineoak.temp,subalpine.temp)


#add 20 in front of the year to get 2016 and 2017, with no separation
fivesites.temp$year2 <-  paste("20",fivesites.temp$year, sep="")
#make a column of month and year with a separation
fivesites.temp$monthyear <-  paste(fivesites.temp$month,fivesites.temp$year2, sep=" ")

fivesites.temp$monthyear <- as.factor(fivesites.temp$monthyear)

length(levels(fivesites.temp$monthyear)) #only 13 months - missing data from T0 to T1 - don't have Oct 2015 to April 2016 - did we only deploy ibuttons at T1?

#make vector of months and years in appropriate order
monthyearlevels <- c( "Apr 2016","May 2016","Jun 2016","Jul 2016","Aug 2016","Sep 2016","Oct 2016","Nov 2016","Dec 2016",
                      "Jan 2017","Feb 2017","Mar 2017", "Apr 2017")

#put them in appropriate order
fivesites.temp$monthyear <- factor(fivesites.temp$monthyear, levels=monthyearlevels)
#put sites into correct order for ggplot
fivesites.temp$site<- factor(fivesites.temp$site, levels = c("Desert","Scrubland","Grassland","Pine-oak","Subalpine"))     
#put the months into chronological instead of alphabetical order for ggplot
fivesites.temp$month <- factor(fivesites.temp$month, levels = c("Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar"))     

fivesites.temp$alpha <- ifelse(fivesites.temp$daynight == "day",1,0.7)
fivesites.temp$alpha  <- factor(fivesites.temp$alpha )

##################################################################
###########Make face plot colored by month 
##################################################################
sp <- ggplot(fivesites.temp, aes(x=Date, y=Value.Mean, col=monthyear)) + geom_point(size=0.5) +  
  labs(x=" ", y="Temperature C", col="Month") + #change y axis label to "Temp C" and remove "Date" for x axis and change legend title
  theme(axis.text.x=element_text(size=10,angle=70, hjust=1)) #change angles and size of x axis labels

#pdf("~/Dropbox/StatsandProgramming/16SElevationGradient/Figures/Temperature/temperature_months_5sites_facet.pdf", height=5, width=7)
sp + facet_wrap( ~ site, ncol=3)
#dev.off()

##################################################################
###########Make face plot colored by month and with transparency for day night
##################################################################
sp <- ggplot(fivesites.temp, aes(x=Date, y=Value.Mean, col=monthyear, alpha=alpha)) + geom_point(size=0.5) +  
        labs(x=" ", y="Temperature C", col="Month", alpha="Night vs Day") + #change y axis label to "Temp C" and remove "Date" for x axis and change legend title
        theme(axis.text.x=element_text(size=10,angle=70, hjust=1)) #change angles and size of x axis labels
# Divide by levels of "site", in the vertical direction
#pdf("~/Dropbox/StatsandProgramming/16SElevationGradient/Figures/Temperature/temperature_months_5sites_facet_transparency.pdf", height=5, width=7)
# Divide by site, going horizontally and wrapping with 3 columns
sp + facet_wrap( ~ site, ncol=3)
#dev.off()



##################################################################
###########Make face plot colored by day nand month
##################################################################

daynight <- ggplot(fivesites.temp, aes(x=Date, y=Value.Mean, col=daynight)) + geom_point(size=0.5) +  
  labs(x=" ", y="Temperature C", col="") + theme(axis.text.x=element_text(size=10,angle=70, hjust=1))
daynight + facet_grid(site ~ .)

# Divide by levels of "site", in the vertical direction
#pdf("~/Dropbox/StatsandProgramming/16SElevationGradient/Figures/Temperature/temperature_daynight_5sites_facet.pdf", height=5, width=7)
# Divide by site, going horizontally and wrapping with 3 columns
daynight + facet_wrap( ~ site, ncol=3)
#dev.off()


############################################################################################################
########dplylr and delving deeper into data - what are avgs per site by week, by month, etc
############################################################################################################
# 1.5. Calculate the means, sd, n, and se.
library(plyr)
names(fivesites.temp)

head(fivesites.temp)

fivesites.temp.year <- ddply(fivesites.temp, c("site"), summarise,
                             mean = mean(Value.Mean, na.rm=TRUE),
                             sd = sd(Value.Mean, na.rm=TRUE),
                             n = sum(!is.na(Value.Mean)),
                             se = sd/sqrt(n),
                             max = max(Value.Mean, na.rm=TRUE),
                             min = min(Value.Mean, na.rm=TRUE),
                             diff = (max-min))

fivesites.temp.year

write.csv(fivesites.temp.year,"~/Dropbox/StatsandProgramming/16SElevationGradient/fivesitestemp2016-17.csv")
#PLOT JUST the average temps + SE for the entire sampling period  (12.7 months)
temp <- ggplot(fivesites.temp.year, aes(x=site, y=mean, col=site)) + geom_point(shape=9, size=1)+
        labs(x="",y="Temperature C",col="Site")+
        geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1)+
       scale_color_manual(labels=c("Desert","Scrubland","Grassland","Pine-Oak","Subalpine"), #manual labels for legend
                   values=c("red", "orange", "green","blue","purple"))#add in manual colors for points/lines

#plot total average tempearture at each site for 13 months 
temp

#add in max and min

temp +   geom_point(data=fivesites.temp.year,aes(x=site,y=max,col=site),size=3) + #add in the max
  geom_point(data=fivesites.temp.year,aes(x=site,y=min,col=site),size=3) #add in the min



####now get day night diffs
fivesites.temp.daynight <- ddply(fivesites.temp, c("site","daynight"), summarise,
                                 mean = mean(Value.Mean, na.rm=TRUE),
                                 sd = sd(Value.Mean, na.rm=TRUE),
                                 n = sum(!is.na(Value.Mean)),
                                 se = sd/sqrt(n)
)

fivesites.temp.daynight

###plot all 5 sites with avg day night temps
ggplot(fivesites.temp.daynight, aes(x=site, y=mean, col=daynight)) + geom_point(shape=9, size=1)+
  labs(x="",y="Temperature C",col="Time of Day")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1)

####now get day night diffs by month
fivesites.temp.month.daynight<- ddply(fivesites.temp, c("site","monthyear","daynight"), summarise,
                                      mean = mean(Value.Mean, na.rm=TRUE),
                                      sd = sd(Value.Mean, na.rm=TRUE),
                                      n = sum(!is.na(Value.Mean)),
                                      se = sd/sqrt(n),
                                      max = max(Value.Mean, na.rm=TRUE),
                                      min = min(Value.Mean, na.rm=TRUE),
                                      diff = (max-min))

fivesites.temp.month.daynight

############################################################################################################
########make a facet plot colored by day night with mean and SE or sd for day night
############################################################################################################
#put in correct order
fivesites.temp.month.daynight$monthyear <- factor(fivesites.temp.month.daynight$monthyear, levels=monthyearlevels)


meantemp_se <- ggplot(fivesites.temp.month.daynight, aes(x=monthyear, y=mean, col=daynight)) + geom_point(size=2)+
  labs(x="",y="Temperature C",col="Time of Day")+
  theme(axis.text.x=element_text(size=8,angle=70, hjust=1))+ #change angles and size of x axis labels
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5) #add in error bars

meantemp_se + facet_wrap(~site, ncol=3)

meantemp_sd <- ggplot(fivesites.temp.month.daynight, aes(x=monthyear, y=mean, col=daynight)) + geom_point(size=2)+
  labs(x="",y="Temperature C",col="Time of Day")+
  theme(axis.text.x=element_text(size=8,angle=70, hjust=1))+ #change angles and size of x axis labels
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.5) #add in error bars

meantemp_sd + facet_wrap(~site, ncol=3)



# Divide by levels of "site", in the vertical direction
pdf("~/Dropbox/StatsandProgramming/16SElevationGradient/Figures/Temperature/temperature_months_daynightmeanplusSD_5sites_facet.pdf", height=5, width=7)
# Divide by site, going horizontally and wrapping with 3 columns
meantemp_sd + facet_wrap(~site, ncol=3)
dev.off()


pdf("~/Dropbox/StatsandProgramming/16SElevationGradient/Figures/Temperature/temperature_months_daynightmeanplusSE_5sites_facet.pdf", height=5, width=7)
# Divide by site, going horizontally and wrapping with 3 columns
meantemp_se + facet_wrap(~site, ncol=3)
dev.off()

############################################################################################################
########Examine number of days below zero
############################################################################################################
##############divide sites by day
fivesites.temp.day.daynight<- ddply(fivesites.temp, c("site","monthyear","days","daynight"), summarise,
                                    mean = mean(Value.Mean, na.rm=TRUE),
                                    sd = sd(Value.Mean, na.rm=TRUE),
                                    n = sum(!is.na(Value.Mean)),
                                    se = sd/sqrt(n))

daysbelowzero <- ddply(fivesites.temp, c("site","monthyear"), summarise,
                       samplesatorbelowzero = length(which(Value.Mean<= 0)),
                       mean = mean(Value.Mean, na.rm=TRUE),
                       sd = sd(Value.Mean, na.rm=TRUE),
                       n = sum(!is.na(Value.Mean)),
                       se = sd/sqrt(n),
                       daysatorbelowzero = 30*samplesatorbelowzero/n)
#find out max and min
max(daysbelowzero$daysatorbelowzero)
min(daysbelowzero$daysatorbelowzero)

colddays <- ggplot(daysbelowzero, aes(x=monthyear, y=daysatorbelowzero, col=site)) +
  geom_point(size=1)+scale_y_continuous(limits =c(0.1,30))+ 
  labs(x="",y="Number of days below 0 C",col="Site")+
  theme(axis.text.x=element_text(size=10,angle=70, hjust=1))+ #change angles and size of x axis labels
  scale_color_manual(labels=c("Desert","Scrubland","Grassland","Pine-Oak","Subalpine"), #manual labels for legend
                   values=c("red", "orange", "green","blue","purple"))#add in manual colors for points/lines

pdf("~/Dropbox/StatsandProgramming/16SElevationGradient/Figures/Temperature/daysbelowzerobytsite.pdf", height=5, width=7)
colddays
#the warning is because I'm setting it as slightly above zero so it removes all the days with values of 0
dev.off()


############################################################################################################
########check out temp ranges across the sites - then avg by day by month by day, by night, by time of day and look at flux etc
############################################################################################################

####check out ranges of temperatures across sites
range(subalpine.temp$Value.Mean)
max(subalpine.temp$Value.Mean) - min(subalpine.temp$Value.Mean) #46
range(desert.temp$Value.Mean)
max(desert.temp$Value.Mean) - min(desert.temp$Value.Mean) #55.75
range(grassland.temp$Value.Mean)
max(grassland.temp$Value.Mean) - min(grassland.temp$Value.Mean) #57.75
range(pineoak.temp$Value.Mean)
max(pineoak.temp$Value.Mean) - min(pineoak.temp$Value.Mean) #47.75
range(scrubland.temp$Value.Mean)
max(scrubland.temp$Value.Mean) - min(scrubland.temp$Value.Mean) #55.5

#desert and scrubland have same range throughout the year. grassland has the highest range. pine oak and subalpine have lower ranges
meantemps <- c(mean(desert.temp$Value.Mean), mean(scrubland.temp$Value.Mean),mean(grassland.temp$Value.Mean),mean(pineoak.temp$Value.Mean),mean(subalpine.temp$Value.Mean))
sdtemps <- c(sd(desert.temp$Value.Mean), sd(scrubland.temp$Value.Mean),sd(grassland.temp$Value.Mean),sd(pineoak.temp$Value.Mean),sd(subalpine.temp$Value.Mean))
maxtemps <- c(max(desert.temp$Value.Mean), max(scrubland.temp$Value.Mean),max(grassland.temp$Value.Mean),max(pineoak.temp$Value.Mean),max(subalpine.temp$Value.Mean))
mintemps <- c(min(desert.temp$Value.Mean), min(scrubland.temp$Value.Mean),min(grassland.temp$Value.Mean),min(pineoak.temp$Value.Mean),min(subalpine.temp$Value.Mean))

sitenames <- c("desert","scrubland","grassland","pine-oak","subalpine")
siteorder <- c(1,2,3,4,5)
colors <- c("red","orange","green","blue","purple")
flux <- maxtemps -mintemps

#make a dataframe
tempranges <- cbind(sitenames,maxtemps,mintemps,flux, meantemps,sdtemps,siteorder, colors)
tempranges <- as.data.frame(tempranges)

#change everything to appropriate factors/numerics/etc
tempranges$siteorder <- as.factor(tempranges$siteorder)
tempranges$colors <- as.character(tempranges$colors)
tempranges$maxtemps <- as.numeric(as.character(tempranges$maxtemps))
tempranges$mintemps <- as.numeric(as.character(tempranges$mintemps))
tempranges$flux <- as.numeric(as.character(tempranges$flux))
tempranges$meantemps <- as.numeric(as.character(tempranges$meantemps))
tempranges$sdtemps <- as.numeric(as.character(tempranges$sdtemps))

tempranges


pdf("~/Dropbox/StatsandProgramming/16SElevationGradient/Figures/Temperature/temperaturerangesacross5sites.pdf", width=7, height=7)
plot(flux ~ siteorder, pch=9, ylim=c(-10,60), ylab= "Temperature C", xlab="")
lines(flux ~ siteorder, data=tempranges,pch=16, col="black")

#add on minimum temps in blue
points(mintemps ~ siteorder, data=tempranges,pch=16, col="blue")
lines(mintemps ~ siteorder, data=tempranges,pch=16, col="blue")
#add on max temps in red
points(maxtemps ~ siteorder, data=tempranges,pch=16, col="red")
lines(maxtemps ~ siteorder, data=tempranges,pch=16, col="red")

#add on averages
points(meantemps ~ siteorder, data=tempranges,pch=15, col=colors)
#put in standard error/deviation bars for averages
x <- c(1,2,3,4,5)
y <- tempranges$meantemps
sd <- tempranges$sdtemps
#use arrows command to put in SE bars
arrows(x,y-sd,x,y+sd, code=3, length=0.02, angle = 90, col=tempranges$colors)


#add on names
mtext("Desert", side=1, line=2, adj=0)
mtext("Scrubland", side=1, line=2, adj=0.25)
mtext("Grassland", side=1, line=2, adj=0.5)
mtext("Pine-Oak", side=1, line=2, adj=0.75)
mtext("Subalpine", side=1, line=2, adj=1)

legend("topright", legend=c("Maximum","Minimum","Flux (1.5 yrs)","Mean + SD") ,pch=c(16,16,9,15), col=c("red","blue","black","black"), lty=1)
dev.off()

write.csv(tempranges,"~/Dropbox/StatsandProgramming/16SElevationGradient/results/temperatureranges.csv")

##############################################################################################################
###########Making plots less efficiently - cow plot, grid extra, etc - exploring different options for plotting 
###################################################################################################
##################################################################
###########ggplot figures functions
##################################################################
tempbydateplot <-function(temps,nameofsite){
  #order month levels appropriately
  temps$month <- factor(temps$month, levels = c("Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar"))     
  ggplot(temps, aes(x=Date, y=Value.Mean, col=month)) + 
    geom_point(size=0.5) +
    #geom_point(size=0.5, show.legend = FALSE) + #make point size smaller and don't show legend
    scale_fill_brewer(palette = "Paired")+    #When you use scale_fill_brewer(palette = "Paired"), the maximum is 12, or 13 ("other phylum" is not colored)
    labs(x=" ", y="Temperature C") + theme_bw()+
    theme(axis.text.x=element_text(size=10,angle=30, hjust=1),
          axis.text.y=element_text(size=10),
          legend.text = element_text(size=10))+
    scale_y_continuous(limits = c(-10, 60))+ #set same y axes forall
  ggtitle(nameofsite) #add in plot title
}

tempbydaynightplot <-function(temps,nameofsite){
  #order month levels appropriately
  ggplot(temps, aes(x=Date, y=Value.Mean, col=daynight)) + 
    geom_point(size=0.5) +
    scale_fill_brewer(palette = "Paired")+    #When you use scale_fill_brewer(palette = "Paired"), the maximum is 12, or 13 ("other phylum" is not colored)
    labs(x="", y="Temperature C") + theme_bw()+
    theme(axis.text.x=element_text(size=10,angle=30, hjust=1),
          axis.text.y=element_text(size=10),
          legend.text = element_text(size=10))+
    scale_y_continuous(limits = c(-10, 60))+ #set same y axes forall
  ggtitle(nameofsite) #add in plot title
}

############################################################################################################
########Make plots of temperature against time by month
############################################################################################################
p1 <- tempbydateplot(desert.temp,"Desert")
#plot scrubland temp 
p2 <- tempbydateplot(scrubland.temp,"Scrubland")
#plot grassland temp
p3 <- tempbydateplot(grassland.temp,"Grassland")
p3 
#plot pineoak temp 
p4 <- tempbydateplot(pineoak.temp,"Pine-Oak")
#ggsave(tempbydateplot(desert.temp),filename=paste("~/Dropbox/StatsandProgramming/16SElevationGradient/Figures/tempbydate_desert.pdf"),width=5, height=3.5)
#plot subalpine temp 
p5<- tempbydateplot(subalpine.temp,"Subalpine")

#################################################################################################################################
########solution to make a common legend for all 5 plots: https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots
#################################################################################################################################
library(grid)

grid_arrange_shared_legend <- function(..., nrow = 1, ncol = length(list(...)), position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
  gl <- c(gl, nrow = nrow, ncol = ncol)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)
  
}


pdf("~/Dropbox/StatsandProgramming/16SElevationGradient/Figures/Temperature/temperature_months_5sites_gridarrange.pdf", height=5,width=8, pointsize=5)
grid_arrange_shared_legend(p1,p4, p2, p5,p3, nrow = 3, ncol = 2)
dev.off()



#################################################################################################################################
########solution to make a common legend for all 5 plots: using cowplot: https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots

#################################################################################################################################
# load cowplot
library(cowplot)

#order month levels appropriately
desert.temp$month <- factor(desert.temp$month, levels = c("Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar"))    
scrubland.temp$month <- factor(scrubland.temp$month, levels = c("Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar"))  
grassland.temp$month <- factor(grassland.temp$month, levels = c("Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar"))     
pineoak.temp$month <- factor(pineoak.temp$month, levels = c("Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar"))     
subalpine.temp$month <- factor(subalpine.temp$month, levels = c("Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar"))     


#make 5 plots - remove y axis from all but desert and pine oak and change margin settings
p1 <- ggplot(desert.temp, aes(x=Date, y=Value.Mean, col=month)) + geom_point(size=0.5) + scale_fill_brewer(palette = "Paired")+ labs(x=" ", y="Temperature C") + theme_bw()+
      scale_y_continuous(limits = c(-10, 60))+ ggtitle("Desert")+theme(axis.text.x=element_text(size=10,angle=30, hjust=1), plot.margin = unit(c(4,2,3,2), "pt"))
p2 <- ggplot(scrubland.temp, aes(x=Date, y=Value.Mean, col=month)) + geom_point(size=0.5) + scale_fill_brewer(palette = "Paired")+ labs(x=" ", y=" ") + theme_bw()+
  scale_y_continuous(limits = c(-10, 60))+ ggtitle("Scrubland")+theme(axis.text.x=element_text(size=10,angle=30, hjust=1),plot.margin = unit(c(4,1,3,2), "pt"))
p3 <- ggplot(grassland.temp, aes(x=Date, y=Value.Mean, col=month)) + geom_point(size=0.5) + scale_fill_brewer(palette = "Paired")+ labs(x=" ", y=" ") + theme_bw()+
  scale_y_continuous(limits = c(-10, 60))+ ggtitle("Grassland")+theme(axis.text.x=element_text(size=10,angle=30, hjust=1),plot.margin = unit(c(4,1,3,2), "pt"))
p4 <- ggplot(pineoak.temp, aes(x=Date, y=Value.Mean, col=month)) + geom_point(size=0.5) + scale_fill_brewer(palette = "Paired")+ labs(x=" ", y="Temperature C ") + theme_bw()+
  scale_y_continuous(limits = c(-10, 60))+ ggtitle("Pine-Oak")+theme(axis.text.x=element_text(size=10,angle=30, hjust=1),plot.margin = unit(c(4,1,3,2), "pt"))
p5 <- ggplot(subalpine.temp, aes(x=Date, y=Value.Mean, col=month)) + geom_point(size=0.5) + scale_fill_brewer(palette = "Paired")+ labs(x=" ", y="") + theme_bw()+
  scale_y_continuous(limits = c(-10, 60))+ ggtitle("Subalpine")+theme(axis.text.x=element_text(size=10,angle=30, hjust=1),plot.margin = unit(c(4,1,3,2), "pt"))

#margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")

# arrange the three plots in a single rowmargin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
prow <- plot_grid( p1 + theme(legend.position="none"),
                   p2 + theme(legend.position="none"),
                   p3 + theme(legend.position="none"),
                   p4 + theme(legend.position="none"),
                   p5 + theme(legend.position="none"),
                   align = 'vh',
                   #labels = c("Desert", "Scrubland", "Grassland","Pine-Oak","Subalpine"),
                   hjust = -1,
                   nrow = 2,ncol=3
)


# extract the legend from one of the plots
# (clearly the whole thing only makes sense if all plots
# have the same legend, so we can arbitrarily pick one.)
legend_b <- get_legend(p1 + theme(legend.position="bottom"))

# add the legend underneath the row we made earlier. Give it 10% of the height
# of one plot (via rel_heights).
p <- plot_grid( prow, legend_b, nrow=2, rel_heights = c(1, .2))

pdf("~/Dropbox/StatsandProgramming/16SElevationGradient/Figures/Temperature/temperature_months_5sites_cowplot.pdf", height=5,width=8, pointsize=5)
p
dev.off()



