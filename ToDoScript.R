#R script for CERI visualization

#Reading in data
#first add new colum in excel (threshold score/IV)
install.packages("xlsx")
library(xlsx)
CERIData2 <- read.csv(file.choose(), header =T)
CERIData2 <-read_excel("SESYNCTrainingQuantitative.xlsx",
    sheet=1,
    na= c("No threshold", "No indicator value",
      "No threshold or indicator value","Score not yet assigned",
      "to be assigned", "Not relevant", "<NA>", "N/A"))
##NA strings doesn't work
install.packages("readxl")
library(readxl)

#column comparison
# in order to compare data frames, the columns that we are comparing must have the same name. So I changed the name of the column in CERIData from "X2" to "Threshold.Score".
colnames(CERIData)[27] <- "Threshold.Score"

library(dplyr)
anti_join(Threshold_Score_Col, CERIData2, by = "Threshold.Score")

#machine 1
library(dplyr)
Threshold_Score_Col <- select(CERIData, Threshold.Score)
save(Threshold_Score_Col, file= "Threshold_Score_Col.Rdata")

#machine 2
load("Threshold_Score_Col.Rdata")
library(dplyr)
select(CERIData, Threshold.Score)== Threshold_Score_Col
st
str(CERIData) 
as.factor(...levels...)
rename?columns

##subset/group/filter/aggregate
library(dplyr)

#Threshold.score has: No threshold, No indicator value, No threshold or indicator value
#Participant.Score..1.4. has: Score not yet assigned, to be assigned, Not relevant, <NA>, N/A
SelectCERIData <- select(CERIData2, Sector, Threshold.Score, Participant.Score..1.4.)




#use in operator to find string and convert to NA
<<<<<<< HEAD
SelectCERIData$Threshold.Score[CERIData$Threshold.Score== "No threshold"]<- "NA"
SelectCERIData$Threshold.Score[CERIData$Threshold.Score== "No indicator value"]<- "NA"
SelectCERIData$Threshold.Score[CERIData$Threshold.Score== "No threshold or indicator value"]<- "NA"
SelectCERIData$Participant.Score..1.4.[CERIData$Participant.Score..1.4.=="Score not yet assigned"]<- "NA"
SelectCERIData$Participant.Score..1.4.[CERIData$Participant.Score..1.4.=="to be assigned"]<- "NA"
SelectCERIData$Participant.Score..1.4.[CERIData$Participant.Score..1.4.=="Not relevant"]<- "NA"
SelectCERIData$Participant.Score..1.4.[CERIData$Participant.Score..1.4.=="<NA>"]<- "NA"
SelectCERIData$Participant.Score..1.4.[CERIData$Participant.Score..1.4.=="N/A"]<- "NA"

##subset out rows without NAs
SelectCERIData_NA <- na.omit(SelectCERIData)
##nope
=======
SelectCERIData[SelectCERIData == "No indicator value"] <- NA
SelectCERIData[SelectCERIData == "No threshold"] <- NA #for some reason this is not converting all the "No threshold"s. see row 62. 
SelectCERIData[SelectCERIData == "No indicator value"] <- NA
SelectCERIData[SelectCERIData == "No threshold or indicator value"]<- NA
SelectCERIData[SelectCERIData == "Score not yet assigned"]<- NA
SelectCERIData[SelectCERIData == "to be assigned"]<- NA
SelectCERIData[SelectCERIData =="Not relevant"]<- NA
SelectCERIData[SelectCERIData =="N/A"]<- NA
SelectCERIData[SelectCERIData == ""] <- NA

# to omit all rows with NA

omit_NA <- na.omit(SelectCERIData)
>>>>>>> 1c31e5323e3148f0917fb37d0e30cbcd81ea04cf

#plot
library(ggplot2)

#a plot of threshold scores vs. participant scores (still including NA values)
CERIPlot1 <- ggplot(data = SelectCERIData, 
    aes(x = Threshold.Score, 
      y = Participant.Score..1.4., color= Sector))+
    geom_point()
<<<<<<< HEAD
##exclude NAs from plot?



=======
# a plot of threshold scores vs. participant scores (omitting NA values)
CERIPlot2 <- ggplot(data = omit_NA, 
                    aes(x = Threshold.Score, 
                        y = Participant.Score..1.4., 
                        color = Sector)) +
                        geom_point()

#the problem with the above plots is that some of the data points overlap so not every point is visible.For example, in the omit_NA plot, there should be 42 points but only 11 are visible on the plot.

#plot the mean of each sector for each column of the omit_NA df. 
#To do this, the first step is to convert the factor variables to numeric.So far I haven't been successful with this.  

num.threshold.score <- as.numeric(as.character(omit_NA$Threshold.Score))
num.participant.score <- as.numeric(as.character(omit_NA$Participant.Score..1.4.))
transmute(omit_NA, num.Threshold.Score = num.threshold.score, num.participant.score)

#To create separate data frames for each sector
Economy_df <- filter(omit_NA, Sector == "Economy")
Energy_df <-filter(omit_NA, Sector == "Energy")
Landuse_df <- filter(omit_NA, Sector == "Land Use/Land Cover")
Naturalenv_df <- filter(omit_NA, Sector == "Natural Environment")
People_df <- filter(omit_NA, Sector == "People")
Trans_df <- filter(omit_NA, Sector == "Transportation")
Water_df <- filter (omit_NA, Sector == "Water")

#plots for each sector

Economyplot <- ggplot(data = Economy_df, 
                      aes(x = Threshold.Score, 
                          y = Participant.Score..1.4.)) +
                geom_point()

Energyplot <- ggplot(data = Energy_df, 
                    aes(x = Threshold.Score, 
                        y = Participant.Score..1.4.)) +
  geom_point()

Landuseplot <- ggplot(data = Landuse_df, 
                      aes(x = Threshold.Score, 
                          y = Participant.Score..1.4.)) +
  geom_point()

Natenv_plot <- ggplot(data = Naturalenv_df, 
                      aes(x = Threshold.Score, 
                          y = Participant.Score..1.4.)) +
  geom_point()

People_plot <- ggplot(data = People_df, 
                      aes(x = Threshold.Score, 
                          y = Participant.Score..1.4.)) +
  geom_point()

Trans_plot <- ggplot(data = Trans_df, 
                     aes(x = Threshold.Score, 
                         y = Participant.Score..1.4.)) +
  geom_point()
>>>>>>> 1c31e5323e3148f0917fb37d0e30cbcd81ea04cf

Water_plot <- ggplot(data = Water_df, 
                     aes(x = Threshold.Score, 
                         y = Participant.Score..1.4.)) +
  geom_point()
##errors

#Re-doing read of Excel with all NAs & Rfriendly headers
CERIData3 <-read.xlsx("SESYNCTrainingQuantitative.xlsx",
                      sheetName="IndicatorsBySectorNA",
                      rowIndex=1:84,
                      colIndex=1:12,
                      as.data.frame=TRUE,
                      header=TRUE,
                      colClasses="numeric","character",
                      "factor","factor","character",
                      "character","character","character",
                      "character", "factor","factor","factor"
                      )
##Nope, that didn't work. Added all these columns at the end
#and ignored headers. Trying again...
#crumbs, can't do read.csv because has all these commas in it
CERIData3 <-read.xlsx("SESYNCTrainingQuantitative.xlsx",
                      sheetName="IndicatorsBySectorNA",
                      rowIndex=1:84,
                      colIndex=1:12,
                      as.data.frame=TRUE,
                      header=TRUE              
                      )
##ok that worked but colClasses are wrong
#trying again
CERIData3 <-read.xlsx("SESYNCTrainingQuantitative.xlsx",
                      sheetName="IndicatorsBySectorNA",
                      rowIndex=1:84,
                      colIndex=1:12,
                      as.data.frame=TRUE,
                      header=TRUE,
                      colClasses=c("numeric","character",
                      "factor","factor","character",
                      "character","character","character",
                      "character","factor","factor","factor"),
                      stringsAsFactors=FALSE)
#tried fixing by adding Stringsas factors=false and adding c()
#now says error adding class "factor" to an invalid object
CERIData3 <-read.xlsx("SESYNCTrainingQuantitative.xlsx",
                      sheetName="IndicatorsBySectorNA",
                      rowIndex=1:84,
                      colIndex=1:12,
                      as.data.frame=TRUE,
                      header=TRUE,
                      stringsAsFactors=FALSE)
#ok this is imported but now need to convert to factors
#looks like there's a way to do this faster but too complicated
CERIData3$Sector <-as.factor(CERIData3$Sector)
CERIData3$ES_RC_LC <-as.factor(CERIData3$ES_RC_LC)
CERIData3$ThresholdScore <-as.factor(CERIData3$ThresholdScore)
CERIData3$ImportanceWeight <-as.factor(CERIData3$ImportanceWeight)
CERIData3$ParticipantScore <-as.factor(CERIData3$ParticipantScore)
##ok worked, now checking levels on them all
levels(CERIData3$Sector)  #ok
levels(CERIData3$ES_RC_LC) #ok (no LC in original)
levels(CERIData3$ThresholdScore) #rats included NA as factor
#exclude=NA doesn't work, in any character permutation

# will move on to try to plot

ScorePlot1 <- ggplot(data = CERIData3, 
                    aes(x = ThresholdScore, 
                        y = ParticipantScore, 
                        color= Sector))+
  geom_point()

print(ScorePlot1)
##maybe differentiate by size? ##count somehow??

#first make transparent
ScorePlot1 <- ggplot(data = CERIData3, 
                     aes(x = ThresholdScore, 
                         y = ParticipantScore, 
                         color= Sector))+
  geom_point(alpha=0.05)
# nope that didn't help just made hard to see dots

#try with stat_smooth (not really expecting much)
ScorePlot1 <- ggplot(data = CERIData3, 
                     aes(x = ThresholdScore, 
                         y = ParticipantScore, 
                         color= Sector))+
  geom_point()+
  stat_smooth()
#nope just added lines to the legend

#try a hist to see what working with here

ScoreHist <- ggplot(data = CERIData3,
                aes(x = ThresholdScore)) +
  geom_histogram(binwidth = 3, color = "white")
  
print(ScoreHist)

##error requires continuous X variable says want stat=count
ScoreHist <- ggplot(data = CERIData3,
                        aes(x = ThresholdScore)) +
  geom_bar()
# ok that shoulds counts for THS # of 1's 2's 3's...
ScoreHist2 <- ggplot(data = CERIData3,
                    aes(x = ParticipantScore)) +
  geom_bar()
print(ScoreHist2)
#ok that works... now how to build comparison??
ScoreGeomBar <- ggplot(data=CERIData3,
              aes(x=ThresholdScore, y=Participant Score))+
           geom_bar(stat="identity", width=0.5)
print(ScoreGeomBar)
#nope, says na.rm=FALSE [so... only w/o NAs?]
#says object not found...??

#try a boxplot just because
ScoreBox<- ggplot(data = CERIData3, 
            aes(x = ThresholdScore, 
                y = ParticipantScore, 
                color= Sector)
            )+
  geom_boxplot()
print(ScoreBox)
#does get more colors for each coordinate

#go back and try to add statsum to geompoint
ScorePlot1 <- ggplot(data = CERIData3, 
                     aes(x = ThresholdScore, 
                         y = ParticipantScore, 
                         color= Sector))+
          geom_point()+
          stat_sum(aes(group=1))+
      scale_size(range = c(3, 10))
print(ScorePlot1)
#unclear how to group what means here??
##can try group=ThresholdScore or group=ParticipantScore
##or sum by stat_sum(aes=size=..n..)
#overal proportion says use stat_sum(aes(group=1))

#next will try geom_jitter
ScorePlotJitter <- ggplot(data = CERIData3, 
                     aes(x = ThresholdScore, 
                         y = ParticipantScore, color=Sector))+
  geom_jitter(width=.5, height=.5)
print(ScorePlotJitter)

#see more complicated jittering with sizes bubbles at
#http://stackoverflow.com/questions/26757026/bubble-chart-with-ggplot2

## ok now try facet_wrap by sector
ScoreFacet <- ggplot(data = CERIData3,
       aes(x = ThresholdScore, y=ParticipantScore)) +
  geom_jitter(width=.25, height=.25) +
  facet_wrap( ~ Sector) +
  labs(title = "SectorScoring",
       x = "Threshold Score",
       y = "Participant Score")
print(ScoreFacet)
#ok it's a start!!