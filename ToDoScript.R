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

#plot
library(ggplot2)

#a plot of threshold scores vs. participant scores (still including NA values)
CERIPlot1 <- ggplot(data = SelectCERIData, 
    aes(x = Threshold.Score, 
      y = Participant.Score..1.4., color= Sector))+
    geom_point()
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

Water_plot <- ggplot(data = Water_df, 
                     aes(x = Threshold.Score, 
                         y = Participant.Score..1.4.)) +
  geom_point()
##errors