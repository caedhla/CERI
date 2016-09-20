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
SelectCERIData <- select(CERIData, NA..4, Threshold.Score, Participant.Score..1.4.)
SELECTCERIDATA_NA <- 
#use in operator to find string and convert to NA
SelectCERIData$Threshold.Score[CERIData$Threshold.Score== "No threshold"]<- "NA"
SelectCERIData$Threshold.Score[CERIData$Threshold.Score== "No indicator value"]<- "NA"
SelectCERIData$Threshold.Score[CERIData$Threshold.Score== "No threshold or indicator value"]<- "NA"
SelectCERIData$Participant.Score..1.4.[CERIData$Participant.Score..1.4.=="Score not yet assigned"]<- "NA"
SelectCERIData$Participant.Score..1.4.[CERIData$Participant.Score..1.4.=="to be assigned"]<- "NA"
SelectCERIData$Participant.Score..1.4.[CERIData$Participant.Score..1.4.=="Not relevant"]<- "NA"
SelectCERIData$Participant.Score..1.4.[CERIData$Participant.Score..1.4.=="<NA>"]<- "NA"
SelectCERIData$Participant.Score..1.4.[CERIData$Participant.Score..1.4.=="N/A"]<- "NA"
SelectCERIData_NA <- subset(SelectCERIData, 
##nope

#plot
library(ggplot2)
CERIPlot1 <- ggplot(data = SelectCERIData, 
    aes(x = Threshold.Score, 
      y = Participant.Score..1.4., color= NA..4))+
    geom_point()

##errors