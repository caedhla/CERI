#R script for CERI visualization

##Reading in data
##first add new colum in excel (threshold score/IV)
install.packages("xlsx")
library(xlsx)
CERIData <-read.xlsx("SESYNCTrainingQuantitative.xlsx",
    sheetIndex=1,
    na.strings= c(""))


str(CERIData) 
as.factor(...levels...)
rename?columns

##subset/group/filter/aggregate
library(dplyr)


#plot
library(ggplot2)
CERIPlot1 <- ggplot(data=CERIData, 
    aes(x=Threshold_Score, y = Participant_Score)
    geom_point()
    )