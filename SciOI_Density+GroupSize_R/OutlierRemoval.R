cat("\014")                                                                                                                                                            # as clc in MATLAB
rm(list = ls(all.names = TRUE))                                                                                                                                      # as clear in MATLAB
# install required packages =============================================================================================================================================================
# install.packages("readxl")                                                                                                                                         # to read xlsx files
# install.packages("ggplot2")
# install.packages("ggthemes")
# call libraries ========================================================================================================================================================================
library(readxl)
library(ggplot2)
library(ggthemes)
# read data from the excel file =========================================================================================================================================================
data <- read_excel("E:\\OngoingAnalysis\\SciOI_R\\SciOI_Density+GroupSize_R\\Responses_Pilot1_10Participants_2024-02-15.xlsx")
DensityData <- data[data[,3]=="Density",]
DensityGroupSize <- data[data[,3]=="GroupSize",]
# Group Density =========================================================================================================================================================================
NP=10                                                                                                                                                            # number of participants
NTrain=10                                                                                                                                                     # number of practice trials
NTest=320                                                                                                                                                         # number of test trials
NCatch=64                                                                                                                                                        # number of catch trials
NTotal=NTrain+NTest
NMain=256                                                                                                                                     # number of main trials in the test session
# =======================================================================================================================================================================================
InstructionIndex <- matrix(, nrow = NP, ncol = 1)
SessionInd <- matrix(, nrow = NP, ncol = NTotal) 
catchTrial <- matrix(, nrow = NP, ncol = NTotal)
Radius <- matrix(, nrow = NP, ncol = NTotal) 
NumberOfAgents <- matrix(, nrow = NP, ncol = NTotal)
AgentHand <- matrix(, nrow = NP, ncol = NTotal)
ParticipantHand <- matrix(, nrow = NP, ncol = NTotal)
responseTime <- matrix(, nrow = NP, ncol = NTotal)  
DominantColor <- matrix(, nrow = NP, ncol = NTotal)   

for (a1 in 1:NP) {
  InstructionIndex <- as.numeric(toString(data[a1,3]))
  for (a2 in 1:NTotal) {
    currentCell <- toString(data[a1,a2+2])
    separatedVals <- strsplit(currentCell,",")
    dataCell <- matrix(unlist(separatedVals),ncol=8,byrow=T)                                                             # ncol is the number of data elements in each cell of the matrix
    SessionInd[a1,a2] <- as.numeric(dataCell[1])
    catchTrial[a1,a2] <- as.numeric(dataCell[2])
    Radius[a1,a2] <- as.numeric(dataCell[3])
    NumberOfAgents[a1,a2] <- as.numeric(dataCell[4])                                                                                       # participant's raised hand (right=1; left=0)
    AgentHand[a1,a2] <- as.numeric(dataCell[5])                                                                # the color of flag in participant's right hand (blue=1; orange=0)
    ParticipantHand[a1,a2]=responseHandP[a1,a2]*rightFlagColorP[a1,a2]                                                                                             # 1:blue; -1:yellow
    responseTime[a1,a2] <- as.numeric(dataCell[6]) 
    DominantColor[a1,a2] <- as.numeric(dataCell[7])
  }
}
# # =====================================================================================================================================================================================
# # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# # =====================================================================================================================================================================================
# # computing the percentage that the agents give the correct answer in practice and main trials. It was important to check that the agents responded around 90 percent of the time 
# # correct. This 90 % comes from the first pilot that we saw participants were correct 90 % of the time. So, we wanted to simulate the same thing with agents, so that participants
# # assume them as real agents with the same accuracy as themselves! --------------------------------------------------------------------------------------------------------------------
# PC1=rowSums(matrix(as.numeric(raisedHandColorG[practiceTrial==1]==DominantColor[practiceTrial==1]),nrow = NP,ncol = NMain))/20*100     # percent correct of agents in practice trials
# PC2=rowSums(matrix(as.numeric(raisedHandColorG[catchTrial==1]==DominantColor[catchTrial==1]),nrow = NP,ncol = NMain))/150*100                # percent correct of agents in main trials
# 
# framedData <- data.frame(1:NP,PC2)
# colnames(framedData) <- c('ParticipantNumber','Accuracy of the group in practice and catch trials') 
# png(file="E:\\OngoingAnalysis\\SciOI_2023-11-17\\R\\GroupAccuracy.png", width=600, height=350)
# ggplot(data = framedData, aes(x = ParticipantNumber, y = PC2)) +  geom_bar(stat="identity",color="black", fill="cadetblue4") + theme_bw(base_size = 14) + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank() , axis.line = element_line(size = .5, linetype = "solid", colour = "black")) + scale_x_continuous(name="Participant's number" , breaks=seq(0,180,10)) + ggtitle("Total time spent on the experiment") + ylab("Total time (s)")
# dev.off()
# # =====================================================================================================================================================================================
# # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# # =====================================================================================================================================================================================
# # TOTAL RESPONSE TIME >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# totalTime <- t(t(c(rowSums(responseTime))))                                                                                                  # each row corresponds to one participants
# 
# # rownames(totalTime) <- as.character(1:NP)
# framedData <- data.frame(1:NP,totalTime)
# colnames(framedData) <- c('ParticipantNumber','totalExperimentTime') 
# # histogram of total time of the experiment -------------------------------------------------------------------------------------------------------------------------
# png(file="E:\\OngoingAnalysis\\SciOI_2023-11-17\\R\\totalTimeHistogram.png", width=600, height=350)
# ggplot(data = framedData, aes(x = totalExperimentTime)) +  geom_histogram(color="black", fill="aquamarine4") + theme_bw(base_size = 14) + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank() , axis.line = element_line(size = .5, linetype = "solid", colour = "black")) + ggtitle("Total time spent on the experiment") + xlab("Total time (s)") + ylab("number of participants")
# dev.off()
# # ggplot(data = framedData, aes(x = totalExperimentTime)) +  geom_histogram() + theme_wsj()+ scale_colour_wsj("colors6")    # wall street journal theme
# # -------------------------------------------------------------------------------------------------------------------------------------------------------------------
# # barplot of the total time participants spent on the experiment ----------------------------------------------------------------------------------------------------
# png(file="E:\\OngoingAnalysis\\SciOI_2023-11-17\\R\\totalTimeBarplot.png", width=600, height=350)
# ggplot(data = framedData, aes(x = ParticipantNumber, y = totalTime)) +  geom_bar(stat="identity",color="black", fill="cadetblue4") + theme_bw(base_size = 14) + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank() , axis.line = element_line(size = .5, linetype = "solid", colour = "black")) + scale_x_continuous(name="Participant's number" , breaks=seq(0,240,10)) + ggtitle("Total time spent on the experiment") + ylab("Total time (s)")
# dev.off()
# # -------------------------------------------------------------------------------------------------------------------------------------------------------------------
# # finding outliers based on Hampel filter ---------------------------------------------------------------------------------------------------------------------------
# # we just consider those participants who did the experiment too fast as outliers in other words we just remove the participants who their total response time was 
# # lower than three MAD from median ----------------------------------------------------------------------------------------------------------------------------------
# totalExperimentTimeLB=median(framedData$totalExperimentTime) - 3 * mad(framedData$totalExperimentTime, constant = 1)
# totalExperimentTimeUB=median(framedData$totalExperimentTime) + 3 * mad(framedData$totalExperimentTime, constant = 1)
# totalExperimentTimeOutliers=t(t(as.numeric(framedData$totalExperimentTime <= totalExperimentTimeLB)))
# outlier1 = c(row(totalExperimentTimeOutliers)[which(!totalExperimentTimeOutliers == 0)])                                                                      # indices of outliers
# # ===================================================================================================================================================================
# # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# # ===================================================================================================================================================================
# # PERFORMANCE OF PRACTICE TRIALS (how correct the participants are) >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# correctPractices=matrix(as.numeric((raisedHandColorP[practiceTrial==1]==DominantColor[practiceTrial==1])),nrow = NP,ncol = NTrain)               # correct=1; wrong=0
# correctCatches=matrix(as.numeric((raisedHandColorP[catchTrial==1]==DominantColor[catchTrial==1])),nrow = NP,ncol = NCatch)                       # correct=1; wrong=0
# correctPracticesPercentage=rowSums(correctPractices)/NTrain*100;
# correctCatchesPercentage=rowSums(correctCatches)/NCatch*100;
# 
# framedData <- data.frame(1:NP,totalTime,correctPracticesPercentage,correctCatchesPercentage)
# colnames(framedData) <- c('ParticipantNumber','totalExperimentTime','correctPracticesPercentage','correctCatchesPercentage') 
# # histogram of correct practice trials ------------------------------------------------------------------------------------------------------------------------------
# png(file="E:\\OngoingAnalysis\\SciOI_2023-11-17\\R\\correctPracticeHistogram.png", width=600, height=350)
# ggplot(data = framedData, aes(x = correctPracticesPercentage)) + geom_histogram(color="black", fill="aquamarine4") + theme_bw(base_size = 14) + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank() , axis.line = element_line(size = .5, linetype = "solid", colour = "black")) + ggtitle("Performance on practice trials") + xlab("correct Percentage") + ylab("number of participants")
# dev.off()
# # -------------------------------------------------------------------------------------------------------------------------------------------------------------------
# # barplot of the correct practice trials ----------------------------------------------------------------------------------------------------------------------------
# png(file="E:\\OngoingAnalysis\\SciOI_2023-11-17\\R\\correctPracticeBarplot.png", width=600, height=350)
# ggplot(data = framedData, aes(x = ParticipantNumber, y = correctPracticesPercentage)) +  geom_bar(stat="identity",color="black", fill="cadetblue4") + theme_bw(base_size = 14) + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank() , axis.line = element_line(size = .5, linetype = "solid", colour = "black")) + scale_x_continuous(name="Participant's number" , breaks=seq(0,180,10)) + ggtitle("Performance on practice trials") + ylab("Correct percentage")
# dev.off()
# # -------------------------------------------------------------------------------------------------------------------------------------------------------------------
# # finding outliers based on Hampel filter ---------------------------------------------------------------------------------------------------------------------------
# correctPracticesPercentageLB=median(framedData$correctPracticesPercentage) - 3 * mad(framedData$correctPracticesPercentage, constant = 1)
# correctPracticesPercentageUB=median(framedData$correctPracticesPercentage) + 3 * mad(framedData$correctPracticesPercentage, constant = 1)
# correctPracticesPercentageOutliers=t(t(as.numeric(framedData$correctPracticesPercentage <= correctPracticesPercentageLB)))
# outlier2 = c(row(correctPracticesPercentageOutliers)[which(!correctPracticesPercentageOutliers == 0)])                                                                      # indices of outliers
# # ==================================================================================================================================================================
# # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# # ==================================================================================================================================================================
# # PERFORMANCE OF CATCH TRIALS (how correct the participants are) >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# # histogram of correct catch trials ---------------------------------------------------------------------------------------------------------------------------------
# png(file="E:\\OngoingAnalysis\\SciOI_2023-11-17\\R\\correctCatchHistogram.png", width=600, height=350)
# ggplot(data = framedData, aes(x = correctCatchesPercentage)) + geom_histogram(color="black", fill="aquamarine4") + theme_bw(base_size = 14) + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank() , axis.line = element_line(size = .5, linetype = "solid", colour = "black")) + ggtitle("Performance on catch trials") + xlab("correct Percentage") + ylab("number of participants")
# dev.off()
# # -------------------------------------------------------------------------------------------------------------------------------------------------------------------
# # barplot of the correct catch trials -------------------------------------------------------------------------------------------------------------------------------
# png(file="E:\\OngoingAnalysis\\SciOI_2023-11-17\\R\\correctCatchBarplot.png", width=600, height=350)
# ggplot(data = framedData, aes(x = ParticipantNumber, y = correctCatchesPercentage)) +  geom_bar(stat="identity",color="black", fill="cadetblue4") + theme_bw(base_size = 14) + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank() , axis.line = element_line(size = .5, linetype = "solid", colour = "black")) + scale_x_continuous(name="Participant's number" , breaks=seq(0,180,10)) + ggtitle("Performance on catch trials") + ylab("Correct percentage")
# dev.off()
# # -------------------------------------------------------------------------------------------------------------------------------------------------------------------
# # finding outliers based on Hampel filter ---------------------------------------------------------------------------------------------------------------------------
# correctCatchesPercentageLB=median(framedData$correctCatchesPercentage) - 3 * mad(framedData$correctCatchesPercentage, constant = 1)
# correctCatchesPercentageUB=median(framedData$correctCatchesPercentage) + 3 * mad(framedData$correctCatchesPercentage, constant = 1)
# correctCatchesPercentageOutliers=t(t(as.numeric(framedData$correctCatchesPercentage <= correctCatchesPercentageLB)))
# outlier3 = (row(correctCatchesPercentageOutliers)[which(!correctCatchesPercentageOutliers == 0)])      
# 
# 
# allOutliers = union(union(outlier1, outlier2), outlier3)
# numberOfRemainingData = NP-length(allOutliers)