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
GroupSizeData <- data[data[,3]=="GroupSize",]
# Group Density =========================================================================================================================================================================
NP=11                                                                                                                                                            # number of participants
NTrain=10                                                                                                                                                     # number of practice trials
NTest=320                                                                                                                                                         # number of test trials
NCatch=64                                                                                                                                                        # number of catch trials
NTotal=NTrain+NTest
NMain=256                                                                                                                                     # number of main trials in the test session
# =======================================================================================================================================================================================
InstructionIndex <- matrix(, nrow = NP, ncol = 1)
SessionInd <- matrix(, nrow = NP, ncol = NTotal) 
catchTrial <- matrix(, nrow = NP, ncol = NTotal)
DominantColor <- matrix(, nrow = NP, ncol = NTotal) 
NumberOfAgents <- matrix(, nrow = NP, ncol = NTotal)
AgentHand <- matrix(, nrow = NP, ncol = NTotal)
ParticipantHand <- matrix(, nrow = NP, ncol = NTotal)
responseTime <- matrix(, nrow = NP, ncol = NTotal)  
nAgents1 <- matrix(, nrow = NP, ncol = NTotal) 
nAgents2 <- matrix(, nrow = NP, ncol = NTotal)

for (a1 in 1:NP) {
  InstructionIndex[a1,1] <- as.numeric(toString(data[a1,4]))                                                # 1=> right hand:yellow ;left hand:blue; 2=> right hand:blue ;left hand:yellow;
  for (a2 in 1:NTotal) {
    currentCell <- toString(GroupSizeData[a1,a2+2])
    separatedVals <- strsplit(currentCell,",")
    dataCell <- matrix(unlist(separatedVals),ncol=9,byrow=T)                                                              # ncol is the number of data elements in each cell of the matrix
    SessionInd[a1,a2] <- as.numeric(dataCell[1])                                                                      
    catchTrial[a1,a2] <- as.numeric(dataCell[2])
    DominantColor[a1,a2] <- as.numeric(dataCell[3])
    NumberOfAgents[a1,a2] <- as.numeric(dataCell[4])                                                                                         # participant's raised hand (right=1; left=0)
    AgentHand[a1,a2] <- as.numeric(dataCell[5])                                                                                                                # left hand:1; right hand:2
    ParticipantHand[a1,a2] <- as.numeric(dataCell[6])                                                                                                          # left hand:2; right hand:1
    responseTime[a1,a2] <- as.numeric(dataCell[7])                                                                                         # this is the RT+fixation duration which was 1s
    nAgents1[a1,a2] <- as.numeric(dataCell[8])                                                                                                                     # blue:1; yellow:2
    nAgents2[a1,a2] <- as.numeric(dataCell[9])
  }
}