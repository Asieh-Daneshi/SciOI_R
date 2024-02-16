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
NP=10                                                                                                                                                            # number of participants
NTrain=20                                                                                                                                                     # number of practice trials
NTest=320                                                                                                                                                         # number of test trials
# NCatch=64                                                                                                                                                        # number of catch trials
NTotal=NTrain+NTest
# NMain=256                                                                                                                                     # number of main trials in the test session
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
  InstructionIndex[a1,1] <- as.numeric(toString(DensityData[a1,4]))                                                # 1=> right hand:yellow ;left hand:blue; 2=> right hand:blue ;left hand:yellow;
  for (a2 in 1:NTotal) {
    currentCell <- toString(DensityData[a1,a2+4])
    separatedVals <- strsplit(currentCell,",")
    dataCell <- matrix(unlist(separatedVals),ncol=8,byrow=T)                                                              # ncol is the number of data elements in each cell of the matrix
    SessionInd[a1,a2] <- as.numeric(dataCell[1])                                                                      
    catchTrial[a1,a2] <- as.numeric(dataCell[2])
    Radius[a1,a2] <- as.numeric(dataCell[3])
    NumberOfAgents[a1,a2] <- as.numeric(dataCell[4])                                                                                         # participant's raised hand (right=1; left=0)
    AgentHand[a1,a2] <- as.numeric(dataCell[5])                                                                                                                # left hand:1; right hand:2
    ParticipantHand[a1,a2] <- as.numeric(dataCell[6])                                                                                                          # left hand:2; right hand:1
    responseTime[a1,a2] <- as.numeric(dataCell[7])                                                                                         # this is the RT+fixation duration which was 1s
    DominantColor[a1,a2] <- as.numeric(dataCell[8])                                                                                                                     # blue:1; yellow:2
  }
}
# Correct percentage in practice session =======================
SessionIndTrain <- SessionInd[,1:NTrain]                                                                      
catchTrialTrain <- catchTrial[,1:NTrain]  
RadiusTrain <- Radius[,1:NTrain]  
NumberOfAgentsTrain <- NumberOfAgents[,1:NTrain]                                                                                            # participant's raised hand (right=1; left=0)
AgentHandTrain <- AgentHand[,1:NTrain]                                                                                                          # left hand:1; right hand:2
ParticipantHandTrain <- ParticipantHand[,1:NTrain]                                                                                                           # left hand:2; right hand:1
responseTimeTrain <- responseTime[,1:NTrain]                                                                                         # this is the RT+fixation duration which was 1s
DominantColorTrain <- DominantColor[,1:NTrain]  

CorrectsTrain=(ParticipantHandTrain==2 & DominantColorTrain==1)|(ParticipantHandTrain==1 & DominantColorTrain==2)
PercentageCorrectTrain <- matrix(, nrow = NP, ncol = 1)
for (a1 in 1:NP){
  CorrectsP=as.numeric(CorrectsTrain[a1,])
  PercentageCorrectTrain[a1,]=sum(CorrectsP)/NTrain*100
}
# Correct percentage in test session =======================
SessionIndTest <- SessionInd[,(NTrain+1):340]                                                                      
catchTrialTest <- catchTrial[,(NTrain+1):NTotal]  
RadiusTest <- Radius[,(NTrain+1):NTotal]  
NumberOfAgentsTest <- NumberOfAgents[,(NTrain+1):NTotal]                                                                                            # participant's raised hand (right=1; left=0)
AgentHandTest <- AgentHand[,(NTrain+1):NTotal]                                                                                                          # left hand:1; right hand:2
ParticipantHandTest <- ParticipantHand[,(NTrain+1):NTotal]                                                                                                           # left hand:2; right hand:1
responseTimeTest <- responseTime[,(NTrain+1):NTotal]                                                                                         # this is the RT+fixation duration which was 1s
DominantColorTest <- DominantColor[,(NTrain+1):NTotal] 

CorrectsCatch=((ParticipantHandTest==2 & DominantColorTest==1)|(ParticipantHandTest==1 & DominantColorTest==2))&(catchTrialTest==1)
PercentageCorrectTest <- matrix(, nrow = NP, ncol = 1)
for (a1 in 1:NP){
  CorrectsP=as.numeric(CorrectsCatch[a1,])
  CatchsP=as.numeric(catchTrialTest[a1,]==1)
  PercentageCorrectTest[a1,]=sum(CorrectsP)/sum(CatchsP)*100
}
# removing first two columns ===================================================
# SessionInd <- SessionInd[,-c(1,2)]                                                                      
# catchTrial <- catchTrial[,-c(1,2)] 
# Radius <- Radius[,-c(1,2)] 
# NumberOfAgents <- NumberOfAgents[,-c(1,2)]                                                                                          # participant's raised hand (right=1; left=0)
# AgentHand <- AgentHand[,-c(1,2)]                                                                                                        # left hand:1; right hand:2
# ParticipantHand <- ParticipantHand[,-c(1,2)]                                                                                                         # left hand:2; right hand:1
# responseTime <- responseTime[,-c(1,2)]                                                                                       # this is the RT+fixation duration which was 1s
# DominantColor <- DominantColor[,-c(1,2)] 
# Percentage of correct responses in practice trials ========================================================================================================================================
# TotalCatch=rowSums(catchTrial,na=TRUE)      # number of catch trials for each participant
# Corrects <- matrix(, nrow = NP, ncol = 1)
# for (a in 1:NP){
#   PH=ParticipantHand[a,];
#   DC=DominantColor[a,];
#   CT=catchTrial[a,];
#   Corrects[a,1]=sum(as.numeric((abs(PH[CT==1]-DC[CT==1])==1)&(PH!=0)),na=TRUE)
# }
# abs(ParticipantHand[catchTrial==1]-DominantColor[catchTrial==1])==1
# # matrix(as.numeric(raisedHandColorG[catchTrial==1]==DominantColor[catchTrial==1]),nrow = NP,ncol = NMain)
# 
# 
# 
# 
# 
