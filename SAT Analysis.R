#set working directory and load packages
setwd("~/Desktop/Final Experiment/SAT")
library(plyr)
library(dplyr)          # data manipulation
library(tidyr)          # data re-shaping
library(ggplot2)        # plotting & data
library(ggsignif)       # plotting
library(ggpubr)       # plotting
library(lme4)           # logit regression
library(MASS)           # chi square
library(stargazer)      #export to R
library(sjPlot)          #correlations
library(car)
# detach("package:plyr", unload=TRUE)
#some things need plyr to run, others need it disabled to run, so just (un)comment it as required

#SAT-Task
#create a list of the input files, one for each type
SATInstanceFiles = list.files(path = "/Users/KRDoroc/Desktop/Final Experiment/SAT",
                              pattern="*InstancesInfo.txt$")
SATInfoFiles = list.files(path = "/Users/KRDoroc/Desktop/Final Experiment/SAT",pattern="*TrialInfo.txt$")
SATTimeStampFiles = list.files(path = "/Users/KRDoroc/Desktop/Final Experiment/SAT",
                               pattern="*TimeStamps.txt$")
#create a list of the participant ID
pIDs=sapply(strsplit(SATInstanceFiles,"_"),function(x) x[1])

#load all the trialinfo files and add participant ID as a column
for(satInfoFile in SATInfoFiles){
  SATTrialInfo = lapply(SATInfoFiles, read.csv, stringsAsFactors=F,skip=1,header = TRUE, sep =";"
                        ,quote = "") %>% 
    bind_rows
  SATTrialInfo <- arrange(SATTrialInfo,block,trial)
  SATTrialInfo$ParticipantID <-(pIDs)
}

#load all the instanceinfo files
for(satInstanceFile in SATInstanceFiles){
  SATInstanceInfo = lapply(SATInstanceFiles, read.csv, stringsAsFactors=F,skip=1,header = TRUE, sep =";"
                           ,quote = "") %>% 
    bind_rows
}

##load all the time stamps files, find the time spent on each trial (deleting some irrelevant columns),
#and add participant ID as a column
for(satTimeStampFile in SATTimeStampFiles){
  SATTimeStamps = lapply(SATTimeStampFiles, read.csv, stringsAsFactors=F,skip=2,header = TRUE, sep =";"
                         ,quote = "") %>% 
    bind_rows
  names(SATTimeStamps)[names(SATTimeStamps) == "eventType"] <- "elapsedTime"
  names(SATTimeStamps)[names(SATTimeStamps) == "instanceNumber"] <- "eventType"
  SATTimeStamps <- SATTimeStamps[ -c(5) ]
  SATTime <- diff(SATTimeStamps$elapsedTime, lag=1, differences=1)/1000
  SATTimeStamps <- SATTimeStamps[-nrow(SATTimeStamps),]
  SATTimeStamps$TimeOnTask <- SATTime
  SATTimeStamps <- subset(SATTimeStamps, eventType=="Trial")
  SATTimeStamps <- SATTimeStamps[ -c(3:4) ]
  SATTimeStamps <- arrange(SATTimeStamps,block,trial)
  SATTimeStamps$ParticipantID <-(pIDs)
}

#only keeps the instance number and type from the instance info
SATInstanceInfo = {dplyr::select(SATInstanceInfo, instanceNumber, type)}

#merge with trial and instance info
SATTrials <- merge(SATTrialInfo,SATInstanceInfo)
SATTrials <- unique(SATTrials)
SATTrials <- arrange(SATTrials,ParticipantID,block,trial)
rownames(SATTrials) <- NULL

#merge time stamps with previous inputs to create a single file with aggregate data
SATAggData <- merge(SATTrials,SATTimeStamps)
SATAggData <- unique(SATAggData)

# #merge decisions & propagations file with previous inputs to create a single file with aggregate data
# SATComplexity <- read.table(file = "ComplexitySAT.csv", header = TRUE, sep = ",")
# SATAggData <- merge(SATAggData,SATComplexity)

#delete unnecessary columns
SATAggData <- SATAggData[ -c(5,7,9) ]

# #create separate data sets based on whether instances have YES or NO solutions
# SATYes <- subset(SATAggData, randomYes.1.Left.No.Right.Yes.==1)
# SATYes <- SATYes[ -c(1:2,5:6) ]
# SATNo <- subset(SATAggData, randomYes.1.Left.No.Right.Yes.==0)
# SATNo <- SATNo[ -c(1:2,5:6) ]

#delete unnecessary columns
SATAggData <- SATAggData[ -c(6) ]

# #check for learning effect
# d <- c(ifelse(SATAggData$block==1, SATAggData$trial, ifelse(SATAggData$block==2, SATAggData$trial+8, 
#                                                             ifelse(SATAggData$block==3, 
#                                                                    SATAggData$trial+16,NA))))
# SATAggData$TrialNumber <- cbind(d)
# SATcorData <- SATAggData[c(5,8)]
# sjt.corr(SATcorData, p.numeric=TRUE, triangle = "lower")
# #no correlation between the trialnumber and accuracy
# 
# #tried to visualise the correlation and learning effect, seems pretty constant throughout
# SAT_LearnPlot <- ggplot(SATAggData, aes(TrialNumber,correct)) + 
#   geom_point() + geom_smooth(fill="blue", colour="darkblue", size=1)
# 
# SATLearn_1 <- subset(SATAggData, TrialNumber<7)
# SATLearn_2 <- subset(SATAggData, TrialNumber>6 & TrialNumber<13)
# SATLearn_3 <- subset(SATAggData, TrialNumber>12)
# # 
# # SATLearn$Block_2[SATAggData$TrialNumber>6 & SATAggData$TrialNumber<13] <- (2)
# # SATLearn$Block_3[SATAggData$TrialNumber>12] <- (3)
# 
# SAT_L1 <- SATLearn_1 %>% # the names of the new data frame and the data frame to be summarised
#   group_by(ParticipantID) %>%
#   summarise(mean = mean(correct))
# SAT_L1$Block <- (1)
# SAT_L2 <- SATLearn_2 %>% # the names of the new data frame and the data frame to be summarised
#   group_by(ParticipantID) %>%
#   summarise(mean = mean(correct))
# SAT_L2$Block <- (2)
# SAT_L3 <- SATLearn_3 %>% # the names of the new data frame and the data frame to be summarised
#   group_by(ParticipantID) %>%
#   summarise(mean = mean(correct))
# SAT_L3$Block <- (3)
# 
# SATL <- rbind(SAT_L1,SAT_L2)
# SATL <- rbind(SATL,SAT_L3)
# SATL <- SATL[,-c(1)]
# ggplot(SATL, aes(Block,mean)) + geom_point() + geom_smooth(fill="blue", colour="darkblue", size=1)
SATAggData <- SATAggData[ -c(1:2) ]





# #check average accuracy and time on instances
# SAT_Inst_Summ <- SATAggData %>%  # the names of the new data frame and the data frame to be summarised
#   group_by(instanceNumber) %>%
#   summarise(Accuracy = mean(correct), Std_Dev = sd(correct), Time = mean(TimeOnTask))
# 
# # View(SAT_Inst_Summ)
# library(openxlsx)
# write.xlsx(SAT_Inst_Summ, 'SAT_Inst.xlsx')
# 
# #check average accuracy and time by participant
# SAT_Part_Summ <- SATAggData %>%  # the names of the new data frame and the data frame to be summarised
#   group_by(ParticipantID) %>%
#   summarise(Accuracy = mean(correct), Std_Dev = sd(correct), Time = mean(TimeOnTask))
# 
# # View(SAT_Part_Summ)
# write.xlsx(SAT_Part_Summ, 'SAT_Part.xlsx')


##check for general pattern and outlier instances
# ggplot(SAT_Inst_Summ, aes(instanceNumber,mean_acc)) + 
#   geom_point() + geom_smooth(fill="blue", colour="darkblue", size=1)
# #looks like it starts easy, gets hard, improves a bit and then plateaus
# ggplot(SAT_Inst_Summ, aes(instanceNumber,mean_time)) +
#   geom_point() + geom_smooth(fill="blue", colour="darkblue", size=1)
# #takes a lot of time and then drops off

# #try removing the most problematic instances: 3 and 7, then repeat analyses
# SATAggData$trim <- "No"
# SATAggData$trim[SATAggData$instanceNumber==3 | SATAggData$instanceNumber==7] <- ("Yes")
# SATAggTrimmed <- subset(SATAggData, trim=="No")
# SATAggTrimmed <- SATAggTrimmed[ -c(11) ]
# SATAggData <- SATAggData[ -c(11) ]






# SAT_Trial_Summ <- SATAggData %>%  # the names of the new data frame and the data frame to be summarised
#   group_by(TrialNumber) %>%
#   summarise(mean_acc = mean(correct), mean_time = mean(TimeOnTask))
# 
# #average accuracy doesnt change much with experience
# ggplot(SAT_Trial_Summ, aes(TrialNumber,mean_acc)) + 
#   geom_point() + geom_smooth(fill="blue", colour="darkblue", size=1)
# 
# #average time decreases significantly with experience
# ggplot(SAT_Trial_Summ, aes(TrialNumber,mean_time)) + 
#   geom_point() + geom_smooth(fill="blue", colour="darkblue", size=1)
# 







# #check if more effort = high performance
# ddply(SATAggData, "type", summarise, Corr=cor(TimeOnTask, correct))
# 
# #check if more effort = high performance
# ddply(SATAggData, "type", summarise, Corr=cor.test(TimeOnTask, correct))
# 
# Effort_Correls <- ddply(SATAggData, .(type), summarise,
#                         Correlation=(cor.test(TimeOnTask, correct)), Description=names(Correlation) )
# Effort_Correls <- subset(Effort_Correls, Description=="estimate" | Description=="p.value")
# #-40% (1% lvl) for type 2, -16%(10% level) for type 5, -26% (1% lvl) for type 6 - rest insignificant
# #higher effort leads to worse performance, if it affects performance at all


#create a column to see if an instance is in the PT (Yes) or not (NO)
SATAggData$InPhaseTransition <- ("No")
SATAggData$InPhaseTransition[SATAggData$type==4 | SATAggData$type==3] <- ("Yes")

#create a column to see if an instance is in the underconstrained region (Under), PT (PT), or overconstrained
#region (Over)
SATAggData$Region[SATAggData$InPhaseTransition=="Yes"] <- ("Phase Transition - HARD")
SATAggData$Region[SATAggData$type<=2] <- ("Phase Transition - EASY")
SATAggData$Region[SATAggData$type==5] <- ("Over-Constrained")
SATAggData$Region[SATAggData$type==6] <- ("Under-Constrained")


SATUnder <- subset(SATAggData, Region=="Under-Constrained")
SATOver <- subset(SATAggData, Region=="Over-Constrained")
SATPT <- subset(SATAggData, Region=="Phase Transition") 

# #repeat above for trimmed file
# #create a column to see if an instance is in the PT (Yes) or not (NO)
# SATAggTrimmed$InPhaseTransition <- ("No")
# SATAggTrimmed$InPhaseTransition[SATAggTrimmed$type==4 | SATAggTrimmed$type==4.5] <- ("Yes")
# 
# #create a column to see if an instance is in the underconstrained region (Under), PT (PT), or overconstrained
# #region (Over)
# SATAggTrimmed$Region[SATAggTrimmed$InPhaseTransition=="Yes"] <- ("Phase Transition")
# SATAggTrimmed$Region[SATAggTrimmed$type==3 | SATAggTrimmed$type==3.5] <- ("Under-Constrained")
# SATAggTrimmed$Region[SATAggTrimmed$type==5 | SATAggTrimmed$type==5.5] <- ("Over-Constrained")



#SAT plots
#create a summary table for accuracy of in vs out PT
SAT_Acc_Sum_PT <- SATAggData %>% # the names of the new data frame and the data frame to be summarised
  group_by(InPhaseTransition) %>%   # the grouping variable
  summarise(mean_PT = mean(correct),  # calculates the mean of each group
            sd_PT = sd(correct), # calculates the standard deviation of each group
            n_PT = n(),  # calculates the sample size per group
            SE_PT = sd(correct)/sqrt(n())) # calculates the standard error of each group

#create a summary table for time spent of in vs out PT
SAT_Time_Sum_PT <- SATAggData %>% # the names of the new data frame and the data frame to be summarised
  group_by(InPhaseTransition) %>%   # the grouping variable
  summarise(mean_PT = mean(TimeOnTask),  # calculates the mean of each group
            sd_PT = sd(TimeOnTask), # calculates the standard deviation of each group
            n_PT = n(),  # calculates the sample size per group
            SE_PT = sd(TimeOnTask)/sqrt(n())) # calculates the standard error of each group

#create a summary table for accuracy of region
  SAT_Acc_Sum_Region <- SATAggData %>% # the names of the new data frame and the data frame to be summarised
  group_by(Region) %>%   # the grouping variable
  summarise(mean_region = mean(correct),  # calculates the mean of each group
            sd_region = sd(correct), # calculates the standard deviation of each group
            n_region = n(),  # calculates the sample size per group
            SE_region = sd(correct)/sqrt(n())) # calculates the standard error of each group

#create a summary table for time spent of region
SAT_Time_Sum_Region <- SATAggData %>% # the names of the new data frame and the data frame to be summarised
  group_by(Region) %>%   # the grouping variable
  summarise(mean_region = mean(TimeOnTask),  # calculates the mean of each group
            sd_region = sd(TimeOnTask), # calculates the standard deviation of each group
            n_region = n(),  # calculates the sample size per group
            SE_region = sd(TimeOnTask)/sqrt(n())) # calculates the standard error of each group
SAT_Time_Sum_Region$Region[SAT_Time_Sum_Region$Region=="Under-Constrained"] <- ("Under")
SAT_Time_Sum_Region$Region[SAT_Time_Sum_Region$Region=="Phase Transition"] <- ("PT")
SAT_Time_Sum_Region$Region[SAT_Time_Sum_Region$Region=="Over-Constrained"] <- ("Over")



# #repeat for trimmed file
# #create a summary table for accuracy of in vs out PT
# SAT_Acc_Sum_PT <- SATAggTrimmed %>% # the names of the new data frame and the data frame to be summarised
#   group_by(InPhaseTransition) %>%   # the grouping variable
#   summarise(mean_PT = mean(correct),  # calculates the mean of each group
#             sd_PT = sd(correct), # calculates the standard deviation of each group
#             n_PT = n(),  # calculates the sample size per group
#             SE_PT = sd(correct)/sqrt(n())) # calculates the standard error of each group
# 
# #create a summary table for time spent of in vs out PT
# SAT_Time_Sum_PT <- SATAggTrimmed %>% # the names of the new data frame and the data frame to be summarised
#   group_by(InPhaseTransition) %>%   # the grouping variable
#   summarise(mean_PT = mean(TimeOnTask),  # calculates the mean of each group
#             sd_PT = sd(TimeOnTask), # calculates the standard deviation of each group
#             n_PT = n(),  # calculates the sample size per group
#             SE_PT = sd(TimeOnTask)/sqrt(n())) # calculates the standard error of each group
# 
# #create a summary table for accuracy of region
# SAT_Acc_Sum_Region <- SATAggTrimmed %>% # the names of the new data frame and the data frame to be summarised
#   group_by(Region) %>%   # the grouping variable
#   summarise(mean_region = mean(correct),  # calculates the mean of each group
#             sd_region = sd(correct), # calculates the standard deviation of each group
#             n_region = n(),  # calculates the sample size per group
#             SE_region = sd(correct)/sqrt(n())) # calculates the standard error of each group
# 
# #create a summary table for time spent of region
# SAT_Time_Sum_Region <- SATAggTrimmed %>% # the names of the new data frame and the data frame to be summarised
#   group_by(Region) %>%   # the grouping variable
#   summarise(mean_region = mean(TimeOnTask),  # calculates the mean of each group
#             sd_region = sd(TimeOnTask), # calculates the standard deviation of each group
#             n_region = n(),  # calculates the sample size per group
#             SE_region = sd(TimeOnTask)/sqrt(n())) # calculates the standard error of each group
# SAT_Time_Sum_Region$Region[SAT_Time_Sum_Region$Region=="Under-Constrained"] <- ("Under")
# SAT_Time_Sum_Region$Region[SAT_Time_Sum_Region$Region=="Phase Transition"] <- ("PT")
# SAT_Time_Sum_Region$Region[SAT_Time_Sum_Region$Region=="Over-Constrained"] <- ("Over")





# SATAggData$TimeOnTask.t <- SATAggData$TimeOnTask
#qqp(SATAggData$TimeOnTask.t, "norm")




#SAT analyses
#importing demographics and merging with existing aggregated data
SATDemographics <- read.table(file = "Demographics.csv", header = TRUE, sep = ",")
SATDemographics$Sex_Female[SATDemographics$Gender=="Female"] <- (1)
SATDemographics$Sex_Female[SATDemographics$Gender=="Male"] <- (0)
SATDemographics$PostGrad[SATDemographics$Years_of_study>3] <- (1)
SATDemographics$PostGrad[SATDemographics$Years_of_study<4] <- (0)
SATDemographics$Commerce[SATDemographics$Degree=="Commerce"] <- (1)
SATDemographics$Commerce[SATDemographics$Degree!="Commerce"] <- (0)
SATDemographics$Early_Afternoon[SATDemographics$Time_of_day=="Early_Afternoon"] <- (1)
SATDemographics$Early_Afternoon[SATDemographics$Time_of_day!="Early_Afternoon"] <- (0)
SATDemographics$Late_Afternoon[SATDemographics$Time_of_day=="Late_Afternoon"] <- (1)
SATDemographics$Late_Afternoon[SATDemographics$Time_of_day!="Late_Afternoon"] <- (0)
names(SATDemographics)[names(SATDemographics) == 'Mode.literal...Elimination'] <- 'Process_Elimination'
names(SATDemographics)[names(SATDemographics) == 'Brute.Force'] <- 'Brute_Force'
names(SATDemographics)[names(SATDemographics) == 'All..ve..ve'] <- 'Uniform_Signs'
SATDemographics$MultipleStrategies <- SATDemographics$Total
SATDemographics$MultipleStrategies[SATDemographics$MultipleStrategies<=1] <- (0)
SATDemographics$MultipleStrategies[SATDemographics$MultipleStrategies>1] <- (1)
SATDemographics <- SATDemographics[ -c(2:4,6:7,11:13) ]

SATAggData <- merge(SATAggData,SATDemographics)

SATAggData$Difficulty[SATAggData$Region=="Under-Constrained"] <- (1)
SATAggData$Difficulty[SATAggData$Region=="Over-Constrained"] <- (2)
SATAggData$Difficulty[SATAggData$Region=="Phase Transition"] <- (3)

SATLearning <- subset(SATAggData, TrialNumber>6)
# SATShuffle <- SATLearning
# SATShuffle$InPhaseTransition <- sample(SATLearning$InPhaseTransition)
#logistic with RE (intercept and slope),demographics = PT insignificant, female & early arvo +ve at 1%
# acc_learn = glmer(correct ~ InPhaseTransition + (1|ParticipantID) + 
#                     (InPhaseTransition|ParticipantID), family=binomial(link="logit"), 
#                   data=SATLearning)
# # summary(acc_learn)




# #repeat for trimmed file
# SATAggTrimmed <- merge(SATAggTrimmed,SATDemographics)


#accuracy FE regressions (PT)
#logistic with FE 
SAT_acc_fe_PT = glm(correct ~ InPhaseTransition,
                    family=binomial(link="logit"), data=SATAggData)
# summary(SAT_acc_fe_PT)

#logistic FE  and learning
SAT_acc_fe_PT_learn = glm(correct ~ InPhaseTransition + TrialNumber,
                          family=binomial(link="logit"), data=SATAggData)
#summary(SAT_acc_fe_PT_learn)
# 
#logistic FE and strat
SAT_acc_fe_PT_strat = glm(correct ~ InPhaseTransition + Process_Elimination + Brute_Force + Uniform_Signs,
                          family=binomial(link="logit"), data=SATAggData)
# summary(SAT_acc_fe_PT_strat)

#logistic FE  and multiple strat
SAT_acc_fe_PT_mstrat = glm(correct ~ InPhaseTransition + MultipleStrategies,
                           family=binomial(link="logit"), data=SATAggData)
#summary(SAT_acc_fe_PT_mstrat)


#logistic with FE using propagations
SAT_acc_fe_prop = glm(correct ~ propagations,
                      family=binomial(link="logit"), data=SATAggData)
#summary(SAT_acc_fe_prop)


#logistic FE and strat
SAT_acc_fe_prop_strat = glm(correct ~ propagations + Process_Elimination + Brute_Force + Uniform_Signs,
                            family=binomial(link="logit"), data=SATAggData)
#summary(SAT_acc_fe_prop_strat)

#logistic FE  and multiple strat
SAT_acc_fe_prop_mstrat = glm(correct ~ propagations + MultipleStrategies,
                             family=binomial(link="logit"), data=SATAggData)
#summary(SAT_acc_fe_prop_mstrat)

# 
# #logistic FE and demographics 
# acc_fe_PT_dem = glm(correct ~ InPhaseTransition + Sex_Female + Age + PostGrad + Commerce + Early_Afternoon
#                     + Late_Afternoon, family=binomial(link="logit"), data=SATAggData)
# #summary(acc_fe_PT_dem)
# 
# #logistic FE and most  cont. 
# acc_fe_PT_all = glm(correct ~ InPhaseTransition + MultipleStrategies + TrialNumber + 
#                       Sex_Female + Age + PostGrad + Commerce + Early_Afternoon + Late_Afternoon,
#                     family=binomial(link="logit"), data=SATAggData)
# #summary(acc_fe_PT_all)
# 
# 
# # stargazer(acc_fe_PT, acc_fe_PT_learn, acc_fe_PT_strat, acc_fe_PT_dem, acc_fe_PT_sig,
# #            acc_fe_PT_all, title="SAT Fixed Effects", align=TRUE)
# 
# 
# 
# 
# 
# 
#accuracy ME regressions (intercept)
#logistic with ME (intercept)
acc_me_PT = glmer(correct ~ InPhaseTransition + (1|ParticipantID), family=binomial(link="logit"), data=SATAggData)
# summary(acc_me_PT)
# 
# #logistic ME (intercept)  and learning 
# acc_me_PT_learn = glmer(correct ~ InPhaseTransition + TrialNumber + (1|ParticipantID),
#                         family=binomial(link="logit"), data=SATAggData)
# #summary(acc_me_PT_learn)
# 
# #logistic ME (intercept)  and strat 
# acc_me_PT_strat = glmer(correct ~ InPhaseTransition + Process_Elimination + Brute_Force + Uniform_Signs + 
#                           MultipleStrategies + (1|ParticipantID), family=binomial(link="logit"), 
#                         data=SATAggData)
# #summary(acc_me_PT_strat)
# 
# #logistic ME (intercept)  and demographics
# acc_me_PT_dem = glmer(correct ~ InPhaseTransition + Sex_Female + Age + PostGrad + Commerce + Early_Afternoon
#                       + Late_Afternoon + (1|ParticipantID), family=binomial(link="logit"), data=SATAggData)
# #summary(acc_me_PT_dem)
# 
# #logistic ME (intercept)  and significant cont. 
# acc_me_PT_sig = glmer(correct ~ InPhaseTransition + MultipleStrategies + TrialNumber + 
#                         Sex_Female + Early_Afternoon + Late_Afternoon + (1|ParticipantID),
#                       family=binomial(link="logit"), data=SATAggData)
# #summary(acc_me_PT_sig)
# 
# #logistic ME (intercept)  and most  cont.
# acc_me_PT_all = glmer(correct ~ InPhaseTransition + MultipleStrategies + TrialNumber + 
#                         Sex_Female + Age + PostGrad + Commerce + Early_Afternoon + Late_Afternoon + (1|ParticipantID),
#                       family=binomial(link="logit"), data=SATAggData)
# #summary(acc_me_PT_all)
# 
# 
# # stargazer(acc_me_PT, acc_me_PT_learn, acc_me_PT_strat, acc_me_PT_dem, acc_me_PT_sig,
# #            acc_me_PT_all, title="SAT Mixed Effects (Intercept)", align=TRUE)
# 
# 
# 
# 
# 
# #accuracy ME regressions (intercept + slope)
# #logistic with ME (intercept and slope)
# SAT_acc_mes_PT = glmer(correct ~ InPhaseTransition + (1|ParticipantID) + (InPhaseTransition|ParticipantID),
#                    family=binomial(link="logit"), data=SATAggData)
# #summary(SAT_acc_mes_PT)
# 
# #logistic ME (intercept and slope)  and learning
# SAT_acc_mes_PT_learn = glmer(correct ~ InPhaseTransition + TrialNumber + (1|ParticipantID) + (InPhaseTransition|ParticipantID),
#                          family=binomial(link="logit"), data=SATAggData)
# #summary(acc_mes_PT_learn)
# # 
# #logistic ME (intercept and slope)  and strat
# SAT_acc_mes_PT_strat = glmer(correct ~ InPhaseTransition + Process_Elimination + Brute_Force + Uniform_Signs +
#                           (InPhaseTransition|ParticipantID),
#                          family=binomial(link="logit"), data=SATAggData)
# #summary(SAT_acc_mes_PT_strat)
# 
# #logistic ME (intercept and slope)  and multiple strat
# SAT_acc_mes_PT_mstrat = glmer(correct ~ InPhaseTransition + MultipleStrategies + (InPhaseTransition|ParticipantID),
#                              family=binomial(link="logit"), data=SATAggData)
# #summary(SAT_acc_mes_PT_mstrat)
# 
# #logistic ME (intercept and slope)  and demographics 
# acc_mes_PT_dem = glmer(correct ~ InPhaseTransition + Sex_Female + Age + PostGrad + Commerce + Early_Afternoon
#                        + Late_Afternoon + (1|ParticipantID) + (InPhaseTransition|ParticipantID),
#                        family=binomial(link="logit"), data=SATAggData)
# #summary(acc_mes_PT_dem)
# 
# #logistic ME (intercept and slope)  and significant cont. 
# acc_mes_PT_sig = glmer(correct ~ InPhaseTransition + MultipleStrategies + TrialNumber + 
#                          Sex_Female + Early_Afternoon + Late_Afternoon + (1|ParticipantID) + (InPhaseTransition|ParticipantID),
#                        family=binomial(link="logit"), data=SATAggData)
# #summary(acc_mes_PT_sig)
# 
# #logistic ME (intercept and slope)  and most  cont. 
# acc_mes_PT_all = glmer(correct ~ InPhaseTransition + MultipleStrategies + TrialNumber + 
#                          Sex_Female + Age + PostGrad + Commerce + Early_Afternoon + Late_Afternoon + 
#                          (1|ParticipantID) + (InPhaseTransition|ParticipantID),
#                        family=binomial(link="logit"), data=SATAggData)
# #summary(acc_mes_PT_all)
# 
# 
# # stargazer(acc_mes_PT, acc_mes_PT_learn, acc_mes_PT_strat, acc_mes_PT_dem, acc_mes_PT_sig,
# #            acc_mes_PT_all, title="SAT Mixed Effects (Intercept + Slope)", align=TRUE)
# 
# 
# 
# 
# 
# 
# 
# #accuracy ME regressions (intercept + slope), with type
# #logistic with ME (intercept and slope) 
# acc_mes_type = glmer(correct ~ Difficulty + (1|ParticipantID) + (Difficulty|ParticipantID), 
#                      family=binomial(link="logit"), data=SATAggData)
# #summary(acc_mes_type)
# 
# #logistic ME (intercept and slope)  and learning
# acc_mes_type_learn = glmer(correct ~ Difficulty + TrialNumber + (1|ParticipantID) + (Difficulty|ParticipantID),
#                            family=binomial(link="logit"), data=SATAggData)
# #summary(acc_mes_type_learn)
# 
# #logistic ME (intercept and slope)  and strat 
# acc_mes_type_strat = glmer(correct ~ Difficulty + Process_Elimination + Brute_Force + Uniform_Signs + 
#                              MultipleStrategies + (Difficulty|ParticipantID),
#                            family=binomial(link="logit"), data=SATAggData)
# #summary(acc_mes_type_strat)
# 
# #logistic ME (intercept and slope)  and demographics 
# acc_mes_type_dem = glmer(correct ~ Difficulty + Sex_Female + Age + PostGrad + Commerce + Early_Afternoon
#                          + Late_Afternoon + (1|ParticipantID) + (Difficulty|ParticipantID),
#                          family=binomial(link="logit"), data=SATAggData)
# #summary(acc_mes_type_dem)
# 
# #logistic ME (intercept and slope)  and significant cont. 
# acc_mes_type_sig = glmer(correct ~ Difficulty + MultipleStrategies + TrialNumber + 
#                            Sex_Female + Early_Afternoon + Late_Afternoon + (1|ParticipantID) + (Difficulty|ParticipantID),
#                          family=binomial(link="logit"), data=SATAggData)
# #summary(acc_mes_type_sig)
# 
# #logistic ME (intercept and slope)  and most  cont. 
# acc_mes_type_all = glmer(correct ~ Difficulty + MultipleStrategies + TrialNumber + 
#                            Sex_Female + Age + PostGrad + Commerce + Early_Afternoon + Late_Afternoon + 
#                            (1|ParticipantID) + (Difficulty|ParticipantID),
#                          family=binomial(link="logit"), data=SATAggData)
# #summary(acc_mes_type_all)
# 
# 
# # stargazer(acc_mes_type, acc_mes_type_learn, acc_mes_type_strat, acc_mes_type_dem, acc_mes_type_sig,
# #           acc_mes_type_all, title="SAT Mixed Effects (Intercept + Slope)", align=TRUE)
# 
# 
# 
# 
# 
# 
# 
# 
# #time FE regressions (PT)
#time FE regressions
#logistic with FE
SAT_time_fe_PT = lm(TimeOnTask ~ InPhaseTransition,
                    data=SATAggData)
summary(SAT_time_fe_PT)

#logistic FE and learning
SAT_time_fe_PT_learn = lm(TimeOnTask ~ InPhaseTransition + TrialNumber,
                          data=SATAggData)
#summary(SAT_time_fe_PT_learn)

#logistic FE  and strat
SAT_time_fe_PT_strat = lm(TimeOnTask ~ InPhaseTransition +  Process_Elimination + Brute_Force + 
                            Uniform_Signs,
                          data=SATAggData)
#summary(SAT_time_fe_PT_strat)


#logistic FE and multiple strat
SAT_time_fe_PT_mstrat = lm(TimeOnTask ~ InPhaseTransition + MultipleStrategies ,data=SATAggData)
#summary(SAT_time_fe_PT_mstrat)



#logistic with FE using propagations
SAT_time_fe_prop = lm(TimeOnTask ~ propagations,
                      data=SATAggData)
# summary(SAT_time_fe_prop)

#logistic FE  and strat
SAT_time_fe_prop_strat = lm(TimeOnTask ~ propagations +  Process_Elimination + Brute_Force + 
                              Uniform_Signs,
                            data=SATAggData)
#summary(SAT_time_fe_prop_strat)

#logistic FE and multiple strat
SAT_time_fe_prop_mstrat = lm(TimeOnTask ~ propagations + MultipleStrategies ,data=SATAggData)
#summary(SAT_time_fe_prop_mstrat)
# 
# #logistic FE and significant cont. 
# time_fe_PT_sig = lm(TimeOnTask ~ InPhaseTransition + Process_Elimination + Brute_Force + MultipleStrategies 
#                     + TrialNumber + Early_Afternoon + Late_Afternoon +  + PostGrad, data=SATAggData)
# #summary(time_fe_PT_sig)
# 
# #logistic FE and most  cont. 
# time_fe_PT_all = lm(TimeOnTask ~ InPhaseTransition + MultipleStrategies + Process_Elimination + Brute_Force + 
#                       Sex_Female + Age + PostGrad + Commerce + Early_Afternoon + Late_Afternoon + TrialNumber,
#                     data=SATAggData)
# #summary(time_fe_PT_all)
# 
# 
# # stargazer(time_fe_PT, time_fe_PT_learn, time_fe_PT_strat, time_fe_PT_dem, time_fe_PT_sig,
# #            time_fe_PT_all, title="SAT Fixed Effects", align=TRUE)
# 
# 
# 
# 
# 
# 
# #time ME regressions (intercept)
# #logistic with ME (intercept) 
# time_me_PT = lmer(TimeOnTask ~ InPhaseTransition + (1|ParticipantID), data=SATAggData)
# #summary(time_me_PT)
# 
# #logistic ME (intercept)  and learning 
# time_me_PT_learn = lmer(TimeOnTask ~ InPhaseTransition + TrialNumber + (1|ParticipantID),
#                         data=SATAggData)
# #summary(time_me_PT_learn)
# 
# #logistic ME (intercept)  and strat
# time_me_PT_strat = lmer(TimeOnTask ~ InPhaseTransition + Process_Elimination + Brute_Force + Uniform_Signs
#                         + MultipleStrategies + (1|ParticipantID), data=SATAggData)
# #summary(time_me_PT_strat)
# 
# #logistic ME (intercept)  and demographics 
# time_me_PT_dem = lmer(TimeOnTask ~ InPhaseTransition + Sex_Female + Age + PostGrad + Commerce + Early_Afternoon
#                       + Late_Afternoon + (1|ParticipantID), data=SATAggData)
# #summary(time_me_PT_dem)
# 
# #logistic ME (intercept)  and most  cont. 
# time_me_PT_all = lmer(TimeOnTask ~ InPhaseTransition + MultipleStrategies +  TrialNumber  + 
#                         Sex_Female + Age + PostGrad + Commerce + Early_Afternoon + Late_Afternoon + (1|ParticipantID),
#                       data=SATAggData)
# #summary(time_me_PT_all)
# 
# 
# # stargazer(time_me_PT, time_me_PT_learn, time_me_PT_strat, time_me_PT_dem,
# #            time_me_PT_all, title="SAT Mixed Effects (Intercept)", align=TRUE)
# 
# 
# 
# 
# 
# #time ME regressions (intercept + slope)
# #logistic with ME (intercept and slope)
# SAT_time_mes_PT = lmer(TimeOnTask ~ InPhaseTransition + (1|ParticipantID) + (InPhaseTransition|ParticipantID),
#                    data=SATAggData)
# #summary(SAT_time_mes_PT)
# 
# #logistic ME (intercept and slope)  and learning
# SAT_time_mes_PT_learn = lmer(TimeOnTask ~ InPhaseTransition + TrialNumber + (1|ParticipantID) + (InPhaseTransition|ParticipantID),
#                          data=SATAggData)
# #summary(SAT_time_mes_PT_learn)
# 
# #logistic ME (intercept and slope)  and strat
# SAT_time_mes_PT_strat = lmer(TimeOnTask ~ InPhaseTransition +  Process_Elimination + Brute_Force + 
#                                Uniform_Signs + (1|ParticipantID) + (InPhaseTransition|ParticipantID),
#                              data=SATAggData)
# #summary(SAT_time_mes_PT_strat)
# # 
# 
# #logistic ME (intercept and slope)  and multiple strat
# SAT_time_mes_PT_mstrat = lmer(TimeOnTask ~ InPhaseTransition + MultipleStrategies + (1|ParticipantID) + 
#                                 (InPhaseTransition|ParticipantID),data=SATAggData)
# #summary(SAT_time_mes_PT_mstrat)

# #logistic ME (intercept and slope)  and demographics
# time_mes_PT_dem = lmer(TimeOnTask ~ InPhaseTransition + Sex_Female + Age + PostGrad + Commerce + Early_Afternoon
#                        + Late_Afternoon + (1|ParticipantID) + (InPhaseTransition|ParticipantID),
#                        data=SATAggData)
# #summary(time_mes_PT_dem)
# 
# #logistic ME (intercept and slope)  and most  cont. 
# time_mes_PT_all = lmer(TimeOnTask ~ InPhaseTransition + MultipleStrategies + 
#                          Sex_Female + Age + PostGrad + Commerce + Early_Afternoon + Late_Afternoon + TrialNumber + 
#                          (1|ParticipantID) + (InPhaseTransition|ParticipantID),
#                        data=SATAggData)
# #summary(time_mes_PT_all)
# 
# 
# # stargazer(time_mes_PT, time_mes_PT_learn, time_mes_PT_strat, time_mes_PT_dem,
# #            time_mes_PT_all, title="SAT Mixed Effects (Intercept + Slope)", align=TRUE)
# 
# 
# 
# 
# 
# 
# 
# #time ME regressions (intercept + slope), with type
# #logistic with ME (intercept and slope) 
# time_mes_type = lmer(TimeOnTask ~ Difficulty + (1|ParticipantID) + (Difficulty|ParticipantID), 
#                      data=SATAggData)
# #summary(time_mes_type)
# 
# #logistic ME (intercept and slope)  and learning
# time_mes_type_learn = lmer(TimeOnTask ~ Difficulty + TrialNumber + (1|ParticipantID) + (Difficulty|ParticipantID),
#                            data=SATAggData)
# #summary(time_mes_type_learn)
# 
# #logistic ME (intercept and slope)  and strat 
# time_mes_type_strat = lmer(TimeOnTask ~ Difficulty + Process_Elimination + Brute_Force + Uniform_Signs
#                            + MultipleStrategies + (Difficulty|ParticipantID), data=SATAggData)
# #summary(time_mes_type_strat)
# 
# #logistic ME (intercept and slope)  and demographics
# time_mes_type_dem = lmer(TimeOnTask ~ Difficulty + Sex_Female + Age + PostGrad + Commerce + Early_Afternoon
#                          + Late_Afternoon + (1|ParticipantID) + (Difficulty|ParticipantID),
#                          data=SATAggData)
# #summary(time_mes_type_dem)
# 
# #logistic ME (intercept and slope)  and most  cont.
# time_mes_type_all = lmer(TimeOnTask ~ Difficulty + MultipleStrategies + TrialNumber + Brute_Force + 
#                            Sex_Female + Age + PostGrad + Commerce + Early_Afternoon + Late_Afternoon + 
#                            (1|ParticipantID) + (Difficulty|ParticipantID),
#                          data=SATAggData)
# #summary(time_mes_type_all)
# 
# 
# # stargazer(time_mes_type, time_mes_type_learn, time_mes_type_strat, time_mes_type_dem,
# #            time_mes_type_all, title="SAT Mixed Effects (Intercept + Slope)", align=TRUE)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #logistic with FE and no controls = Time insignificant
# acc_YES = glm(correct ~ TimeOnTask, family=binomial(link="logit"), data=SATYes)
# #summary(acc_YES)
# 
# #logistic with FE and no controls = Time significantly negative at 1% - opposite of what we'd expect
# acc_No = glm(correct ~ TimeOnTask, family=binomial(link="logit"), data=SATNo)
# #summary(acc_No)
# 
# #logistic with FE and no controls = Time significantly negative at 5% - opposite of what we'd expect
# acc_time = glm(correct ~ TimeOnTask, family=binomial(link="logit"), data=SATAggData)
# #summary(acc_time)
# 
# 
# #logistic with FE and no controls = Time significantly negative at 1%
# acc_Under = glm(correct ~ TimeOnTask, family=binomial(link="logit"), data=SATUnder)
# #summary(acc_Under)
# 
# #logistic with FE and no controls = Time significantly positive at 5%
# acc_Over = glm(correct ~ TimeOnTask, family=binomial(link="logit"), data=SATOver)
# #summary(acc_Over)
# 
# #logistic with FE and no controls = Time insignificant
# acc_PT = glm(correct ~ TimeOnTask, family=binomial(link="logit"), data=SATPT)
# #summary(acc_PT)


SATAggDataPT <- subset(SATAggData, type<=4)

  #SAT summary stats
  #two-sampled t-test for differences in mean accuracy b/w in/out of PT, not significant
tbl = table(SATAggDataPT$InPhaseTransition, SATAggDataPT$correct)
# View(tbl)
chisq.test(tbl)
# #two-sampled t-test for differences in mean time spent b/w in/out of PT, significant at 2%
# t.test(TimeOnTask ~ InPhaseTransition, data = SATAggData)
# 
# #two-sampled t-test for differences in mean accuracy b/w under-constrained & PT, not significant
# SAT_under_in <- filter(SATAggData, SATAggData$Region=="PT" | SATAggData$Region=="Under")
# tbl = table(SAT_under_in$Region, SAT_under_in$correct) 
# chisq.test(tbl)
# #two-sampled t-test for differences in mean time spent b/w under-constrained & PT, significant at 1%
# t.test(TimeOnTask ~ Region, data = SAT_under_in)
# 
# #two-sampled t-test for differences in mean accuracy b/w over-constrained & PT, not significant
# SAT_over_in <- filter(SATAggData, SATAggData$Region=="PT" | SATAggData$Region=="Over")
# tbl = table(SAT_over_in$Region, SAT_over_in$correct) 
# chisq.test(tbl)
# #two-sampled t-test for differences in mean time spent b/w over-constrained & PT, significant at 5%
# t.test(TimeOnTask ~ Region, data = SAT_over_in)
# 

#aggregate(SATAggData[,c(1,3)], by = list(SATAggData$type),mean, na.rm = TRUE)




