#set working directory and load packages
setwd("~/Desktop/Final Experiment/TSP")
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

#TSP-Task
#set working directory and load packages
setwd("~/Desktop/Final Experiment/TSP")

#create a list of the input files, one for each type
TSPInstanceFiles = list.files(path = "/Users/KRDoroc/Desktop/Final Experiment/TSP",
                              pattern="*InstancesInfo.txt$")
TSPInfoFiles = list.files(path = "/Users/KRDoroc/Desktop/Final Experiment/TSP",pattern="*TrialInfo.txt$")
TSPTimeStampFiles = list.files(path = "/Users/KRDoroc/Desktop/Final Experiment/TSP",
                               pattern="*TimeStamps.txt$")
#create a list of the participant ID
pIDs=sapply(strsplit(TSPInstanceFiles,"_"),function(x) x[1])

#load all the trialinfo files and add participant ID as a column
for(tspInfoFile in TSPInfoFiles){
  TSPTrialInfo = lapply(TSPInfoFiles, read.csv, stringsAsFactors=F,skip=1,header = TRUE, sep =";"
                        ,quote = "") %>% 
    bind_rows
  TSPTrialInfo <- arrange(TSPTrialInfo,block,trial)
  TSPTrialInfo$ParticipantID <-(pIDs)
}

#load all the instanceinfo files
for(tspInstanceFile in TSPInstanceFiles){
  TSPInstanceInfo = lapply(TSPInstanceFiles, read.csv, stringsAsFactors=F,skip=1,header = TRUE, sep =";"
                           ,quote = "") %>% 
    bind_rows
}

##load all the time stamps files, find the time spent on each trial (deleting some irrelevant columns),
#and add participant ID as a column
for(tspTimeStampFile in TSPTimeStampFiles){
  TSPTimeStamps = lapply(TSPTimeStampFiles, read.csv, stringsAsFactors=F,skip=2,header = TRUE, sep =";"
                         ,quote = "") %>% 
    bind_rows
  TSPTime <- diff(TSPTimeStamps$elapsedTime, lag=1, differences=1)/1000
  TSPTimeStamps <- TSPTimeStamps[-nrow(TSPTimeStamps),]
  TSPTimeStamps$TimeOnTask <- TSPTime
  TSPTimeStamps <- subset(TSPTimeStamps, eventType=="Trial")
  TSPTimeStamps <- TSPTimeStamps[ -c(3:4) ]
  TSPTimeStamps <- arrange(TSPTimeStamps,block,trial)
  TSPTimeStamps$ParticipantID <-(pIDs)
}

#only keeps the instance number and type from the instance info
TSPInstanceInfo = {dplyr::select(TSPInstanceInfo, instanceNumber, type)}

#merge with trial and instance info
TSPTrials <- merge(TSPTrialInfo,TSPInstanceInfo)
TSPTrials <- unique(TSPTrials)
TSPTrials <- arrange(TSPTrials,ParticipantID,block,trial)
rownames(TSPTrials) <- NULL

#merge time stamps with previous inputs to create a single file with aggregate data
TSPAggData <- merge(TSPTrials,TSPTimeStamps)
TSPAggData <- unique(TSPAggData)

# #merge decisions & propagations file with previous inputs to create a single file with aggregate data
# TSPComplexity <- read.table(file = "ComplexityTSP.csv", header = TRUE, sep = ",")
# TSPAggData <- merge(TSPAggData,TSPComplexity)

#delete unnecessary columns
TSPAggData <- TSPAggData[ -c(5,7:9,11) ]

#create separate data sets based on whether instances have YES or NO solutions
TSPYes <- subset(TSPAggData, randomYes.1.Left.No.Right.Yes.==1)
TSPYes <- TSPYes[ -c(1:2,5:6) ]
TSPNo <- subset(TSPAggData, randomYes.1.Left.No.Right.Yes.==0)
TSPNo <- TSPNo[ -c(1:2,5:6) ]

#delete unnecessary columns
TSPAggData <- TSPAggData[ -c(6) ]

# #check for learning effect
# d <- c(ifelse(TSPAggData$block==1, TSPAggData$trial, ifelse(TSPAggData$block==2, TSPAggData$trial+9,NA)))
# TSPAggData$TrialNumber <- cbind(d)
# TSPcorData <- TSPAggData[c(4,5)]
# sjt.corr(TSPcorData, p.numeric=TRUE, triangle = "lower")
# #significant correlation (2% lvl) of 11% - no evidence of a learning effect with the TSP
# #plotting the correlation, it improves from 75% to 92% at trial 11, but then drops back down like a reverse U
# TSP_LearnPlot <- ggplot(TSPAggData, aes(TrialNumber,correct)) + geom_point() + 
#   geom_smooth(fill="blue", colour="darkblue", size=1)
# 
# TSPAggData <- TSPAggData[ -c(1:2) ]
# 
# #check if more effort = high performance
# ddply(TSPAggData, "type", summarise, Corr=cor.test(TimeOnTask, correct))
# 
# Effort_Correls <- ddply(TSPAggData, .(type), summarise,
#                         Correlation=(cor.test(TimeOnTask, correct)), Description=names(Correlation) )
# Effort_Correls <- subset(Effort_Correls, Description=="estimate" | Description=="p.value")
# #-22% (10% lvl) and -25%(5% level) correlations on types 1.05 and 1.15 (under-constrained) - rest insignificant

#create a column to see if an instance is in the PT (Yes) or not (NO)
TSPAggData$InPhaseTransition <- ("No")
TSPAggData$InPhaseTransition[TSPAggData$type==4 | TSPAggData$type==3] <- ("Yes")

#create a column to see if an instance is in the underconstrained region (Under), PT (PT), or overconstrained
#region (Over)
TSPAggData$Region[TSPAggData$InPhaseTransition=="Yes"] <- ("Phase Transition - hard")
TSPAggData$Region[TSPAggData$type<=2] <- ("Phase Transition - easy")
TSPAggData$Region[TSPAggData$type==5] <- ("Over-Constrained")
TSPAggData$Region[TSPAggData$type==6] <- ("Under-Constrained")

TSPUnder <- subset(TSPAggData, Region=="Under-Constrained")
TSPOver <- subset(TSPAggData, Region=="Over-Constrained")
TSPPT <- subset(TSPAggData, Region=="Phase Transition") 


# #check average accuracy and time on instances
# TSP_Inst_Summ <- TSPAggData %>%  # the names of the new data frame and the data frame to be summarised
#   group_by(instanceNumber) %>%
#   summarise(Accuracy = mean(correct), Std_Dev = sd(correct), Time = mean(TimeOnTask))
# 
# library(openxlsx)
# write.xlsx(TSP_Inst_Summ, 'TSP_Inst.xlsx')
# 
# #check average accuracy and time by participant
# TSP_Part_Summ <- TSPAggData %>%  # the names of the new data frame and the data frame to be summarised
#   group_by(ParticipantID) %>%
#   summarise(Accuracy = mean(correct), Std_Dev = sd(correct), Time = mean(TimeOnTask))
# 
# write.xlsx(TSP_Part_Summ, 'TSP_Part.xlsx')



#TSP plots
#create a summary table for accuracy of in vs out PT
TSP_Acc_Sum_PT <- TSPAggData %>% # the names of the new data frame and the data frame to be summarised
  group_by(InPhaseTransition) %>%   # the grouping variable
  summarise(mean_PT = mean(correct),  # calculates the mean of each group
            sd_PT = sd(correct), # calculates the standard deviation of each group
            n_PT = n(),  # calculates the sample size per group
            SE_PT = sd(correct)/sqrt(n())) # calculates the standard error of each group

#create a summary table for time spent of in vs out PT
TSP_Time_Sum_PT <- TSPAggData %>% # the names of the new data frame and the data frame to be summarised
  group_by(InPhaseTransition) %>%   # the grouping variable
  summarise(mean_PT = mean(TimeOnTask),  # calculates the mean of each group
            sd_PT = sd(TimeOnTask), # calculates the standard deviation of each group
            n_PT = n(),  # calculates the sample size per group
            SE_PT = sd(TimeOnTask)/sqrt(n())) # calculates the standard error of each group

#create a summary table for accuracy of region
TSP_Acc_Sum_Region <- TSPAggData %>% # the names of the new data frame and the data frame to be summarised
  group_by(Region) %>%   # the grouping variable
  summarise(mean_region = mean(correct),  # calculates the mean of each group
            sd_region = sd(correct), # calculates the standard deviation of each group
            n_region = n(),  # calculates the sample size per group
            SE_region = sd(correct)/sqrt(n())) # calculates the standard error of each group

#create a summary table for time spent of region
TSP_Time_Sum_Region <- TSPAggData %>% # the names of the new data frame and the data frame to be summarised
  group_by(Region) %>%   # the grouping variable
  summarise(mean_region = mean(TimeOnTask),  # calculates the mean of each group
            sd_region = sd(TimeOnTask), # calculates the standard deviation of each group
            n_region = n(),  # calculates the sample size per group
            SE_region = sd(TimeOnTask)/sqrt(n())) # calculates the standard error of each group
TSP_Time_Sum_Region$Region[TSP_Time_Sum_Region$Region=="Under-Constrained"] <- ("Under")
TSP_Time_Sum_Region$Region[TSP_Time_Sum_Region$Region=="Phase Transition"] <- ("PT")
TSP_Time_Sum_Region$Region[TSP_Time_Sum_Region$Region=="Over-Constrained"] <- ("Over")



# TSPAggData$TimeOnTask.t <- TSPAggData$TimeOnTask
# qqp(TSPAggData$TimeOnTask.t, "norm")





#TSP analyses
#importing demographics and merging with existing aggregated data
TSPDemographics <- read.table(file = "Demographics.csv", header = TRUE, sep = ",")
TSPDemographics$Sex_Female[TSPDemographics$Gender=="Female"] <- (1)
TSPDemographics$Sex_Female[TSPDemographics$Gender=="Male"] <- (0)
TSPDemographics$PostGrad[TSPDemographics$Years_of_study>3] <- (1)
TSPDemographics$PostGrad[TSPDemographics$Years_of_study<4] <- (0)
TSPDemographics$Commerce[TSPDemographics$Degree=="Commerce"] <- (1)
TSPDemographics$Commerce[TSPDemographics$Degree!="Commerce"] <- (0)
TSPDemographics$Early_Afternoon[TSPDemographics$Time_of_day=="Early_Afternoon"] <- (1)
TSPDemographics$Early_Afternoon[TSPDemographics$Time_of_day!="Early_Afternoon"] <- (0)
TSPDemographics$Late_Afternoon[TSPDemographics$Time_of_day=="Late_Afternoon"] <- (1)
TSPDemographics$Late_Afternoon[TSPDemographics$Time_of_day!="Late_Afternoon"] <- (0)
names(TSPDemographics)[names(TSPDemographics) == 'Convex.Hull'] <- 'Convex_Hull'
names(TSPDemographics)[names(TSPDemographics) == 'Nearest.Neighbour'] <- 'Nearest_Neighbour'
TSPDemographics$MultipleStrategies <- TSPDemographics$Total
TSPDemographics$MultipleStrategies[TSPDemographics$MultipleStrategies<=1] <- (0)
TSPDemographics$MultipleStrategies[TSPDemographics$MultipleStrategies>1] <- (1)
TSPDemographics <- TSPDemographics[ -c(2:4,6:8,10,12:15) ]


TSPAggData <- merge(TSPAggData,TSPDemographics)

TSPAggData$Difficulty[TSPAggData$Region=="Under-Constrained"] <- (1)
TSPAggData$Difficulty[TSPAggData$Region=="Over-Constrained"] <- (2)
TSPAggData$Difficulty[TSPAggData$Region=="Phase Transition"] <- (3)

TSPLearning <- subset(TSPAggData, TrialNumber>6)


# #accuracy FE regressions (PT)
#accuracy FE regressions
#logistic with FE
TSP_acc_fe_PT = glm(correct ~ InPhaseTransition,
                    family=binomial(link="logit"), data=TSPAggData)
# summary(TSP_acc_fe_PT)

#logistic FE  and learning
TSP_acc_fe_PT_learn = glm(correct ~ InPhaseTransition + TrialNumber,
                          family=binomial(link="logit"), data=TSPAggData)
#summary(TSP_acc_fe_PT_learn)

#logistic FE  and strat
TSP_acc_fe_PT_strat = glm(correct ~ InPhaseTransition + Convex_Hull + Nearest_Neighbour,
                          family=binomial(link="logit"), data=TSPAggData)
#summary(TSP_acc_fe_PT_strat)

#logistic FE  and multiple strat
TSP_acc_fe_PT_mstrat = glm(correct ~ InPhaseTransition +
                             MultipleStrategies,
                           family=binomial(link="logit"), data=TSPAggData)
#summary(TSP_acc_fe_PT_mstrat)

#logistic with FE using propagations as main IV
TSP_acc_fe_prop = glm(correct ~ propagations,
                      family=binomial(link="logit"), data=TSPAggData)
#summary(TSP_acc_fe_prop)

#logistic FE  and learning
TSP_acc_fe_prop_learn = glm(correct ~ propagations + TrialNumber,
                            family=binomial(link="logit"), data=TSPAggData)
#summary(TSP_acc_fe_prop_learn)

#logistic FE  and strat
TSP_acc_fe_prop_strat = glm(correct ~ propagations + Convex_Hull + Nearest_Neighbour,
                            family=binomial(link="logit"), data=TSPAggData)
#summary(TSP_acc_fe_prop_strat)

#logistic FE  and multiple strat
TSP_acc_fe_prop_mstrat = glm(correct ~ propagations +
                               MultipleStrategies,
                             family=binomial(link="logit"), data=TSPAggData)
#summary(TSP_acc_fe_prop_mstrat)

# 
# #logistic FE and demographics 
# acc_fe_PT_dem = glm(correct ~ InPhaseTransition + Sex_Female + Age + PostGrad + Commerce + Early_Afternoon
#                     + Late_Afternoon, family=binomial(link="logit"), data=TSPAggData)
# #summary(acc_fe_PT_dem)
# 
# #logistic FE and significant cont. 
# acc_fe_PT_sig = glm(correct ~ InPhaseTransition + Age + MultipleStrategies,
#                     family=binomial(link="logit"), data=TSPAggData)
# #summary(acc_fe_PT_sig)
# 
# #logistic FE and most  cont. 
# acc_fe_PT_all = glm(correct ~ InPhaseTransition + MultipleStrategies + 
#                       Sex_Female + Age + PostGrad + Commerce + Early_Afternoon + Late_Afternoon,
#                     family=binomial(link="logit"), data=TSPAggData)
# #summary(acc_fe_PT_all)
# 
# 
# # stargazer(acc_fe_PT, acc_fe_PT_learn, acc_fe_PT_strat, acc_fe_PT_dem, acc_fe_PT_sig,
# #            acc_fe_PT_all, title="TSP Fixed Effects", align=TRUE)
# 
# 
# 
# 
# 
# 
#accuracy ME regressions (intercept)
#logistic with ME (intercept)
acc_me_PT = glmer(correct ~ InPhaseTransition + (1|ParticipantID), family=binomial(link="logit"), data=TSPAggData)
#summary(acc_me_PT)
# 
# #logistic ME (intercept)  and learning 
# acc_me_PT_learn = glmer(correct ~ InPhaseTransition + TrialNumber + (1|ParticipantID),
#                         family=binomial(link="logit"), data=TSPAggData)
# #summary(acc_me_PT_learn)
# 
# #logistic ME (intercept)  and strat 
# acc_me_PT_strat = glmer(correct ~ InPhaseTransition + Convex_Hull + Nearest_Neighbour + 
#                           MultipleStrategies + (1|ParticipantID), family=binomial(link="logit"), 
#                         data=TSPAggData)
# #summary(acc_me_PT_strat)
# 
# #logistic ME (intercept)  and demographics
# acc_me_PT_dem = glmer(correct ~ InPhaseTransition + Sex_Female + Age + PostGrad + Commerce + Early_Afternoon
#                       + Late_Afternoon + (1|ParticipantID), family=binomial(link="logit"), data=TSPAggData)
# #summary(acc_me_PT_dem)
# 
# #logistic ME (intercept)  and significant cont. 
# acc_me_PT_sig = glmer(correct ~ InPhaseTransition + MultipleStrategies + Age + (1|ParticipantID),
#                       family=binomial(link="logit"), data=TSPAggData)
# #summary(acc_me_PT_sig)
# 
# #logistic ME (intercept)  and most  cont.
# acc_me_PT_all = glmer(correct ~ InPhaseTransition + MultipleStrategies + 
#                         Sex_Female + Age + PostGrad + Commerce + Early_Afternoon + Late_Afternoon + (1|ParticipantID),
#                       family=binomial(link="logit"), data=TSPAggData)
# #summary(acc_me_PT_all)
# 
# 
# # stargazer(acc_me_PT, acc_me_PT_learn, acc_me_PT_strat, acc_me_PT_dem, acc_me_PT_sig,
# #            acc_me_PT_all, title="TSP Mixed Effects (Intercept)", align=TRUE)
# 
# 
# 
# 
# 
# #accuracy ME regressions (intercept + slope)
# #logistic with ME (intercept and slope)
# TSP_acc_mes_PT = glmer(correct ~ InPhaseTransition + (1|ParticipantID) + (InPhaseTransition|ParticipantID),
#                    family=binomial(link="logit"), data=TSPAggData)
# #summary(TSP_acc_mes_PT)
# 
# #logistic ME (intercept and slope)  and learning
# TSP_acc_mes_PT_learn = glmer(correct ~ InPhaseTransition + TrialNumber + (1|ParticipantID) + (InPhaseTransition|ParticipantID),
#                          family=binomial(link="logit"), data=TSPAggData)
# #summary(TSP_acc_mes_PT_learn)
# 
# #logistic ME (intercept and slope)  and strat
# TSP_acc_mes_PT_strat = glmer(correct ~ InPhaseTransition + Convex_Hull + Nearest_Neighbour +
#                           (InPhaseTransition|ParticipantID) + (1|ParticipantID),
#                          family=binomial(link="logit"), data=TSPAggData)
# #summary(TSP_acc_mes_PT_strat)
# 
# #logistic ME (intercept and slope)  and multiple strat
# TSP_acc_mes_PT_mstrat = glmer(correct ~ InPhaseTransition +
#                                MultipleStrategies + (InPhaseTransition|ParticipantID) + (1|ParticipantID),
#                              family=binomial(link="logit"), data=TSPAggData)
# #summary(TSP_acc_mes_PT_mstrat)

# #logistic ME (intercept and slope)  and demographics 
# acc_mes_PT_dem = glmer(correct ~ InPhaseTransition + Sex_Female + Age + PostGrad + Commerce + Early_Afternoon
#                        + Late_Afternoon + (1|ParticipantID) + (InPhaseTransition|ParticipantID),
#                        family=binomial(link="logit"), data=TSPAggData)
# #summary(acc_mes_PT_dem)
# 
# #logistic ME (intercept and slope)  and significant cont. 
# acc_mes_PT_sig = glmer(correct ~ InPhaseTransition + MultipleStrategies + Age
#                        + (1|ParticipantID) + (InPhaseTransition|ParticipantID),
#                        family=binomial(link="logit"), data=TSPAggData)
# #summary(acc_mes_PT_sig)
# 
# #logistic ME (intercept and slope)  and most  cont. 
# acc_mes_PT_all = glmer(correct ~ InPhaseTransition + MultipleStrategies + 
#                          Sex_Female + Age + PostGrad + Commerce + Early_Afternoon + Late_Afternoon + 
#                          (1|ParticipantID) + (InPhaseTransition|ParticipantID),
#                        family=binomial(link="logit"), data=TSPAggData)
# #summary(acc_mes_PT_all)
# 
# 
# # stargazer(acc_mes_PT, acc_mes_PT_learn, acc_mes_PT_strat, acc_mes_PT_dem, acc_mes_PT_sig,
# #            acc_mes_PT_all, title="TSP Mixed Effects (Intercept + Slope)", align=TRUE)
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
#                      family=binomial(link="logit"), data=TSPAggData)
# #summary(acc_mes_type)
# 
# #logistic ME (intercept and slope)  and learning
# acc_mes_type_learn = glmer(correct ~ Difficulty + TrialNumber + (1|ParticipantID) + (Difficulty|ParticipantID),
#                            family=binomial(link="logit"), data=TSPAggData)
# #summary(acc_mes_type_learn)
# 
# #logistic ME (intercept and slope)  and strat 
# acc_mes_type_strat = glmer(correct ~ Difficulty + Convex_Hull + Nearest_Neighbour + 
#                              MultipleStrategies + (Difficulty|ParticipantID),
#                            family=binomial(link="logit"), data=TSPAggData)
# #summary(acc_mes_type_strat)
# 
# #logistic ME (intercept and slope)  and demographics 
# acc_mes_type_dem = glmer(correct ~ Difficulty + Sex_Female + Age + PostGrad + Commerce + Early_Afternoon
#                          + Late_Afternoon + (1|ParticipantID) + (Difficulty|ParticipantID),
#                          family=binomial(link="logit"), data=TSPAggData)
# #summary(acc_mes_type_dem)
# 
# #logistic ME (intercept and slope)  and significant cont. 
# acc_mes_type_sig = glmer(correct ~ Difficulty + MultipleStrategies + Age + 
#                            (1|ParticipantID) + (Difficulty|ParticipantID),
#                          family=binomial(link="logit"), data=TSPAggData)
# #summary(acc_mes_type_sig)
# 
# #logistic ME (intercept and slope)  and most  cont. 
# acc_mes_type_all = glmer(correct ~ Difficulty + MultipleStrategies + 
#                            Sex_Female + Age + PostGrad + Commerce + Early_Afternoon + Late_Afternoon + 
#                            (1|ParticipantID) + (Difficulty|ParticipantID),
#                          family=binomial(link="logit"), data=TSPAggData)
# #summary(acc_mes_type_all)
# 
# 
# # stargazer(acc_mes_type, acc_mes_type_learn, acc_mes_type_strat, acc_mes_type_dem, acc_mes_type_sig,
# #            acc_mes_type_all, title="TSP Mixed Effects (Intercept + Slope)", align=TRUE)
# 
# 
# 
# 
# 
# 
# 
# 
# #time FE regressions (PT)
#logistic with FE
TSP_time_fe_PT = lm(TimeOnTask ~ InPhaseTransition,
                    data=TSPAggData)
#summary(TSP_time_fe_PT)

#logistic FE  and learning
TSP_time_fe_PT_learn = lm(TimeOnTask ~ InPhaseTransition + TrialNumber,
                          data=TSPAggData)
#summary(TSP_time_fe_PT_learn)

#logistic FE and strat
TSP_time_fe_PT_strat = lm(TimeOnTask ~ InPhaseTransition + Convex_Hull + Nearest_Neighbour,data=TSPAggData)
#summary(TSP_time_fe_PT_strat)

#logistic FE and multiple strat
TSP_time_fe_PT_mstrat = lm(TimeOnTask ~ InPhaseTransition + MultipleStrategies,data=TSPAggData)
#summary(TSP_time_fe_PT_mstrat)



#logistic with FE using propagations
TSP_time_fe_prop = lm(TimeOnTask ~ propagations,
                      data=TSPAggData)
#summary(TSP_time_fe_prop)

# #logistic FE  and learning
# TSP_time_fe_prop_learn = lm(TimeOnTask ~ propagations + TrialNumber,
#                           data=TSPAggData)
# #summary(TSP_time_fe_prop_learn)

#logistic FE and strat
TSP_time_fe_prop_strat = lm(TimeOnTask ~ propagations + Convex_Hull + Nearest_Neighbour,data=TSPAggData)
#summary(TSP_time_fe_prop_strat)

#logistic FE and multiple strat
TSP_time_fe_prop_mstrat = lm(TimeOnTask ~ propagations + MultipleStrategies,data=TSPAggData)
#summary(TSP_time_fe_prop_mstrat)
# 
# #logistic FE and demographics
# time_fe_PT_dem = lm(TimeOnTask ~ InPhaseTransition + Sex_Female + Age + PostGrad + Commerce + Early_Afternoon
#                     + Late_Afternoon, data=TSPAggData)
# #summary(time_fe_PT_dem)
# 
# #logistic FE and most  cont. 
# time_fe_PT_all = lm(TimeOnTask ~ InPhaseTransition + MultipleStrategies + Convex_Hull + 
#                       Sex_Female + Age + PostGrad + Commerce + Early_Afternoon + Late_Afternoon + TrialNumber,
#                     data=TSPAggData)
# #summary(time_fe_PT_all)
# 
# 
# # stargazer(time_fe_PT, time_fe_PT_learn, time_fe_PT_strat, time_fe_PT_dem,
# #            time_fe_PT_all, title="TSP Fixed Effects", align=TRUE)
# 
# 
# 
# 
# 
# 
# #time ME regressions (intercept)
# #logistic with ME (intercept) 
# time_me_PT = lmer(TimeOnTask ~ InPhaseTransition + (1|ParticipantID), data=TSPAggData)
# #summary(time_me_PT)
# 
# #logistic ME (intercept)  and learning 
# time_me_PT_learn = lmer(TimeOnTask ~ InPhaseTransition + TrialNumber + (1|ParticipantID),
#                         data=TSPAggData)
# #summary(time_me_PT_learn)
# 
# #logistic ME (intercept)  and strat
# time_me_PT_strat = lmer(TimeOnTask ~ InPhaseTransition + Convex_Hull + Nearest_Neighbour
#                         + MultipleStrategies + (1|ParticipantID), data=TSPAggData)
# #summary(time_me_PT_strat)
# 
# #logistic ME (intercept)  and demographics 
# time_me_PT_dem = lmer(TimeOnTask ~ InPhaseTransition + Sex_Female + Age + PostGrad + Commerce + Early_Afternoon
#                       + Late_Afternoon + (1|ParticipantID), data=TSPAggData)
# #summary(time_me_PT_dem)
# 
# #logistic ME (intercept)  and most  cont. 
# time_me_PT_all = lmer(TimeOnTask ~ InPhaseTransition + Convex_Hull + MultipleStrategies +  TrialNumber  + 
#                         Sex_Female + Age + PostGrad + Commerce + Early_Afternoon + Late_Afternoon + (1|ParticipantID),
#                       data=TSPAggData)
# #summary(time_me_PT_all)
# 
# 
# # stargazer(time_me_PT, time_me_PT_learn, time_me_PT_strat, time_me_PT_dem,
# #            time_me_PT_all, title="TSP Mixed Effects (Intercept)", align=TRUE)
# 
# 
# 
# 
# 
#time ME regressions (intercept + slope)
# #logistic with ME (intercept and slope)
# TSP_time_mes_PT = lmer(TimeOnTask ~ InPhaseTransition + (1|ParticipantID) + (InPhaseTransition|ParticipantID),
#                    data=TSPAggData)
# #summary(TSP_time_mes_PT)
# 
# #logistic ME (intercept and slope)  and learning
# TSP_time_mes_PT_learn = lmer(TimeOnTask ~ InPhaseTransition + TrialNumber + (1|ParticipantID) + (InPhaseTransition|ParticipantID),
#                          data=TSPAggData)
# #summary(TSP_time_mes_PT_learn)
# 
# #logistic ME (intercept and slope)  and strat
# TSP_time_mes_PT_strat = lmer(TimeOnTask ~ InPhaseTransition + Convex_Hull + Nearest_Neighbour
#                         + (1|ParticipantID) + (InPhaseTransition|ParticipantID),data=TSPAggData)
# #summary(TSP_time_mes_PT_strat)
# 
# #logistic ME (intercept and slope)  and multiple strat
# TSP_time_mes_PT_mstrat = lmer(TimeOnTask ~ InPhaseTransition + MultipleStrategies  + (1|ParticipantID) + 
#                                 (InPhaseTransition|ParticipantID),data=TSPAggData)
# #summary(TSP_time_mes_PT_mstrat)

# #logistic ME (intercept and slope)  and demographics
# time_mes_PT_dem = lmer(TimeOnTask ~ InPhaseTransition + Sex_Female + Age + PostGrad + Commerce + Early_Afternoon
#                        + Late_Afternoon + (1|ParticipantID) + (InPhaseTransition|ParticipantID),
#                        data=TSPLearning)
# #summary(time_mes_PT_dem)
# 
# #logistic ME (intercept and slope)  and most  cont. 
# time_mes_PT_all = lmer(TimeOnTask ~ InPhaseTransition + MultipleStrategies + Convex_Hull +
#                          Sex_Female + Age + PostGrad + Commerce + Early_Afternoon + Late_Afternoon + TrialNumber + 
#                          (1|ParticipantID) + (InPhaseTransition|ParticipantID),
#                        data=TSPLearning)
# #summary(time_mes_PT_all)
# 
# 
# # stargazer(time_mes_PT, time_mes_PT_learn, time_mes_PT_strat, time_mes_PT_dem,
# #           time_mes_PT_all, title="TSP Mixed Effects (Intercept + Slope)", align=TRUE)
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
#                      data=TSPAggData)
# #summary(time_mes_type)
# 
# #logistic ME (intercept and slope)  and learning
# time_mes_type_learn = lmer(TimeOnTask ~ Difficulty + TrialNumber + (1|ParticipantID) + (Difficulty|ParticipantID),
#                            data=TSPAggData)
# #summary(time_mes_type_learn)
# 
# #logistic ME (intercept and slope)  and strat 
# time_mes_type_strat = lmer(TimeOnTask ~ Difficulty + Convex_Hull + Nearest_Neighbour
#                            + MultipleStrategies + (Difficulty|ParticipantID), data=TSPAggData)
# #summary(time_mes_type_strat)
# 
# #logistic ME (intercept and slope)  and demographics
# time_mes_type_dem = lmer(TimeOnTask ~ Difficulty + Sex_Female + Age + PostGrad + Commerce + Early_Afternoon
#                          + Late_Afternoon + (1|ParticipantID) + (Difficulty|ParticipantID),
#                          data=TSPAggData)
# #summary(time_mes_type_dem)
# 
# #logistic ME (intercept and slope)  and most  cont.
# time_mes_type_all = lmer(TimeOnTask ~ Difficulty + MultipleStrategies + TrialNumber + Convex_Hull + 
#                            Sex_Female + Age + PostGrad + Commerce + Early_Afternoon + Late_Afternoon + 
#                            (1|ParticipantID) + (Difficulty|ParticipantID),
#                          data=TSPAggData)
# #summary(time_mes_type_all)
# 
# 
# # stargazer(time_mes_type, time_mes_type_learn, time_mes_type_strat, time_mes_type_dem,
# #            time_mes_type_all, title="TSP Mixed Effects (Intercept + Slope)", align=TRUE)
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
# 
# 
# 
# 
# 
# #logistic with FE and no controls = Time insignificant
# acc_YES = glm(correct ~ TimeOnTask, family=binomial(link="logit"), data=TSPYes)
# #summary(acc_YES)
# 
# #logistic with FE and no controls = Time significantly negative at 5% - opposite of what we'd expect
# acc_No = glm(correct ~ TimeOnTask, family=binomial(link="logit"), data=TSPNo)
# #summary(acc_No)
# 
# 
# #logistic with FE and no controls = Time significantly negative at 5%
# acc_Under = glm(correct ~ TimeOnTask, family=binomial(link="logit"), data=TSPUnder)
# #summary(acc_Under)
# 
# #logistic with FE and no controls = Time insignificant
# acc_Over = glm(correct ~ TimeOnTask, family=binomial(link="logit"), data=TSPOver)
# #summary(acc_Over)
# 
# #logistic with FE and no controls = Time insignificant
# acc_PT = glm(correct ~ TimeOnTask, family=binomial(link="logit"), data=TSPPT)
# #summary(acc_PT)



# TSPAggDataPT <- subset(TSPAggData, type<=4)
# 
# 
# #TSP summary stats
# #two-sampled t-test for differences in mean accuracy b/w in/out of PT, significant at 1%
# tbl = table(TSPAggDataPT$InPhaseTransition, TSPAggDataPT$correct)
# chisq.test(tbl)
# #two-sampled t-test for differences in mean time spent b/w in/out of PT, significant at 1%
# t.test(TimeOnTask ~ InPhaseTransition, data = TSPAggData)
# 
# #two-sampled t-test for differences in mean accuracy b/w under-constrained & PT, significant at 1%
# TSP_under_in <- filter(TSPAggData, TSPAggData$Region=="Phase Transition" | TSPAggData$Region=="Under-Constrained")
# tbl = table(TSP_under_in$Region, TSP_under_in$correct) 
# chisq.test(tbl)
# #two-sampled t-test for differences in mean time spent b/w under-constrained & PT, significant at 1%
# t.test(TimeOnTask ~ Region, data = TSP_under_in)
# 
# #two-sampled t-test for differences in mean accuracy b/w over-constrained & PT, significant at 1%
# TSPAggData <- TSPAggData[-c(4)]
# TSP_over_in <- filter(TSPAggData, TSPAggData$Region=="Phase Transition" | TSPAggData$Region=="Over-Constrained")
# tbl = table(TSP_over_in$Region, TSP_over_in$correct)
# chisq.test(tbl)
# #two-sampled t-test for differences in mean time spent b/w over-constrained & PT, insignificant
# t.test(TimeOnTask ~ Region, data = TSP_over_in)


#aggregate(TSPAggData[,c(1,3)], by = list(TSPAggData$type),mean, na.rm = TRUE)


