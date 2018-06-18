#set working directory and load packages
setwd("~/Desktop/Final Experiment/KS")
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

#create a list of the input files, one for each type
KSInstanceFiles = list.files(path = "/Users/KRDoroc/Desktop/Final Experiment/KS",
                             pattern="*InstancesInfo.txt$")
KSInfoFiles = list.files(path = "/Users/KRDoroc/Desktop/Final Experiment/KS",pattern="*TrialInfo.txt$")
KSTimeStampFiles = list.files(path = "/Users/KRDoroc/Desktop/Final Experiment/KS",
                              pattern="*TimeStamps.txt$")
#create a list of the participant ID
pIDs=sapply(strsplit(KSInstanceFiles,"_"),function(x) x[1])

#load all the trialinfo files and add participant ID as a column
for(ksInfoFile in KSInfoFiles){
  KSTrialInfo = lapply(KSInfoFiles, read.csv, stringsAsFactors=F,skip=1,header = TRUE, sep =";"
                       ,quote = "") %>% 
    bind_rows
  KSTrialInfo <- arrange(KSTrialInfo,block,trial)
  KSTrialInfo$ParticipantID <-(pIDs)
}

#load all the instanceinfo files
for(ksInstanceFile in KSInstanceFiles){
  KSInstanceInfo = lapply(KSInstanceFiles, read.csv, stringsAsFactors=F,skip=1,header = TRUE, sep =";"
                          ,quote = "") %>% 
    bind_rows
}

##load all the time stamps files, find the time spent on each trial (deleting some irrelevant columns),
#and add participant ID as a column
for(ksTimeStampFile in KSTimeStampFiles){
  KSTimeStamps = lapply(KSTimeStampFiles, read.csv, stringsAsFactors=F,skip=2,header = TRUE, sep =";"
                        ,quote = "") %>% 
    bind_rows
  KSTime <- diff(KSTimeStamps$elapsedTime, lag=1, differences=1)/1000
  KSTimeStamps <- KSTimeStamps[-nrow(KSTimeStamps),]
  KSTimeStamps$TimeOnTask <- KSTime
  KSTimeStamps <- subset(KSTimeStamps, eventType=="Trial")
  KSTimeStamps <- KSTimeStamps[ -c(3:4) ]
  KSTimeStamps <- arrange(KSTimeStamps,block,trial)
  KSTimeStamps$ParticipantID <-(pIDs)
}

#only keeps the instance number and type from the instance info
KSInstanceInfo = {dplyr::select(KSInstanceInfo, instanceNumber, type)}

#merge with trial and instance info
KSTrials <- merge(KSTrialInfo,KSInstanceInfo)
KSTrials <- unique(KSTrials)
KSTrials <- arrange(KSTrials,ParticipantID,block,trial)
rownames(KSTrials) <- NULL

#merge time stamps with previous inputs to create a single file with aggregate data
KSAggData <- merge(KSTrials,KSTimeStamps)
KSAggData <- unique(KSAggData)

# #merge decisions & propagations file with previous inputs to create a single file with aggregate data
# KSComplexity <- read.table(file = "ComplexityKS.csv", header = TRUE, sep = ",")
# KSAggData <- merge(KSAggData,KSComplexity)


#delete unnecessary columns
KSAggData <- KSAggData[ -c(1,5,7,9:10) ]

#create separate data sets based on whether instances have YES or NO solutions
KSYes <- subset(KSAggData, randomYes.1.Left.No.Right.Yes.==1)
KSYes <- KSYes[ -c(1:2,5:6) ]
KSNo <- subset(KSAggData, randomYes.1.Left.No.Right.Yes.==0)
KSNo <- KSNo[ -c(1:2,5:6) ]

#delete unnecessary columns
KSAggData <- KSAggData[ -c(5) ]


# #check for learning effect
# d <- c(ifelse(KSAggData$block==1, KSAggData$trial, ifelse(KSAggData$block==2, KSAggData$trial+12,NA)))
# KSAggData$TrialNumber <- cbind(d)
# KScorData <- KSAggData[c(4,7)]
# sjt.corr(KScorData, p.numeric=TRUE, triangle = "lower")
# #insignificant correlation of -2% - no evidence of a learning effect with the KS
# 
# #tried to visualise the correlation and learning effect, if anything they get worse over time
# KS_LearnPlot <- ggplot(KSAggData, aes(TrialNumber,correct)) + geom_point() + 
#   geom_smooth(fill="blue", colour="darkblue", size=1)


# #check if more effort = high performance
# ddply(KSAggData, "type", summarise, Corr=cor(TimeOnTask, correct))
# 
# #check if more effort = high performance
# ddply(KSAggData, "type", summarise, Corr=cor.test(TimeOnTask, correct))
# 
# Effort_Correls <- ddply(KSAggData, .(type), summarise,
#                         Correlation=(cor.test(TimeOnTask, correct)), Description=names(Correlation) )
# Effort_Correls <- subset(Effort_Correls, Description=="estimate" | Description=="p.value")
# #-15% (10% lvl) and -33%(1% level) for types 5 & 6 (under-constrained) - the rest insignificant

# #check average accuracy and time on instances
# KS_Inst_Summ <- KSAggData %>%  # the names of the new data frame and the data frame to be summarised
#   group_by(instanceNumber) %>%
#   summarise(Accuracy = mean(correct), Std_Dev = sd(correct), Time = mean(TimeOnTask))
# 
# library(openxlsx)
# write.xlsx(KS_Inst_Summ, 'KS_Inst.xlsx')
# 
# #check average accuracy and time by participant
# KS_Part_Summ <- KSAggData %>%  # the names of the new data frame and the data frame to be summarised
#   group_by(ParticipantID) %>%
#   summarise(Accuracy = mean(correct), Std_Dev = sd(correct), Time = mean(TimeOnTask))
# 
# write.xlsx(KS_Part_Summ, 'KS_Part.xlsx')
# 

#create a column to see if an instance is in the PT (Yes) or not (NO)
KSAggData$InPhaseTransition <- ("No")
KSAggData$InPhaseTransition[KSAggData$type==4 | KSAggData$type==3] <- ("Yes")

#create a column to see if an instance is in the underconstrained region (Under), PT (PT), or overconstrained
#region (Over)
KSAggData$Region[KSAggData$InPhaseTransition=="Yes"] <- ("Phase Transition - Hard")
KSAggData$Region[KSAggData$type<=2] <- ("Phase Transition - Easy")
KSAggData$Region[KSAggData$type==6] <- ("Under-Constrained")
KSAggData$Region[KSAggData$type==5] <- ("Over-Constrained")

KSUnder <- subset(KSAggData, Region=="Under-Constrained")
KSOver <- subset(KSAggData, Region=="Over-Constrained")
KSPT <- subset(KSAggData, Region=="Phase Transition") 





#KS plots
#create a summary table for accuracy of in vs out PT
KS_Acc_Sum_PT <- KSAggData %>% # the names of the new data frame and the data frame to be summarised
  group_by(InPhaseTransition) %>%   # the grouping variable
  summarise(mean_PT = mean(correct),  # calculates the mean of each group
            sd_PT = sd(correct), # calculates the standard deviation of each group
            n_PT = n(),  # calculates the sample size per group
            SE_PT = sd(correct)/sqrt(n())) # calculates the standard error of each group

#create a summary table for time spent of in vs out PT
KS_Time_Sum_PT <- KSAggData %>% # the names of the new data frame and the data frame to be summarised
  group_by(InPhaseTransition) %>%   # the grouping variable
  summarise(mean_PT = mean(TimeOnTask),  # calculates the mean of each group
            sd_PT = sd(TimeOnTask), # calculates the standard deviation of each group
            n_PT = n(),  # calculates the sample size per group
            SE_PT = sd(TimeOnTask)/sqrt(n())) # calculates the standard error of each group

#create a summary table for accuracy of region
KS_Acc_Sum_Region <- KSAggData %>% # the names of the new data frame and the data frame to be summarised
  group_by(Region) %>%   # the grouping variable
  summarise(mean_region = mean(correct),  # calculates the mean of each group
            sd_region = sd(correct), # calculates the standard deviation of each group
            n_region = n(),  # calculates the sample size per group
            SE_region = sd(correct)/sqrt(n())) # calculates the standard error of each group

#create a summary table for time spent of region
KS_Time_Sum_Region <- KSAggData %>% # the names of the new data frame and the data frame to be summarised
  group_by(Region) %>%   # the grouping variable
  summarise(mean_region = mean(TimeOnTask),  # calculates the mean of each group
            sd_region = sd(TimeOnTask), # calculates the standard deviation of each group
            n_region = n(),  # calculates the sample size per group
            SE_region = sd(TimeOnTask)/sqrt(n())) # calculates the standard error of each group
KS_Time_Sum_Region$Region[KS_Time_Sum_Region$Region=="Under-Constrained"] <- ("Under")
KS_Time_Sum_Region$Region[KS_Time_Sum_Region$Region=="Phase Transition"] <- ("PT")
KS_Time_Sum_Region$Region[KS_Time_Sum_Region$Region=="Over-Constrained"] <- ("Over")


# 
# #accuracy in vs out PT graph
# Acc_PT_Sum <- rbind(
#   mutate(KS_Acc_Sum_PT, category = "KS"),
#   mutate(TSP_Acc_Sum_PT, category = "TSP"),
#   mutate(SAT_Acc_Sum_PT, category = "3-SAT")
# )
# 
# Acc_PT_Plot <- ggplot(Acc_PT_Sum, aes(InPhaseTransition, mean_PT), fill=category) + 
#   geom_col() +  
#   scale_y_continuous(labels = scales::percent) +
#   coord_cartesian(ylim=c(0.5, 1)) +
#   geom_bar(stat="identity", fill=c("#999999","#999999","#66CC00","#66CC00","#FF9933","#FF9933")) +
#   geom_text(aes(label= scales::percent(round(mean_PT,digits=2)), vjust=-3)) +
#   geom_errorbar(aes(ymin = mean_PT - SE_PT, ymax = mean_PT + SE_PT), width=0.2) + facet_wrap(~category)
# # Acc_PT_Plot + labs(y="Accuracy   ± SEM", x = "In Phase Transition") + theme_classic()
# 
# #regional accuracy graph
# Acc_Region_Sum <- rbind(
#   mutate(KS_Acc_Sum_Region, category = "KS"),
#   mutate(TSP_Acc_Sum_Region, category = "TSP"),
#   mutate(SAT_Acc_Sum_Region, category = "3-SAT")
# )
# 
# Acc_Region_Sum$Region[Acc_Region_Sum$Region=="Under-Constrained"] <- ("Under")
# Acc_Region_Sum$Region[Acc_Region_Sum$Region=="Phase Transition"] <- ("PT")
# Acc_Region_Sum$Region[Acc_Region_Sum$Region=="Over-Constrained"] <- ("Over")
# 
# Acc_Region_Plot <- ggplot(Acc_Region_Sum, aes(Region, mean_region), fill=category) + 
#   geom_col() +  
#   scale_y_continuous(labels = scales::percent) +
#   coord_cartesian(ylim=c(0.5, 1)) +
#   geom_bar(stat="identity", fill=c("#999999","#999999","#999999","#66CC00","#66CC00","#66CC00","#FF9933","#FF9933","#FF9933")) +
#   geom_text(aes(label= scales::percent(round(mean_region,digits=2)), vjust=-3)) +
#   geom_errorbar(aes(ymin = mean_region - SE_region, ymax = mean_region + SE_region), width=0.2) + facet_wrap(~category)
# # Acc_Region_Plot + labs(y="Accuracy   ± SEM", x = "Region") + theme_classic()

# #time in vs out PT
# grid.arrange(SAT_Time_PT_Plot,KS_Time_PT_Plot,TSP_Time_PT_Plot,ncol=3,nrow=1)
# 
# 
# #time per region
# grid.arrange(SAT_Time_Region_Plot,KS_Time_Region_Plot,TSP_Time_Region_Plot,ncol=3,nrow=1)



# KSAggData$TimeOnTask.t <- KSAggData$TimeOnTask
# qqp(KSAggData$TimeOnTask.t, "norm")


#KS analyses
#importing demographics and merging with existing aggregated data
KSDemographics <- read.table(file = "Demographics.csv", header = TRUE, sep = ",")
KSDemographics$Sex_Female[KSDemographics$Gender=="Female"] <- (1)
KSDemographics$Sex_Female[KSDemographics$Gender=="Male"] <- (0)
KSDemographics$PostGrad[KSDemographics$Years_of_study>3] <- (1)
KSDemographics$PostGrad[KSDemographics$Years_of_study<4] <- (0)
KSDemographics$Commerce[KSDemographics$Degree=="Commerce"] <- (1)
KSDemographics$Commerce[KSDemographics$Degree!="Commerce"] <- (0)
KSDemographics$Early_Afternoon[KSDemographics$Time_of_day=="Early_Afternoon"] <- (1)
KSDemographics$Early_Afternoon[KSDemographics$Time_of_day!="Early_Afternoon"] <- (0)
KSDemographics$Late_Afternoon[KSDemographics$Time_of_day=="Late_Afternoon"] <- (1)
KSDemographics$Late_Afternoon[KSDemographics$Time_of_day!="Late_Afternoon"] <- (0)
names(KSDemographics)[names(KSDemographics) == 'Feasible.with.all.Items'] <- 'Feasible'
names(KSDemographics)[names(KSDemographics) == 'Large.Items..money.'] <- 'Large_Items'
names(KSDemographics)[names(KSDemographics) == 'Prioritising.one.property'] <- 'Prioritising_Properties'
KSDemographics$MultipleStrategies <- KSDemographics$Total
KSDemographics$MultipleStrategies[KSDemographics$MultipleStrategies<=1] <- (0)
KSDemographics$MultipleStrategies[KSDemographics$MultipleStrategies>1] <- (1)
KSDemographics <- KSDemographics[ -c(2,4,6:7,12:13) ]

KSAggData <- merge(KSAggData,KSDemographics)

KSAggData$Difficulty[KSAggData$Region=="Under-Constrained"] <- (1)
KSAggData$Difficulty[KSAggData$Region=="Over-Constrained"] <- (2)
KSAggData$Difficulty[KSAggData$Region=="Phase Transition"] <- (3)





#accuracy FE regressions (PT)
#logistic with FE and no controls = PT significantly negative at 1%
acc_fe_PT = glm(correct ~ InPhaseTransition, family=binomial(link="logit"), data=KSAggData)
# summary(acc_fe_PT)
# BIC(acc_fe_PT)
# 
# #logistic FE and learning = no learning effect
# acc_fe_PT_learn = glm(correct ~ InPhaseTransition + TrialNumber, family=binomial(link="logit"), 
#                       data=KSAggData)
# #summary(acc_fe_PT_learn)
# 
# #logistic FE and strat = PT significantly negative at 1%, greedy & Prioritising sig +ve at 1%
# acc_fe_PT_strat = glm(correct ~ InPhaseTransition + Greedy + Feasible + Large_Items + 
#                         Prioritising_Properties + MultipleStrategies, family=binomial(link="logit"), 
#                       data=KSAggData)
# #summary(acc_fe_PT_strat)
# 
# #logistic FE and demographics = PT significantly negative at 1%, postgrad & female +ve at 3%
# acc_fe_PT_dem = glm(correct ~ InPhaseTransition + Sex_Female + Age + PostGrad + Commerce + Early_Afternoon
#                     + Late_Afternoon, family=binomial(link="logit"), data=KSAggData)
# #summary(acc_fe_PT_dem)
# 
# #logistic FE and significant cont. = PT significantly negative at 1%, postgrad & female +ve at 3%
# acc_fe_PT_sig = glm(correct ~ InPhaseTransition + Greedy + Prioritising_Properties + MultipleStrategies + 
#                       Sex_Female + PostGrad,
#                     family=binomial(link="logit"), data=KSAggData)
# #summary(acc_fe_PT_sig)
# 
# #logistic FE and most  cont. = PT significantly negative at 1%, postgrad & female +ve at 3%
# acc_fe_PT_all = glm(correct ~ InPhaseTransition + Greedy + Prioritising_Properties + MultipleStrategies + 
#                       Sex_Female + Age + PostGrad + Commerce + Early_Afternoon + Late_Afternoon,
#                     family=binomial(link="logit"), data=KSAggData)
# #summary(acc_fe_PT_all)
# 
# 
# # stargazer(acc_fe_PT, acc_fe_PT_learn, acc_fe_PT_strat, acc_fe_PT_dem, acc_fe_PT_sig,
# #            acc_fe_PT_all, title="KS Fixed Effects", align=TRUE)
# 
# 
# 
# 
# 
# 
# #accuracy ME regressions (intercept)
#logistic with ME (intercept) = PT significantly negative at 1%
KS_acc_me_PT = glmer(correct ~ InPhaseTransition + (1|ParticipantID),
                     family=binomial(link="logit"), data=KSAggData)
# summary(KS_acc_me_PT)

#logistic ME (intercept)  and learning = no learning effect
KS_acc_me_PT_learn = glmer(correct ~ InPhaseTransition + TrialNumber + (1|ParticipantID),
                           family=binomial(link="logit"), data=KSAggData)
#summary(KS_acc_me_PT_learn)

#logistic ME (intercept)  and strat = PT significantly negative at 1%, greedy & Prioritising sig +ve at 1%
KS_acc_me_PT_strat = glmer(correct ~ InPhaseTransition + Greedy + Feasible + Large_Items + (1|ParticipantID) +
                             Prioritising_Properties,
                           family=binomial(link="logit"), data=KSAggData)
#summary(KS_acc_me_PT_strat)

#logistic ME (intercept)  and multiple strat = PT significantly negative at 1%, greedy & Prioritising sig +ve at 1%
KS_acc_me_PT_mstrat = glmer(correct ~ InPhaseTransition + (1|ParticipantID) +
                              MultipleStrategies,
                            family=binomial(link="logit"), data=KSAggData)
#summary(KS_acc_me_PT_mstrat)
# 
# #logistic ME (intercept)  and demographics = PT significantly negative at 1%, postgrad & female +ve at 3%
# acc_me_PT_dem = glmer(correct ~ InPhaseTransition + Sex_Female + Age + PostGrad + Commerce + Early_Afternoon
#                       + Late_Afternoon + (1|ParticipantID), family=binomial(link="logit"), data=KSAggData)
# #summary(acc_me_PT_dem)
# 
# #logistic ME (intercept)  and significant cont. = PT significantly negative at 1%, postgrad & female +ve at 3%
# acc_me_PT_sig = glmer(correct ~ InPhaseTransition + Greedy + Prioritising_Properties + MultipleStrategies + 
#                         Sex_Female + PostGrad + (1|ParticipantID),
#                       family=binomial(link="logit"), data=KSAggData)
# #summary(acc_me_PT_sig)
# 
# #logistic ME (intercept)  and most  cont. = PT significantly negative at 1%, postgrad & female +ve at 3%
# acc_me_PT_all = glmer(correct ~ InPhaseTransition + Greedy + Prioritising_Properties + MultipleStrategies + 
#                         Sex_Female + Age + PostGrad + Commerce + Early_Afternoon + Late_Afternoon + (1|ParticipantID),
#                       family=binomial(link="logit"), data=KSAggData)
# #summary(acc_me_PT_all)
# 
# 
# # stargazer(acc_me_PT, acc_me_PT_learn, acc_me_PT_strat, acc_me_PT_dem, acc_me_PT_sig,
# #            acc_me_PT_all, title="KS Mixed Effects (Intercept)", align=TRUE)
# 
# 
# 
# 
# 
#accuracy ME regressions (intercept + slope)
# #logistic with ME (intercept and slope) = PT significantly negative at 1%
# KS_acc_mes_PT = glmer(correct ~ InPhaseTransition + (1|ParticipantID) + (InPhaseTransition|ParticipantID),
#                    family=binomial(link="logit"), data=KSAggData)
# #summary(KS_acc_mes_PT)
# 
# #logistic ME (intercept and slope)  and learning = no learning effect
# KS_acc_mes_PT_learn = glmer(correct ~ InPhaseTransition + TrialNumber + (1|ParticipantID) + (InPhaseTransition|ParticipantID),
#                          family=binomial(link="logit"), data=KSAggData)
# #summary(KS_acc_mes_PT_learn)
# 
# #logistic ME (intercept and slope)  and strat = PT significantly negative at 1%, greedy & Prioritising sig +ve at 1%
# KS_acc_mes_PT_strat = glmer(correct ~ InPhaseTransition + Greedy + Feasible + Large_Items + (1|ParticipantID) +
#                            Prioritising_Properties + (InPhaseTransition|ParticipantID),
#                          family=binomial(link="logit"), data=KSAggData)
# #summary(KS_acc_mes_PT_strat)
# 
# #logistic ME (intercept and slope)  and multiple strat = PT significantly negative at 1%, greedy & Prioritising sig +ve at 1%
# KS_acc_mes_PT_mstrat = glmer(correct ~ InPhaseTransition + (1|ParticipantID) +
#                             MultipleStrategies + (InPhaseTransition|ParticipantID),
#                             family=binomial(link="logit"), data=KSAggData)
# #summary(KS_acc_mes_PT_mstrat)

# #logistic ME (intercept and slope)  and demographics = PT significantly negative at 1%, postgrad & female +ve at 3%
# acc_mes_PT_dem = glmer(correct ~ InPhaseTransition + Sex_Female + Age + PostGrad + Commerce + Early_Afternoon
#                        + Late_Afternoon + (1|ParticipantID) + (InPhaseTransition|ParticipantID),
#                        family=binomial(link="logit"), data=KSAggData)
# #summary(acc_mes_PT_dem)
# 
# #logistic ME (intercept and slope)  and significant cont. = PT significantly negative at 1%, postgrad & female +ve at 3%
# acc_mes_PT_sig = glmer(correct ~ InPhaseTransition + Greedy + Prioritising_Properties + MultipleStrategies + 
#                          Sex_Female + PostGrad + (1|ParticipantID) + (InPhaseTransition|ParticipantID),
#                        family=binomial(link="logit"), data=KSAggData)
# #summary(acc_mes_PT_sig)
# 
# #logistic ME (intercept and slope)  and most  cont. = PT significantly negative at 1%, postgrad & female +ve at 3%
# acc_mes_PT_all = glmer(correct ~ InPhaseTransition + Greedy + Prioritising_Properties + MultipleStrategies + 
#                          Sex_Female + Age + PostGrad + Commerce + Early_Afternoon + Late_Afternoon + 
#                          (1|ParticipantID) + (InPhaseTransition|ParticipantID),
#                        family=binomial(link="logit"), data=KSAggData)
# #summary(acc_mes_PT_all)
# 
# 
# # stargazer(acc_mes_PT, acc_mes_PT_learn, acc_mes_PT_strat, acc_mes_PT_dem, acc_mes_PT_sig,
# #            acc_mes_PT_all, title="KS Mixed Effects (Intercept + Slope)", align=TRUE)
# 
# 
# 
# 
# 
# 
# 
# #accuracy ME regressions (intercept + slope), with type
# #logistic with ME (intercept and slope) = PT significantly negative at 1%
# acc_mes_type = glmer(correct ~ Difficulty + (1|ParticipantID) + (Difficulty|ParticipantID), 
#                      family=binomial(link="logit"), data=KSAggData)
# #summary(acc_mes_type)
# 
# #logistic ME (intercept and slope)  and learning = no learning effect
# acc_mes_type_learn = glmer(correct ~ Difficulty + TrialNumber + (1|ParticipantID) + (Difficulty|ParticipantID),
#                            family=binomial(link="logit"), data=KSAggData)
# #summary(acc_mes_type_learn)
# 
# #logistic ME (intercept and slope)  and strat = PT significantly negative at 1%, greedy & Prioritising sig +ve at 1%
# acc_mes_type_strat = glmer(correct ~ Difficulty + Greedy + Feasible + Large_Items + (1|ParticipantID) + 
#                              Prioritising_Properties + MultipleStrategies + (Difficulty|ParticipantID),
#                            family=binomial(link="logit"), data=KSAggData)
# #summary(acc_mes_type_strat)
# 
# #logistic ME (intercept and slope)  and demographics = PT significantly negative at 1%, postgrad & female +ve at 3%
# acc_mes_type_dem = glmer(correct ~ Difficulty + Sex_Female + Age + PostGrad + Commerce + Early_Afternoon
#                          + Late_Afternoon + (1|ParticipantID) + (Difficulty|ParticipantID),
#                          family=binomial(link="logit"), data=KSAggData)
# #summary(acc_mes_type_dem)
# 
# #logistic ME (intercept and slope)  and significant cont. = PT significantly negative at 1%, postgrad & female +ve at 3%
# acc_mes_type_sig = glmer(correct ~ Difficulty + Greedy + Prioritising_Properties + MultipleStrategies + 
#                            Sex_Female + PostGrad + (1|ParticipantID) + (Difficulty|ParticipantID),
#                          family=binomial(link="logit"), data=KSAggData)
# #summary(acc_mes_type_sig)
# 
# #logistic ME (intercept and slope)  and most  cont. = PT significantly negative at 1%, postgrad & female +ve at 3%
# acc_mes_type_all = glmer(correct ~ Difficulty + Greedy + Prioritising_Properties + MultipleStrategies + 
#                            Sex_Female + Age + PostGrad + Commerce + Early_Afternoon + Late_Afternoon + 
#                            (1|ParticipantID) + (Difficulty|ParticipantID),
#                          family=binomial(link="logit"), data=KSAggData)
# #summary(acc_mes_type_all)
# 
# 
# # stargazer(acc_mes_type, acc_mes_type_learn, acc_mes_type_strat, acc_mes_type_dem, acc_mes_type_sig,
# #            acc_mes_type_all, title="KS Mixed Effects (Intercept + Slope)", align=TRUE)
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
# #accuracy ME regressions (intercept), with propagations
KS_acc_me_prop = glmer(correct ~ propagations + (1|ParticipantID),
                       family=binomial(link="logit"), data=KSAggData)
#summary(KS_acc_me_prop)

#logistic ME (intercept)  and learning = no learning effect
KS_acc_me_prop_learn = glmer(correct ~ propagations + TrialNumber + (1|ParticipantID),
                             family=binomial(link="logit"), data=KSAggData)
#summary(KS_acc_me_prop_learn)

#logistic ME (intercept)  and strat = PT significantly negative at 1%, greedy & Prioritising sig +ve at 1%
KS_acc_me_prop_strat = glmer(correct ~ propagations + Greedy + Feasible + Large_Items + (1|ParticipantID) +
                               Prioritising_Properties,
                             family=binomial(link="logit"), data=KSAggData)
#summary(KS_acc_me_prop_strat)

#logistic ME (intercept)  and multiple strat = PT significantly negative at 1%, greedy & Prioritising sig +ve at 1%
KS_acc_me_prop_mstrat = glmer(correct ~ propagations + (1|ParticipantID) +
                                MultipleStrategies,
                              family=binomial(link="logit"), data=KSAggData)
#summary(KS_acc_me_prop_mstrat)
# 
# #logistic ME (interceprop and slope)  and demographics = prop significantly negative at 1%, postgrad & female +ve at 3%
# acc_mes_prop_dem = glmer(correct ~ propagations + Sex_Female + Age + PostGrad + Commerce + Early_Afternoon
#                          + Late_Afternoon + (1|ParticipantID) + (propagations|ParticipantID),
#                          family=binomial(link="logit"), data=KSAggData)
# #summary(acc_mes_prop_dem)
# 
# #logistic ME (interceprop and slope)  and significant cont. = prop significantly negative at 1%, postgrad & female +ve at 3%
# acc_mes_prop_sig = glmer(correct ~ propagations + Greedy + Prioritising_Properties + MultipleStrategies + 
#                            Sex_Female + PostGrad + (1|ParticipantID) + (propagations|ParticipantID),
#                          family=binomial(link="logit"), data=KSAggData)
# #summary(acc_mes_prop_sig)
# 
# #logistic ME (interceprop and slope)  and most  cont. = prop significantly negative at 1%, postgrad & female +ve at 3%
# acc_mes_prop_all = glmer(correct ~ propagations + Greedy + Prioritising_Properties + MultipleStrategies + 
#                            Sex_Female + Age + PostGrad + Commerce + Early_Afternoon + Late_Afternoon + 
#                            (1|ParticipantID) + (propagations|ParticipantID),
#                          family=binomial(link="logit"), data=KSAggData)
# #summary(acc_mes_prop_all)
# 
# 
# # stargazer(acc_mes_prop, acc_mes_prop_learn, acc_mes_prop_strat, acc_mes_prop_dem, acc_mes_prop_sig,
# #           acc_mes_prop_all, title="KS Mixed Effects (Intercept + Slope)", align=TRUE)
# 
# 
# 
# 
# 
# 
# 
# 
# #accuracy ME regressions (intercept + slope), with decisions
# #logistic with ME (intercedec and slope) = dec significantly negative at 1%
# acc_mes_dec = glmer(correct ~ decisions + (1|ParticipantID) + (decisions|ParticipantID), 
#                     family=binomial(link="logit"), data=KSAggData)
# #summary(acc_mes_dec)
# 
# #logistic ME (intercedec and slope)  and learning = no learning effect
# acc_mes_dec_learn = glmer(correct ~ decisions + TrialNumber + (1|ParticipantID) + (decisions|ParticipantID),
#                           family=binomial(link="logit"), data=KSAggData)
# #summary(acc_mes_dec_learn)
# 
# #logistic ME (intercedec and slope)  and strat = dec significantly negative at 1%, greedy & Prioritising sig +ve at 1%
# acc_mes_dec_strat = glmer(correct ~ decisions + Greedy + Feasible + Large_Items + (1|ParticipantID) + 
#                             Prioritising_Properties + MultipleStrategies + (decisions|ParticipantID),
#                           family=binomial(link="logit"), data=KSAggData)
# #summary(acc_mes_dec_strat)
# 
# #logistic ME (intercedec and slope)  and demographics = dec significantly negative at 1%, postgrad & female +ve at 3%
# acc_mes_dec_dem = glmer(correct ~ decisions + Sex_Female + Age + PostGrad + Commerce + Early_Afternoon
#                         + Late_Afternoon + (1|ParticipantID) + (decisions|ParticipantID),
#                         family=binomial(link="logit"), data=KSAggData)
# #summary(acc_mes_dec_dem)
# 
# #logistic ME (intercedec and slope)  and significant cont. = dec significantly negative at 1%, postgrad & female +ve at 3%
# acc_mes_dec_sig = glmer(correct ~ decisions + Greedy+ Feasible + Large_Items + Prioritising_Properties + MultipleStrategies + 
#                           Sex_Female + PostGrad + (1|ParticipantID) + (decisions|ParticipantID),
#                         family=binomial(link="logit"), data=KSAggData)
# #summary(acc_mes_dec_sig)
# 
# #logistic ME (intercedec and slope)  and most  cont. = dec significantly negative at 1%, postgrad & female +ve at 3%
# acc_mes_dec_all = glmer(correct ~ decisions + Greedy+ Feasible + Large_Items + Prioritising_Properties + MultipleStrategies + 
#                           Sex_Female + Age + PostGrad + Commerce + Early_Afternoon + Late_Afternoon + 
#                           (1|ParticipantID) + (decisions|ParticipantID),
#                         family=binomial(link="logit"), data=KSAggData)
# #summary(acc_mes_dec_all)
# 
# 
# # stargazer(acc_mes_dec, acc_mes_dec_learn, acc_mes_dec_strat, acc_mes_dec_dem, acc_mes_dec_sig,
# #           acc_mes_dec_all, title="KS Mixed Effects (Intercept + Slope)", align=TRUE)
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
# #time FE regressions (PT)
# #logistic with FE and no controls = PT significantly negative at 1%
# time_fe_PT = lm(TimeOnTask ~ InPhaseTransition, data=KSAggData)
# #summary(time_fe_PT)
# 
# #logistic FE and learning = no learning effect
# time_fe_PT_learn = lm(TimeOnTask ~ InPhaseTransition + TrialNumber, 
#                       data=KSAggData)
# #summary(time_fe_PT_learn)
# 
# #logistic FE and strat = PT significantly negative at 1%, greedy & Prioritising sig +ve at 1%
# time_fe_PT_strat = lm(TimeOnTask ~ InPhaseTransition + Greedy + Feasible + Large_Items + 
#                         Prioritising_Properties + MultipleStrategies, 
#                       data=KSAggData)
# #summary(time_fe_PT_strat)
# 
# #logistic FE and demographics = PT significantly negative at 1%, postgrad & female +ve at 3%
# time_fe_PT_dem = lm(TimeOnTask ~ InPhaseTransition + Sex_Female + Age + PostGrad + Commerce + Early_Afternoon
#                     + Late_Afternoon, data=KSAggData)
# #summary(time_fe_PT_dem)
# 
# #logistic FE and significant cont. = PT significantly negative at 1%, postgrad & female +ve at 3%
# time_fe_PT_sig = lm(TimeOnTask ~ InPhaseTransition + Greedy + MultipleStrategies + TrialNumber + 
#                       Commerce + Early_Afternoon + Late_Afternoon +  + PostGrad,
#                     data=KSAggData)
# #summary(time_fe_PT_sig)
# 
# #logistic FE and most  cont. = PT significantly negative at 1%, postgrad & female +ve at 3%
# time_fe_PT_all = lm(TimeOnTask ~ InPhaseTransition + Greedy + MultipleStrategies + 
#                       Sex_Female + Age + PostGrad + Commerce + Early_Afternoon + Late_Afternoon + TrialNumber,
#                     data=KSAggData)
# #summary(time_fe_PT_all)
# 
# 
# # stargazer(time_fe_PT, time_fe_PT_learn, time_fe_PT_strat, time_fe_PT_dem, time_fe_PT_sig,
# #            time_fe_PT_all, title="KS Fixed Effects", align=TRUE)
# 
# 
# 
# 
# 
# 
# #time ME regressions (intercept)
#logistic with ME (intercept) = PT significantly negative at 1%
KS_time_me_PT = lmer(TimeOnTask ~ InPhaseTransition + (1|ParticipantID),
                     data=KSAggData)
#summary(KS_time_me_PT)

#logistic ME (intercept)  and learning = no learning effect
KS_time_me_PT_learn = lmer(TimeOnTask ~ InPhaseTransition + TrialNumber + (1|ParticipantID),
                           data=KSAggData)
#summary(KS_time_me_PT_learn)

#logistic ME (intercept)  and strat = PT significantly negative at 1%, greedy & Prioritising sig +ve at 1%
KS_time_me_PT_strat = lmer(TimeOnTask ~ InPhaseTransition + Greedy + Feasible + Large_Items + (1|ParticipantID) +
                             Prioritising_Properties,
                           data=KSAggData)
#summary(KS_time_me_PT_strat)

#logistic ME (intercept)  and multiple strat = PT significantly negative at 1%, greedy & Prioritising sig +ve at 1%
KS_time_me_PT_mstrat = lmer(TimeOnTask ~ InPhaseTransition + (1|ParticipantID) +
                              MultipleStrategies,
                            data=KSAggData)
#summary(KS_time_me_PT_mstrat)
# 
# #logistic ME (intercept)  and demographics = PT significantly negative at 1%, postgrad & female +ve at 3%
# time_me_PT_dem = lmer(TimeOnTask ~ InPhaseTransition + Sex_Female + Age + PostGrad + Commerce + Early_Afternoon
#                       + Late_Afternoon + (1|ParticipantID), data=KSAggData)
# #summary(time_me_PT_dem)
# 
# #logistic ME (intercept)  and significant cont. = PT significantly negative at 1%, postgrad & female +ve at 3%
# time_me_PT_sig = lmer(TimeOnTask ~ InPhaseTransition + MultipleStrategies + TrialNumber  + 
#                         (1|ParticipantID),
#                       data=KSAggData)
# #summary(time_me_PT_sig)
# 
# #logistic ME (intercept)  and most  cont. = PT significantly negative at 1%, postgrad & female +ve at 3%
# time_me_PT_all = lmer(TimeOnTask ~ InPhaseTransition + MultipleStrategies +  TrialNumber  + 
#                         Sex_Female + Age + PostGrad + Commerce + Early_Afternoon + Late_Afternoon + (1|ParticipantID),
#                       data=KSAggData)
# #summary(time_me_PT_all)
# 
# 
# # stargazer(time_me_PT, time_me_PT_learn, time_me_PT_strat, time_me_PT_dem, time_me_PT_sig,
# #            time_me_PT_all, title="KS Mixed Effects (Intercept)", align=TRUE)
# 
# 
# 
# 
# 
#time ME regressions (intercept + slope)
# #logistic with ME (intercept and slope) = PT significantly negative at 1%
# KS_time_mes_PT = lmer(TimeOnTask ~ InPhaseTransition + (1|ParticipantID) + (InPhaseTransition|ParticipantID),
#                    data=KSAggData)
# #summary(KS_time_mes_PT)
# 
# #logistic ME (intercept and slope)  and learning = no learning effect
# KS_time_mes_PT_learn = lmer(TimeOnTask ~ InPhaseTransition + TrialNumber + (1|ParticipantID) + (InPhaseTransition|ParticipantID),
#                          data=KSAggData)
# #summary(KS_time_mes_PT_learn)
# 
# #logistic ME (intercept and slope)  and strat = PT significantly negative at 1%, greedy & Prioritising sig +ve at 1%
# KS_time_mes_PT_strat = lmer(TimeOnTask ~ InPhaseTransition + Greedy + Feasible + Large_Items + (1|ParticipantID) +
#                            Prioritising_Properties + (InPhaseTransition|ParticipantID),
#                          data=KSAggData)
# #summary(KS_time_mes_PT_strat)
# 
# #logistic ME (intercept and slope)  and multiple strat = PT significantly negative at 1%, greedy & Prioritising sig +ve at 1%
# KS_time_mes_PT_mstrat = lmer(TimeOnTask ~ InPhaseTransition + (1|ParticipantID) +
#                               MultipleStrategies + (InPhaseTransition|ParticipantID),
#                             data=KSAggData)
# #summary(KS_time_mes_PT_mstrat)

# #logistic ME (intercept and slope)  and demographics = PT significantly negative at 1%, postgrad & female +ve at 3%
# time_mes_PT_dem = lmer(TimeOnTask ~ InPhaseTransition + Sex_Female + Age + PostGrad + Commerce + Early_Afternoon
#                        + Late_Afternoon + (1|ParticipantID) + (InPhaseTransition|ParticipantID),
#                        data=KSAggData)
# #summary(time_mes_PT_dem)
# 
# #logistic ME (intercept and slope)  and most  cont. = PT significantly negative at 1%, postgrad & female +ve at 3%
# time_mes_PT_all = lmer(TimeOnTask ~ InPhaseTransition + MultipleStrategies + 
#                          Sex_Female + Age + PostGrad + Commerce + Early_Afternoon + Late_Afternoon + TrialNumber + 
#                          (1|ParticipantID) + (InPhaseTransition|ParticipantID),
#                        data=KSAggData)
# #summary(time_mes_PT_all)
# 
# 
# # stargazer(time_mes_PT, time_mes_PT_learn, time_mes_PT_strat, time_mes_PT_dem,
# #            time_mes_PT_all, title="KS Mixed Effects (Intercept + Slope)", align=TRUE)
# 
# 
# 
# 
# 
# 
# 
# #time ME regressions (intercept + slope), with type
# #logistic with ME (intercept and slope) = PT significantly negative at 1%
# time_mes_type = lmer(TimeOnTask ~ Difficulty + (1|ParticipantID) + (Difficulty|ParticipantID), 
#                      data=KSAggData)
# #summary(time_mes_type)
# 
# #logistic ME (intercept and slope)  and learning = no learning effect
# time_mes_type_learn = lmer(TimeOnTask ~ Difficulty + TrialNumber + (1|ParticipantID) + (Difficulty|ParticipantID),
#                            data=KSAggData)
# #summary(time_mes_type_learn)
# 
# #logistic ME (intercept and slope)  and strat = PT significantly negative at 1%, greedy & Prioritising sig +ve at 1%
# time_mes_type_strat = lmer(TimeOnTask ~ Difficulty + Greedy + Feasible + Large_Items + (1|ParticipantID) + 
#                              Prioritising_Properties + MultipleStrategies + (Difficulty|ParticipantID),
#                            data=KSAggData)
# #summary(time_mes_type_strat)
# 
# #logistic ME (intercept and slope)  and demographics = PT significantly negative at 1%, postgrad & female +ve at 3%
# time_mes_type_dem = lmer(TimeOnTask ~ Difficulty + Sex_Female + Age + PostGrad + Commerce + Early_Afternoon
#                          + Late_Afternoon + (1|ParticipantID) + (Difficulty|ParticipantID),
#                          data=KSAggData)
# #summary(time_mes_type_dem)
# 
# #logistic ME (intercept and slope)  and most  cont. = PT significantly negative at 1%, postgrad & female +ve at 3%
# time_mes_type_all = lmer(TimeOnTask ~ Difficulty + MultipleStrategies + TrialNumber + 
#                            Sex_Female + Age + PostGrad + Commerce + Early_Afternoon + Late_Afternoon + 
#                            (1|ParticipantID) + (Difficulty|ParticipantID),
#                          data=KSAggData)
# #summary(time_mes_type_all)
# 
# 
# # stargazer(time_mes_type, time_mes_type_learn, time_mes_type_strat, time_mes_type_dem,
# #            time_mes_type_all, title="KS Mixed Effects (Intercept + Slope)", align=TRUE)
# 
# 
# 
# 
# 
# 
# #time ME regressions (interceprop + slope), with propagations
KS_time_me_prop = lmer(TimeOnTask ~ propagations + (1|ParticipantID),
                       data=KSAggData)
#summary(KS_time_me_prop)

#logistic ME (intercept)  and learning = no learning effect
KS_time_me_prop_learn = lmer(TimeOnTask ~ propagations + TrialNumber + (1|ParticipantID),
                             data=KSAggData)
#summary(KS_time_me_prop_learn)

#logistic ME (intercept)  and strat = PT significantly negative at 1%, greedy & Prioritising sig +ve at 1%
KS_time_me_prop_strat = lmer(TimeOnTask ~ propagations + Greedy + Feasible + Large_Items + (1|ParticipantID) +
                               Prioritising_Properties,
                             data=KSAggData)
#summary(KS_time_me_prop_strat)

#logistic ME (intercept)  and multiple strat = PT significantly negative at 1%, greedy & Prioritising sig +ve at 1%
KS_time_me_prop_mstrat = lmer(TimeOnTask ~ propagations + (1|ParticipantID) +
                                MultipleStrategies,
                              data=KSAggData)
#summary(KS_time_me_prop_mstrat)
# 
# #logistic ME (interceprop and slope)  and demographics = prop significantly negative at 1%, postgrad & female +ve at 3%
# time_mes_prop_dem = lmer(TimeOnTask ~ propagations + Sex_Female + Age + PostGrad + Commerce + Early_Afternoon
#                          + Late_Afternoon + (1|ParticipantID) + (propagations|ParticipantID),
#                          data=KSAggData)
# #summary(time_mes_prop_dem)
# 
# #logistic ME (interceprop and slope)  and most  cont. = prop significantly negative at 1%, postgrad & female +ve at 3%
# time_mes_prop_all = lmer(TimeOnTask ~ propagations + MultipleStrategies + 
#                            Sex_Female + Age + PostGrad + Commerce + Early_Afternoon + Late_Afternoon + 
#                            (1|ParticipantID) + (propagations|ParticipantID) + TrialNumber,
#                          data=KSAggData)
# #summary(time_mes_prop_all)
# 
# 
# # stargazer(time_mes_prop, time_mes_prop_learn, time_mes_prop_strat, time_mes_prop_dem,
# #           time_mes_prop_all, title="KS Mixed Effects (Interceprop + Slope)", align=TRUE)
# 
# 
# 
# 
# 
# 
# 
# 
# #time ME regressions (intercedec + slope), with decisions
# #logistic with ME (intercedec and slope) = dec significantly negative at 1%
# time_mes_dec = lmer(TimeOnTask ~ decisions + (1|ParticipantID) + (decisions|ParticipantID), 
#                     data=KSAggData)
# #summary(time_mes_dec)
# 
# #logistic ME (intercedec and slope)  and learning = no learning effect
# time_mes_dec_learn = lmer(TimeOnTask ~ decisions + TrialNumber + (1|ParticipantID) + (decisions|ParticipantID),
#                           data=KSAggData)
# #summary(time_mes_dec_learn)
# 
# #logistic ME (intercedec and slope)  and strat = dec significantly negative at 1%, greedy & Prioritising sig +ve at 1%
# time_mes_dec_strat = lmer(TimeOnTask ~ decisions + Greedy + Feasible + Large_Items + (1|ParticipantID) + 
#                             Prioritising_Properties + MultipleStrategies + (decisions|ParticipantID),
#                           data=KSAggData)
# #summary(time_mes_dec_strat)
# 
# #logistic ME (intercedec and slope)  and demographics = dec significantly negative at 1%, postgrad & female +ve at 3%
# time_mes_dec_dem = lmer(TimeOnTask ~ decisions + Sex_Female + Age + PostGrad + Commerce + Early_Afternoon
#                         + Late_Afternoon + (1|ParticipantID) + (decisions|ParticipantID),
#                         data=KSAggData)
# #summary(time_mes_dec_dem)
# 
# #logistic ME (intercedec and slope)  and most  cont. = dec significantly negative at 1%, postgrad & female +ve at 3%
# time_mes_dec_all = lmer(TimeOnTask ~ decisions + MultipleStrategies + 
#                           Sex_Female + Age + PostGrad + Commerce + Early_Afternoon + Late_Afternoon + 
#                           (1|ParticipantID) + (decisions|ParticipantID) + TrialNumber,
#                         data=KSAggData)
# #summary(time_mes_dec_all)
# 
# 
# # stargazer(time_mes_dec, time_mes_dec_learn, time_mes_dec_strat, time_mes_dec_dem,
# #           time_mes_dec_all, title="KS Mixed Effects (Intercedec + Slope)", align=TRUE)







# KSAggDataPT <- subset(KSAggData, type<=4)


# #KS summary stats
# #two-sampled t-test for differences in mean accuracy b/w in/out of PT, significant at 1%
# tbl = table(KSAggDataPT$InPhaseTransition, KSAggDataPT$correct)
# chisq.test(tbl)
# #two-sampled t-test for differences in mean time spent b/w in/out of PT, significant at 1%
# t.test(TimeOnTask ~ InPhaseTransition, data = KSAggData)
# 
# #two-sampled t-test for differences in mean accuracy b/w under-constrained & PT, significant at 1%
# KS_under_in <- filter(KSAggData, KSAggData$Region=="PT" | KSAggData$Region=="Under")
# tbl = table(KS_under_in$Region, KS_under_in$correct) 
# chisq.test(tbl)
# #two-sampled t-test for differences in mean time spent b/w under-constrained & PT, significant at 1%
# t.test(TimeOnTask ~ Region, data = KS_under_in)
# 
# #two-sampled t-test for differences in mean accuracy b/w over-constrained & PT, significant at 5%
# KS_over_in <- filter(KSAggData, KSAggData$Region=="PT" | KSAggData$Region=="Over")
# tbl = table(KS_over_in$Region, KS_over_in$correct) 
# chisq.test(tbl)
# #two-sampled t-test for differences in mean time spent b/w over-constrained & PT, significant at 3%
# t.test(TimeOnTask ~ Region, data = KS_over_in)


#aggregate(KSAggData[,c(1,3)], by = list(KSAggData$type),mean, na.rm = TRUE)




# library(plm)            # regression
# time_fe = plm(TimeOnTask ~ InPhaseTransition, data=KSAggData,model="random")
# summary(time_fe)



# #accuracy yes.no regressions
# #logistic with FE and no controls = Time significantly negative at 1%
# acc_YES = glm(correct ~ TimeOnTask, family=binomial(link="logit"), data=KSYes)
# summary(acc_YES)
# 
# #logistic with FE and no controls = Time significantly negative at 5% - opposite of what we'd expect
# acc_No = glm(correct ~ TimeOnTask, family=binomial(link="logit"), data=KSNo)
# summary(acc_No)
# 
# #logistic with FE and no controls = Time significantly negative at 1% - opposite of what we'd expect
# acc_time = glm(correct ~ TimeOnTask, family=binomial(link="logit"), data=KSAggData)
# summary(acc_time)
# 
# 
# #logistic with FE and no controls = Time insignificant
# acc_Under = glm(correct ~ TimeOnTask, family=binomial(link="logit"), data=KSUnder)
# summary(acc_Under)
# 
# #logistic with FE and no controls = Time significantly negative at 10%
# acc_Over = glm(correct ~ TimeOnTask, family=binomial(link="logit"), data=KSOver)
# summary(acc_Over)
# 
# #logistic with FE and no controls = Time insignificant
# acc_PT = glm(correct ~ TimeOnTask, family=binomial(link="logit"), data=KSPT)
# summary(acc_PT)
