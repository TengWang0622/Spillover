##########################################################################################################
#
#                                    P   R   O   J   E   C   T
#                                                of
#               Healthcare system resilience and adaptation policies under climate hazards
#
###########################################################################################################

# Developed by Teng Wang, Hanxu Shi

# Contact: wang.teng19@alumni.imperial.ac.uk
#          shx@bjmu.edu.cn

# Version - 20240325

# Description: Main Script

############################################
#             Preparation
############################################

library(readxl)
library(tidyverse)
#library(dlnm)
#library(splines)
#library(survival)
#library(mvmeta)
library(dplyr)
library(magrittr)
library(Matrix)

library(foreach)
library(doParallel)
library(progress)

library(lme4)
#library(glmerTest)

library(ape)

library(MatchIt)
library(WeightIt)
library(MASS)
library(cli)


library(fixest)
library(ggplot2)
library(stats)
library(lubridate)

library(openxlsx)
##########################################################################################
#                            Customized Functions
##########################################################################################


Aggregate_DID=function(df_DID){
  
  DID_Simplified=data.frame()
  
  cli_progress_bar('Processing O3 dataset', total=length(seq(1,nrow(df_DID),No_subgroup)), type='tasks',
                   format="{cli::pb_bar} {pb_percent} @ {cli::pb_current}:{cli::pb_total} ETA: {cli::pb_eta}")
  for (l in seq(1,nrow(df_DID),No_subgroup)){
    
    Group_temp=df_DID[l:(l+No_subgroup-1),]
    
    Group_Male=Group_temp[which(Group_temp$Gender=="Male"),"All_cause"]
    Group_Female=Group_temp[which(Group_temp$Gender=="Female"),"All_cause"]
    
    Group_18=Group_temp[which(Group_temp$Age_g=="<18"),"All_cause"]
    Group_18_44=Group_temp[which(Group_temp$Age_g=="18_44"),"All_cause"]
    Group_45_64=Group_temp[which(Group_temp$Age_g=="45_64y"),"All_cause"]
    Group_65=Group_temp[which(Group_temp$Age_g=="65y"),"All_cause"]
    
    Group_ED=Group_temp[which(Group_temp$Transfer=="ED"),"All_cause"]
    Group_outpatient=Group_temp[which(Group_temp$Transfer=="outpatient"),"All_cause"]
    Group_Others=Group_temp[which(Group_temp$Transfer=="Others"),"All_cause"]
    
    DID_temp=cbind(datetime=format(as.POSIXct(Group_temp$datetime[1], origin = "1970-01-01"), "%Y-%m-%d"),County=Group_temp$County[1],CountyID=Group_temp$CountyID[1],
                   NoDays=Group_temp$NoDays[1],caseTime=Group_temp$caseTime[1],caseCounty=Group_temp$caseCounty[1],case=Group_temp$case[1],#EventID=Group_temp$EventID[1],
                   Insurance_payment=sum(Group_temp$Insurance_payment),Self_payment=sum(Group_temp$Self_payment),zfy=sum(Group_temp$zfy),
                   
                   tempmaxC=round(5/9*(Group_temp$tempmax[1]-32),digits=1),tempC=round(5/9*(Group_temp$temp[1]-32),digits=1),tempminC=round(5/9*(Group_temp$tempmin[1]-32),digits=1),
                   humidity=Group_temp$humidity[1],pressure=Group_temp$pressure[1],windspeed=Group_temp$windspeed[1],precip=Group_temp$precip[1],uvindex=Group_temp$uvindex[1],
                   solarenergy=Group_temp$solarenergy[1],solarradiation=Group_temp$solarradiation[1],cloudcover=Group_temp$cloudcover[1],winddir=Group_temp$winddir[1],
                   
                   All_cause=sum(Group_temp$All_cause),
                   Infectious=sum(Group_temp$Infectious),Neoplasms=sum(Group_temp$Neoplasms),Malnutrition=sum(Group_temp$Malnutrition),
                   Endocrine=sum(Group_temp$Endocrine),Neuro=sum(Group_temp$Neuro),Respiratory=sum(Group_temp$Respiratory),
                   Genitourinary=sum(Group_temp$Genitourinary),Blood=sum(Group_temp$Blood),Injuries=sum(Group_temp$Injuries),
                   CVD=sum(Group_temp$CVD),Mental=sum(Group_temp$Mental),Others=sum(Group_temp$Others),
                   
                   Male=sum(Group_Male),Female=sum(Group_Female),
                   
                   ED=sum(Group_ED),outpatient=sum(Group_outpatient),UnknownTransfer=sum(Group_Others),
                   Age18=sum(Group_18),Age18_44=sum(Group_18_44),Age45_64=sum(Group_45_64),Age65=sum(Group_65))
    
    
    DID_Simplified=rbind(DID_Simplified,DID_temp)
    cli_progress_update()
  }
  
  
  DID_Simplified[,"tempmaxC"] <- as.numeric(DID_Simplified[,"tempmaxC"])
  DID_Simplified[,"tempC"] <- as.numeric(DID_Simplified[,"tempC"])
  DID_Simplified[,"tempminC"] <- as.numeric(DID_Simplified[,"tempminC"])
  DID_Simplified[,"humidity"] <- as.numeric(DID_Simplified[,"humidity"])
  DID_Simplified[,"pressure"] <- as.numeric(DID_Simplified[,"pressure"])
  DID_Simplified[,"windspeed"] <- as.numeric(DID_Simplified[,"windspeed"])
  DID_Simplified[,"precip"] <- as.numeric(DID_Simplified[,"precip"])
  DID_Simplified[,"uvindex"] <- as.numeric(DID_Simplified[,"uvindex"])
  DID_Simplified[,"solarenergy"] <- as.numeric(DID_Simplified[,"solarenergy"])
  DID_Simplified[,"solarradiation"] <- as.numeric(DID_Simplified[,"solarradiation"])
  DID_Simplified[,"cloudcover"] <- as.numeric(DID_Simplified[,"cloudcover"])
  DID_Simplified[,"winddir"] <- as.numeric(DID_Simplified[,"winddir"])
  
  DID_Simplified[,"All_cause"] <- as.numeric(DID_Simplified[,"All_cause"])
  DID_Simplified[,"Infectious"] <- as.numeric(DID_Simplified[,"Infectious"])
  DID_Simplified[,"Neoplasms"] <- as.numeric(DID_Simplified[,"Neoplasms"])
  DID_Simplified[,"Malnutrition"] <- as.numeric(DID_Simplified[,"Malnutrition"])
  DID_Simplified[,"Endocrine"] <- as.numeric(DID_Simplified[,"Endocrine"])
  DID_Simplified[,"Neuro"] <- as.numeric(DID_Simplified[,"Neuro"])
  DID_Simplified[,"Respiratory"] <- as.numeric(DID_Simplified[,"Respiratory"])
  DID_Simplified[,"Genitourinary"] <- as.numeric(DID_Simplified[,"Genitourinary"])
  DID_Simplified[,"Blood"] <- as.numeric(DID_Simplified[,"Blood"])
  DID_Simplified[,"Injuries"] <- as.numeric(DID_Simplified[,"Injuries"])
  DID_Simplified[,"CVD"] <- as.numeric(DID_Simplified[,"CVD"])
  DID_Simplified[,"Mental"] <- as.numeric(DID_Simplified[,"Mental"])
  DID_Simplified[,"Others"] <- as.numeric(DID_Simplified[,"Others"])
  
  DID_Simplified[,"Insurance_payment"] <- as.numeric(DID_Simplified[,"Insurance_payment"])
  DID_Simplified[,"Self_payment"] <- as.numeric(DID_Simplified[,"Self_payment"])
  DID_Simplified[,"zfy"] <- as.numeric(DID_Simplified[,"zfy"])
  
  DID_Simplified[,"ED"] <- as.numeric(DID_Simplified[,"ED"])
  DID_Simplified[,"outpatient"] <- as.numeric(DID_Simplified[,"outpatient"])
  DID_Simplified[,"UnknownTransfer"] <- as.numeric(DID_Simplified[,"UnknownTransfer"])
  
  DID_Simplified[,"Age18"] <- as.numeric(DID_Simplified[,"Age18"])
  DID_Simplified[,"Age18_44"] <- as.numeric(DID_Simplified[,"Age18_44"])
  DID_Simplified[,"Age45_64"] <- as.numeric(DID_Simplified[,"Age45_64"])
  DID_Simplified[,"Age65"] <- as.numeric(DID_Simplified[,"Age65"])
  
  DID_Simplified[,"Male"] <- as.numeric(DID_Simplified[,"Male"])
  DID_Simplified[,"Female"] <- as.numeric(DID_Simplified[,"Female"])
  
  DID_Simplified[,"NoDays"] <- as.numeric(DID_Simplified[,"NoDays"])
  DID_Simplified[,"caseTime"] <- as.numeric(DID_Simplified[,"caseTime"])
  DID_Simplified[,"caseCounty"] <- as.numeric(DID_Simplified[,"caseCounty"])
  #DID_Simplified[,"EventID"] <- as.numeric(DID_Simplified[,"EventID"])
  DID_Simplified[,"CountyID"] <- as.numeric(DID_Simplified[,"CountyID"])
  
  return(DID_Simplified)
  
}


# =================== Parallel Trend =========================

ParallelTrend_data=function(SrcData,Disease){
  
  Row_County=c(1, which(SrcData$County[-1] != SrcData$County[-length(SrcData$County)]) + 1)
  Row_NoDays=c(1, which(SrcData$NoDays[-1] != SrcData$NoDays[-length(SrcData$NoDays)]) + 1)
  
  Row_seperate=sort(union(Row_County, Row_NoDays))
  Duration_seperate=diff(Row_seperate)
  
  PT_data=data.frame()
  for(i in 1:length(Duration_seperate)){
    
    Row_start=Row_seperate[i]
    Duration_temp=Duration_seperate[i]
    
    post=SrcData$caseTime[Row_start]
    treated=SrcData$caseCounty[Row_start]
    county=SrcData$County[Row_start]
    
    Group=data.frame()
    for(j in 1:Duration_temp){
      Row_Day=Row_start+j-1
      datetime=SrcData$datetime[Row_Day]
      
      post=SrcData$caseTime[Row_Day]
      
      day=ifelse(post == 0, -(Duration_temp/2+1)+j, j-Duration_temp/2-1)
      rel_time=day
      rel_time_group = case_when(
        rel_time <= -3 ~ "-3+",
        rel_time >= 3 ~ "3+",
        TRUE ~ as.character(rel_time)
      )
      
      All_cause=SrcData$All_cause[Row_start-1+j]
      Infectious=SrcData$Infectious[Row_start-1+j]
      Neoplasms=SrcData$Neoplasms[Row_start-1+j]
      Malnutrition=SrcData$Malnutrition[Row_start-1+j]
      Endocrine=SrcData$Endocrine[Row_start-1+j]
      Neuro=SrcData$Neuro[Row_start-1+j]
      Respiratory=SrcData$Respiratory[Row_start-1+j]
      Genitourinary=SrcData$Genitourinary[Row_start-1+j]
      Blood=SrcData$Blood[Row_start-1+j]
      Injuries=SrcData$Injuries[Row_start-1+j]
      CVD=SrcData$CVD[Row_start-1+j]
      Mental=SrcData$Mental[Row_start-1+j]
      Others=SrcData$Others[Row_start-1+j]
      
      Male=SrcData$Male[Row_start-1+j]
      Female=SrcData$Female[Row_start-1+j]
      
      ED=SrcData$ED[Row_start-1+j]
      outpatient=SrcData$outpatient[Row_start-1+j]
      UnknownTransfer=SrcData$UnknownTransfer[Row_start-1+j]
      
      Age18=SrcData$Age18[Row_start-1+j]
      Age18_44=SrcData$Age18_44[Row_start-1+j]
      Age45_64=SrcData$Age45_64[Row_start-1+j]
      Age65=SrcData$Age65[Row_start-1+j]
      
      GroupTemp=c(county,datetime,day,treated,post,rel_time,rel_time_group,
                  All_cause,Infectious,Neoplasms,Malnutrition,Endocrine,Neuro,Respiratory,Genitourinary,Blood,Injuries,CVD,Mental,Others,
                  Male,Female,ED,outpatient,UnknownTransfer,Age18,Age18_44,Age45_64,Age65)
      Group=rbind(Group,GroupTemp)
    }
    colnames(Group)=c("county","datetime","day","treated","post","rel_time","rel_time_group",
                      "All_cause","Infectious","Neoplasms","Malnutrition","Endocrine","Neuro","Respiratory","Genitourinary","Blood","Injuries","CVD","Mental","Others",
                      "Male","Female","ED","outpatient","UnknownTransfer","Age18","Age18_44","Age45_64","Age65")
    
    PT_data=rbind(PT_data,Group)
  }
  
  PT_data$treated=as.numeric(PT_data$treated)
  PT_data$post=as.numeric(PT_data$post)
  
  PT_data$All_cause=as.numeric(PT_data$All_cause)
  PT_data$Infectious=as.numeric(PT_data$Infectious)
  PT_data$Neoplasms=as.numeric(PT_data$Neoplasms)
  PT_data$Malnutrition=as.numeric(PT_data$Malnutrition)
  PT_data$Endocrine=as.numeric(PT_data$Endocrine)
  PT_data$Neuro=as.numeric(PT_data$Neuro)
  PT_data$Respiratory=as.numeric(PT_data$Respiratory)
  PT_data$Genitourinary=as.numeric(PT_data$Genitourinary)
  PT_data$Blood=as.numeric(PT_data$Blood)
  PT_data$Injuries=as.numeric(PT_data$Injuries)
  PT_data$CVD=as.numeric(PT_data$CVD)
  PT_data$Mental=as.numeric(PT_data$Mental)
  PT_data$Others=as.numeric(PT_data$Others)
  
  PT_data$Male=as.numeric(PT_data$Male)
  PT_data$Female=as.numeric(PT_data$Female)
  
  PT_data$ED=as.numeric(PT_data$ED)
  PT_data$outpatient=as.numeric(PT_data$outpatient)
  PT_data$UnknownTransfer=as.numeric(PT_data$UnknownTransfer)
  
  PT_data$Age18=as.numeric(PT_data$Age18)
  PT_data$Age18_44=as.numeric(PT_data$Age18_44)
  PT_data$Age45_64=as.numeric(PT_data$Age45_64)
  PT_data$Age65=as.numeric(PT_data$Age65)
  
  return(PT_data)
}

# ======================= DID Output ==========================

DID_Output=function(df_Mod,Disease_Target){ # To calcualte the mean with 95 CI automatically
  
  
  df_Pan=df_Mod[!(df_Mod$datetime >= as.Date('2020-01-01') & df_Mod$datetime <= as.Date('2020-12-31')), ]
  
  # ---------------------------------- DID ----------------------------------------------------
  
  #eval(parse(text = paste("Result_Org=glmer(",Disease_Target," ~ caseCounty*caseTime + (1|datetime) + (1|County) + offset(log(Pop*as.numeric(NoDays))), data =df_Mod)", sep = "")))
  #eval(parse(text = paste("Result_Org=glmer(",Disease_Target," ~ caseCounty*caseTime + (1|datetime) + (1|County) + offset(log(as.numeric(NoDays))), data =df_Mod)", sep = "")))
  
  # ------------- glmer GAUSSIAN ----------------------
  eval(parse(text = paste("Result_Org=glmer(log(",Disease_Target,"+1) ~ caseCounty*caseTime + (1|datetime) + (1|County), data =df_Mod)", sep = "")))
  eval(parse(text = paste("Result_Sen=glmer(log(",Disease_Target,"+1) ~ caseCounty*caseTime + (1|County) , data =df_Mod)", sep = "")))
  eval(parse(text = paste("Result_Pan=glmer(log(",Disease_Target,"+1) ~ caseCounty*caseTime + (1|datetime) + (1|County) , data =df_Pan)", sep = "")))
  
  # ------------- glmer POISSON ----------------------
  #eval(parse(text = paste("Result_Org=glmer(",Disease_Target," ~ caseCounty*caseTime + (1|datetime) + (1|County), data =df_Mod, family = poisson)", sep = "")))
  #eval(parse(text = paste("Result_Org=glmer(",Disease_Target," ~ caseCounty*caseTime + (1|County) , data =df_Mod, family = poisson)", sep = "")))
  
  # ------------- glmer NB ----------------------
  #eval(parse(text = paste("Result_Org=glmer.nb(",Disease_Target," ~ caseCounty*caseTime + (1|datetime) + (1|County), data =df_Mod)", sep = "")))
  #eval(parse(text = paste("Result_Org=glmer.nb(",Disease_Target," ~ caseCounty*caseTime + (1|County) , data =df_Mod)", sep = "")))
  
  # EXP
  #ci_fixed_log=confint(Result_Org, method="Wald", level = 0.95)
  #ci_interaction_log=ci_fixed_log["caseCounty:caseTime", ]
  #percent_change_log=(exp(ci_interaction_log) - 1) * 100
  
  #Expect=round((exp(fixef(Result_Org)["caseCounty:caseTime"]) - 1) * 100, 2)
  #Lower_val=round(percent_change_log[1], 2)
  #Upper_val=round(percent_change_log[2], 2)
  
  #Output_DID=paste(Expect,' [',Lower_val,', ',Upper_val,']',sep="")
  
  # ============== DID ==================
  
  ci_fixed_log=confint(Result_Org, method="Wald", level = 0.95)
  ci_interaction_log=ci_fixed_log["caseCounty:caseTime", ]
  
  Expect=round(fixef(Result_Org)["caseCounty:caseTime"] * 100, 2)
  Lower_val=round(ci_interaction_log[1]*100, 2)
  Upper_val=round(ci_interaction_log[2]*100, 2)
  
  Output_DID=paste(Expect,' [',Lower_val,', ',Upper_val,']',sep="")
  
  # ============ Sensitivity ================
  
  ci_fixed_log=confint(Result_Sen, method="Wald", level = 0.95)
  ci_interaction_log=ci_fixed_log["caseCounty:caseTime", ]
  
  Expect=round(fixef(Result_Sen)["caseCounty:caseTime"] * 100, 2)
  Lower_val=round(ci_interaction_log[1]*100, 2)
  Upper_val=round(ci_interaction_log[2]*100, 2)
  
  Output_Sen=paste(Expect,' [',Lower_val,', ',Upper_val,']',sep="")
  
  # ============ Exclude Pandemic ================
  
  ci_fixed_log=confint(Result_Pan, method="Wald", level = 0.95)
  ci_interaction_log=ci_fixed_log["caseCounty:caseTime", ]
  
  Expect=round(fixef(Result_Pan)["caseCounty:caseTime"] * 100, 2)
  Lower_val=round(ci_interaction_log[1]*100, 2)
  Upper_val=round(ci_interaction_log[2]*100, 2)
  
  Output_Pan=paste(Expect,' [',Lower_val,', ',Upper_val,']',sep="")
  
  # ============ Pre-post ================
  
  df_T0C1=df_Mod[which(df_Mod[,"caseTime"]==0 & df_Mod[,"caseCounty"]==1),]
  df_T1C1=df_Mod[which(df_Mod[,"caseTime"]==1 & df_Mod[,"caseCounty"]==1),]
  
  Prepost_Affected=rbind(df_T0C1,df_T1C1)
  
  df_T0C0=df_Mod[which(df_Mod[,"caseTime"]==0 & df_Mod[,"caseCounty"]==0),]
  df_T1C0=df_Mod[which(df_Mod[,"caseTime"]==1 & df_Mod[,"caseCounty"]==0),]
  
  Prepost_Control=rbind(df_T0C0,df_T1C0)
  
  eval(parse(text = paste('Val_T0C1=df_T0C1$',Disease_Target,sep="")))
  eval(parse(text = paste('Val_T1C1=df_T1C1$',Disease_Target,sep="")))
  eval(parse(text = paste('Val_T0C0=df_T0C0$',Disease_Target,sep="")))
  eval(parse(text = paste('Val_T1C0=df_T1C0$',Disease_Target,sep="")))
  
  Summary_T0C0=cbind(SUM=sum(Val_T0C0),AVE=mean(Val_T0C0),MAX=max(Val_T0C0))
  Summary_T1C0=cbind(SUM=sum(Val_T1C0),AVE=mean(Val_T1C0),MAX=max(Val_T1C0))
  Summary_T0C1=cbind(SUM=sum(Val_T0C1),AVE=mean(Val_T0C1),MAX=max(Val_T0C1))
  Summary_T1C1=cbind(SUM=sum(Val_T1C1),AVE=mean(Val_T1C1),MAX=max(Val_T1C1))
  
  Summary_Val=rbind(Summary_T0C0,Summary_T1C0,Summary_T0C1,Summary_T1C1)
  
  Control_Summary=paste(sum(Val_T0C0),'(',round(mean(Val_T0C0),2),' ',max(Val_T0C0),') vs ',sum(Val_T1C0),'(',round(mean(Val_T1C0),2),', ',max(Val_T1C0),')',sep="")
  Affected_Summary=paste(sum(Val_T0C1),'(',round(mean(Val_T0C1),2),' ',max(Val_T0C1),') vs ',sum(Val_T1C1),'(',round(mean(Val_T1C1),2),', ',max(Val_T1C1),')',sep="")
  
  
  # ------------- Prepost affected ---------------------------------------------
  #eval(parse(text = paste("Result_Prepost=glmer(log(",Disease_Target,"+1) ~ caseTime + (1|datetime), data=Prepost_Affected)", sep = "")))
  
  #ci_fixed_log=confint(Result_Prepost, method="Wald", level = 0.95)["caseTime", ]
  #Expect=round(fixef(Result_Prepost)["caseTime"]*100, 2)
  #Lower_val=round(ci_fixed_log[1]*100, 2)
  #Upper_val=round(ci_fixed_log[2]*100, 2)
  
  #Output_Prepost_Affected=paste(Expect,' [',Lower_val,', ',Upper_val,']',sep="")
  
  # ------------- Prepost control -----------------------------------------------
  #eval(parse(text = paste("Result_Prepost_Control=glmer(log(",Disease_Target,"+1) ~ caseTime + (1|datetime), data=Prepost_Control)", sep = "")))
  
  #ci_fixed_log=confint(Result_Prepost_Control, method="Wald", level = 0.95)["caseTime", ]
  #Expect=round(fixef(Result_Prepost_Control)["caseTime"]*100, 2)
  #Lower_val=round(ci_fixed_log[1]*100, 2)
  #Upper_val=round(ci_fixed_log[2]*100, 2)
  
  #Output_Prepost_Control=paste(Expect,' [',Lower_val,', ',Upper_val,']',sep="")
  
  Output=rbind(Control_Summary,
               Affected_Summary,
               #Prepost_Control=Output_Prepost_Control,
               #Prepost=Output_Prepost_Affected,
               DID=Output_DID,
               Sen=Output_Sen,
               Pan=Output_Pan)
  
  print(Output)
}


###################################################################
#                    Pre-settings
###################################################################

No_subgroup=36

# Specify the event, period and disease ----------------------------------↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

Event_type="WinterStorm"      # Storm, Flood, Cyclone, WinterStorm
ObsWindow='Enduring'          # Warning, Enduring, Post, After, RTH
Disease='Respiratory'         # Malnutrition, Respiratory, Injuries, Infectious, Mental, Neoplasms, Endocrine, Blood, Neuro,
                              # Genitourinary, CVD, Others------------------------------- INPUT
# Loading files

FilePos="~/Library/Mobile Documents/com~apple~CloudDocs/Project/Spillover/Result Disaster/"     # Disaster
#FilePos="~/Library/Mobile Documents/com~apple~CloudDocs/Project/Spillover/Result 0.83/"        # Buffer - I
#FilePos="~/Library/Mobile Documents/com~apple~CloudDocs/Project/Spillover/Result 0.83 1.80/"   # Buffer - II

ModelType="/DID_" 
DID_Tier0=readRDS(paste(FilePos,Event_type,ModelType,"T0",ObsWindow,".rds",sep = ""))
DID_Tier1=readRDS(paste(FilePos,Event_type,ModelType,"T1",ObsWindow,".rds",sep = ""))
DID_Tier2=readRDS(paste(FilePos,Event_type,ModelType,"T2",ObsWindow,".rds",sep = ""))

# Aggregation

DID0_Agg=Aggregate_DID(DID_Tier0)
DID1_Agg=Aggregate_DID(DID_Tier1)
DID2_Agg=Aggregate_DID(DID_Tier2)

# Output
ORG0=DID_Output(DID0_Agg,Disease)
ORG1=DID_Output(DID1_Agg,Disease)
ORG2=DID_Output(DID2_Agg,Disease)

#########################################################################
#                        Parallel Trend Test
#########################################################################

PT_df=ParallelTrend_data(DID0_Agg,Disease)

eval(parse(text = paste('event_study=feols(log(',Disease,'+1) ~ i(rel_time_group, treated, ref = -1) | county + rel_time_group, data = PT_df)',sep="")))

coef_summary=summary(event_study)
coef_plot=coefplot(event_study)

coef_data=data.frame(
  term = names(coef(event_study)),
  estimate = coef(event_study),
  se = se(event_study)
) %>%
  mutate(
    ci_low = estimate - 1.96 * se,
    ci_high = estimate + 1.96 * se,
    time = as.numeric(gsub("rel_time_group::(-?\\d+)\\+?:treated", "\\1", term)),
    post_event = time >= 0,
    ci_crosses_zero = sign(ci_low) != sign(ci_high)
  ) %>%
  filter(!is.na(time))  # Remove the ref, usually -1

ref_row=data.frame(term='rel_time_group::-1:reference', estimate = 0, se=0,ci_low = 0, ci_high = 0,time=-1,post_event="FALSE",ci_crosses_zero="TRUE")
ref_row_right=data.frame(term='rel_time_group::-1:reference', estimate = 0, se=0,ci_low = 0, ci_high = 0,time=-1,post_event="TRUE",ci_crosses_zero="TRUE")

coef_data=rbind(coef_data, ref_row,ref_row_right) %>% arrange(time)

ggplot(coef_data, aes(x = time, y = estimate, color = post_event)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  geom_vline(xintercept = -0, linetype = "dashed", color = "black") +
  
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high),alpha=0.1,fill="blue",linetype="blank")+
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2, alpha = 1) +
  geom_point(size = 3, alpha = 1) +
  #geom_point(data = subset(coef_data, ci_crosses_zero), 
  #           shape = 15, size = 5, alpha = 0.6) +
  
  scale_color_manual(values = c("blue", "blue"))        
  labs(
    x = "Time (day)",
    y = "Estimated coefficients"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold",size = 16),
    panel.grid = element_blank(),
    panel.background = element_rect(color = "black", linewidth =1),
    axis.text = element_text(size = 14),
    legend.position = "none"
  ) +
  scale_x_continuous(breaks = seq(-14, 14, by = 1))


# Save the PT figure
eval(parse(text = paste('ggsave("/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Spillover/Result_PT/Disaster/',Event_type,'/',Disease,'PT.png", width = 8, height = 6, dpi = 600)',sep="")))

# Save the PT file
eval(parse(text = paste('write.xlsx(PT_df,"/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Spillover/Result_PT/Disaster/',Event_type,'/',Disease,'_PT_df.xlsx", rowNames = FALSE)',sep="")))



