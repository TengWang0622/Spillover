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
#library(lmerTest)

library(ape)

library(MatchIt)
library(WeightIt)
library(MASS)
library(cli)
library(readxl)

library(openxlsx)
# ============================ Loading files ============================

# County profile
CountyProfile=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/CountyProfile.xlsx")

# Disasters
TypeDisaster_province_Formal=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Disasters_Classified/TypeDisaster_province_Formal.xlsx")
TypeDisaster_city_Formal=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Disasters_Classified/TypeDisaster_city_Formal.xlsx")
TypeDisaster_county_Formal=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Disasters_Classified/TypeDisaster_county_Formal_GenHW.xlsx")

# Other src
Geo_all=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Geo_all.xlsx")
GDP=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/GDP.rds")
Eco=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Eco.rds")

# PSM
CityProfile=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Resilience/CityProfile.xlsx")
AirQuality=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/AirQuality.xlsx")
City_Resilience_Domain_Element_Indicator=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Resilience/City_Resilience_Domain_Element_Indicator.xlsx")

# Hospitalizations
Case_all=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Case_all.rds")

# ======================= Manipulation ===========================

colnames(Case_all)[colnames(Case_all) == "date"] <- "datetime"
Case_all$datetime <- force_tz(Case_all$datetime,tzone='UTC')

Eco[,2:ncol(Eco)]=lapply(Eco[,2:ncol(Eco)],as.numeric)



#################################################################
#                Event Classification
#################################################################

# Input files

df_Province=TypeDisaster_province_Formal
df_City=TypeDisaster_city_Formal
df_County=TypeDisaster_county_Formal

# ============ User inputs ============= --------------------------------------------------------------------- !!!!!!!!!!!

Event_type="Storm" # Storm, Flood, Cyclone, WinterStorm, Sand, Drought # Heatwave(DryHeatwave, HumidHeatwave)
Event_level="ALL"    # all level - ALL; Billion - B; Million - M
StudyRegion="Spillover"     # "Spillover"



#################################################################
#           County Classification - Resilience Index
#################################################################

# ------------------ Directly load the resilience level from preliminary results ----------------------

if(StudyRegion=="Local"){
  CityT0=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/ResilienceLevel/CityT0.rds")
  CityT1=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/ResilienceLevel/CityT1.rds")
  CityT2=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/ResilienceLevel/CityT2.rds")
  CityT3=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/ResilienceLevel/CityT3.rds")
  
  CountyT0=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/ResilienceLevel/CountyT0.rds")
  CountyT1=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/ResilienceLevel/CountyT1.rds")
  CountyT2=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/ResilienceLevel/CountyT2.rds")
  CountyT3=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/ResilienceLevel/CountyT3.rds")
} else if(StudyRegion=="Spillover"){
  
  CityT0=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/Spillover/Src/HealthLevel/CityT0.rds")
  CityT1=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/Spillover/Src/HealthLevel/CityT1.rds")
  CityT2=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/Spillover/Src/HealthLevel/CityT2.rds")
  
  CountyT0=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/Spillover/Src/HealthLevel/CountyT0.rds")
  CountyT1=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/Spillover/Src/HealthLevel/CountyT1.rds")
  CountyT2=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/Spillover/Src/HealthLevel/CountyT2.rds")
  
}



# Default values

No_subgroup=36
No_controlCounty=4

result_pre=0 
result_warn=-1                   
result_case=1
result_post=2
result_after=3

Add_lower=0.9
Add_upper=1.1

# Event filter
df_EProvince=df_Province[df_Province$Disaster==Event_type,]
df_ECity=df_City[df_City$Disaster==Event_type,]
df_ECounty=df_County[df_County$Disaster==Event_type,]

# ================== Pre-processing ======================

# --------------------- Province --------------------- 

No_Event=length(unique(df_EProvince$Events))

if(No_Event>0){
  Judgement_Province=1
  ID=unique(df_EProvince$Events)
  
  Event_info=data.frame()
  for(i in 1:No_Event){
    
    Loc_row=which(df_EProvince$Events==ID[i])
    Group=df_EProvince[Loc_row,]
    Loc=df_EProvince$Province
    StartT=df_EProvince$Begin[Loc_row]
    EndT=df_EProvince$End[Loc_row]
    No_days=difftime(EndT, StartT, units = "days")+1
    Damage=df_EProvince$`Total Damage, Adjusted ('000 US$)`[Loc_row]
    
    No_subLoc=nrow(Group)
    
    LocLIST=data.frame()
    for(j in 1:No_subLoc){
      subLoc=Group$Province[j]
      
      AllLoc_row=which(GDP$Province==subLoc)
      subGroup=GDP[AllLoc_row,]
      AllLoc=GDP$County[AllLoc_row]
      AllGDP=GDP$GDP_t[AllLoc_row]
      
      LocLIST_temp=data.frame(AllLoc,AllGDP)
      LocLIST=rbind(LocLIST,LocLIST_temp)
    }
    
    LocLIST=na.omit(LocLIST)
    
    LocLIST$EventID=i
    LocLIST$Start=StartT[1]
    LocLIST$End=EndT[1]
    LocLIST$No_days=No_days[1]
    LocLIST$DamageTotal=Damage[1]
    LocLIST$DamageCounty=LocLIST$AllGDP/sum(LocLIST$AllGDP)*Damage[1]
    
    Event_info=rbind(Event_info,LocLIST) # Produce the Event information
  }
  
  Event_Province=Event_info
} else{
  Judgement_Province=0
}

#  ---------------------  City  --------------------- 

No_Event=length(unique(df_ECity$Events))

if(No_Event>0){
  Judgement_City=1
  ID=unique(df_ECity$Events)
  
  Event_info=data.frame()
  for(i in 1:No_Event){
    
    Loc_row=which(df_ECity$Events==ID[i])
    Group=df_ECity[Loc_row,]
    Loc=df_ECity$City
    StartT=df_ECity$Begin[Loc_row]
    EndT=df_ECity$End[Loc_row]
    No_days=difftime(EndT, StartT, units = "days")+1
    Damage=df_ECity$`Total Damage, Adjusted ('000 US$)`[Loc_row]
    
    No_subLoc=nrow(Group)
    
    LocLIST=data.frame()
    for(j in 1:No_subLoc){
      subLoc=Group$City[j]
      
      AllLoc_row=which(GDP$City==subLoc)
      subGroup=GDP[AllLoc_row,]
      AllLoc=GDP$County[AllLoc_row]
      AllGDP=GDP$GDP_t[AllLoc_row]
      
      LocLIST_temp=data.frame(AllLoc,AllGDP)
      LocLIST=rbind(LocLIST,LocLIST_temp)
    }
    
    LocLIST=na.omit(LocLIST)
    
    LocLIST$EventID=i
    LocLIST$Start=StartT[1]
    LocLIST$End=EndT[1]
    LocLIST$No_days=No_days[1]
    LocLIST$DamageTotal=Damage[1]
    LocLIST$DamageCounty=LocLIST$AllGDP/sum(LocLIST$AllGDP)*Damage[1]
    
    Event_info=rbind(Event_info,LocLIST) # Produce the Event information
  } 
  
  Event_City=Event_info
} else{
  Judgement_City=0
}

#  ---------------------  County  --------------------- 

No_Event=length(unique(df_ECounty$Events))

if(No_Event>0){
  Judgement_County=1
  ID=unique(df_ECounty$Events)
  
  Event_info=data.frame()
  for(i in 1:No_Event){
    
    Loc_row=which(df_ECounty$Events==ID[i])
    Group=df_ECounty[Loc_row,]
    Loc=df_ECounty$County
    StartT=df_ECounty$Begin[Loc_row]
    EndT=df_ECounty$End[Loc_row]
    No_days=difftime(EndT, StartT, units = "days")+1
    Damage=df_ECounty$`Total Damage, Adjusted ('000 US$)`[Loc_row]
    
    No_subLoc=nrow(Group)
    
    LocLIST=data.frame()
    for(j in 1:No_subLoc){
      subLoc=Group$County[j]
      
      AllLoc_row=which(GDP$County==subLoc)
      subGroup=GDP[AllLoc_row,]
      AllLoc=GDP$County[AllLoc_row]
      AllGDP=GDP$GDP_t[AllLoc_row]
      
      LocLIST_temp=data.frame(AllLoc,AllGDP)
      LocLIST=rbind(LocLIST,LocLIST_temp)
    }
    
    LocLIST=na.omit(LocLIST)
    
    LocLIST$EventID=i
    LocLIST$Start=StartT[1]
    LocLIST$End=EndT[1]
    LocLIST$No_days=No_days[1]
    LocLIST$DamageTotal=Damage[1]
    LocLIST$DamageCounty=LocLIST$AllGDP/sum(LocLIST$AllGDP)*Damage[1]
    
    Event_info=rbind(Event_info,LocLIST) # Produce the Event information
  }
  
  Event_County=Event_info
  
} else{
  Judgement_County=0
}

# ================== Combine all the counties ================== 

Event_info=data.frame()
if(Judgement_Province==1){
  Event_info=rbind(Event_info,Event_Province)
}
if(Judgement_City==1){
  Event_info=rbind(Event_info,Event_City)
}  
if(Judgement_County==1){
  Event_info=rbind(Event_info,Event_County)
}  

Event_info_ALL=Event_info

if(Event_level=="ALL"){
  Event_info=Event_info_ALL
}
if(Event_level=="M"){
  Event_info_M=Event_info[Event_info$DamageCounty<=1000,]
  Event_info=Event_info[Event_info$DamageCounty<=1000,]
  if(nrow(Event_info_M)==0){
    print("No M-level event is found!")
  }
  if(nrow(Event_info_M)>0){
    RatioEvent=round(nrow(Event_info_M)/nrow(Event_info_ALL),2)
    print(c("The proportion of M-level events",Event_type,RatioEvent))
  }
}
if(Event_level=="B"){
  Event_info_B=Event_info[Event_info$DamageCounty>1000,]
  Event_info=Event_info[Event_info$DamageCounty>1000,]
  if(nrow(Event_info_B)==0){
    print("No B-level event is found!")
  }
  if(nrow(Event_info_B)>0){
    RatioEvent=round(nrow(Event_info_B)/nrow(Event_info_ALL),2)
    print(c("The proportion of B-level events",Event_type,RatioEvent))
  }
}

# Remove the NA rows

Event_info=na.omit((Event_info))


##########################################################################
#                 Identify the Neighbour regions
##########################################################################

# Distance matrix

df_Geo=Geo_all

df_Geo=separate(df_Geo, Address, into = c("Latitude", "Longitude"), sep = ",", remove = FALSE)
temp_Geo=na.omit(df_Geo)
df_Geo$NoGeo=seq(1,nrow(df_Geo),by=1)

Matrix_dists=1/as.matrix(dist(cbind(temp_Geo$Latitude,temp_Geo$Longitude)))
diag(Matrix_dists)=0


GeoDist=as.matrix(dist(cbind(temp_Geo$Latitude,temp_Geo$Longitude)))

EventIDALL_Nei=unique(Event_info$EventID)


Event_info_Neighbour=data.frame()
for(i in 1:length(EventIDALL_Nei)){
  
  EventID=EventIDALL_Nei[i]
  
  EventLocal=Event_info[which(Event_info$EventID==EventID),]
  
  AllGDP=0
  Start=EventLocal$Start[1]
  End=EventLocal$End[1]
  No_days=EventLocal$No_days[1]
  DamageTotal=EventLocal$DamageTotal[1]
  DamageCounty=EventLocal$DamageCounty[1]
  CheckEvent=EventLocal$CheckEvent[1]
  
  NeiList=data.frame()
  for(j in 1:nrow(EventLocal)){
    GeoPos=which(df_Geo$County==EventLocal$AllLoc[j])
    
    if(length(GeoPos)==1){
      
      NeiCountyCheck=df_Geo$County[which(GeoDist[, GeoPos] > 1.8 & GeoDist[,GeoPos] <= 2.7)]   # 0.83, 1.8, 2.7
      
      NeiCounty=NeiCountyCheck[!NeiCountyCheck %in% EventLocal$AllLoc]
      
      if(length(NeiCounty>0)){
        
        TempNeiCounty=data.frame(AllLoc=NeiCounty,AllGDP=0)
        TempNeiCounty$EventID=EventID
        TempNeiCounty$Start=EventLocal$Start[1]
        TempNeiCounty$End=EventLocal$End[1]
        TempNeiCounty$No_days=EventLocal$No_days[1]
        TempNeiCounty$DamageTotal=EventLocal$DamageTotal[1]
        TempNeiCounty$DamageCounty=EventLocal$DamageCounty[1]
        TempNeiCounty$CheckEvent=EventLocal$CheckEvent[1]
        
        NeiList=rbind(NeiList,TempNeiCounty)
        
      }
    }
  }
  
  if(nrow(NeiList)>0){
    NeiList_unique=NeiList %>% distinct(AllLoc, .keep_all = TRUE)
    Event_info_Neighbour=rbind(Event_info_Neighbour,NeiList_unique)
  }
}

#write.xlsx(Event_info_Neighbour, '/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Spillover/Src/Disasters_Classified/Event_info_Neighbour_Drought.xlsx', rowNames = TRUE)

# Specifiy which region to study

if(StudyRegion=="Spillover"){
  Event_info=Event_info_Neighbour
}


###################################################################
#                     Pre-Post Analysis
###################################################################

# Default settings
val_Pre=0
val_Warn=10
val_Case=11
val_Post=12
val_After=13

# Input data files
df_Case=Case_all

df_Case$tempC=round(5/9*(df_Case$temp-32),digits=1)

# Manipulation

df_PrepostALL=data.frame()

cli_progress_bar('Processing O3 dataset', total=nrow(Event_info), type='tasks',
                 format="{cli::pb_bar} {pb_percent} @ {cli::pb_current}:{cli::pb_total} ETA: {cli::pb_eta}")

CountyID=0

for(k in 1:nrow(Event_info)){
  
  Start=Event_info$Start[k]
  CaseCounty=Event_info$AllLoc[k]
  CaseDay=Event_info$No_days[k]
  Event_info$CheckEvent=2
  
  matching_row=which(df_Case$datetime==Start & df_Case$County==CaseCounty)
  
  if(length(matching_row)>0){
    Start_row=matching_row[1]
    Event_info$CheckEvent[k]=1
    
    pre=CaseDay # Observation window control
    warn=7
    post=14
    after=14
    
    Pre_row=(Start_row-No_subgroup*(pre+warn)):(Start_row-No_subgroup*(warn)-1)
    Warn_row=(Start_row-No_subgroup*(warn)):(Start_row-1)
    Case_row=Start_row:(Start_row+No_subgroup*(CaseDay)-1)
    Post_row=(Start_row+No_subgroup*(CaseDay)):(Start_row+No_subgroup*(CaseDay+post)-1)
    After_row=(Start_row+No_subgroup*(CaseDay+post)):(Start_row+No_subgroup*(CaseDay+post+after)-1)
    
    df_Prepost_Pre=df_Case[Pre_row,]
    df_Prepost_Pre$case=val_Pre
    
    df_Prepost_Warn=df_Case[Warn_row,]
    df_Prepost_Warn$case=val_Warn
    
    df_Prepost_Case=df_Case[Case_row,]
    df_Prepost_Case$case=val_Case
    
    df_Prepost_Post=df_Case[Post_row,]
    df_Prepost_Post$case=val_Post
    
    df_Prepost_After=df_Case[After_row,]
    df_Prepost_After$case=val_After
    
    
    df_Prepost_temp=rbind(df_Prepost_Pre,df_Prepost_Warn,df_Prepost_Case,df_Prepost_Post,df_Prepost_After)
    df_Prepost_temp$EventID=Event_info$EventID[k]
    
    CountyID=CountyID+1
    df_Prepost_temp$CountyID=CountyID
    df_Prepost_temp$NoDays=CaseDay
    
    df_PrepostALL=rbind(df_PrepostALL,df_Prepost_temp)
  } else{
    Event_info$CheckEvent[k]=0
  }
  cli_progress_update()
}

#saveRDS(df_PrepostALL,'/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result/df_PrepostALL.rds')


#############################################################
#             Pre-post dataset manipulation
#############################################################

# select the observation window of interest

df_PrepostALL$caseTime=2

df_PrepostALL$caseTime[df_PrepostALL$case==val_Pre]=result_pre
df_PrepostALL$caseTime[df_PrepostALL$case==val_Warn]=result_warn
df_PrepostALL$caseTime[df_PrepostALL$case==val_Case]=result_case
df_PrepostALL$caseTime[df_PrepostALL$case==val_Post]=result_post
df_PrepostALL$caseTime[df_PrepostALL$case==val_After]=result_after

df_Prepost_selected=df_PrepostALL[df_PrepostALL$caseTime==0 | df_PrepostALL$caseTime == 1,]

Prepost_Simplified=data.frame()

cli_progress_bar('Processing O3 dataset', total=length(seq(1,nrow(df_Prepost_selected),No_subgroup)), type='tasks',
                 format="{cli::pb_bar} {pb_percent} @ {cli::pb_current}:{cli::pb_total} ETA: {cli::pb_eta}")
for (l in seq(1,nrow(df_Prepost_selected),No_subgroup)){
  
  Group_temp=df_Prepost_selected[l:(l+No_subgroup-1),]
  
  Group_Male=Group_temp[which(Group_temp$Gender=="Male"),"All_cause"]
  Group_Female=Group_temp[which(Group_temp$Gender=="Female"),"All_cause"]
  
  Group_18=Group_temp[which(Group_temp$Age_g=="<18"),"All_cause"]
  Group_18_44=Group_temp[which(Group_temp$Age_g=="18_44"),"All_cause"]
  Group_45_64=Group_temp[which(Group_temp$Age_g=="45_64y"),"All_cause"]
  Group_65=Group_temp[which(Group_temp$Age_g=="65y"),"All_cause"]
  
  Group_ED=Group_temp[which(Group_temp$Transfer=="ED"),"All_cause"]
  Group_otherpatient=Group_temp[which(Group_temp$Transfer=="otherpatient"),"All_cause"]
  Group_Others=Group_temp[which(Group_temp$Transfer=="Others"),"All_cause"]
  
  prepost_temp=cbind(datetime=format(as.POSIXct(Group_temp$datetime[1], origin = "1970-01-01"), "%Y-%m-%d"),County=Group_temp$County[1],
                     Insurance_payment=sum(Group_temp$Insurance_payment),Self_payment=sum(Group_temp$Self_payment),zfy=sum(Group_temp$zfy),
                     All_cause=sum(Group_temp$All_cause),
                     Infectious=sum(Group_temp$Infectious),Neoplasms=sum(Group_temp$Neoplasms),Malnutrition=sum(Group_temp$Malnutrition),
                     Endocrine=sum(Group_temp$Endocrine),Neuro=sum(Group_temp$Neuro),Respiratory=sum(Group_temp$Respiratory),
                     Genitourinary=sum(Group_temp$Genitourinary),Blood=sum(Group_temp$Blood),Injuries=sum(Group_temp$Injuries),
                     CVD=sum(Group_temp$CVD),Mental=sum(Group_temp$Mental),Others=sum(Group_temp$Others),
                     Male=sum(Group_Male),Female=sum(Group_Female),
                     ED=sum(Group_ED),otherpatient=sum(Group_otherpatient),UnknownTransfer=sum(Group_Others),
                     Age18=sum(Group_18),Age18_44=sum(Group_18_44),Age45_64=sum(Group_45_64),Age65=sum(Group_65),
                     NoDays=Group_temp$NoDays[1],caseTime=Group_temp$caseTime[1],EventID=Group_temp$EventID[1],CountyID=Group_temp$CountyID[1])
  
  Prepost_Simplified=rbind(Prepost_Simplified,prepost_temp)
  cli_progress_update()
}

#saveRDS(Prepost_Simplified,'desktop/Prepost_Simplified.rds') # -------------------------------------------------- SAVE Result

# Prepost_Simplified

Prepost_Simplified[,"All_cause"] <- as.numeric(Prepost_Simplified[,"All_cause"])
Prepost_Simplified[,"Infectious"] <- as.numeric(Prepost_Simplified[,"Infectious"])
Prepost_Simplified[,"Neoplasms"] <- as.numeric(Prepost_Simplified[,"Neoplasms"])
Prepost_Simplified[,"Malnutrition"] <- as.numeric(Prepost_Simplified[,"Malnutrition"])
Prepost_Simplified[,"Endocrine"] <- as.numeric(Prepost_Simplified[,"Endocrine"])
Prepost_Simplified[,"Neuro"] <- as.numeric(Prepost_Simplified[,"Neuro"])
Prepost_Simplified[,"Respiratory"] <- as.numeric(Prepost_Simplified[,"Respiratory"])
Prepost_Simplified[,"Genitourinary"] <- as.numeric(Prepost_Simplified[,"Genitourinary"])
Prepost_Simplified[,"Blood"] <- as.numeric(Prepost_Simplified[,"Blood"])
Prepost_Simplified[,"Injuries"] <- as.numeric(Prepost_Simplified[,"Injuries"])
Prepost_Simplified[,"CVD"] <- as.numeric(Prepost_Simplified[,"CVD"])
Prepost_Simplified[,"Mental"] <- as.numeric(Prepost_Simplified[,"Mental"])
Prepost_Simplified[,"Others"] <- as.numeric(Prepost_Simplified[,"Others"])

Prepost_Simplified[,"Insurance_payment"] <- as.numeric(Prepost_Simplified[,"Insurance_payment"])
Prepost_Simplified[,"Self_payment"] <- as.numeric(Prepost_Simplified[,"Self_payment"])
Prepost_Simplified[,"zfy"] <- as.numeric(Prepost_Simplified[,"zfy"])

Prepost_Simplified[,"ED"] <- as.numeric(Prepost_Simplified[,"ED"])
Prepost_Simplified[,"otherpatient"] <- as.numeric(Prepost_Simplified[,"otherpatient"])
Prepost_Simplified[,"UnknownTransfer"] <- as.numeric(Prepost_Simplified[,"UnknownTransfer"])

Prepost_Simplified[,"Age18"] <- as.numeric(Prepost_Simplified[,"Age18"])
Prepost_Simplified[,"Age18_44"] <- as.numeric(Prepost_Simplified[,"Age18_44"])
Prepost_Simplified[,"Age45_64"] <- as.numeric(Prepost_Simplified[,"Age45_64"])
Prepost_Simplified[,"Age65"] <- as.numeric(Prepost_Simplified[,"Age65"])

Prepost_Simplified[,"Male"] <- as.numeric(Prepost_Simplified[,"Male"])
Prepost_Simplified[,"Female"] <- as.numeric(Prepost_Simplified[,"Female"])

Prepost_Simplified[,"NoDays"] <- as.numeric(Prepost_Simplified[,"NoDays"])
Prepost_Simplified[,"caseTime"] <- as.numeric(Prepost_Simplified[,"caseTime"])
Prepost_Simplified[,"EventID"] <- as.numeric(Prepost_Simplified[,"EventID"])
Prepost_Simplified[,"CountyID"] <- as.numeric(Prepost_Simplified[,"CountyID"])

# df_PrepostALL

df_PrepostALL[,"All_cause"] <- as.numeric(df_PrepostALL[,"All_cause"])
df_PrepostALL[,"Infectious"] <- as.numeric(df_PrepostALL[,"Infectious"])
df_PrepostALL[,"Neoplasms"] <- as.numeric(df_PrepostALL[,"Neoplasms"])
df_PrepostALL[,"Malnutrition"] <- as.numeric(df_PrepostALL[,"Malnutrition"])
df_PrepostALL[,"Endocrine"] <- as.numeric(df_PrepostALL[,"Endocrine"])
df_PrepostALL[,"Neuro"] <- as.numeric(df_PrepostALL[,"Neuro"])
df_PrepostALL[,"Respiratory"] <- as.numeric(df_PrepostALL[,"Respiratory"])
df_PrepostALL[,"Genitourinary"] <- as.numeric(df_PrepostALL[,"Genitourinary"])
df_PrepostALL[,"Blood"] <- as.numeric(df_PrepostALL[,"Blood"])
df_PrepostALL[,"Injuries"] <- as.numeric(df_PrepostALL[,"Injuries"])
df_PrepostALL[,"CVD"] <- as.numeric(df_PrepostALL[,"CVD"])
df_PrepostALL[,"Mental"] <- as.numeric(df_PrepostALL[,"Mental"])
df_PrepostALL[,"Others"] <- as.numeric(df_PrepostALL[,"Others"])

df_PrepostALL[,"Insurance_payment"] <- as.numeric(df_PrepostALL[,"Insurance_payment"])
df_PrepostALL[,"Self_payment"] <- as.numeric(df_PrepostALL[,"Self_payment"])
df_PrepostALL[,"zfy"] <- as.numeric(df_PrepostALL[,"zfy"])

#df_PrepostALL[,"Male"] <- as.numeric(df_PrepostALL[,"Male"])
#df_PrepostALL[,"Female"] <- as.numeric(df_PrepostALL[,"Female"])

df_PrepostALL[,"NoDays"] <- as.numeric(df_PrepostALL[,"NoDays"])
df_PrepostALL[,"caseTime"] <- as.numeric(df_PrepostALL[,"caseTime"])
df_PrepostALL[,"EventID"] <- as.numeric(df_PrepostALL[,"EventID"])
df_PrepostALL[,"CountyID"] <- as.numeric(df_PrepostALL[,"CountyID"])

# ================= Resilience filter =================

Prepost_Simplified_Tier0=Prepost_Simplified[which(Prepost_Simplified$County %in% CountyT0),]
Prepost_Simplified_Tier1=Prepost_Simplified[which(Prepost_Simplified$County %in% CountyT1),]
Prepost_Simplified_Tier2=Prepost_Simplified[which(Prepost_Simplified$County %in% CountyT2),]

Prepost_Tier0=df_Prepost_selected[which(df_Prepost_selected$County %in% CountyT0),]
Prepost_Tier1=df_Prepost_selected[which(df_Prepost_selected$County %in% CountyT1),]
Prepost_Tier2=df_Prepost_selected[which(df_Prepost_selected$County %in% CountyT2),]



#############################################################
#                    DID ANALYSIS
#############################################################

# =============== Compute Moran's I ===================

# ---------------- Global Moran's I ---------------------
df_Geo=Geo_all

df_Geo=separate(df_Geo, Address, into = c("Latitude", "Longitude"), sep = ",", remove = FALSE)

temp_Geo=na.omit(df_Geo)

df_Geo$NoGeo=seq(1,nrow(df_Geo),by=1)

Matrix_dists=1/as.matrix(dist(cbind(temp_Geo$Latitude,temp_Geo$Longitude)))
diag(Matrix_dists)=0

MI_All_cause=Moran.I(temp_Geo$All_cause,Matrix_dists)

MI_Infectious=Moran.I(temp_Geo$Infectious,Matrix_dists)
MI_Neoplasms=Moran.I(temp_Geo$Neoplasms,Matrix_dists)
MI_Malnutrition=Moran.I(temp_Geo$Malnutrition,Matrix_dists)
MI_Endocrine=Moran.I(temp_Geo$Endocrine,Matrix_dists)
MI_Neuro=Moran.I(temp_Geo$Neuro,Matrix_dists)
MI_Respiratory=Moran.I(temp_Geo$Respiratory,Matrix_dists)
MI_Genitourinary=Moran.I(temp_Geo$Genitourinary,Matrix_dists)
MI_Blood=Moran.I(temp_Geo$Blood,Matrix_dists)
MI_Injuries=Moran.I(temp_Geo$Injuries,Matrix_dists)
MI_CVD=Moran.I(temp_Geo$CVD,Matrix_dists)
MI_Mental=Moran.I(temp_Geo$Mental,Matrix_dists)
MI_Others=Moran.I(temp_Geo$Others,Matrix_dists)

MI_result=cbind(MI_Infectious,MI_Neoplasms,MI_Malnutrition,MI_Endocrine,MI_Neuro,MI_Respiratory,
                MI_Genitourinary,MI_Blood,MI_Injuries,MI_CVD,MI_Mental,MI_Others,MI_All_cause)
print(MI_result)

# -------------------- Local Moran's I ----------------------

compute_localMI=function(value,Latitude,Longitude){
  MatrixW=1/as.matrix(dist(cbind(Latitude,Longitude)))
  diag(MatrixW)=0
  ave_value=mean(value)
  delta_value=value-ave_value
  m2=sum(delta_value*delta_value)/length(value)
  output=delta_value/m2*(MatrixW %*% delta_value)/rowSums(MatrixW)
}

MI_Lc_Infectious=compute_localMI(temp_Geo$Infectious,temp_Geo$Latitude,temp_Geo$Longitude)
MI_Lc_Neoplasms=compute_localMI(temp_Geo$Neoplasms,temp_Geo$Latitude,temp_Geo$Longitude)
MI_Lc_Malnutrition=compute_localMI(temp_Geo$Malnutrition,temp_Geo$Latitude,temp_Geo$Longitude)
MI_Lc_Endocrine=compute_localMI(temp_Geo$Endocrine,temp_Geo$Latitude,temp_Geo$Longitude)
MI_Lc_Neuro=compute_localMI(temp_Geo$Neuro,temp_Geo$Latitude,temp_Geo$Longitude)
MI_Lc_Respiratory=compute_localMI(temp_Geo$Respiratory,temp_Geo$Latitude,temp_Geo$Longitude)
MI_Lc_Genitourinary=compute_localMI(temp_Geo$Genitourinary,temp_Geo$Latitude,temp_Geo$Longitude)
MI_Lc_Blood=compute_localMI(temp_Geo$Blood,temp_Geo$Latitude,temp_Geo$Longitude)
MI_Lc_Injuries=compute_localMI(temp_Geo$Injuries,temp_Geo$Latitude,temp_Geo$Longitude)
MI_Lc_CVD=compute_localMI(temp_Geo$CVD,temp_Geo$Latitude,temp_Geo$Longitude)
MI_Lc_Mental=compute_localMI(temp_Geo$Mental,temp_Geo$Latitude,temp_Geo$Longitude)
MI_Lc_Others=compute_localMI(temp_Geo$Others,temp_Geo$Latitude,temp_Geo$Longitude)

MI_Lc_All_cause=compute_localMI(temp_Geo$All_cause,temp_Geo$Latitude,temp_Geo$Longitude)

MI_Lc_result=cbind(MI_Lc_Infectious,MI_Lc_Neoplasms,MI_Lc_Malnutrition,MI_Lc_Endocrine,MI_Lc_Neuro,MI_Lc_Respiratory,
                   MI_Lc_Genitourinary,MI_Lc_Blood,MI_Lc_Injuries,MI_Lc_CVD,MI_Lc_Mental,MI_Lc_Others,MI_Lc_All_cause)
print(MI_Lc_result)

# Check spatial autocorrelation
MI_Lc_check=ifelse(MI_Lc_result<0.5,1,0)#---------------------------------------------------------------------------- Input


# ================= Check the physical distance (remove the nearby counties) =================

Check_distance=as.matrix(dist(cbind(temp_Geo$Latitude,temp_Geo$Longitude)))
dist_limit=quantile(Check_distance,0.9)
Check_distance=ifelse(Check_distance>4 & Check_distance<=dist_limit,1,0)# ----------------------- 非临近control地区索引#矩阵#

# ================= Check the County & date (remove the event counties) =================

EventCountyCheck=matrix(1, nrow = length(unique(df_PrepostALL$EventID)), ncol = length(df_Geo$County))
for(ii in 1:length(unique(df_PrepostALL$EventID))){
  EventCounty=unique(df_PrepostALL$County[df_PrepostALL$EventID==ii]) # find the unique counties for each event
  EventCountyCheck[ii,which(df_Geo$County %in% EventCounty)]=0 # tag the event county = 0
}

# ================= Combine all the selection criteria and compute the candidate matrix ===================

Candidate_Find=function(Prepost){          # ----------- Be careful about the input dataset!
  CandidateCountyCheck=matrix(0, nrow = length(unique(Prepost$CountyID)), ncol = length(df_Geo$County))
  for(jj in 1:length(unique(Prepost$CountyID))){
    CandidateCountyCheck[jj,]=MI_Lc_check[,1]*Check_distance[which(df_Geo$County==Prepost$County[jj]),]*EventCountyCheck[Prepost$EventID[jj],]
  }
  return(CandidateCountyCheck)
}


##################################################################
#           Protensity Score Matching (PSM) Approach
##################################################################

# Identify the weather and society condition - prepared for PSM  

df_weather=data.frame(County=df_Case$County,datetime=df_Case$datetime,tempC=df_Case$tempC,tempmax=df_Case$tempmax,humidity=df_Case$humidity,precip=df_Case$precip) # Pre-define the PSM matrix
df_weather=df_weather[seq(1,nrow(df_weather),by=No_subgroup),]  # The daily weather dataset, each row is a date

df_Eco=Eco
df_Eco=rename(df_Eco, 'prate'='性别比\n(女=100)')

# Define the function of PSM_Match


PSM_Match=function(Prepost,Tier){
  
  County_result=data.frame()     # Save for the matched groups info
  
  cli_progress_bar('Processing O3 dataset', total=length(unique(Prepost$CountyID)), type='tasks',
                   format="{cli::pb_bar} {pb_percent} @ {cli::pb_current}:{cli::pb_total} ETA: {cli::pb_eta}")
  
  for(kk in 1:length(unique(Prepost$CountyID))){
    
    ID_all=sort(unique(Prepost$CountyID))
    
    tempPSM=Prepost[Prepost$CountyID==ID_all[kk],] # The caseCounty info
    
    EventRow=nrow(tempPSM)/2+1
    EventDatetime=tempPSM$datetime[EventRow]
    EventDays=tempPSM$NoDays[1]
    EventEnd=tempPSM$datetime[nrow(tempPSM)]
    
    #PSM_matchset=left_join(df_Geo,PSM_dataset,by=c("County"="地区"))
    PSM_matchset=left_join(df_Geo,df_Eco,by=c("County"="地区"))
    
    PSM_matchset$男未=as.numeric(PSM_matchset$男未)# -------------------------------- May need more numeric transfer
    PSM_matchset[is.na(PSM_matchset)]= 0
    
    # Add the ambient environment info for specific day
    PSM_add=data.frame()
    for(ll in 1:nrow(df_Geo)){
      County_Target=df_Geo$County[ll]
      TempC_Target=df_weather$tempC[df_weather$County==County_Target & df_weather$datetime==EventDatetime]
      tempmax_Target=df_weather$tempmax[df_weather$County==County_Target & df_weather$datetime==EventDatetime]
      humidity_Target=df_weather$humidity[df_weather$County==County_Target & df_weather$datetime==EventDatetime]
      precip_Target=df_weather$precip[df_weather$County==County_Target & df_weather$datetime==EventDatetime]
      if(length(TempC_Target)==0){
        TempC_Target=df_weather$tempC[df_weather$County==tempPSM$County[EventRow] & df_weather$datetime==EventDatetime]
        tempmax_Target=df_weather$tempmax[df_weather$County==tempPSM$County[EventRow] & df_weather$datetime==EventDatetime]
        humidity_Target=df_weather$humidity[df_weather$County==tempPSM$County[EventRow] & df_weather$datetime==EventDatetime]
        precip_Target=df_weather$precip[df_weather$County==tempPSM$County[EventRow] & df_weather$datetime==EventDatetime]
      }
      
      County_Match_temp=cbind(County_Target,TempC_Target,tempmax_Target,humidity_Target,precip_Target)
      PSM_add=rbind(PSM_add,County_Match_temp)
    }
    
    PSM_matchset=left_join(PSM_matchset,PSM_add,by=c("County"="County_Target"))
    
    Can_Matrix=Candidate_Find(Prepost)
    Can_Matrix_temp=Can_Matrix[kk,]
    
    PSM_matchset$PSM=Can_Matrix_temp
    
    OrderPSMdata=PSM_matchset[order(PSM_matchset$County != tempPSM$County[1]), ]
    
    # Find the city
    OrderPSMdata$RowNo=1:nrow(OrderPSMdata)
    OrderPSMdata=left_join(OrderPSMdata, GDP[,c("County","City","Province","gdp")], by = "County")
    OrderPSMdata=OrderPSMdata[!duplicated(OrderPSMdata$RowNo), ]
    
    OrderPSMdata$City[is.na(OrderPSMdata$City)]="AVE"
    OrderPSMdata$Province[is.na(OrderPSMdata$Province)]="AVE"
    
    # Fill the form
    OrderPSMdata$gdp=as.numeric(OrderPSMdata$gdp)
    gdp_cleaned=OrderPSMdata[is.finite(OrderPSMdata$gdp), ]
    avg_gdp=mean(gdp_cleaned$gdp)
    OrderPSMdata$gdp[!is.finite(OrderPSMdata$gdp)]=avg_gdp*runif(sum(!is.finite(OrderPSMdata$gdp)), min = Add_lower, max = Add_upper)
    
    # Add the other indicators
    
    # Add the air quality suggested by pollutants
    EventDatetime_TXT=format(floor_date(EventDatetime, "month"), "%Y-%m")
    AirQuality_Temp=AirQuality[AirQuality$Date==EventDatetime_TXT,c("Date","City","AQI","Level","PM25","PM10","SO2","CO","NO2","O3")]
    
    OrderPSMdata=left_join(OrderPSMdata, AirQuality_Temp, by = "City")
    OrderPSMdata$Date=EventDatetime
    
    OrderPSMdata$AQI=as.numeric(OrderPSMdata$AQI)
    OrderPSMdata$PM25=as.numeric(OrderPSMdata$PM25)
    OrderPSMdata$PM10=as.numeric(OrderPSMdata$PM10)
    OrderPSMdata$SO2=as.numeric(OrderPSMdata$SO2)
    OrderPSMdata$CO=as.numeric(OrderPSMdata$CO)
    OrderPSMdata$NO2=as.numeric(OrderPSMdata$NO2)
    OrderPSMdata$O3=as.numeric(OrderPSMdata$O3)
    
    OrderPSMdata$Level[is.na(OrderPSMdata$Level)]="AVE"
    OrderPSMdata$AQI[is.na(OrderPSMdata$AQI)]=round(mean(OrderPSMdata$AQI, na.rm = TRUE))
    OrderPSMdata$PM25[is.na(OrderPSMdata$PM25)]=round(mean(OrderPSMdata$PM25, na.rm = TRUE))
    OrderPSMdata$PM10[is.na(OrderPSMdata$PM10)]=round(mean(OrderPSMdata$PM10, na.rm = TRUE))
    OrderPSMdata$SO2[is.na(OrderPSMdata$SO2)]=round(mean(OrderPSMdata$SO2, na.rm = TRUE))
    OrderPSMdata$CO[is.na(OrderPSMdata$CO)]=round(mean(OrderPSMdata$CO, na.rm = TRUE))
    OrderPSMdata$NO2[is.na(OrderPSMdata$NO2)]=round(mean(OrderPSMdata$NO2, na.rm = TRUE))
    OrderPSMdata$O3[is.na(OrderPSMdata$O3)]=round(mean(OrderPSMdata$O3, na.rm = TRUE))
    
    # Other factors
    
    CityProfile_Add=CityProfile[,c("Chinese","GRP_Growth_Rate","Household_Saving","AgeRatio","EDURatio","GreenRatio")]
    OrderPSMdata=left_join(OrderPSMdata,CityProfile_Add, by = c("City"="Chinese"))
    
    OrderPSMdata$GRP_Growth_Rate=as.numeric(OrderPSMdata$GRP_Growth_Rate)
    OrderPSMdata$Household_Saving=as.numeric(OrderPSMdata$Household_Saving)
    OrderPSMdata$AgeRatio=as.numeric(OrderPSMdata$AgeRatio)
    OrderPSMdata$EDURatio=as.numeric(OrderPSMdata$EDURatio)
    OrderPSMdata$GreenRatio=as.numeric(OrderPSMdata$GreenRatio)
    
    OrderPSMdata$GRP_Growth_Rate[is.na(OrderPSMdata$GRP_Growth_Rate)]=mean(OrderPSMdata$GRP_Growth_Rate, na.rm = TRUE)*runif(sum(!is.finite(OrderPSMdata$GRP_Growth_Rate)), min = Add_lower, max = Add_upper)
    OrderPSMdata$Household_Saving[is.na(OrderPSMdata$Household_Saving)]=mean(OrderPSMdata$Household_Saving, na.rm = TRUE)*runif(sum(!is.finite(OrderPSMdata$Household_Saving)), min = Add_lower, max = Add_upper)
    OrderPSMdata$AgeRatio[is.na(OrderPSMdata$AgeRatio)]=mean(OrderPSMdata$AgeRatio, na.rm = TRUE)*runif(sum(!is.finite(OrderPSMdata$AgeRatio)), min = Add_lower, max = Add_upper)
    OrderPSMdata$EDURatio[is.na(OrderPSMdata$EDURatio)]=mean(OrderPSMdata$EDURatio, na.rm = TRUE)*runif(sum(!is.finite(OrderPSMdata$EDURatio)), min = Add_lower, max = Add_upper)
    OrderPSMdata$GreenRatio[is.na(OrderPSMdata$GreenRatio)]=mean(OrderPSMdata$GreenRatio, na.rm = TRUE)*runif(sum(!is.finite(OrderPSMdata$GreenRatio)), min = Add_lower, max = Add_upper)
    
    # From the resilience framework
    
    ResilienceF_Add=City_Resilience_Domain_Element_Indicator[,c("Population","Dim_Env_B","Dim_Hea_A")]   # Population - population density, Dim_Env_B - Transportation system, Dim_Hea_A - Care system
    ResilienceF_Add$Chinese=CityProfile$Chinese
    OrderPSMdata=left_join(OrderPSMdata,ResilienceF_Add, by = c("City"="Chinese"))
    
    OrderPSMdata$Population=as.numeric(OrderPSMdata$Population)
    OrderPSMdata$Dim_Env_B=as.numeric(OrderPSMdata$Dim_Env_B)
    OrderPSMdata$Dim_Hea_A=as.numeric(OrderPSMdata$Dim_Hea_A)
    
    OrderPSMdata$Population[is.na(OrderPSMdata$Population)]=mean(OrderPSMdata$Population, na.rm = TRUE)*runif(sum(!is.finite(OrderPSMdata$Population)), min = Add_lower, max = Add_upper)
    OrderPSMdata$Dim_Env_B[is.na(OrderPSMdata$Dim_Env_B)]=mean(OrderPSMdata$Dim_Env_B, na.rm = TRUE)*runif(sum(!is.finite(OrderPSMdata$Dim_Env_B)), min = Add_lower, max = Add_upper)
    OrderPSMdata$Dim_Hea_A[is.na(OrderPSMdata$Dim_Hea_A)]=mean(OrderPSMdata$Dim_Hea_A, na.rm = TRUE)*runif(sum(!is.finite(OrderPSMdata$Dim_Hea_A)), min = Add_lower, max = Add_upper)
    
    # Manipulations
    
    OrderPSMdata$Health_Cost=OrderPSMdata$zfy/OrderPSMdata$All_cause
    OrderPSMdata$Pop_total=OrderPSMdata$`合 计`
    
    # Tags: affected=1, control=0
    
    AffectedC=unique(Prepost$County)
    
    OrderPSMdata$Tag=0
    OrderPSMdata$Tag[which(OrderPSMdata$County %in% AffectedC)]=1
    
    OrderPSMdata$TempC_Target=as.numeric(OrderPSMdata$TempC_Target)
    OrderPSMdata$tempmax_Target=as.numeric(OrderPSMdata$tempmax_Target)
    OrderPSMdata$humidity_Target=as.numeric(OrderPSMdata$humidity_Target)
    OrderPSMdata$precip_Target=as.numeric(OrderPSMdata$precip_Target)
    
    #================================== PSM ==================================
    
    # --------------- NOTE ------------------------------------------------------------------------------------------
    # Confounding factors: gdp, GRP_Growth_Rate, Household_Saving,
    #                      Population, urban, prate, AgeRatio, EDURatio, work_rate,     # Population - population density
    #                      GreenRatio, Dim_Env_B, AQI, PM25, PM10, SO2, CO, NO2, O3, TempC_Target, tempmax_Target, humidity_Target,precip_Target
    #                      Dim_Hea_A, Health_Cost
    #
    # Note: variables are already normalized
    #
    #     Tag ~ gdp+GRP_Growth_Rate+Household_Saving+  
    #           Population+urban+prate+AgeRatio+EDURatio+work_rate+  
    #           GreenRatio+Dim_Env_B+PM25+PM10+SO2+CO+NO2+O3+TempC_Target+tempmax_Target+humidity_Target+precip_Target+
    #           Dim_Hea_A+Health_Cost
    
    #     Storm:           gdp+GRP_Growth_Rate+Household_Saving+  Population+urban+prate+AgeRatio+EDURatio+work_rate+  GreenRatio+Dim_Env_B+  Dim_Hea_A+Health_Cost
    #     Flood:           gdp+GRP_Growth_Rate+Household_Saving+  Population+urban+prate+AgeRatio+EDURatio+work_rate+  GreenRatio+Dim_Env_B+  Dim_Hea_A+Health_Cost
    #     Cyclone:         gdp+GRP_Growth_Rate+Household_Saving+  Population+urban+prate+AgeRatio+EDURatio+work_rate+  GreenRatio+Dim_Env_B+  Dim_Hea_A+Health_Cost
    #     WinterStorm:     gdp+GRP_Growth_Rate+Household_Saving+  Population+urban+prate+AgeRatio+EDURatio+work_rate+  GreenRatio+Dim_Env_B+  Dim_Hea_A+Health_Cost
    #     Sand:            gdp+GRP_Growth_Rate+Household_Saving+    Population+urban+prate+AgeRatio+work_rate+EDURatio   +tempmax_Target  +Dim_Hea_A
    #     Drought:         gdp+GRP_Growth_Rate+Household_Saving+  Population+urban+prate+AgeRatio+EDURatio+work_rate+  Dim_Env_B+PM25+PM10+SO2+CO+NO2+O3+   Dim_Hea_A+Health_Cost
    #     Heatwave:        gdp+GRP_Growth_Rate+Household_Saving+  Population+urban+prate+AgeRatio+EDURatio+work_rate+  GreenRatio+Dim_Env_B+PM25+PM10+SO2+CO+NO2+O3+humidity_Target+precip_Target   +Dim_Hea_A+Health_Cost
    
    # ---------------------------------------------------------------------------------------------------------
    
    m = matchit(Tag~ gdp+GRP_Growth_Rate+Household_Saving+  Population+urban+prate+AgeRatio+EDURatio+work_rate+  GreenRatio+Dim_Env_B+  Dim_Hea_A+Health_Cost, 
                data=OrderPSMdata,
                distance='logit',
                method='nearest',
                replace=TRUE,
                ratio=No_controlCounty)
    
    # ========================================================================
    ID=ID_all[kk]
    CaseSeq=c(1,rep(0,No_controlCounty))
    
    Group=c("1",m$match.matrix[1,])
    Group[1]=OrderPSMdata$County[as.numeric(Group[1])]
    Group[2]=OrderPSMdata$County[as.numeric(Group[2])]
    Group[3]=OrderPSMdata$County[as.numeric(Group[3])]
    Group[4]=OrderPSMdata$County[as.numeric(Group[4])]
    Group[5]=OrderPSMdata$County[as.numeric(Group[5])]  
    
    temp_output=data.frame(CountyID=ID,County=Group,NoDays=as.numeric(EventDays),datetime=EventDatetime,start=EventDatetime,end=EventEnd,CaseCounty=CaseSeq)
    
    temp_add=OrderPSMdata[as.numeric(c("1",m$match.matrix[1,])),c("County","gdp", "GRP_Growth_Rate", "Household_Saving",
                                                                  "Population", "urban", "prate", "AgeRatio", "EDURatio", "work_rate",
                                                                  "GreenRatio", "Dim_Env_B", "AQI", "PM25", "PM10", "SO2", "CO", "NO2", "O3", "TempC_Target", "tempmax_Target", "humidity_Target","precip_Target",
                                                                  "Dim_Hea_A", "Health_Cost")]
    temp_output=cbind(temp_output,temp_add)
    County_result=rbind(County_result,temp_output)
    
    cli_progress_update()
  }
  return(County_result)
}

# Apply the PSM_Match function to the src data frames

GroupT0=data.frame()
GroupT1=data.frame()
GroupT2=data.frame()

GroupT1=PSM_Match(Prepost_Tier1,CountyT1)
GroupT2=PSM_Match(Prepost_Tier2,CountyT2)

GroupT0=rbind(GroupT1,GroupT2)

# Build the fill function

Fill_control=function(GroupT){
  GroupT$SeqID=1:nrow(GroupT)
  Control_List=GroupT[GroupT$CaseCounty == 0,]
  
  GroupT$IHDEvent=2 # -------------------------------------------------------------------------!
  
  df_ControlALL=data.frame()
  
  cli_progress_bar('Processing O3 dataset', total=nrow(Control_List), type='tasks',
                   format="{cli::pb_bar} {pb_percent} @ {cli::pb_current}:{cli::pb_total} ETA: {cli::pb_eta}")
  
  for(i in 1:nrow(Control_List)){
    Control_temp=Control_List[i,]
    Period=Control_temp$NoDays # ------ Period=CaseDay
    
    pre=Period
    post=14
    after=14
    
    matching_row=which(df_Case$datetime==Control_temp$start & df_Case$County==Control_temp$County)
    
    if(length(matching_row)>0){
      
      Start_row=matching_row[1]
      
      Pre_row=(Start_row-No_subgroup*(pre+warn)):(Start_row-No_subgroup*(warn)-1)
      Warn_row=(Start_row-No_subgroup*(warn)):(Start_row-1)
      Case_row=Start_row:(Start_row+No_subgroup*(Period)-1)
      Post_row=(Start_row+No_subgroup*(Period)):(Start_row+No_subgroup*(Period+post)-1)
      After_row=(Start_row+No_subgroup*(Period+post)):(Start_row+No_subgroup*(Period+post+after)-1)
      
      df_Control_Pre=df_Case[Pre_row,]
      df_Control_Pre$case=val_Pre
      
      df_Control_Warn=df_Case[Warn_row,]
      df_Control_Warn$case=val_Warn
      
      df_Control_Case=df_Case[Case_row,]
      df_Control_Case$case=val_Case
      
      df_Control_Post=df_Case[Post_row,]
      df_Control_Post$case=val_Post
      
      df_Control_After=df_Case[After_row,]
      df_Control_After$case=val_After
      
      df_Control_temp=rbind(df_Control_Pre,df_Control_Warn,df_Control_Case,df_Control_Post,df_Control_After)
      
      df_Control_temp$CountyID=Control_temp$CountyID[1]
      df_Control_temp$NoDays=Period
      
      df_ControlALL=rbind(df_ControlALL,df_Control_temp)
    }
    
    cli_progress_update()
  }
  
  return(df_ControlALL)
}

Control_T0_ALL=data.frame()
Control_T1_ALL=data.frame()
Control_T2_ALL=data.frame()

#Control_T0_ALL=Fill_control(GroupT0)
Control_T1_ALL=Fill_control(GroupT1)# The full info of control counties
Control_T2_ALL=Fill_control(GroupT2)

Control_T0_ALL=rbind(Control_T1_ALL,Control_T2_ALL)


# select the observation window of interest

Control_select=function(df_ControlALL){
  df_ControlALL$caseTime=2
  
  df_ControlALL$caseTime[df_ControlALL$case==val_Pre]=result_pre
  df_ControlALL$caseTime[df_ControlALL$case==val_Warn]=result_warn
  df_ControlALL$caseTime[df_ControlALL$case==val_Case]=result_case
  df_ControlALL$caseTime[df_ControlALL$case==val_Post]=result_post
  df_ControlALL$caseTime[df_ControlALL$case==val_After]=result_after
  
  df_Control_selected=df_ControlALL[df_ControlALL$caseTime==0 | df_ControlALL$caseTime == 1,]
  
  df_Control_selected$caseCounty=0
  
  return(df_Control_selected)
}

Control_Tier0=Control_select(Control_T0_ALL)
Control_Tier1=Control_select(Control_T1_ALL)
Control_Tier2=Control_select(Control_T2_ALL)


df_Prepost_selected$caseCounty=1

Prepost_Tier0=df_Prepost_selected[which(df_Prepost_selected$County %in% CountyT0),]
Prepost_Tier1=df_Prepost_selected[which(df_Prepost_selected$County %in% CountyT1),]
Prepost_Tier2=df_Prepost_selected[which(df_Prepost_selected$County %in% CountyT2),]


DID_Tier0=rbind(subset(Prepost_Tier0,select=-EventID),Control_Tier0)
DID_Tier1=rbind(subset(Prepost_Tier1,select=-EventID),Control_Tier1)
DID_Tier2=rbind(subset(Prepost_Tier2,select=-EventID),Control_Tier2)

DID_Tier0[is.na(DID_Tier0)]=0
DID_Tier1[is.na(DID_Tier1)]=0
DID_Tier2[is.na(DID_Tier2)]=0


#####################################################################
#                        Post Processing
#####################################################################

# ======================= Brief overview ======================

No_County_level_events=length(unique(Prepost_Tier0$CountyID))
No_duration_days=round(nrow(Prepost_Tier0)/No_subgroup/2/No_County_level_events,2)

print(paste("No. of county-specific ***",Event_level,"*** level ***",Event_type,"*** events =",No_County_level_events))
print(paste("No. of averaged duration days =",No_duration_days))



# ============================= SAVE ==============================

FilePos="~/Library/Mobile Documents/com~apple~CloudDocs/Project/Spillover/Result/"
ModelType="/DID_"
ObsWindow='Enduring'

saveRDS(DID_Tier0,paste(FilePos,Event_type,ModelType,"T0",ObsWindow,".rds",sep = ""))
saveRDS(DID_Tier1,paste(FilePos,Event_type,ModelType,"T1",ObsWindow,".rds",sep = ""))
saveRDS(DID_Tier2,paste(FilePos,Event_type,ModelType,"T2",ObsWindow,".rds",sep = ""))

ModelType="/Prepost_"

saveRDS(Prepost_Tier0,paste(FilePos,Event_type,ModelType,"T0",ObsWindow,".rds",sep = ""))
saveRDS(Prepost_Tier1,paste(FilePos,Event_type,ModelType,"T1",ObsWindow,".rds",sep = ""))
saveRDS(Prepost_Tier2,paste(FilePos,Event_type,ModelType,"T2",ObsWindow,".rds",sep = ""))

# Save PSM match results
saveRDS(GroupT0,paste(FilePos,Event_type,"/GroupT0.rds",sep = ""))
saveRDS(GroupT1,paste(FilePos,Event_type,"/GroupT1.rds",sep = ""))
saveRDS(GroupT2,paste(FilePos,Event_type,"/GroupT2.rds",sep = ""))
