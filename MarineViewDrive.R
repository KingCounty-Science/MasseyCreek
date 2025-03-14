#=== === === ####
#Code for marine view drive S 

#=== === === 


MSTMassey <- MasseyOverallResults
library(dplyr)
library("psych")
library(tidyr)

MSTMassey <- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(Locator %in% c("MAS_CNV2603_CB", "MAS_CNV2603", "MAS_CB2241", "MAS_CB2240", "MAS_CB2242", "MAS_CNV2607","MAS_CB2245", "MAS_CB2243", "MAS_CB2390", "MAS_CB2244", "MAS_CB2249", "MAS_CB2250", "MAS_CB2251", "MAS_CB2248", "MAS_CB2247", "MAS_CNV2616","MAS_CNV2614", "MAS_CB4886", "MAS_CB2395","MAS_CB2399","MAS_CB2397", "MAS_CB2402","MAS_CB2400", "MAS_CB2252", "MAS_CB2405","MAS_CB2403", "MAS_CB2257", "MAS_CB2253", "MAS_CNV2637", "MAS_CNV2637_UP", "MAS_CNV6447","MAS_CNV2638_UP", "MAS_CNV9839","MAS_CNV2639", "MAS_CB2316"))

#Lab results####

MasseyFollowResultsMVD <- MSTMassey %>%
  filter(between (Date, as.Date("2024-1-01"), as.Date("2024-12-30"))) %>%
  filter(SampleType == "Lab EC") %>%
  filter(!Replicate %in% c("Y2")) %>%
  rename(LabEC = Result) %>%
  select(Locator, Date, LabEC)

View(MasseyFollowResults)

#R card results####

MasseyFollowRCardMVD <- MSTMassey %>%
  filter(between (Date, as.Date("2024-1-01"), as.Date("2024-12-30"))) %>%
  filter(SampleType == "R-Card") %>%
  filter(!Replicate %in% c("Y2")) %>%
  rename(RCardEC = Result) %>%
  select(Locator, Date, RCardEC)

View(MasseyFollowRCard )

#Biomarker Results - Dog####
MasseyFollowDogMVD <- MSTMassey %>%
  filter(between (Date, as.Date("2024-1-01"), as.Date("2024-12-30"))) %>%
  filter(SampleType == "Dog1-Bac") %>%
  filter(!Replicate %in% c("Y2")) %>%
  mutate(Result = as.numeric(Result)) %>%
  rename(Dog_biomarker = Result) %>%
  select(Locator, Date, Dog_biomarker)

options(scipen = 999)

View(MasseyFollowDog)

#Biomarker Results - Human####

MasseyFollowHuMVD<- MSTMassey %>%
  filter(between (Date, as.Date("2024-1-01"), as.Date("2024-12-30"))) %>%
  filter(SampleType == "Hu2-Bac") %>%
  filter(!Replicate %in% c("Y2")) %>%
  mutate(Result = as.numeric(Result)) %>%
  rename(Hu_biomarker = Result) %>%
  select(Locator, Date, Hu_biomarker)

options(scipen = 999)

View(MasseyFollowHu)

#Observation only ####

MasseyFollowObsMVD <- MSTMassey %>%
  filter(between (Date, as.Date("2024-1-01"), as.Date("2024-12-30"))) %>%
  filter(SampleType == "Observation") %>%
  mutate(Result = 1) %>%
  rename(nObs = Result) %>%
  select(Locator, Date, nObs) %>%
  arrange(Locator)

View(MasseyFollowObsMVD)


#Join all Together ####
MasseyFollowAllMVD <- MasseyFollowResultsMVD %>%
  full_join(MasseyFollowRCardMVD, by = c("Locator", "Date")) %>%
  full_join(MasseyFollowDogMVD, by = c("Locator", "Date")) %>%
  full_join(MasseyFollowHuMVD, by = c("Locator", "Date")) %>%
  full_join(MasseyFollowObsMVD, by = c("Locator", "Date")) %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), ~replace_na(., "NS"))) %>%
  mutate(nObs = na_if(nObs, "NS"))

View(MasseyFollowAllMVD)
MasseyFollowAllMVD <-MasseyFollowAllMVD %>% arrange(Locator)

downstreamupstream<-c("MAS_CNV2603_CB", "MAS_CNV2603", "MAS_CB2241", "MAS_CB2240", "MAS_CB2242", "MAS_CNV2607","MAS_CB2245", "MAS_CB2243", "MAS_CB2390", "MAS_CB2244", "MAS_CB2249", "MAS_CB2250", "MAS_CB2251", "MAS_CB2248", "MAS_CB2247", "MAS_CNV2616","MAS_CNV2614", "MAS_CB4886", "MAS_CB2395","MAS_CB2399","MAS_CB2397", "MAS_CB2402","MAS_CB2400", "MAS_CB2252", "MAS_CB2405","MAS_CB2403", "MAS_CB2257", "MAS_CB2253", "MAS_CNV2637", "MAS_CNV2637_UP", "MAS_CNV6447","MAS_CNV2638_UP", "MAS_CNV9839","MAS_CNV2639", "MAS_CB2316")

MasseyFollowAllMVD <-MasseyFollowAllMVD %>%
  arrange(Date) %>%
  mutate(Locator = factor(Locator, levels = downstreamupstream)) %>%
  arrange(Locator)

View(MasseyFollowAllMVD)

write.csv(MasseyFollowAllMVD, "MasseyFollowAllMVD.csv")
