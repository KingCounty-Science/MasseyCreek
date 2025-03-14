#=== === === ####
#Code for MAS_CNV0437

#=== === === 


MSTMassey <- MasseyOverallResults
library(dplyr)
library("psych")
library(tidyr)

MSTMassey <- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(Locator %in% c("MAS_CNV0437"))

#Lab results####

MasseyFollowResults0437<- MSTMassey %>%
  filter(between (Date, as.Date("2024-1-01"), as.Date("2024-12-30"))) %>%
  filter(SampleType == "Lab EC") %>%
  filter(!Replicate %in% c("Y2")) %>%
  rename(LabEC = Result) %>%
  select(Locator, Date, LabEC)

View(MasseyFollowResults0437)

#R card results####

MasseyFollowRCard0437 <- MSTMassey %>%
  filter(between (Date, as.Date("2024-1-01"), as.Date("2024-12-30"))) %>%
  filter(SampleType == "R-Card") %>%
  filter(!Replicate %in% c("Y2")) %>%
  rename(RCardEC = Result) %>%
  select(Locator, Date, RCardEC)

View(MasseyFollowRCard0437)

#Biomarker Results - Dog####
MasseyFollowDog0437 <- MSTMassey %>%
  filter(between (Date, as.Date("2024-1-01"), as.Date("2024-12-30"))) %>%
  filter(SampleType == "Dog1-Bac") %>%
  filter(!Replicate %in% c("Y2")) %>%
  mutate(Result = as.numeric(Result)) %>%
  rename(Dog_biomarker = Result) %>%
  select(Locator, Date, Dog_biomarker)

options(scipen = 999)

View(MasseyFollowDog0437)

#Biomarker Results - Human####

MasseyFollowHu0437<- MSTMassey %>%
  filter(between (Date, as.Date("2024-1-01"), as.Date("2024-12-30"))) %>%
  filter(SampleType == "Hu2-Bac") %>%
  filter(!Replicate %in% c("Y2")) %>%
  mutate(Result = as.numeric(Result)) %>%
  rename(Hu_biomarker = Result) %>%
  select(Locator, Date, Hu_biomarker)

options(scipen = 999)

View(MasseyFollowHu0437)

#Observation only ####

MasseyFollowObs0437 <- MSTMassey %>%
  filter(between (Date, as.Date("2024-1-01"), as.Date("2024-12-30"))) %>%
  filter(SampleType == "Observation") %>%
  mutate(Result = 1) %>%
  rename(nObs = Result) %>%
  select(Locator, Date, nObs) %>%
  arrange(Locator)

View(MasseyFollowObs0437)


#Join all Together ####
MasseyFollowAll0437 <- MasseyFollowResults0437 %>%
  full_join(MasseyFollowRCard0437, by = c("Locator", "Date")) %>%
  full_join(MasseyFollowDog0437, by = c("Locator", "Date")) %>%
  full_join(MasseyFollowHu0437, by = c("Locator", "Date")) %>%
  full_join(MasseyFollowObs0437, by = c("Locator", "Date")) %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), ~replace_na(., "NS"))) %>%
  mutate(nObs = na_if(nObs, "NS"))

View(MasseyFollowAll0437)
MasseyFollowAll0437 <-MasseyFollowAll0437 %>% arrange(Locator)

downstreamupstream<-c("MAS_CNV0437")

MasseyFollowAll0437<-MasseyFollowAll0437 %>%
  arrange(Date) %>%
  mutate(Locator = factor(Locator, levels = downstreamupstream)) %>%
  arrange(Locator)

View(MasseyFollowAll0437)

write.csv(MasseyFollowAll0437, "MasseyFollowAll0437.csv")
