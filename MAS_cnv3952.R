#=== === === ####
#Code for MAS_CNV3952

#=== === === 


MSTMassey <- MasseyOverallResults
library(dplyr)
library("psych")
library(tidyr)

MSTMassey <- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(Locator %in% c("MAS_CNV3952", "MAS_CNV3964", "MAS_CNV3950", "MAS_CNV3962", "MAS_OSD0775"))

#Lab results####

MasseyFollowResults3952 <- MSTMassey %>%
  filter(between (Date, as.Date("2024-1-01"), as.Date("2024-12-30"))) %>%
  filter(SampleType == "Lab EC") %>%
  filter(!Replicate %in% c("Y2")) %>%
  rename(LabEC = Result) %>%
  select(Locator, Date, LabEC)

View(MasseyFollowResults3952)

#R card results####

MasseyFollowRCard3952 <- MSTMassey %>%
  filter(between (Date, as.Date("2024-1-01"), as.Date("2024-12-30"))) %>%
  filter(SampleType == "R-Card") %>%
  filter(!Replicate %in% c("Y2")) %>%
  rename(RCardEC = Result) %>%
  select(Locator, Date, RCardEC)

View(MasseyFollowRCard3952)

#Biomarker Results - Dog####
MasseyFollowDog3952 <- MSTMassey %>%
  filter(between (Date, as.Date("2024-1-01"), as.Date("2024-12-30"))) %>%
  filter(SampleType == "Dog1-Bac") %>%
  filter(!Replicate %in% c("Y2")) %>%
  mutate(Result = as.numeric(Result)) %>%
  rename(Dog_biomarker = Result) %>%
  select(Locator, Date, Dog_biomarker)

options(scipen = 999)

View(MasseyFollowDog3952)

#Biomarker Results - Human####

MasseyFollowHu3952<- MSTMassey %>%
  filter(between (Date, as.Date("2024-1-01"), as.Date("2024-12-30"))) %>%
  filter(SampleType == "Hu2-Bac") %>%
  filter(!Replicate %in% c("Y2")) %>%
  mutate(Result = as.numeric(Result)) %>%
  rename(Hu_biomarker = Result) %>%
  select(Locator, Date, Hu_biomarker)

options(scipen = 999)

View(MasseyFollowHu3952)

#Observation only ####

MasseyFollowObs3952 <- MSTMassey %>%
  filter(between (Date, as.Date("2024-1-01"), as.Date("2024-12-30"))) %>%
  filter(SampleType == "Observation") %>%
  mutate(Result = 1) %>%
  rename(nObs = Result) %>%
  select(Locator, Date, nObs) %>%
  arrange(Locator)

View(MasseyFollowObs3952)


#Join all Together ####
MasseyFollowAll3952 <- MasseyFollowResults3952 %>%
  full_join(MasseyFollowRCard3952, by = c("Locator", "Date")) %>%
  full_join(MasseyFollowDog3952, by = c("Locator", "Date")) %>%
  full_join(MasseyFollowHu3952, by = c("Locator", "Date")) %>%
  full_join(MasseyFollowObs3952, by = c("Locator", "Date")) %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), ~replace_na(., "NS"))) %>%
  mutate(nObs = na_if(nObs, "NS"))

View(MasseyFollowAll3952)
MasseyFollowAll3952 <-MasseyFollowAll3952 %>% arrange(Locator)

downstreamupstream<-c("MAS_CNV3952", "MAS_CNV3964", "MAS_CNV3950", "MAS_CNV3962", "MAS_OSD0775")

MasseyFollowAll3952 <-MasseyFollowAll3952 %>%
  arrange(Date) %>%
  mutate(Locator = factor(Locator, levels = downstreamupstream)) %>%
  arrange(Locator)

View(MasseyFollowAll3952)

write.csv(MasseyFollowAll3952, "MasseyFollowAll3952.csv")
