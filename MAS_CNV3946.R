#=== === === ####
#Code for MAS_CNV3946

#=== === === 


MSTMassey <- MasseyOverallResults
library(dplyr)
library("psych")
library(tidyr)

MSTMassey <- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(Locator %in% c("MAS_CNV3946", "MAS_OSD0977", "MAS_OSD0953", "MAS_OSD0976", "MAS_CNV3945"))

#Lab results####

MasseyFollowResults3946 <- MSTMassey %>%
  filter(between (Date, as.Date("2024-1-01"), as.Date("2024-12-30"))) %>%
  filter(SampleType == "Lab EC") %>%
  filter(!Replicate %in% c("Y2")) %>%
  rename(LabEC = Result) %>%
  select(Locator, Date, LabEC)

View(MasseyFollowResults3946)

#R card results####

MasseyFollowRCard3946 <- MSTMassey %>%
  filter(between (Date, as.Date("2024-1-01"), as.Date("2024-12-30"))) %>%
  filter(SampleType == "R-Card") %>%
  filter(!Replicate %in% c("Y2")) %>%
  rename(RCardEC = Result) %>%
  select(Locator, Date, RCardEC)

View(MasseyFollowRCard3946)

#Biomarker Results - Dog####
MasseyFollowDog3946 <- MSTMassey %>%
  filter(between (Date, as.Date("2024-1-01"), as.Date("2024-12-30"))) %>%
  filter(SampleType == "Dog1-Bac") %>%
  filter(!Replicate %in% c("Y2")) %>%
  mutate(Result = as.numeric(Result)) %>%
  rename(Dog_biomarker = Result) %>%
  select(Locator, Date, Dog_biomarker)

options(scipen = 999)

View(MasseyFollowDog3946)

#Biomarker Results - Human####

MasseyFollowHu3946<- MSTMassey %>%
  filter(between (Date, as.Date("2024-1-01"), as.Date("2024-12-30"))) %>%
  filter(SampleType == "Hu2-Bac") %>%
  filter(!Replicate %in% c("Y2")) %>%
  mutate(Result = as.numeric(Result)) %>%
  rename(Hu_biomarker = Result) %>%
  select(Locator, Date, Hu_biomarker)

options(scipen = 999)

View(MasseyFollowHu3946)

#Observation only ####

MasseyFollowObs3946 <- MSTMassey %>%
  filter(between (Date, as.Date("2024-1-01"), as.Date("2024-12-30"))) %>%
  filter(SampleType == "Observation") %>%
  mutate(Result = 1) %>%
  rename(nObs = Result) %>%
  select(Locator, Date, nObs) %>%
  arrange(Locator)

View(MasseyFollowObs3946)


#Join all Together ####
MasseyFollowAll3946 <- MasseyFollowResults3946 %>%
  full_join(MasseyFollowRCard3946, by = c("Locator", "Date")) %>%
  full_join(MasseyFollowDog3946, by = c("Locator", "Date")) %>%
  full_join(MasseyFollowHu3946, by = c("Locator", "Date")) %>%
  full_join(MasseyFollowObs3946, by = c("Locator", "Date")) %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), ~replace_na(., "NS"))) %>%
  mutate(nObs = na_if(nObs, "NS"))

View(MasseyFollowAll3946)
MasseyFollowAll3946 <-MasseyFollowAll3946 %>% arrange(Locator)

downstreamupstream<-c("MAS_CNV3946", "MAS_OSD0977", "MAS_OSD0953", "MAS_OSD0976", "MAS_CNV3945")

MasseyFollowAll3946 <-MasseyFollowAll3946 %>%
  arrange(Date) %>%
  mutate(Locator = factor(Locator, levels = downstreamupstream)) %>%
  arrange(Locator)

View(MasseyFollowAll3946)

write.csv(MasseyFollowAll3946, "MasseyFollowAll3946.csv")
