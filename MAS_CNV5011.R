#=== === === ####
#Code for MAS_CNV5011

#=== === === 


MSTMassey <- MasseyOverallResults
library(dplyr)
library("psych")
library(tidyr)

MSTMassey <- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(Locator %in% c("MAS_OSD0979", "MAS_OSD3966","MAS_CNV5011", "MAS_CNV5011_S", "MAS_CNV5011_SE", "MAS_CB3070", "MAS_OSD0999", "MAS_OSD0998", "MAS_18PLS_1", "MAS_18PLS_2"))

#Lab results####

MasseyFollowResults5011 <- MSTMassey %>%
  filter(between (Date, as.Date("2024-1-01"), as.Date("2024-12-30"))) %>%
  filter(SampleType == "Lab EC") %>%
  filter(!Replicate %in% c("Y2")) %>%
  rename(LabEC = Result) %>%
  select(Locator, Date, LabEC)

View(MasseyFollowResults5011)

#R card results####

MasseyFollowRCard5011 <- MSTMassey %>%
  filter(between (Date, as.Date("2024-1-01"), as.Date("2024-12-30"))) %>%
  filter(SampleType == "R-Card") %>%
  filter(!Replicate %in% c("Y2")) %>%
  rename(RCardEC = Result) %>%
  select(Locator, Date, RCardEC)

View(MasseyFollowRCard5011)

#Biomarker Results - Dog####
MasseyFollowDog5011 <- MSTMassey %>%
  filter(between (Date, as.Date("2024-1-01"), as.Date("2024-12-30"))) %>%
  filter(SampleType == "Dog1-Bac") %>%
  filter(!Replicate %in% c("Y2")) %>%
  mutate(Result = as.numeric(Result)) %>%
  rename(Dog_biomarker = Result) %>%
  select(Locator, Date, Dog_biomarker)

options(scipen = 999)

View(MasseyFollowDog5011)

#Biomarker Results - Human####

MasseyFollowHu5011<- MSTMassey %>%
  filter(between (Date, as.Date("2024-1-01"), as.Date("2024-12-30"))) %>%
  filter(SampleType == "Hu2-Bac") %>%
  filter(!Replicate %in% c("Y2")) %>%
  mutate(Result = as.numeric(Result)) %>%
  rename(Hu_biomarker = Result) %>%
  select(Locator, Date, Hu_biomarker)

options(scipen = 999)

View(MasseyFollowHu5011)

#Observation only ####

MasseyFollowObs5011 <- MSTMassey %>%
  filter(between (Date, as.Date("2024-1-01"), as.Date("2024-12-30"))) %>%
  filter(SampleType == "Observation") %>%
  mutate(Result = 1) %>%
  rename(nObs = Result) %>%
  select(Locator, Date, nObs) %>%
  arrange(Locator)

View(MasseyFollowObs5011)


#Join all Together ####
MasseyFollowAll5011 <- MasseyFollowResults5011 %>%
  full_join(MasseyFollowRCard5011, by = c("Locator", "Date")) %>%
  full_join(MasseyFollowDog5011, by = c("Locator", "Date")) %>%
  full_join(MasseyFollowHu5011, by = c("Locator", "Date")) %>%
  full_join(MasseyFollowObs5011, by = c("Locator", "Date")) %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), ~replace_na(., "NS"))) %>%
  mutate(nObs = na_if(nObs, "NS"))

View(MasseyFollowAll5011)
MasseyFollowAll5011 <-MasseyFollowAll5011 %>% arrange(Locator)

downstreamupstream<-c("MAS_OSD0979", "MAS_OSD3966","MAS_CNV5011", "MAS_CNV5011_S", "MAS_CNV5011_SE", "MAS_CB3070", "MAS_OSD0999", "MAS_OSD0998", "MAS_18PLS_1", "MAS_18PLS_2")

MasseyFollowAll5011 <-MasseyFollowAll5011 %>%
  arrange(Date) %>%
  mutate(Locator = factor(Locator, levels = downstreamupstream)) %>%
  arrange(Locator)

View(MasseyFollowAll5011)

write.csv(MasseyFollowAll5011, "MasseyFollowAll5011.csv")
