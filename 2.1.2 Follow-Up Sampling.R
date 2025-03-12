

MSTMassey <- MasseyOverallResults
library(dplyr)
library("psych")

MSTMassey <- MSTMassey %>%
  filter(Project == "Massey Creek")

#Lab results####

MasseyFollowResults <- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(between (Date, as.Date("2023-12-01"), as.Date("2023-12-30"))) %>%
  filter(SampleType == "Lab EC") %>%
  filter(!Source %in% c("Stream")) %>%
  filter(!Replicate %in% c("Y2")) %>%
  filter(!Locator %in% c("FLD_BLK")) %>%
  select(Locator, Result)

View(MasseyFollowResults)

#R card results####

MasseyFollowRCard <- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(between (Date, as.Date("2023-12-01"), as.Date("2023-12-30"))) %>%
  filter(SampleType == "R-Card") %>%
  filter(!Source %in% c("Stream")) %>%
  filter(!Replicate %in% c("Y2")) %>%
  filter(!Locator %in% c("FLD_BLK")) %>%
  select(Locator, Result)

View(MasseyFollowRCard )

#Biomarker Results - Dog####
MasseyFollowDog<- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(between (Date, as.Date("2023-12-01"), as.Date("2023-12-30"))) %>%
  filter(SampleType == "Dog1-Bac") %>%
  filter(!Source %in% c("Stream")) %>%
  filter(!Replicate %in% c("Y2")) %>%
  filter(!Locator %in% c("FLD_BLK")) %>%
  mutate(Result = as.numeric(Result)) %>%
  rename(Dog_biomarker = Result) %>%
  select(Locator, Dog_biomarker)

options(scipen = 999)

View(MasseyFollowDog)

#Biomarker Results - Human####

MasseyFollowHu<- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(between (Date, as.Date("2023-12-01"), as.Date("2023-12-30"))) %>%
  filter(SampleType == "Hu2-Bac") %>%
  filter(!Source %in% c("Stream")) %>%
  filter(!Replicate %in% c("Y2")) %>%
  filter(!Locator %in% c("FLD_BLK")) %>%
  mutate(Result = as.numeric(Result)) %>%
  rename(Hu_biomarker = Result) %>%
  select(Locator, Hu_biomarker)

options(scipen = 999)

View(MasseyFollowHu)

#Observation only ####

MasseyFollowObs <- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(between (Date, as.Date("2023-12-01"), as.Date("2023-12-30"))) %>%
  filter(SampleType == "Observation") %>%
  filter(!Source %in% c("Stream")) %>%
  filter(!Replicate %in% c("Y2")) %>%
  filter(!Locator %in% c("FLD_BLK")) %>%
  select(Locator, Result)

View(MasseyFollowObs)

#Join Biomarker Tables Together ####
MasseyFollowbiomarkers<- MasseyFollowDog %>%
  left_join(MasseyFollowHu, by = "Locator")
View(MasseyFollowbiomarkers)

#Export all Tables ####

MasseyFollowResults <-MasseyFollowResults %>% arrange(Locator)
write.csv(MasseyFollowResults, "MasseyFollowResults.csv")

MasseyFollowRCard <-MasseyFollowRCard %>% arrange(Locator)
write.csv(MasseyFollowRCard, "MasseyFollowRcard.csv")

MasseyFollowbiomarkers <-MasseyFollowbiomarkers %>% arrange(Locator)
write.csv(MasseyFollowbiomarkers, "MasseyFollowbiomarkers.csv")

getwd()

# === === === Stream samples ####
#Lab results####

MasseyFollowResults <- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(between (Date, as.Date("2023-12-01"), as.Date("2023-12-30"))) %>%
  filter(SampleType == "Lab EC") %>%
  filter(Source == "Stream") %>%
  filter(!Replicate %in% c("Y2")) %>%
  filter(!Locator %in% c("FLD_BLK")) %>%
  select(Locator, Result)

View(MasseyFollowResults)

#R card results####

MasseyFollowRCard <- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(between (Date, as.Date("2023-12-01"), as.Date("2023-12-30"))) %>%
  filter(SampleType == "R-Card") %>%
  filter(Source == "Stream") %>%
  filter(!Replicate %in% c("Y2")) %>%
  filter(!Locator %in% c("FLD_BLK")) %>%
  select(Locator, Result)

View(MasseyFollowRCard )

#Biomarker Results - Dog####
MasseyFollowDog<- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(between (Date, as.Date("2023-12-01"), as.Date("2023-12-30"))) %>%
  filter(SampleType == "Dog1-Bac") %>%
  filter(Source == "Stream") %>%
  filter(!Replicate %in% c("Y2")) %>%
  filter(!Locator %in% c("FLD_BLK")) %>%
  mutate(Result = as.numeric(Result)) %>%
  rename(Dog_biomarker = Result) %>%
  select(Locator, Dog_biomarker)

options(scipen = 999)

View(MasseyFollowDog)

#Biomarker Results - Human####

MasseyFollowHu<- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(between (Date, as.Date("2023-12-01"), as.Date("2023-12-30"))) %>%
  filter(SampleType == "Hu2-Bac") %>%
  filter(Source == "Stream") %>%
  filter(!Replicate %in% c("Y2")) %>%
  filter(!Locator %in% c("FLD_BLK")) %>%
  mutate(Result = as.numeric(Result)) %>%
  rename(Hu_biomarker = Result) %>%
  select(Locator, Hu_biomarker)

options(scipen = 999)

View(MasseyFollowHu)

#Observation only ####

MasseyFollowObs <- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(between (Date, as.Date("2023-12-01"), as.Date("2023-12-30"))) %>%
  filter(SampleType == "Observation") %>%
  filter(Source == "Stream") %>%
  filter(!Replicate %in% c("Y2")) %>%
  filter(!Locator %in% c("FLD_BLK")) %>%
  select(Locator, Result)

View(MasseyFollowObs)

#Join Biomarker Tables Together ####
MasseyFollowbiomarkers<- MasseyFollowDog %>%
  left_join(MasseyFollowHu, by = "Locator")
View(MasseyFollowbiomarkers)

#Export all Tables ####

MasseyFollowResults <-MasseyFollowResults %>% arrange(Locator)
write.csv(MasseyFollowResults, "StreamMasseyFollowResults.csv")

MasseyFollowRCard <-MasseyFollowRCard %>% arrange(Locator)
write.csv(MasseyFollowRCard, "StreamMasseyFollowRcard.csv")

MasseyFollowbiomarkers <-MasseyFollowbiomarkers %>% arrange(Locator)
write.csv(MasseyFollowbiomarkers, "StreamMasseyFollowbiomarkers.csv")

getwd()