

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
  rename(LabEC = Result) %>%
  select(Locator, Date, LabEC)

View(MasseyFollowResults)

#R card results####

MasseyFollowRCard <- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(between (Date, as.Date("2023-12-01"), as.Date("2023-12-30"))) %>%
  filter(SampleType == "R-Card") %>%
  filter(!Source %in% c("Stream")) %>%
  filter(!Replicate %in% c("Y2")) %>%
  filter(!Locator %in% c("FLD_BLK")) %>%
  rename(RCardEC = Result) %>%
  select(Locator, Date, RCardEC)

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
  select(Locator, Date, Dog_biomarker)

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
  select(Locator, Date, Hu_biomarker)

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
  mutate(Result = 1) %>%
  rename(nObs = Result) %>%
  select(Locator, Date, nObs)

View(MasseyFollowObs)

#Join all Together ####
MasseyFollowAll <- MasseyFollowResults %>%
  full_join(MasseyFollowRCard, by = c("Locator", "Date")) %>%
  full_join(MasseyFollowDog, by = c("Locator", "Date")) %>%
  full_join(MasseyFollowHu, by = c("Locator", "Date")) %>%
  full_join(MasseyFollowObs, by = c("Locator", "Date")) %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), ~replace_na(., "NS"))) %>%
  mutate(nObs = na_if(nObs, "NS"))

View(MasseyFollowAll)
MasseyFollowAll <-MasseyFollowAll %>% arrange(Locator)
write.csv(MasseyFollowAll, "MasseyFollowAll.csv")

#Export all Tables ####

#MasseyFollowResults <-MasseyFollowResults %>% arrange(Locator)
#write.csv(MasseyFollowResults, "MasseyFollowResults.csv")

#MasseyFollowRCard <-MasseyFollowRCard %>% arrange(Locator)
#write.csv(MasseyFollowRCard, "MasseyFollowRcard.csv")

#MasseyFollowbiomarkers <-MasseyFollowbiomarkers %>% arrange(Locator)
#write.csv(MasseyFollowbiomarkers, "MasseyFollowbiomarkers.csv")

getwd()

# === === === Stream samples ####
#Lab results####

MasseyFollowLabECStream <- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(between (Date, as.Date("2023-12-01"), as.Date("2023-12-30"))) %>%
  filter(SampleType == "Lab EC") %>%
  filter(Source == "Stream") %>%
  filter(!Replicate %in% c("Y2")) %>%
  filter(!Locator %in% c("FLD_BLK")) %>%
  rename(LabEC= Result) %>%
  select(Locator, Date, LabEC)

View(MasseyFollowLabECStream)

#R card results####

MasseyFollowRCardStream <- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(between (Date, as.Date("2023-12-01"), as.Date("2023-12-30"))) %>%
  filter(SampleType == "R-Card") %>%
  filter(Source == "Stream") %>%
  filter(!Replicate %in% c("Y2")) %>%
  filter(!Locator %in% c("FLD_BLK")) %>%
  rename(Rcard = Result) %>%
  select(Locator, Date, Rcard)

View(MasseyFollowRCardStream)

#Biomarker Results - Dog####
MasseyFollowDogStream<- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(between (Date, as.Date("2023-12-01"), as.Date("2023-12-30"))) %>%
  filter(SampleType == "Dog1-Bac") %>%
  filter(Source == "Stream") %>%
  filter(!Replicate %in% c("Y2")) %>%
  filter(!Locator %in% c("FLD_BLK")) %>%
  mutate(Result = as.numeric(Result)) %>%
  rename(Dog_biomarker = Result) %>%
  select(Locator, Date, Dog_biomarker)

options(scipen = 999)

View(MasseyFollowDogStream)

#Biomarker Results - Human####

MasseyFollowHuStream<- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(between (Date, as.Date("2023-12-01"), as.Date("2023-12-30"))) %>%
  filter(SampleType == "Hu2-Bac") %>%
  filter(Source == "Stream") %>%
  filter(!Replicate %in% c("Y2")) %>%
  filter(!Locator %in% c("FLD_BLK")) %>%
  mutate(Result = as.numeric(Result)) %>%
  rename(Hu_biomarker = Result) %>%
  select(Locator, Date, Hu_biomarker)

options(scipen = 999)

View(MasseyFollowHuStream)

#Join all Together ####
MasseyFollowAllStream <- MasseyFollowLabECStream %>%
  full_join(MasseyFollowRCardStream, by = c("Locator", "Date")) %>%
  full_join(MasseyFollowDogStream, by = c("Locator", "Date")) %>%
  full_join(MasseyFollowHuStream, by = c("Locator", "Date"))

View(MasseyFollowAllStream)
MasseyFollowAllStream <-MasseyFollowAllStream%>% arrange(Locator)
write.csv(MasseyFollowAllStream, "MasseyFollowAllStream.csv")

#Observation only ####

#MasseyFollowObs <- MSTMassey %>%
  #filter(Project == "Massey Creek") %>%
  #filter(between (Date, as.Date("2023-12-01"), as.Date("2023-12-30"))) %>%
  #filter(SampleType == "Observation") %>%
  #filter(Source == "Stream") %>%
  #filter(!Replicate %in% c("Y2")) %>%
  #filter(!Locator %in% c("FLD_BLK")) %>%
  #select(Locator, Result)

#View(MasseyFollowObs)

#Join Biomarker Tables Together ####
#MasseyFollowbiomarkers<- MasseyFollowDog %>%
 # left_join(MasseyFollowHu, by = "Locator")
#View(MasseyFollowbiomarkers)

#Export all Tables ####

#MasseyFollowResults <-MasseyFollowResults %>% arrange(Locator)
#(MasseyFollowResults, "StreamMasseyFollowResults.csv")

#MasseyFollowRCard <-MasseyFollowRCard %>% arrange(Locator)
#write.csv(MasseyFollowRCard, "StreamMasseyFollowRcard.csv")

# <-MasseyFollowbiomarkers %>% arrange(Locator)
#write.csv(MasseyFollowbiomarkers, "StreamMasseyFollowbiomarkers.csv")

#getwd()