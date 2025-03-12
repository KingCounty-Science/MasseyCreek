

MSTMassey <- MasseyOverallResults
library(dplyr)
library("psych")

MSTMassey <- MSTMassey %>%
  filter(Project == "Massey Creek")

#Geomean ####

MasseyFollowGeomean <- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(between (Date, as.Date("2023-12-01"), as.Date("2023-12-30"))) %>%
  filter(SampleType == "Lab EC") %>%
  filter(!Source %in% c("Stream")) %>%
  filter(!Replicate %in% c("Y2")) %>%
  filter(!Locator %in% c("FLD_BLK")) %>%
  mutate(Result = replace(Result, Result== "0", 1)) %>%
  mutate(Result = replace(Result, Result== "NA", 1)) %>%
  mutate(Result = as.numeric(Result)) %>%
  group_by(Locator) %>%
  summarise(geomean = geometric.mean(Result, na.rm=FALSE)) %>%
  mutate(round(geomean)) %>%
  rename(Geomean = "round(geomean)") %>%
  select(Locator, Geomean)

options(scipen = 999)

View(MasseyFollowGeomean)

#Max R Cards ####

MasseyFollowMax <- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(!Source %in% c("Stream")) %>%
  filter(between (Date, as.Date("2023-12-01"), as.Date("2023-12-30"))) %>%
  filter(SampleType == "Lab EC") %>%
  filter(!Replicate %in% c("Y2")) %>%
  filter(!Locator %in% c("FLD_BLK")) %>%
  mutate(Result = replace(Result, Result== "0", 1)) %>%
  mutate(Result = replace(Result, Result== "NA", 1)) %>%
  mutate(Result = as.numeric(Result)) %>%
  group_by(Locator) %>%
  summarise(Max = max(Result, na.rm=FALSE)) %>%
  select(Locator, Max)

View(MasseyFollowMax)

#n Lab samples ####


MasseyFollowN <- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(between (Date, as.Date("2023-12-01"), as.Date("2023-12-30"))) %>%
  filter(SampleType == "Lab EC") %>%
  filter(!Replicate %in% c("Y2")) %>%
  filter(!Locator %in% c("FLD_BLK")) %>%
  filter(!Source %in% c("Stream")) %>%
  group_by(Locator) %>%
  mutate(n=n()) %>%
  distinct(Locator, .keep_all = TRUE) %>%
  select(Locator, n)

View(MasseyFollowN)

