
###2.1.1 Initial screening phase - R card samples from September 2023 to November 2023

###load in dataset 
MSTMassey <- MasseyOverallResults

# Check unique values in Result column - this code checks to make sure it's numerical
#unique(MSTMassey$Result)

###Filter to only Massey Creek sites (There was one woodmont site)
library(dplyr)
MSTMassey<- MSTMassey %>%
  filter(Project == "Massey Creek")

### For samples with 0 cfu / 100mL or in otherwords, <MDL, change them to 1 
#to calculate geomean

###Also beforehand, filter to only those analyzed with r card
#install.packages("psych")
library("psych")


MasseyGeomean <- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(between (Date, as.Date("2023-09-01"), as.Date("2023-11-30"))) %>%
  filter(SampleType == "R-Card") %>%
  filter(!Replicate %in% c("Y2")) %>%
  filter(!Locator %in% c("FLD_BLK")) %>%
  mutate(Result = replace(Result, Result== "0", 1)) %>%
  mutate(Result = replace(Result, Result== "NA", 1)) %>%
  mutate(Result = replace(Result, Result== "est. 144480 (@10x dilution)", 144480)) %>%
  mutate(Result = replace(Result, Result== ">100k (@10x dilution)", 100000)) %>%
  mutate(Result = as.numeric(Result)) %>%
  group_by(Locator) %>%
  summarise(geomean = geometric.mean(Result, na.rm=FALSE)) %>%
  mutate(round(geomean)) %>%
  rename(Geomean = "round(geomean)") %>%
  select(Locator, Geomean)

options(scipen = 999)

View(MasseyGeomean)


######Max

MasseyMax <- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(between (Date, as.Date("2023-09-01"), as.Date("2023-11-30"))) %>%
  filter(SampleType == "R-Card") %>%
  filter(!Replicate %in% c("Y2")) %>%
  filter(!Locator %in% c("FLD_BLK")) %>%
  mutate(Result = replace(Result, Result== "0", 1)) %>%
  mutate(Result = replace(Result, Result== "NA", 1)) %>%
  mutate(Result = replace(Result, Result== "est. 144480 (@10x dilution)", 144480)) %>%
  mutate(Result = replace(Result, Result== ">100k (@10x dilution)", 100000)) %>%
  mutate(Result = as.numeric(Result)) %>%
  group_by(Locator) %>%
  summarise(Max = max(Result, na.rm=FALSE)) %>%
  select(Locator, Max)

View(MasseyMax)

######n


MasseyN <- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(between (Date, as.Date("2023-09-01"), as.Date("2023-11-30"))) %>%
  filter(SampleType == "R-Card") %>%
  filter(!Replicate %in% c("Y2")) %>%
  filter(!Locator %in% c("FLD_BLK")) %>%
  group_by(Locator) %>%
  mutate(n=n()) %>%
  distinct(Locator, .keep_all = TRUE) %>%
  select(Locator, n)

View(MasseyN)

#####n=>200 cfu/100mL 
####Leaving off point - need to figure out how to count n > 200 cfu/100mL

MasseyN <- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(between (Date, as.Date("2023-09-01"), as.Date("2023-11-30"))) %>%
  filter(SampleType == "R-Card") %>%
  filter(!Replicate %in% c("Y2")) %>%
  filter(!Locator %in% c("FLD_BLK")) %>%
  group_by(Locator) %>%
  mutate(n=n()) %>%
  distinct(Locator, .keep_all = TRUE) %>%
  select(Locator, n)

View(MasseyN)
#####median

MasseyMedian <- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(between (Date, as.Date("2023-09-01"), as.Date("2023-11-30"))) %>%
  filter(SampleType == "R-Card") %>%
  filter(!Replicate %in% c("Y2")) %>%
  filter(!Locator %in% c("FLD_BLK")) %>%
  mutate(Result = replace(Result, Result== "0", 1)) %>%
  mutate(Result = replace(Result, Result== "NA", 1)) %>%
  mutate(Result = replace(Result, Result== "est. 144480 (@10x dilution)", 144480)) %>%
  mutate(Result = replace(Result, Result== ">100k (@10x dilution)", 100000)) %>%
  mutate(Result = as.numeric(Result)) %>%
  group_by(Locator) %>%
  summarise(Median = median(Result, na.rm=FALSE)) %>%
  select(Locator, Median)

View(MasseyMedian)
##### Join everything together by locator

InitialRCards <- MasseyN %>%
  left_join (MasseyGeomean, by = "Locator") %>%
  left_join (MasseyMedian, by = "Locator") %>%
  left_join (MasseyMax, by = "Locator") %>%
  left_join (NThreshold, by = "Locator")
  
  


View(InitialRCards)

write.csv(InitialRCards, "InitialRCards.csv")
getwd()
