
#2.1.1 Initial screening phase - R card samples from September 2023 to November 2023####

#=== === === === === === 
#Table for 2.1.1 (summary statistics for the initial screening phase)

#This code filters R card samples from the master spreadsheet and calculates geometric mean, max, median, n and n samples that are above the threshold. No other tables are needed for this section because only R-cards were taken in this initial screening phase. Blanks and field replicates are not counted in analyses. 

#=== === === === === === 

#load in dataset ####
MSTMassey <- MasseyOverallResults
rm(MasseyGeomean)
rm(MasseyMax)
rm(MasseyN)
rm(InitialRCards)
rm(MasseyNThreshold)

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

MasseyNThreshold <- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(between (Date, as.Date("2023-09-01"), as.Date("2023-11-30"))) %>%
  filter(SampleType == "R-Card") %>%
  filter(!Replicate %in% c("Y2")) %>%
  filter(!Locator %in% c("FLD_BLK")) %>%
  group_by(Locator) %>%
  summarise(NThreshold = sum(Result > 200, na.rm = FALSE)) %>%
  distinct(Locator, .keep_all = TRUE) %>%
  select(Locator, NThreshold)

View(MasseyNThreshold)
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
  left_join (MasseyNThreshold, by = "Locator")
  
  


View(InitialRCards)

InitialRCards <-InitialRCards %>% arrange(Locator)

write.csv(InitialRCards, "InitialRCards.csv")
getwd()


### How many locations with visits during this phase (including just observations)

MasseyNLocationAll<- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(between (Date, as.Date("2023-09-01"), as.Date("2023-11-30"))) %>%
  distinct(Locator) %>%
  pull(Locator)

View(MasseyNLocationAll)    
list(MasseyNLocationAll)

###How many r card samples had fecal contamination? (i.e., any samples >1)

MasseytotalContamination<- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(between (Date, as.Date("2023-09-01"), as.Date("2023-11-30"))) %>%
  filter(SampleType %in% c("R-Card")) %>%
  filter(!Replicate %in% c("Y2")) %>%
  filter(!Locator %in% c("FLD_BLK")) %>%
  summarise(Result = sum(Result > 0, na.rm = FALSE))
View(MasseytotalContamination)  

###How many r card samples total 
MasseytotalRcards<- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(between (Date, as.Date("2023-09-01"), as.Date("2023-11-30"))) %>%
  filter(SampleType %in% c("R-Card")) %>%
  filter(!Replicate %in% c("Y2")) %>%
  filter(!Locator %in% c("FLD_BLK")) %>%
  summarise(count=n())

View(MasseytotalRcards)    


###Percent of R cards with presence of fecal contamination 
150/187 *100

###How many r card samples had fecal contamination above threshold?

MasseytotalThreshold<- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(between (Date, as.Date("2023-09-01"), as.Date("2023-11-30"))) %>%
  filter(SampleType %in% c("R-Card")) %>%
  filter(!Replicate %in% c("Y2")) %>%
  filter(!Locator %in% c("FLD_BLK")) %>%
  summarise(Result = sum(Result > 200, na.rm = FALSE))
View(MasseytotalThreshold)  

###Percent of R cards with fecal contamination above threshold
110/187

###Number of sampling events (i.e., number of sampling dates)
MasseyNDates<- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(between (Date, as.Date("2023-09-01"), as.Date("2023-11-30"))) %>%
  distinct(Date) %>%
  summarise(count=n())
