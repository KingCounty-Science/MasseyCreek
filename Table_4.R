###Goal: Create table 4: R-card E. coli results for in-stream sampling locations 
setwd("C:\\Users\\diayeh\\OneDrive - King County\\PICScienceFiles\\Projects\\Massey Creek_NEP_MST_2023-2026\\Data and Analysis")

#Code to set the file explorer on the bottom right: 
rstudioapi::filesPaneNavigate(getwd())
MasseyOverallResults <- read_excel("MasseyOverallResults.xlsx", 
                                   +     sheet = "Source_Tracking")

setwd("C://Users//diayeh//OneDrive - King County//PICScienceFiles//Projects//Massey Creek_NEP_MST_2023-2026//Data and Analysis//Final_Report")

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
  filter(Source == "Stream") %>%
  filter(SampleType == "R-Card") %>%
  filter(!Replicate %in% c("Y2")) %>%
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

### Add column - max

MasseyMax <- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(Source == "Stream") %>%
  filter(SampleType == "R-Card") %>%
  filter(!Replicate %in% c("Y2")) %>%
  mutate(Result = replace(Result, Result== "0", 1)) %>%
  mutate(Result = replace(Result, Result== "NA", 1)) %>%
  mutate(Result = replace(Result, Result== "est. 144480 (@10x dilution)", 144480)) %>%
  mutate(Result = replace(Result, Result== ">100k (@10x dilution)", 100000)) %>%
  mutate(Result = as.numeric(Result)) %>%
  group_by(Locator) %>%
  summarise(Max = max(Result, na.rm=FALSE)) %>%
  select(Locator, Max)

View(MasseyMax)

### Add column - n of all r card samples 

###Next steps - figure out if source is all correct on the main spreadsheet 
### reload spreadsheet and recalculate table 3

MasseyN <- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(Source == "Stream") %>%
  filter(SampleType == "R-Card") %>%
  filter(!Replicate %in% c("Y2")) %>%
  group_by(Locator) %>%
  mutate(n=n()) %>%
  distinct(Locator, .keep_all = TRUE) %>%
  select(Locator, n)

View(MasseyN)

Table4 <- MasseyGeomean %>%
  left_join (MasseyMax, by = "Locator") %>%
  left_join (MasseyN, by = "Locator")

View(Table4)

write.csv(Table4, "MasseyPIC_Table4.csv")
getwd()

###Calculate table 5 (lab samples for in stream sites)

MasseyGeomeanLabEC <- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(Source == "Stream") %>%
  filter(SampleType == "Lab EC") %>%
  filter(!Replicate %in% c("Y2")) %>%
  mutate(Result = replace(Result, Result== "0", 1)) %>%
  mutate(Result = replace(Result, Result== "NA", 1)) %>%
  mutate(Result = replace(Result, Result== ">800,000", 800000)) %>%
  mutate(Result = as.numeric(Result)) %>%
  group_by(Locator) %>%
  summarise(geomean = geometric.mean(Result, na.rm=FALSE)) %>%
  mutate(round(geomean)) %>%
  rename(Geomean = "round(geomean)") %>%
  select(Locator, Geomean)

options(scipen = 999)

View(MasseyGeomeanLabEC)


MasseyLabMax <- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(Source == "Stream") %>%
  filter(SampleType == "Lab EC") %>%
  filter(!Replicate %in% c("Y2")) %>%
  mutate(Result = replace(Result, Result== "0", 1)) %>%
  mutate(Result = replace(Result, Result== "NA", 1)) %>%
  mutate(Result = replace(Result, Result== ">800,000", 800000)) %>%
  mutate(Result = as.numeric(Result)) %>%
  group_by(Locator) %>%
  summarise(Max = max(Result, na.rm=FALSE)) %>%
  select(Locator, Max)

View(MasseyLabMax)
##Include n for each sample

MasseyECn <- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(Source == "Stream") %>%
  filter(SampleType == "Lab EC") %>%
  filter(!Replicate %in% c("Y2")) %>%
  group_by(Locator) %>%
  mutate(n=n()) %>%
  distinct(Locator, .keep_all = TRUE) %>%
  select(Locator, n)

View(MasseyECn)

MasseyLabEC<- MasseyGeomeanLabEC %>%
  left_join(MasseyLabMax, by = "Locator") %>%
  left_join (MasseyECn, by = "Locator")

View(MasseyLabEC)

write.csv(MasseyLabEC, "MasseyPIC_Table5.csv")
getwd()

### 
##HU 2 biomarkers 


##Dog bac 1 samples 

