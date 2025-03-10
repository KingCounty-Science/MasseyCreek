###Load in sourcetracking tab of Massey Overall Results
###Goal - Create dataframe with Date, Sites visited, R-card samples, dry sites, 
###lab EC/FC and qPCR biomarkers, and qPCR dog biomarkers)

###Load in dataset
MST<-MasseyOverallResults

###Filter to only Massey Creek sites (There was one woodmont site)
library(dplyr)
MSTMassey<- MST %>%
  filter(Project == "Massey Creek")

###Count of sites visited within a day 

nSites<- MSTMassey %>%
  group_by(Date) %>%
  summarise(unique_count=n_distinct(Locator))

View(nSites)

###Count of Sites analyzed for R-cards, excluding blanks and replicates

rCardSamples<-MSTMassey %>%
  filter(SampleType == "R-Card") %>%
  filter(!Replicate %in% c("Y2")) %>%
  filter(!Locator %in% c("FLD_BLK")) %>%
  group_by(Date) %>%
  summarise(nRCard = n())

#View(rCardSamples)

###Count of sites analyzed for EC/FC, excluding blanks and replicates

ECLabSamples<-MSTMassey %>%
  filter(SampleType == "Lab EC") %>%
  filter(!Replicate %in% c("Y2")) %>%
  filter(!Locator %in% c("FLD_BLK")) %>%
  group_by(Date) %>%
  summarise(nEC = n())

#View(ECLabSamples)

###Count of sites analyzed for biomarkers (counting samples that test for both
#dog and human biomarkers as one sample)


###Count of sites analyzed for dog-1 biomarker

##filter by sample type = dog or hu, but only count distinct lab samples - 
#includes replicates if there are any

BiomarkerSamples<- MSTMassey %>%
  filter(SampleType == "Dog1-Bac" | SampleType == "Hu2-Bac") %>%
  filter(!Replicate %in% c("Y2")) %>%
  filter(!Locator %in% c("FLD_BLK")) %>%
  group_by(Date) %>%
  distinct(LabSampleNum) %>%
  summarise(biomarkers = n())

#View(BiomarkerSamples)

###Count of sites that were not sampled because they were dry or had no flow (i.e., stagnant)
nObservations<- MSTMassey %>%
  filter(SampleType=="Observation") %>%
  filter(!Replicate %in% c("Y2")) %>%
  filter(!Locator %in% c("FLD_BLK")) %>%
  group_by(Date) %>%
  summarise(Obs = n())
#View(nObservations)

###Left join everything together 

Table3<- nSites %>%
  left_join (rCardSamples, by = "Date") %>%
  left_join (nObservations, by = "Date") %>%
  left_join (ECLabSamples, by = "Date") %>%
  left_join (BiomarkerSamples, by = "Date")

#View(Table3)

df <- Table3 %>%
  mutate(across(everything() & !Date, ~if_else(is.na(.), "0", .)))

#Rename df 
df <- df %>%
  rename(
    "Sites Visited (n)" = "unique_count",
    "R-Card Samples (n)" = "nRCard",
    "Dry sites (n)" = "Obs",
    "Samples analyzed by KCEL for EC (n)" = "nEC",
    "Samples analyzed by KCEL for biomarkers (n)" = "biomarkers"
    
  )

####Export columns 
write.csv(df, "MasseyPIC_Table3FinalFinal_2025.csv")
getwd()
