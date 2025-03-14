#=== === === ####
#Code for MAS_CNV5011

#=== === === 


MSTMassey <- MasseyOverallResults
library(dplyr)
library("psych")
library(tidyr)

MSTMassey <- MSTMassey %>%
  filter(Project == "Massey Creek") %>%
  filter(Locator %in% c("MAS_CNV2895", "MAS_16", "MAS_WALL_1", "MAS_04", "MAS_OSS_OUTFALL_2", "MAS_OSS_OUTFALL", "MAS_04C", "MAS_04A", "MAS_OUTFALL_1", "MAS_04B", "MAS_POND_1", "MAS_SEEP_1", "MAS_CS0284_N", "MAS_CS0284_E", "MAS_18A", "MAS_18B", "MAS_18C", "MAS_18", "MAS_18D", "MAS_18E", "MAS_18F", "MAS_18G", "MAS_18I", "MAS_18K", "MAS_18L", "MAS_18O", "MAS_18N", "MAS_18P", "MAS_18R", "MAS_18S", "MAS_18T", "MAS_06B", "MAS_06C", "MAS_06A", "MAS_SEEP_2", "MAS_06", "MAS_07", "MAS_OSD1860", "MAS_CNV3875", "MAS_CNV1870")) ##make sure to add all sites in the wetland area as well to this one - the trib ones can go in a separate chunk

#Lab results####

MasseyStream<- MSTMassey %>%
  filter(between (Date, as.Date("2024-1-01"), as.Date("2024-12-30"))) %>%
  filter(SampleType %in% c("Lab EC", "R-Card")) %>%
  filter(!Replicate %in% c("Y2")) %>%
  select(Locator, Date, Result)

View(MasseyStream)



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
