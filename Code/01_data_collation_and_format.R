## collate data for analysis

## temp data
## bio data

library(lubridate)
library(zoo)
library(fs)
# install.packages("Microsoft365R")
library(Microsoft365R)

library(tidyverse)
library(sf)
library(tidylog)

## modelled temp data
# Rogers et al Model - metrics only and can only match by year
# tempEst - daily and metrics
# HecRas - daily and metrics - get from LAR study/LWA
# iTreeCool - daily and metrics (limited) - get from CSOM

## Observed data
# from CEDEN/SMC database - may be worth getting a full update of temp data at bio sites (ask Jeff or Megan)

## Bio data
# from SMC database - also may be worth getting a full update of temp data (ask Jeff or Megan)


# Bio Data ----------------------------------------------------------------


bioData <- read.csv("ignore/Bio/SMC_bioassessment2.csv")
head(bioData)

# CSCI

csci <- bioData %>%
  select(masterid:comid, csci:csci_fieldreplicate) %>% ## only columns needed
  distinct() %>% ## remove duplicates
  separate_wider_delim(csci_sampledate, delim = "-", names = c("Year", "Month", "Day"), cols_remove = F) %>% ## separate date
  # select(-Day, -Month) %>% ## remove month and day
  mutate(BioMetric = "CSCI") %>%
  rename(fieldreplicate = csci_fieldreplicate, Score = csci, sampleDate = csci_sampledate)

head(csci)
length(unique(csci$masterid)) ## 6269

# ASCI

asci <- bioData %>%
  select(masterid:comid, assemblage:asci_replicate, asci_result) %>%
  filter(assemblage =="Hybrid", metric == "ASCI") %>%
  distinct() %>%
  separate_wider_delim(asci_sampledate, delim = "-", names = c("Year", "Month", "Day"), cols_remove = F) %>% ## separate date
  select( -metric_type, -assemblage) %>% ## remove month and day
  rename(BioMetric = metric, fieldreplicate = asci_replicate, Score = asci_result, sampleDate = asci_sampledate)

head(asci)

BioData <- bind_rows(csci, asci)

## save 
write.csv(BioData, "ignore/Bio/00_all_bio_data.csv")

head(BioData)

### get list of sites
sites <- BioData %>%
  select(masterid, latitude, longitude, comid) %>%
  distinct()

## take max score from replicate

BioData <- BioData %>%
  group_by(masterid, Year, BioMetric) %>% ## group by site and year and ffm
  mutate(MaxScore = max(Score)) %>%
  mutate(Match = ifelse(MaxScore == Score, "Yes", "No")) %>%
  filter(Match == "Yes" ) %>%
  select(-Match, -fieldreplicate)

range(BioData$Year) # "1994" "2024"

# TempEst -----------------------------------------------------------------

## Metrics
TempData <- read.csv("ignore/modelledTemp/biosites_TempMetrics.csv") 
head(TempData2)

## separate column "id" to master id and comid
## format data
TempData2 <- TempData %>%
  select(id:lon, year:tmax_ab30count) %>%
  separate_wider_delim(id, delim = "_", names = c("B", "masterid", "comid"), too_many = "debug") %>% ## separate by _ 
  # - some masterids include a _ 
  separate_wider_delim(comid, delim = "-", names = c("C", "comid"), too_few = "align_start", too_many = "merge") %>% ## separate comid and COMID
  filter(!comid %in% c("Not", "-9999", "0")) %>% # remove comids that aren't comids
  mutate(masterid2 = ifelse(id_pieces == 4, paste0(masterid, "_", C), masterid)) %>% ## manually add masterids with _
  separate_wider_delim(id_remainder, delim = "-", names = c("C2","comid2"), too_few = "align_start") %>% ## separate comid-xxxx
  mutate(comid = ifelse(is.na(comid), comid2, comid)) %>% # add comids taken above to OG comid col
  select(-c(id_pieces, B, id,C, C2, comid2, masterid,id_ok, lat, lon)) %>% ## remove columns from formatting
  rename(masterid = masterid2, Year = year) %>% #rename
  mutate(Year = as.character(Year)) 

## remove metrics not needed
TempData2 <- TempData2 %>%
  dplyr::select(-c(starts_with("humidity"), starts_with("lst"), contains("temp.plus"), contains("anom"), 
                   contains("count"), contains("diff"), contains("doy"))) %>%
  pivot_longer(tmod_min7rmn:tmax_max7rav, names_to = "Mets", values_to = "Temp")


length(unique(TempData2$masterid)) ## 1509
length(unique(TempData2$comid)) ## 1013

## get sites 
sites <- TempData2 %>%
  select(masterid, comid) %>%
  distinct()

## save 
write.csv(sites, "ignore/01_temp_sites.csv")


## daily

#note directory where all csvs are stored (temp modelling teams channel)
directory_for_files <- "/Users/katieirving/Library/CloudStorage/OneDrive-SharedLibraries-SCCWRP/Temp Modelling - General/Data/biosites_output/"

#create list from directory for all files, may need to specify column type if there are empty cells within dataset
biosite_list <- directory_for_files %>% 
  dir_ls() %>% 
  map(
    .f = function(path) {
      read_csv(path) %>% 
        mutate(date = as.character(date))
    }
  )


#create dataframe of all sites
biosite_tbl <- biosite_list %>% 
  set_names(dir_ls(directory_for_files)) %>% 
  bind_rows(.id = "file.path")

## format masterid, date etc
biosite_tblx <- biosite_tbl %>%
  separate_wider_delim(id, delim = "-", names = c("C", "comid"), too_few = "align_start", too_many = "merge") %>%
  separate_wider_delim(C, delim = "_", names = c("C", "masterid", "C2"), too_few = "align_start", too_many = "merge") %>%
  mutate(Date = parse_date_time(date, orders = c("ymd", "mdy")))

head(biosite_tblx) 


# Rogers et al Model ------------------------------------------------------

load(file = "ignore/modelledTemp/baseline_stream_temp.RData")

baseline_stream_temp <- baseline_stream_temp %>% ungroup() %>%
  pivot_longer(Max_Wkly_Mean_StreamT:Mean_Wkl_Rng_StreamT, names_to = "Mets", values_to = "Temp") %>% ## make long
  select(-Max_Wkl_Max_StreamT_grt_30_) %>% ## remove metric non temp
  rename(comid = COMID, Year = year) %>% ## rename to match bio
  mutate(comid = as.character(comid), Year = as.character(Year))

# Observed data -----------------------------------------------------------

## funtion to return number of digits 
digits <- function(n) {
  count <- 0
  if (n == 0) {
    return(1)
  } else {
    while (n / 10 >= 0.1) {
      count <- count + 1
      n <- n %/% 10
    }
    return(count)
  }
}

## upload data and format
obs <- read.csv("ignore/ObservedTemp/CEDEN_data_WithCOMID_052125.csv") %>%
  drop_na(masterid) %>% ## remove na from site
  separate_wider_delim(SampleDate, delim = "/", names = c("Month", "Day", "Year"), cols_remove = F)  %>% ## separate date
  separate_wider_delim(Year, delim = " ", names = c("Year", "Time")) %>% ## separate date
  mutate(Year = paste0("20", Year), Day = str_pad(as.character(Day), width = 2, pad = "0"),
         Month = str_pad(as.character(Month), width = 2, pad = "0")) %>% ## change formats
  mutate(Date = as.character(paste0(Year, Month, Day))) %>%
  select(-c(CollectionDepth:Analyte, county:Huc12_name, TargetLatitude, TargetLongitude, comid)) %>%
  rename(Temp = Result) %>%
  mutate(Type = "Observation", Mets = "Spot")
  

head(obs)


# Join Phase 1 data -------------------------------------------------------

## join bio data to observed data
## match dates from observed data in modelled data
## join modelled data

head(BioDatax)
## Bio data
BioDatax <- BioData %>%
  separate_wider_delim(Day, delim = " ", names = c("Day", "Time"), too_few = "align_start") %>% ## separate date
  select(-Time) %>%
  mutate(Date = as.character(paste0(Year, Month, Day)))
  
names(BioDatax)

## Observed data
head(obs)
names(obs)

## join by master id and date

BioObs <- inner_join(BioDatax, obs, by = c("masterid", "Date", "Day", "Month", "Year")) # 2,874 matched

length(unique(BioObs$masterid)) ## 909

#### find dates in modelled data to atch bio data
head(BioDatax)

###################
## Rogers data 
head(baseline_stream_temp)

## can only match by year and comid - caveat!!!!

BioRogers <- inner_join(BioDatax, baseline_stream_temp, by = c("comid", "Year"))  %>% # 5,965 matched
  mutate(Type = "RogersEtAl")
length(unique(BioRogers$masterid)) ## 542

####################
## TempEst

## format date in tempest

tempeest <- biosite_tblx %>%
  select(-c(file.path, C, C2, elevation:day)) %>% ## remove columns
  separate_wider_delim(Date, delim = " ", names = c("Date", "C"), too_few = "align_start") %>% ## separate date
  separate_wider_delim(Date, delim = "-", names = c("Year", "Month", "Day"), too_few = "align_start") %>% ## separate date
  mutate(Date = as.character(paste0(Year, Month, Day))) %>% ## match bio date
  mutate(Type = "TempEst") %>%
  select(-C, -comid)
  
head(tempeest)

## join with bio by masterid and date

BioTempest <- inner_join(BioDatax, tempeest, by = c("masterid", "Date",  "Day", "Month", "Year")) %>% # 6,245 matched
  pivot_longer(temp.anom:temp.max, names_to = "Mets", values_to = "Temp") %>%
  select(-lat, -lon)

length(unique(BioTempest$masterid)) ## 1282


# Join all together -------------------------------------------------------

head(BioObs)
head(BioRogers)
head(BioTempest)


AllTemp <- bind_rows(BioObs, BioRogers, BioTempest)

## save 
save(AllTemp, file = "ignore/bio_temp_phase1_data.RData")


# Join for phase 2 --------------------------------------------------------

## join bio data to modelled metrics
## match year and masterid

head(TempData2)
head(BioData)
unique(TempData2$season)
## TempEst

BioTempMets <- inner_join(BioData, TempData2, by = c("masterid", "Year", "comid")) %>% ## join
  filter(season == "all") %>% ## take annual not seasonal for now
  mutate(Type = "TempEst")

head(BioTempMets)

# Rogers

# same as above as only annual metrics
head(BioRogers)

AllTempMets <- bind_rows(BioTempMets, BioRogers)

## save
save(AllTempMets, file = "ignore/bio_temp_phase2_data.RData")
