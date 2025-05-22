## eco response curves 
## Phase 2

library(lubridate)
library(zoo)
library(fs)
library(tidyverse)
library(sf)
library(tidylog)

## directory for figures
out.dir <- "Figures/"


# Data --------------------------------------------------------------------

load(file = "ignore/bio_temp_phase2_data.RData") 

## remove range metrics
AllTempMets <- AllTempMets %>%
  dplyr::filter(!Mets %in% c("Max_Wkl_Rng_StreamT", "Mean_Wkl_Rng_StreamT")) 

head(AllTempMets)

unique(AllTempMets$Mets)
unique(AllTempMets$Type)

# Model set up  -----------------------------------------------------------

## bio 
biol.endpoints<-c("CSCI", "ASCI")

## temp
temp.endpoints<- unique(na.omit(AllTempMets$Mets))
temp.endpoints

# Thresholds for index
index.thresholds <- c(0.86, 0.79) ## can change/add modified thresholds


## make griod to loop through
bio_h_summary<-  expand.grid(biol.endpoints=biol.endpoints,temp.endpoints=temp.endpoints, 
                             index.thresholds= index.thresholds, stringsAsFactors = F)

  
# GLMs --------------------------------------------------------------------
i=13
## model of each configuration
log.lm <-lapply(1:nrow(bio_h_summary), function(i)
{
  
  tmet<-as.character(bio_h_summary[i,"temp.endpoints"])
  bmet<-as.character(bio_h_summary[i,"biol.endpoints"])
  # imet<-as.character(bio_h_summary[i,"index.thresholds"])
  # mmet<-as.character(bio_h_summary[i,"temp.model"])
  
  mydat<-AllTempMets %>%
    filter(BioMetric == bmet,
           Mets == tmet) %>%
    select(Score, Temp, comid, masterid, Type) %>% ## only metrics needed
    drop_na(Score, Temp) %>%
    filter_all(all_vars(!is.infinite(.))) %>% ## remove all missing values
    distinct()
  # head(mydat)
  ## use different threshold for each index
  if(bmet == "CSCI") {
    
    mydat$Condition<-ifelse(mydat$Score < 0.79 ,0, 1) ## convert to binary
    
  } else {
    
    mydat$Condition<-ifelse(mydat$Score < 0.86 ,0, 1) ## convert to binary
    
  }
  
  
  mydat<-mydat[order(mydat$Score),] ## order by csci value
  glm(Condition~Temp, family=binomial(link="logit"), data=mydat) ### glm
  
  
})

## save models
save(log.lm, file = "ignore/03_glms_csci_asci_all_temp_metrics.RData")

### get rsqds and pvals
for(i in 1:length(log.lm)) {
  
  mod <- summary(log.lm[[i]])
  bio_h_summary$AIC[i] <- mod$aic ##1-mod$deviance/mod$null.deviance ## mcfaddens r2
  bio_h_summary$PValue[i] <- mod$coefficients[8]
  bio_h_summary$McFaddensR2[i] <- 1-mod$deviance/mod$null.deviance
  bio_h_summary$n[i] <- mod$df[2]+1
}

## save configs and r sqds
save(bio_h_summary, file="output_data/03_glm_rsqds.RData")
bio_h_summary

csci_coefs <- bio_h_summary


# Predictions -------------------------------------------------------------
## blank df
DF <- NULL
DF <- as.data.frame(DF)

### get predictions and fitted values
for(i in 1:length(log.lm)) {
  
  bio <- bio_h_summary[i,"biol.endpoints"]
  temp <- bio_h_summary[i,"temp.endpoints"]
  ind <- bio_h_summary[i,"index.thresholds"]
  # seas <- bio_h_summary[i,"seasons"]
  
  data<-AllTempMets %>%
    filter(BioMetric == bio,
           Mets == temp) %>%
    select(Score, Temp, comid, masterid, Type) %>% ## only metrics needed
    drop_na(Score, Temp) %>%
    filter_all(all_vars(!is.infinite(.))) %>% ## remove all missing values
    distinct()
  # head(data)
  ## new data - plus and minus 10% as a start
  tempvalues <- seq(range(data$Temp)[1]-1,range(data$Temp)[2]+10,0.05)
  
  ## get model, predict, extract all data and categories
  mod <- log.lm[[i]]
  predictedVals <- predict.glm(mod,list(Temp = tempvalues),  type = "response")
  DFX <- as.data.frame(predictedVals)
  DFX$Value <- tempvalues
  DFX$Bio <- bio
  DFX$Variable <- temp
  DFX$BioThreshold <- ind
  # DFX$Season <- seas
  DFX$MinVal <-  range(data$Temp)[1]
  DFX$MaxVal <-  range(data$Temp)[2]
  DFX$Model <- data$Type[1]
  
  DF <- bind_rows(DF, DFX)
  
}
DF

# Figures -----------------------------------------------------------------

head(DF)
unique(DF$Type)

DF <- DF %>%
  mutate(VariableH = case_when(Variable == "tmod_min7rmn" ~ "Weekly Min of Daily Mean Temp",
                                Variable == "tmax_min7rmn" ~ "Weekly Min of Max Daily Temp",
                                Variable == "tmax_max7rmx" ~ "Weekly Max of Max Daily Temp",
                                Variable == "tmod_max7rmx" ~ "Weekly Max of Daily Mean Temp",
                                # "tmod_avdiff" = "Weekly Range of Daily Mean Temp",
                                Variable ==  "tmax_max7rav" ~ "Weekly Max of Max Daily Temp",
                                Variable == "tmod_max7rav" ~ "Weekly Max of Daily Mean Temp",
                                Variable == "Max_Wkly_Mean_StreamT" ~ "Weekly Max of Daily Mean Temp",
                                Variable == "Max_Wkl_Max_StreamT" ~ "Weekly Max of Max Daily Temp",
                                Variable == "Min_Wkl_Min_StreamT" ~ "Weekly Min of Max Daily Temp")) %>%
  mutate(Model = factor(Model), 
         VariableH = factor(VariableH),
         BioThreshold = factor(BioThreshold, levels = c("0.79", "0.86")),
         Bio = factor(Bio, levels = c("CSCI", "ASCI")))

## check for NAs - all good!
# ind <-which(is.na(DF$VariableH))
# DF[ind,]

### predicted figures
mets <- unique(DF$Bio)
mets



unique(DF$Bio)

m=1
## plot
for(m in 1:length(mets)) {
  
  T1 <- (ggplot(subset(DF, Bio == mets[m]), aes(y=predictedVals, x=Value, group = Model, color = Model)) +
           # geom_point(size=0.2) +
           # geom_line( linewidth = 1)+
           stat_smooth(method = "lm", formula = y ~ x + I(x^2), linewidth = 1)+
           # geom_hline(yintercept = 0.6,  linetype="dashed", linewidth=0.5, color = "grey50") +
           facet_wrap(~VariableH, scales = "free") +
           scale_x_continuous(name="Water Temp (°C)") +
           scale_y_continuous(name = paste0("Probability of Good ", mets[m])))
  
  T1
  
  file.name1 <- paste0(out.dir, "03_", mets[m], "_temp_response_predicted_glm_simple.jpg")
  ggsave(T1, filename=file.name1, dpi=300, height=5, width=7.5)
}


## save data 

save(DF, file = "ignore/03_predictions_eco_response_phase2.RData")
