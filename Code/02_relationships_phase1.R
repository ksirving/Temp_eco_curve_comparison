## Phase 1
# relationships between csci/asci and temp
## observations and daily temp from models

library(lubridate)
library(zoo)
library(fs)
library(tidyverse)
library(sf)
library(tidylog)

## directory for figures
out.dir <- "Figures/"


# Data --------------------------------------------------------------------

load(file = "ignore/bio_temp_phase1_data.RData")
head(AllTemp)

unique(AllTemp$Mets)

## remove non direct temp measures
AllTemp2 <- AllTemp %>%
  filter(Mets %in% c("Spot", "temp.mod", "Max_Wkly_Mean_StreamT"),
         !Temp < 0, !Temp > 500) %>%
  mutate(Mets = factor(Mets, levels = c("Spot", "temp.mod", "Max_Wkly_Mean_StreamT"),
                       labels = c("Observed (spot)", "TempEst (Mod)", "Rogers et al (7Day Mean)")))

# Plot --------------------------------------------------------------------

p1 <- ggplot(data = AllTemp2, aes(x=Temp, y = Score, colour = Mets)) +
  # geom_point() +
  # geom_smooth(method = "lm") +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), linewidth = 1)+
  facet_wrap(~BioMetric) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

p1

file.name1 <- paste0(out.dir, "01_basic_relationships_phase1.jpg")
ggsave(p1, filename=file.name1, dpi=300, height=6, width=6)



# Correlations of temp ----------------------------------------------------

head(AllTemp2)
## format 
cortemp <- AllTemp2 %>%
  select(masterid, Year, Month, Day, Temp, Mets, Date) %>%
  distinct() %>% ## some duplications, fixed later
  group_by(masterid, Year, Month, Day, Mets, Date) %>%
  filter(row_number()==1) %>% ## take first row for now
  pivot_wider(names_from = Mets, values_from = Temp) ## make wide
names(cortemp)

## only temp data
cortempx <- cortemp[,6:8]

cors <- cor(cortempx, use = "na.or.complete")

write.csv(cors, "Tables/02_correlations_temp.csv")
