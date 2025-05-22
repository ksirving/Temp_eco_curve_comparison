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
  geom_smooth(method = "loess") +
  facet_wrap(~BioMetric) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

p1

file.name1 <- paste0(out.dir, "01_basic_relationships_phase1.jpg")
ggsave(p1, filename=file.name1, dpi=300, height=6, width=6)


