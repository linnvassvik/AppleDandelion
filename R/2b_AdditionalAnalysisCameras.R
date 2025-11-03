library(readxl)
library(tidyverse)
library(lubridate)
library(glmmTMB)
library(performance)
library(emmeans)
library(ggeffects)

PolliObs_2023 <- read_excel("Data/PolliObs_2023.xlsx") %>% 
  filter(Apple_variety != 'Aroma', Location != 'Hoyen') %>% 
  select(-Time, -Comments, -Comments_picture, -On_flower) %>% 
  filter(Taxanomic_group != 'Fly', Taxanomic_group != 'Butterfly', Taxanomic_group != 'Hoverfly')


PolliObs_summary <- PolliObs_2023 %>%
  mutate(Date = dmy(Date),
         DOY = yday(Date)) %>%
  group_by(Location, Tree, DOY, Apple_variety, Taxanomic_group) %>%
  summarise(N_visits = n()) %>%
  ungroup() %>% 
  rename(Taxonomic_group = Taxanomic_group) %>% 
  mutate(Tree = as.character(Tree))
  


Poll23 <- glmmTMB(N_visits ~ Apple_variety * Taxonomic_group + (1|Location),
                  family = nbinom2,
                  data = PolliObs_summary)


summary(Poll23)


