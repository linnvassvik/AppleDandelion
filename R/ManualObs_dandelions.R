library(tidyverse)
library(readxl)
library(glmmTMB)
library(performance)
library(lubridate)
library(emmeans)
library(ggeffects)
library(ggridges)


#phenology, same as for Camera_mowing treatment, but with Summerred and ground instead
PhenologyDandelion2 <- read_excel("Data/Phenology_SRandDisc.xlsx") %>% 
  select(-ID, -Weather, -Other_flowers, -Comment) %>% 
  filter(Species == 'Summerred') %>% 
  mutate(Time = as_hms(Time)) %>% 
  mutate(DOY = dmy(Date),DOY = yday(DOY)) %>% 
  rename(Apple_variety = Species) %>% 
  rename(NOpen = '#Open') %>% 
  rename(NBuds = '#Buds') %>% 
  rename(NWithered = '#Withered') %>% 
  rename(POpen = '%Open')


PhenoDand <- PhenologyDandelion %>% 
  filter(Where == 'Ground') %>% 
  select(-Branch, -Time, -Date, -POpen)

PhenoApple <- PhenologyDandelion %>% 
  filter(Where == 'Tree') %>% 
  group_by(Location, Apple_variety, Where, Tree, DOY) %>%
  summarise(NBuds = sum(NBuds, na.rm = TRUE),
            NOpen = sum(NOpen, na.rm = TRUE),
            NWithered = sum(NWithered, na.rm = TRUE)) %>%
  ungroup() 

PhenologyCombined <- bind_rows(PhenoApple, PhenoDand)

PhenologyDandelion <- PhenologyCombined %>% 
  mutate(NTot = (NBuds + NOpen + NWithered)) %>% 
  mutate(Percentage = (NOpen / NTot) * 100)



Visits <- read_excel("Data/Visits2.xlsx")

Visits2 <- Visits %>% 
  select(-Comment, -Weather) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(Date = dmy(Date)) %>% 
  mutate(Time = hms::parse_hm(Time)) %>% 
  filter(row_number() != 445) %>% 
  mutate(DOY = yday(Date))

Visits3a <- Visits2 %>% 
  select(-Other, -'Andrena cineraria', -'Andrena haemorrhoa', -'Bombus pratorum', -'Andrena sp', -'Bombus sensu stricto', -'Bombus hypnorum', -'Lasioglossum sp', -'Andrena scotica', -'Andrena nigroaena', - Hoverfly, -Fly) 

Visits3 <- Visits3a %>% 
  pivot_longer(cols = Honeybee:Solitarybee, names_to = "Pollinator", values_to = "Count")


### Species ####
Visits4 <- Visits2 %>% 
  select(-Bumblebee, -Solitarybee, -Hoverfly, -Fly, -Other, -DOY) %>% 
  pivot_longer(cols = Honeybee:'Andrena nigroaena', names_to = "Pollinator", values_to = "Count")

Visits4_summarised <- Visits4 %>%
  group_by(Pollinator, Type) %>%
  summarize(Total_Count = sum(Count)) %>%
  ungroup() %>%
  mutate(Percent = (Total_Count / sum(Total_Count)) * 100) %>% 
  mutate(Pollinator = if_else(Pollinator == "Honeybee", "Apis mellifera", Pollinator))


Visits4_summarised <- Visits4_summarised %>% 
  mutate(Pollinator = as.character(Pollinator))


### TEST DIFFERENCE IN BEE GROUPS (not enough data for species) VISITING APPLE AND DANDELION
ModelVisit2 <- glmmTMB(Count ~ Type * Pollinator + (1 | Location),
                       family = nbinom2,
                       data = Visits3)

summary(ModelVisit2)
check_model(ModelVisit2)

emmeans(ModelVisit2, pairwise ~ Type * Pollinator)
emmeans(ModelVisit2, pairwise ~ Pollinator)


SpeciesVisit <- Visits4_summarised %>% 
  ggplot(aes(y = Total_Count, x = Pollinator, fill = Type)) + 
  geom_col(position = position_dodge()) +
  geom_text(aes(label = Total_Count), position = position_dodge(width = 0.9), vjust = -0.5, size = 5) +
  scale_fill_manual(name = "",
                    values = c("Tree" = "#7F646C", "Ground" = "#CC9966"),
                    labels = c("Tree" = "Apple flowers", "Ground" = "Dandelions")) +
  theme_minimal() +
  labs(title = NULL,
       x = "",
       y = "Total number") +
  ggtitle("") + 
  theme(plot.title = element_text(hjust = 1),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 16, face = "italic"),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 16))

ggsave(SpeciesVisit, filename = "Figures/SpeciesVisit.jpeg", height = 8, width = 13)








### Group ####
# Filter data for Before apple flowering (DOY 132-134)
Visits3_filtered1 <- Visits3 %>%
  filter(DOY >= 132 & DOY <= 134)

# Filter data for After apple flowering (DOY 135-142)
Visits3_filtered2 <- Visits3 %>%
  filter(DOY >= 135 & DOY <= 142)

# Rearrange order of Pollinator
Visits3_filtered1 <- Visits3_filtered1 %>%
  mutate(Pollinator = factor(Pollinator, levels = c("Honeybee", "Solitarybee", "Bumblebee", "Hoverfly", "Fly")))

Visits3_filtered2 <- Visits3_filtered2 %>%
  mutate(Pollinator = factor(Pollinator, levels = c("Honeybee", "Solitarybee", "Bumblebee", "Hoverfly", "Fly")))

# Summarize data to get total Count for each Pollinator and Type
Visits3_summarized1 <- Visits3_filtered1 %>%
  group_by(Pollinator, Type) %>%
  summarize(Total_Count = sum(Count)) %>%
  ungroup() %>%
  mutate(Percent = (Total_Count / sum(Total_Count)) * 100,
         Period = "Before apple flowering")

Visits3_summarized2 <- Visits3_filtered2 %>%
  group_by(Pollinator, Type) %>%
  summarize(Total_Count = sum(Count)) %>%
  ungroup() %>%
  mutate(Percent = (Total_Count / sum(Total_Count)) * 100,
         Period = "After apple flowering")

# Combine summarized data for plotting
Visits3_combined <- bind_rows(Visits3_summarized1, Visits3_summarized2)

Visits3_combined <- Visits3_combined %>%
  mutate(Period = factor(Period, levels = c("Before apple flowering", "After apple flowering")))

# Plotting the combined data with percentages and labels
Visits_combined <- Visits3_combined %>% 
  ggplot(aes(y = Percent, x = Pollinator, fill = Type)) + 
  geom_col(position = position_dodge()) +
  scale_fill_manual(name = "",
                    values = c("Tree" = "#7F646C", "Ground" = "#CC9966"),
                    labels = c("Tree" = "Apple flowers", "Ground" = "Dandelions")) +
  theme_minimal() +
  geom_text(aes(label = paste0(round(Percent, 1), "%")),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 4,
            color = "black") +
  labs(title = NULL,
       x = "",
       y = "Percentage (%)") +
  facet_wrap(~ Period, scales = "free_x") +
  ggtitle("Percentage of visits") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~ Period, scales = "free_x")
ggsave(Visits_combined, filename = "Figures/Visits_combined.jpeg", height = 8, width = 12)












# ######## PHENOLOGY AND ACTIVITY -----------------------------------------

PhenologyDandelion %>%
  ggplot(aes(x = DOY, y = NOpen, color = Where, fill = Where)) +
  geom_smooth(alpha = 0.2, method = "gam", method.args = list(family = quasipoisson)) +
  labs(x = "Day of the year (DOY)", y = "Average number of open flowers") +
  scale_x_continuous(breaks = seq(min(PhenologyDandelion$DOY), max(PhenologyDandelion$DOY), by = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Number of open apple flowers on two branches and dandelions in 1x1 m quadrat") +
  scale_color_manual(name = "",
                     values = c("Tree" = "#7F646C", "Ground" = "#CC9966"),
                     labels = c("Tree" = "Apple flowers", "Ground" = "Dandelions")) +
  scale_fill_manual(name = "",
                    values = c("Tree" = "#7F646C", "Ground" = "#CC9966"),
                    labels = c("Tree" = "Apple flowers", "Ground" = "Dandelions"))



PhenoManObs <- PhenologyDandelion %>%
  ggplot(aes(x = DOY, y = NOpen, color = Where, fill = Where)) +
  stat_smooth(method = "gam",
              formula = y ~ s(x, k = 5),
              method.args = list(family = gaussian(link = "log")),
              geom = "area",
              fullrange = TRUE,
              alpha = 0.3, se = FALSE) +
  stat_smooth(method = "gam",
              formula = y ~ s(x, k = 5),
              method.args = list(family = gaussian(link = "log")),
              geom = "line",
              fullrange = TRUE,
              size = 1.1, se = FALSE) +
  labs(x = "Day of the year (DOY)", y = "Number of open flowers per\nor two branches of apples or\nor 1x1 m plot of dandelions") +
  scale_x_continuous(breaks = seq(min(PhenologyDandelion$DOY), max(PhenologyDandelion$DOY), by = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("") +
  scale_color_manual(name = "",
                     values = c("Tree" = "#7F646C", "Ground" = "#CC9966"),
                     labels = c("Tree" = "Apple flowers", "Ground" = "Dandelions")) +
  scale_fill_manual(name = "",
                    values = c("Tree" = "#7F646C", "Ground" = "#CC9966"),
                    labels = c("Tree" = "Apple flowers", "Ground" = "Dandelions")) +
  facet_wrap(~ Location)




PolliManObs <- Visits3 %>% 
  ggplot(aes(x = DOY, y = Count, color = Type, fill = Type)) +
  stat_smooth(method = "gam",
              formula = y ~ s(x, k = 5),
              method.args = list(family = gaussian(link = "log")),
              geom = "area",
              alpha = 0.3, se = FALSE) +
  stat_smooth(method = "gam",
              formula = y ~ s(x, k = 5),
              method.args = list(family = gaussian(link = "log")),
              geom = "line",
              size = 1.1, se = FALSE) +
  labs(x = "Day of the year (DOY)", y = "Number of pollinator visits per\nor 5 min observation", color = "Observation", title = "") +
  theme_minimal() +
  scale_x_continuous(limits = c(min(PhenologyDandelion$DOY), max(PhenologyDandelion$DOY)),
    breaks = sort(unique(c(seq(min(PhenologyDandelion$DOY), max(PhenologyDandelion$DOY), by = 1), 131, 142)))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(name = "",
                     values = c("Tree" = "#7F646C", "Ground" = "#CC9966"),
                     labels = c("Tree" = "Apple flowers", "Ground" = "Dandelions")) +
  scale_fill_manual(name = "",
                    values = c("Tree" = "#7F646C", "Ground" = "#CC9966"),
                    labels = c("Tree" = "Apple flowers", "Ground" = "Dandelions")) +
  facet_wrap(~ Location)


ManualObs <- ggarrange(PhenoManObs, PolliManObs, nrow = 2, common.legend = TRUE, legend = "bottom", labels = c("a", "b"))


ggsave("Figures/ManualObs.png", plot = ManualObs, width = 10, height = 8, dpi = 300)







##########
############

PhenologyDandelion_New <- PhenologyDandelion %>%
  select(-Apple_variety)


Visits3_New <- Visits3 %>% 
  select(-Time, -ID, -Date) %>%
  rename(Where = Type) %>% 
  mutate(Tree = as.character(Tree))

Merged_data2 <- PhenologyDandelion_New %>%
  inner_join(Visits3_New, by = c("Tree", "Location", "DOY", "Where"))

ManualVis_per_flower <- Merged_data2 %>%
  filter(DOY >= 135 & DOY <= 142) %>%
  group_by(Location, Tree, DOY, Where, Pollinator) %>%
  summarise(
    N_visits = sum(Count, na.rm = TRUE),
    NOpen = first(NOpen),  # optional: include if you want flower counts
    .groups = "drop"
  )




ManualVis_per_flower %>%
  distinct(Location, Tree, Where, DOY, NOpen) %>%
  group_by(Location, Where) %>%
  summarise(total_NOpen = sum(NOpen, na.rm = TRUE), .groups = "drop")

ManualVis_per_flower %>%
  distinct(Location, Tree, Where, DOY, N_visits) %>%
  group_by(Location, Where) %>%
  summarise(total_N_visits = sum(N_visits, na.rm = TRUE), .groups = "drop")


###


ManualObsModel1 <- glmmTMB(N_visits ~ Where * Location + (1 | Location),
                           offset = log(NOpen),
                           family = nbinom2,
                           data = ManualVis_per_flower)

ManualObsModel1b <- glmmTMB(N_visits ~ Where * Pollinator + (1 | Location),
                           offset = log(NOpen),
                           family = nbinom2,
                           data = ManualVis_per_flower)


## DOY DOESNT WORK
ManualObsModel2 <- glmmTMB(N_visits ~ Where * (DOY + I(DOY)^2) + (1 | Location),
                           offset = log(NOpen),
                           family = nbinom2,
                           data = ManualVis_per_flower)

ManualObsModel2 <- glmmTMB(N_visits ~ Where * DOY + (1 | Location),
                           offset = log(NOpen),
                           family = nbinom2,
                           data = ManualVis_per_flower)

summary(ManualObsModel1b)
check_model(ManualObsModel1d)

emmeans(ManualObsModel1b, pairwise ~ Where * Pollinator)



## Split by pollinator
BB_visit <- ManualVis_per_flower %>% 
  filter(Pollinator == 'Bumblebee')

ManualObsModel3a <- glmmTMB(N_visits ~ Where * Location + (1 | Location),
                           offset = log(NOpen),
                           family = nbinom2,
                           data = BB_visit)

HB_visit <- ManualVis_per_flower %>% 
  filter(Pollinator == 'Honeybee')

ManualObsModel3b <- glmmTMB(N_visits ~ Where * Location + (1 | Location),
                           offset = log(NOpen),
                           family = nbinom2,
                           data = HB_visit)

SB_visit <- ManualVis_per_flower %>% 
  filter(Pollinator == 'Solitarybee')

ManualObsModel3c <- glmmTMB(N_visits ~ Where * Location + (1 | Location),
                           offset = log(NOpen),
                           family = nbinom2,
                           data = SB_visit)


summary(ManualObsModel3c)
check_model(ManualObsModel1) #not great, but not bad

emmeans(ManualObsModel3b, pairwise ~ Location)
