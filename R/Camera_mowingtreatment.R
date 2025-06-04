library(tidyverse)
library(readxl)
library(glmmTMB)
library(performance)
library(lubridate)
library(emmeans)
library(ggeffects)
library(ggridges)


# Upload data on visits mow and not mow -----------------------------------

Visits24 <- read_excel("Data/PollinatorVisitation_Mowing project_2024.xlsx") %>% 
  filter(Taxanomic_group != 'Hoverfly') %>% 
  filter(Taxanomic_group != 'Butterfly') %>% 
  select(c(-Comments, -Comments_picture,)) %>%
  mutate(Time = as_hms(Time)) %>% 
  mutate(DOY = dmy(Date),DOY = yday(DOY)) %>% 
  mutate(Tree = as.character(Tree)) 
#From 2023 where they mowed both discovery and summerred
Visits23 <- read_excel("Data/PolliObs_2023.xlsx") %>% 
  filter(Taxanomic_group != 'Hoverfly') %>% 
  filter(Taxanomic_group != 'Butterfly') %>% 
  filter(Taxanomic_group != 'Fly') %>% 
  filter(Location != 'Hoyen') %>% 
  filter(Apple_variety != 'Aroma') %>% 
  select(c(-Cluster_number, -Groups, -Comments, -Comments_picture)) %>%
  mutate(Time = as_hms(Time)) %>% 
  mutate(DOY = dmy(Date),DOY = yday(DOY))


bee_counts24 <- Visits24 %>%
  group_by(Tree, Location, Apple_variety, Taxanomic_group, DOY) %>%
  summarise(visits = n(), .groups = "drop")

Visits24 %>%
  group_by(Location, Apple_variety) %>%
  summarise(visits = n(), .groups = "drop")

Visits23 %>%
  group_by(Location, Apple_variety) %>%
  summarise(visits = n(), .groups = "drop")



# Upload data on phenology SR and Disc ------------------------------------

Phenology24 <- read_excel("Data/Phenology_SRandDisc.xlsx") %>% 
  select(-ID, -Weather, -Other_flowers, -Comment) %>% 
  filter(Where == 'Tree') %>% 
  mutate(Time = as_hms(Time)) %>% 
  mutate(DOY = dmy(Date),DOY = yday(DOY)) %>% 
  rename(Apple_variety = Species)
  

Phenology24 <- Phenology24 %>%
  group_by(Location, Apple_variety, Tree, DOY) %>%
  summarise(Total_Buds = sum(`#Buds`, na.rm = TRUE),
    Total_Open = sum(`#Open`, na.rm = TRUE),
    Total_Withered = sum(`#Withered`, na.rm = TRUE),
    Avg_Percent_Open = mean(`%Open`, na.rm = TRUE)) %>%
  ungroup()



# Merge visits and phenology ----------------------------------------------

Merged_data <- Phenology24 %>%
  inner_join(Visits24, by = c("Tree", "Location", "Apple_variety", "DOY"))

#Get visits/flower for each taxanomic group, day, location, variety and tree

Visits_per_flower <- Merged_data %>%
  group_by(Location, Apple_variety, Tree, DOY, Taxanomic_group) %>%
  summarise(
    N_visits = n(),
    Total_Open = first(Total_Open), 
    Visits_per_flower = N_visits / Total_Open) %>%
  ungroup() %>% 
  rename(Taxonomic_group = Taxanomic_group)







Model1 <- glmmTMB(N_visits ~ Apple_variety + (1 | Location),
                  offset = log(Total_Open),
                  family = nbinom2,
                  data = Visits_per_flower)


summary(Model1)
check_model(Model1)



Model2 <- glmmTMB(N_visits ~ Apple_variety * Taxonomic_group + (1 | Location),
                  offset = log(Total_Open),
                  family = nbinom2,
                  data = Visits_per_flower)

Model3 <- glmmTMB(N_visits ~ Apple_variety + (1 | Location),
                  offset = log(Total_Open),
                  family = nbinom2,
                  data = Visits_per_flower)

Model4 <- glmmTMB(N_visits ~ Taxonomic_group + (1 | Location),
                  offset = log(Total_Open),
                  family = nbinom2,
                  data = Visits_per_flower)


summary(Model2)
check_model(Model2)

emmeans(Model2, pairwise ~ Apple_variety * Taxonomic_group)
emmeans(Model2, pairwise ~ Taxonomic_group)






#Plot model output for mowing vs no mowing


newdata <- expand.grid(
  Total_Open = seq(1, 221, by = 5),
  Apple_variety = c("Discovery", "Summerred"))

newdata$log_Total_Open <- log(newdata$Total_Open)

pred <- predict(Model3, 
                newdata = newdata, 
                type = "link", 
                se.fit = TRUE, 
                re.form = NA)

newdata$fit <- exp(pred$fit)
newdata$lower <- exp(pred$fit - 1.96 * pred$se.fit)
newdata$upper <- exp(pred$fit + 1.96 * pred$se.fit)

newdata$Mowing <- ifelse(newdata$Apple_variety == "Discovery", "Mowing", "No mowing")
Visits_per_flower$Mowing <- ifelse(Visits_per_flower$Apple_variety == "Discovery", "Mowing", "No mowing")


MowingProject <- ggplot() +
  geom_ribbon(data = newdata, 
              aes(x = Total_Open, ymin = lower, ymax = upper, fill = Mowing), 
              alpha = 0.3) +
  geom_line(data = newdata, 
            aes(x = Total_Open, y = fit, color = Mowing), 
            size = 1.2) +
  geom_point(data = Visits_per_flower, 
             aes(x = Total_Open, y = N_visits, color = Mowing), 
             alpha = 0.5, position = position_jitter(width = 2, height = 0.2)) +
  scale_color_manual(values = c("Mowing" = "#669999", "No mowing" = "#7F646C")) +
  scale_fill_manual(values = c("Mowing" = "#669999", "No mowing" = "#7F646C")) +
  labs(x = "Number of open apple flowers",
    y = "Number of pollinator visits",
    color = "",
    fill = "") +
  theme_minimal(base_size = 14)

ggsave("Figures/MowingProject_plot.png", plot = MowingProject, width = 10, height = 8, dpi = 300)



#Dandelion: #CC9966, Discovery: #669999, Summerred: #7F646C



#plot over phenology
Phenology24 <- Phenology24 %>% 
  mutate(Total_flowers = Total_Buds + Total_Withered)


Pheno_mowing <- Phenology24 %>%
  mutate(Apple_variety = fct_recode(Apple_variety,
                                    "Mowing" = "Discovery",
                                    "No mowing" = "Summerred")) %>%
  ggplot(aes(x = DOY, y = Total_Open, fill = Apple_variety, color = Apple_variety)) +
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
  scale_fill_manual(values = c("Mowing" = "#669999", "No mowing" = "#7F646C")) +
  scale_color_manual(values = c("Mowing" = "#669999", "No mowing" = "#7F646C")) +
  scale_x_continuous(breaks = seq(min(Phenology24$DOY), max(Phenology24$DOY), by = 1)) +
  labs(x = "Day of year (DOY)", y = "Number of open flowers", color = "", fill = "") +
  theme_minimal(base_size = 14)

ggsave("Figures/Pheno_mowing.png", plot = Pheno_mowing, width = 10, height = 8, dpi = 300)

MowingTreatment <- ggarrange(MowingProject, Pheno_mowing, common.legend = TRUE, legend = "bottom", labels = c("a", "b"))
ggsave("Figures/MowingTreatment.png", plot = MowingTreatment, width = 10, height = 6, dpi = 300)
