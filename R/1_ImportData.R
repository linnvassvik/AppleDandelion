#Data for paper on dandelions effect of apple flower pollinators 

#Packages
library(readxl)
library(tidyverse) 
library(lubridate)
library(hms)
library(tidylog)



# Manual observations on apple and dandelion ------------------------------

#Phenology, same as for Camera_mowing treatment, but with Summerred and ground instead

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

PhenoDand <- PhenologyDandelion2 %>% 
  filter(Where == 'Ground') %>% 
  select(-Branch, -Time, -Date, -POpen)

PhenoApple <- PhenologyDandelion2 %>% 
  filter(Where == 'Tree') %>% 
  group_by(Location, Apple_variety, Where, Tree, DOY) %>%
  summarise(NBuds = sum(NBuds, na.rm = TRUE),
            NOpen = sum(NOpen, na.rm = TRUE),
            NWithered = sum(NWithered, na.rm = TRUE)) %>%
  ungroup() 

PhenologyCombined <- bind_rows(PhenoApple, PhenoDand) %>% 
  mutate(NTot = (NBuds + NOpen + NWithered)) %>% 
  mutate(Percentage = (NOpen / NTot) * 100) %>%
  select(-Apple_variety)


#Pollinator visits

Visits <- read_excel("Data/Visits2.xlsx") %>% 
  select(-Comment, -Weather) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(Date = dmy(Date)) %>% 
  filter(row_number() != 445) %>% 
  mutate(DOY = yday(Date)) 

Visits_group <- Visits %>% 
  select(-Other, -'Andrena cineraria', -'Andrena haemorrhoa', -'Bombus pratorum', -'Andrena sp', -'Bombus sensu stricto', -'Bombus hypnorum', -'Lasioglossum sp', -'Andrena scotica', -'Andrena nigroaena', - Hoverfly, -Fly) 

Visits_pivot <- Visits_group %>% 
  pivot_longer(cols = Honeybee:Solitarybee, names_to = "Pollinator", values_to = "Count")



#Dataframes to test difference in visits/flower on dandelions and apple during apple flowering
Visits_group_DOY <- Visits_pivot %>% 
  select(-Time, -ID, -Date) %>%
  rename(Where = Type) %>% 
  mutate(Tree = as.character(Tree)) %>% 
  group_by(Where, Location, Tree, DOY, Pollinator) %>%
  summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = "Pollinator", values_from = "Count")

#Add missing pheology for dandelions on DPY 135, just used same values as for DOY 136 since DOY 135 is the first day of revordings
new_rows <- expand.grid(Location = "Berle", Tree = "2", DOY = "135",
  Where = "Dandelion", Pollinator = c("Bumblebee", "Honeybee", "Solitary"),
  stringsAsFactors = FALSE) %>%
  mutate(N_visits = 0, NOpen = 18, Observation = "manual")


ManualVis_per_flower  <- PhenologyCombined %>%
  left_join(Visits_group_DOY, by = c("Tree", "Location", "DOY", "Where")) %>% 
  mutate(Bumblebee = replace_na(Bumblebee, 0),
         Honeybee = replace_na(Honeybee, 0),
         Solitarybee = replace_na(Solitarybee, 0)) %>% 
  pivot_longer(cols = c(Bumblebee, Honeybee, Solitarybee), names_to = "Pollinator", values_to = "Count") %>%
  filter(DOY >= 135 & DOY <= 142) %>%
  group_by(Location, Tree, DOY, Where, Pollinator) %>%
  summarise(N_visits = sum(Count, na.rm = TRUE),
            NOpen = first(NOpen), .groups = "drop") %>% 
  mutate(Observation = "manual") %>% 
  mutate(Where = if_else(Where == "Tree", "Summerred", Where)) %>% 
  mutate(Where = if_else(Where == "Ground", "Dandelion", Where)) %>%
  mutate(Pollinator = if_else(Pollinator == "Solitarybee", "Solitary", Pollinator),
         DOY = as.factor(DOY)) %>% 
  bind_rows(new_rows) %>%
  arrange(Location, Tree, as.numeric(as.character(DOY)), Where, Pollinator) %>% 
  mutate(DOY = as.factor(DOY))



# Camera observations on apples with mowing treatments --------------------

Visits24 <- read_excel("Data/PollinatorVisitation_Mowing project_2024.xlsx") %>% 
  filter(Taxanomic_group != 'Hoverfly') %>% 
  filter(Taxanomic_group != 'Butterfly') %>% 
  select(-Comments, -Comments_picture) %>%
  mutate(Time = as_hms(Time),
         DOY = dmy(Date),
         DOY = yday(DOY),
         Tree = as.character(Tree),
         Count = if_else(is.na(Taxanomic_group), 0, 1)) %>% 
  group_by(Location, Tree, Apple_variety, Temperature, Date, Time, Foraging, DOY, Taxanomic_group) %>%
  summarise(Count = sum(Count), .groups = "drop") %>%
  pivot_wider(names_from = "Taxanomic_group", values_from = "Count") %>%
  mutate(Bumblebee = if_else(is.na(Bumblebee), 0, if_else(Bumblebee > 0, 1, 0)),
         Honeybee  = if_else(is.na(Honeybee), 0, if_else(Honeybee > 0, 1, 0)),
         Solitary  = if_else(is.na(Solitary), 0, if_else(Solitary > 0, 1, 0)),
         Unknown   = if_else(is.na(Unknown), 0, if_else(Unknown > 0, 1, 0))) %>% 
  select(-Time, -Date, -Foraging, -Temperature)


#Import phenology data, but filtered for only discovery and summerred (mowing treatments)

Phenology24 <- read_excel("Data/Phenology_SRandDisc.xlsx") %>% 
  select(-ID, -Weather, -Other_flowers, -Comment) %>% 
  filter(Where == 'Tree') %>% 
  mutate(Time = as_hms(Time)) %>% 
  mutate(DOY = dmy(Date),DOY = yday(DOY)) %>% 
  rename(Apple_variety = Species) %>%
  group_by(Location, Apple_variety, Tree, DOY) %>%
  summarise(Total_Buds = sum(`#Buds`, na.rm = TRUE),
            Total_Open = sum(`#Open`, na.rm = TRUE),
            Total_Withered = sum(`#Withered`, na.rm = TRUE)) %>%
  ungroup()


Merged_camera <- Phenology24 %>%
  full_join(Visits24, by = c("Tree", "Location", "Apple_variety", "DOY")) %>%
  group_by(Location, Apple_variety, Tree) %>% 
  mutate(Total_Open = approx(DOY, Total_Open, DOY, rule = 2, ties = mean)$y,
         Total_Buds = approx(DOY, Total_Buds, DOY, rule = 2, ties = mean)$y,
         Total_Withered = approx(DOY, Total_Withered, DOY, rule = 2,ties = mean)$y,
         Bumblebee = replace_na(Bumblebee, 0),
         Honeybee  = replace_na(Honeybee, 0),
         Solitary  = replace_na(Solitary, 0),
         Unknown   = replace_na(Unknown, 0)) %>%
  ungroup()



CameraVis_per_flower <- Merged_camera %>% 
  pivot_longer(cols = c(Bumblebee, Honeybee, Solitary, Unknown), 
               names_to = "Pollinator", values_to = "Count") %>%  
  group_by(Location, Apple_variety, Tree, DOY, Pollinator, Total_Open) %>% 
  summarise(N_visits = sum(Count, na.rm = TRUE)) %>%
  filter(DOY >= 135 & DOY <= 142) %>% 
  mutate(Observation = "camera") %>% 
  rename(NOpen = "Total_Open", Where = "Apple_variety") %>% 
  mutate(DOY = as.factor(DOY))



# Combinde camera and manual ----------------------------------------------
IntegrationModel <- bind_rows(ManualVis_per_flower, CameraVis_per_flower) %>%
  group_by(Location, Tree, DOY) %>%
  mutate(NOpen_Summerred = NOpen[Where == "Summerred"][1],
         NOpen_Discovery = NOpen[Where == "Discovery"][1],
         NOpen_Dandelion = NOpen[Where == "Dandelion"][1], 
         CompApple = case_when(Observation == "manual" & Where == "Dandelion" ~ NOpen[Where == "Summerred"][1],
                               Observation == "manual" & Where %in% c("Summerred", "Discovery") ~ 0,
                               TRUE ~ NA_real_),
         CompDandelion = case_when((Observation == "manual" | Observation == "camera") & Where == "Summerred" ~ NOpen_Dandelion,
                                   (Observation == "manual" | Observation == "camera") & Where == "Discovery" ~ 0,
                                   Observation == "manual" & Where == "Dandelion" ~ 0,
                                   TRUE ~ NA_real_)) %>%
  ungroup() %>% 
  filter(!(Location == "Høyen" & Observation == "camera"))
  

#Get missing values for DOY138 interpolated from other days
DOY138values <- IntegrationModel %>%
  filter(Observation == "manual", Where == "Summerred") %>%
  group_by(Location, Tree, Pollinator) %>%
  arrange(as.numeric(as.character(DOY))) %>%
  summarise(NOpen_Dandelion_138 = approx(x = as.numeric(as.character(DOY)), y = NOpen_Dandelion, xout = 138)$y, .groups = "drop")



#Import IntegrationModel with DOY138
IntegrationModel_2 <- read_excel("Excel/IntegrationModel_wDOY138.xlsx") %>% 
  mutate(CompApple = replace_na(CompApple, 0),
         DOY = as.factor(DOY)) 


IntegrationModel_2 <- IntegrationModel_2 %>%
  filter(NOpen != 0) %>% 
  filter(Pollinator != 'Unknown')

