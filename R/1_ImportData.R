#Data for paper on dandelions effect of apple flower pollinators 

#Packages
library(readxl)
library(tidyverse)
library(lubridate)
library(hms)



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

PhenologyCombined <- bind_rows(PhenoApple, PhenoDand)

PhenologyDandelion <- PhenologyCombined %>% 
  mutate(NTot = (NBuds + NOpen + NWithered)) %>% 
  mutate(Percentage = (NOpen / NTot) * 100)


#Pollinator visits

Visits <- read_excel("Data/Visits2.xlsx") %>% 
  

Visits2 <- Visits %>% 
  select(-Comment, -Weather) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(Date = dmy(Date)) %>% 
  filter(row_number() != 445) %>% 
  mutate(DOY = yday(Date))

Visits3a <- Visits2 %>% 
  select(-Other, -'Andrena cineraria', -'Andrena haemorrhoa', -'Bombus pratorum', -'Andrena sp', -'Bombus sensu stricto', -'Bombus hypnorum', -'Lasioglossum sp', -'Andrena scotica', -'Andrena nigroaena', - Hoverfly, -Fly) 

Visits3 <- Visits3a %>% 
  pivot_longer(cols = Honeybee:Solitarybee, names_to = "Pollinator", values_to = "Count")


#Species data
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


#Dataframes to test difference in visits/flower on dandelions and apple during apple flowering
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



## Include time


ManualVis_per_flower %>%
  distinct(Location, Tree, Where, DOY, NOpen) %>%
  group_by(Location, Where) %>%
  summarise(total_NOpen = sum(NOpen, na.rm = TRUE), .groups = "drop")

ManualVis_per_flower %>%
  distinct(Location, Tree, Where, DOY, N_visits) %>%
  group_by(Location, Where) %>%
  summarise(total_N_visits = sum(N_visits, na.rm = TRUE), .groups = "drop")



#Dataframe to look at differences in pollinator visits before and during apple flowering
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


# Camera observations on apples with mowing treatments --------------------

Visits24 <- read_excel("Data/PollinatorVisitation_Mowing project_2024.xlsx") %>% 
  filter(Taxanomic_group != 'Hoverfly') %>% 
  filter(Taxanomic_group != 'Butterfly') %>% 
  select(c(-Comments, -Comments_picture,)) %>%
  mutate(Time = as_hms(Time)) %>% 
  mutate(DOY = dmy(Date),DOY = yday(DOY)) %>% 
  mutate(Tree = as.character(Tree)) 


#From 2023 where they mowed both discovery and summerred
# Visits23 <- read_excel("Data/PolliObs_2023.xlsx") %>% 
#   filter(Taxanomic_group != 'Hoverfly') %>% 
#   filter(Taxanomic_group != 'Butterfly') %>% 
#   filter(Taxanomic_group != 'Fly') %>% 
#   filter(Location != 'Hoyen') %>% 
#   filter(Apple_variety != 'Aroma') %>% 
#   select(c(-Cluster_number, -Groups, -Comments, -Comments_picture)) %>%
#   mutate(Time = as_hms(Time)) %>% 
#   mutate(DOY = dmy(Date),DOY = yday(DOY))


Visits24 %>%
  group_by(Tree, Location, Apple_variety, Taxanomic_group, DOY) %>%
  summarise(visits = n(), .groups = "drop")

Visits24 %>%
  group_by(Location, Apple_variety) %>%
  summarise(visits = n(), .groups = "drop")

# Visits23 %>%
#   group_by(Location, Apple_variety) %>%
#   summarise(visits = n(), .groups = "drop")



#Import phenology data, but filtered for only discovery and summerred (mowing treatments)

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



#Merge phenology and visits

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




# Other -------------------------------------------------------------------

#Reuploading visits as a csv to get time correct

Visits_other <- read.csv("Data/Visits2.csv")
  
  
Visits2_other <- Visits_other %>% 
  select(-Comment, -Weather) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(Date = dmy(Date)) %>% 
  filter(row_number() != 445) %>% 
  mutate(DOY = yday(Date))

Visits3a_other <- Visits2_other %>% 
  select(-Other, -'Andrena.cineraria', -'Andrena.haemorrhoa', -'Bombus.pratorum', -'Andrena.sp', -'Bombus.sensu.stricto', -'Bombus.hypnorum', -'Lasioglossum.sp', -'Andrena.scotica', -'Andrena.nigroaena', - Hoverfly, -Fly) 

Visits3_other <- Visits3a_other %>% 
  pivot_longer(cols = Honeybee:Solitarybee, names_to = "Pollinator", values_to = "Count")
