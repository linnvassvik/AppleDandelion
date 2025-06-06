source("R/1_ImportData.R")


#Packages
library(glmmTMB)
library(performance)
library(emmeans)




# Q1: Do pollinators prefer dandelions over apple? ------------------------

#Difference in flowers:

ModelFlower <- glmmTMB(NOpen ~ Where * Location,
                       family = nbinom2,
                       data = ManualVis_per_flower)



summary(ModelFlower)

emm <- emmeans(ModelFlower, ~ Where * Location)
pairwise_comparisons <- contrast(emm, method = "pairwise", adjust = "tukey")
summary(pairwise_comparisons)


#Difference in visits/flower during apple flowering

ManualObsModel1a <- glmmTMB(N_visits ~ Where + Location,
                            offset = log(NOpen),
                            family = nbinom2,
                            data = ManualVis_per_flower)


summary(ManualObsModel1e)

emm <- emmeans(ManualObsModel1a, ~ Location)
pairwise_comparisons <- contrast(emm, method = "pairwise", adjust = "tukey")
summary(pairwise_comparisons)



## DOY 

## Make DOY a smaller number
# ManualVis_per_flower <- ManualVis_per_flower %>%
#   mutate(DOY_c = scale(DOY, center = TRUE, scale = FALSE))  # Center only
# 
# ManualObsModel3c <- glmmTMB(N_visits ~ Where * (DOY_c + I(DOY_c^2)) + (1 | Location),
#                             offset = log(NOpen),
#                             family = nbinom2,
#                             data = ManualVis_per_flower)



ManualObsModel3b <- glmmTMB(N_visits ~ Where * DOY,
                           offset = log(NOpen),
                           family = nbinom2,
                           data = ManualVis_per_flower)


summary(ManualObsModel3b)
check_model(ManualObsModel3b)


emm <- emmeans(ManualObsModel3b, ~ pairwise ~ Where | DOY, at = list(DOY = 135:142))
pairwise_comparisons <- contrast(emm, method = "pairwise", adjust = "tukey")
summary(pairwise_comparisons)




# Q2: Are some taxanomic groups only attracted to apple or dandelions --------

#Test for different groups of pollinators (bumblebees, honeybees and solitary bees), not enough data on species
ManualObsModel2b <- glmmTMB(N_visits ~ Where * Pollinator + (1 | Location),
                            offset = log(NOpen),
                            family = nbinom2,
                            data = ManualVis_per_flower)

summary(ManualObsModel2c)

emm <- emmeans(ManualObsModel2b, ~ Where * Pollinator)
pairwise_comparisons <- contrast(emm, method = "pairwise", adjust = "tukey")
summary(pairwise_comparisons)


# ManualObsModel2c <- glmmTMB(N_visits ~ Location + Pollinator,
#                             offset = log(NOpen),
#                             family = nbinom2,
#                             data = ManualVis_per_flower)
# 
# 
# 
# 
# emm <- emmeans(ManualObsModel2c, ~ Location + Pollinator)
# pairwise_comparisons <- contrast(emm, method = "pairwise", adjust = "tukey")
# summary(pairwise_comparisons)
# 
# 

# Q3: What effect does mowing have on pollinator visitations? -------------

Model2b <- glmmTMB(N_visits ~ Apple_variety + Taxonomic_group + (1 | Location),
                  offset = log(Total_Open),
                  family = nbinom2,
                  data = Visits_per_flower)



summary(Model2b)
check_model(Model2)

emm <- emmeans(Model2b, ~ Taxonomic_group)
pairwise_comparisons <- contrast(emm, method = "pairwise", adjust = "tukey")
summary(pairwise_comparisons)


# General stats -----------------------------------------------------------

ManualVis_per_flower %>% 
  group_by(Where, Pollinator) %>% 
  summarise(Total_visits = sum(N_visits)) %>% 
  ungroup()

ManualVis_per_flower %>% 
  group_by(Where, Pollinator) %>% 
  summarise(Total_flowers = sum(NOpen)) %>% 
  ungroup()


ManualVis_per_flower %>% 
  group_by(Where, Location) %>% 
  summarise(Total_flowers = sum(NOpen)) %>% 
  ungroup()


ManualVis_per_flower %>% 
  group_by(Location, Pollinator) %>% 
  summarise(Total_visits = sum(N_visits)) %>% 
  ungroup()




# Additional --------------------------------------------------------------

# VisitsDOY <- glmmTMB(n ~ DOY * Taxanomic_group,
#                       family = nbinom2,
#                       data = Visits24_DOY)
# 
# VisitsDOY2 <- glmmTMB(n ~ DOY * Taxanomic_group + (1 | Location),
#                      family = nbinom2,
#                      data = Visits24_DOY)
# 
# summary(VisitsDOY2)
# check_model(VisitsDOY)
# 
# 
# VisitsTime <- glmmTMB(n ~ Hour * Taxanomic_group + (1 | Location),
#                       family = nbinom2,
#                       data = Visits24_Time)
# 
# summary(VisitsTime)
# check_model(VisitsTime)
# 
# 
# 
# VisitsTemp <- glmmTMB(n ~ Temperature * Taxanomic_group + (1 | Location),
#                       family = nbinom2,
#                       data = Visits24_Temp)
# 
# 
# 
# summary(VisitsTemp)
# check_model(VisitsTemp)
