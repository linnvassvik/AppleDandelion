source("R/1_ImportData.R")


#Packages
library(glmmTMB)
library(performance)
library(emmeans)




# Q1: Do pollinators prefer dandelions over apple? ------------------------

#Difference in visits/flower during apple flowering

ManualObsModel1a <- glmmTMB(N_visits ~ Where + Location,
                            offset = log(NOpen),
                            family = nbinom2,
                            data = ManualVis_per_flower)

ManualObsModel1b <- glmmTMB(N_visits ~ Where * Location,
                            offset = log(NOpen),
                            family = nbinom2,
                            data = ManualVis_per_flower)

anova(ManualObsModel1d, ManualObsModel1e)


summary(ManualObsModel1e)
emmeans(ManualObsModel1d, pairwise ~ Location)



## DOY 
ManualObsModel3a <- glmmTMB(N_visits ~ Where * I(DOY^2) + (1 | Location),
                           offset = log(NOpen),
                           family = nbinom2,
                           data = ManualVis_per_flower)


## Make DOY a smaller number
ManualVis_per_flower <- ManualVis_per_flower %>%
  mutate(DOY_c = scale(DOY, center = TRUE, scale = FALSE))  # Center only

ManualObsModel3c <- glmmTMB(N_visits ~ Where * (DOY_c + I(DOY_c^2)) + (1 | Location),
                            offset = log(NOpen),
                            family = nbinom2,
                            data = ManualVis_per_flower)



ManualObsModel3b <- glmmTMB(N_visits ~ Where * DOY,
                           offset = log(NOpen),
                           family = nbinom2,
                           data = ManualVis_per_flower)


AIC(ManualObsModel3a, ManualObsModel3c, ManualObsModel3b)

#Model 3c highest AIC but best model fit

summary(ManualObsModel3b)
check_model(ManualObsModel3b)

emmeans(ManualObsModel3b, pairwise ~ Where * DOY)
emmeans(ManualObsModel3b, pairwise ~ Where | DOY, at = list(DOY = 135:142))





# Q2: Are some taxanomic groups only attracted to apple or dandelions --------

#Test for different groups of pollinators (bumblebees, honeybees and solitary bees), not enough data on species
ManualObsModel2b <- glmmTMB(N_visits ~ Where * Pollinator + (1 | Location),
                            offset = log(NOpen),
                            family = nbinom2,
                            data = ManualVis_per_flower)

emmeans(ManualObsModel2b, pairwise ~ Pollinator)

ManualObsModel2c <- glmmTMB(N_visits ~ Location + Pollinator,
                            offset = log(NOpen),
                            family = nbinom2,
                            data = ManualVis_per_flower)

emm <- emmeans(ManualObsModel2c, ~ Location * Pollinator, type = "response")
pairs(emm)

summary(ManualObsModel2c)



##NOT USED IN PAPER
BB_visit <- ManualVis_per_flower %>% 
  filter(Pollinator == 'Bumblebee')

ManualObsModel4a <- glmmTMB(N_visits ~ Where * Location + (1 | Location),
                            offset = log(NOpen),
                            family = nbinom2,
                            data = BB_visit)

HB_visit <- ManualVis_per_flower %>% 
  filter(Pollinator == 'Honeybee')

ManualObsModel4b <- glmmTMB(N_visits ~ Where * Location + (1 | Location),
                            offset = log(NOpen),
                            family = nbinom2,
                            data = HB_visit)

SB_visit <- ManualVis_per_flower %>% 
  filter(Pollinator == 'Solitarybee')

ManualObsModel4c <- glmmTMB(N_visits ~ Where * Location + (1 | Location),
                            offset = log(NOpen),
                            family = nbinom2,
                            data = SB_visit)


summary(ManualObsModel5)
check_model(ManualObsModel1) #not great, but not bad

emmeans(ManualObsModel5, pairwise ~ Pollinator)


ManualObsModel5 <- glmmTMB(N_visits ~ Pollinator,
                           offset = log(NOpen),
                           family = nbinom2,
                           data = ManualVis_per_flower)


# Q3: What effect does mowing have on pollinator visitations? -------------


Model2 <- glmmTMB(N_visits ~ Apple_variety * Taxonomic_group + (1 | Location),
                  offset = log(Total_Open),
                  family = nbinom2,
                  data = Visits_per_flower)

Model2b <- glmmTMB(N_visits ~ Apple_variety + Taxonomic_group + (1 | Location),
                  offset = log(Total_Open),
                  family = nbinom2,
                  data = Visits_per_flower)


summary(Model2)
check_model(Model2)

emmeans(Model2, pairwise ~ Apple_variety * Taxonomic_group)
emmeans(Model2, pairwise ~ Taxonomic_group)



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



ModelFlower <- glmmTMB(NOpen ~ Where * Location,
                        family = nbinom2,
                        data = ManualVis_per_flower)



summary(ModelPollinators)
emmeans(ModelPollinators, pairwise ~ Pollinator * Location)

# Additional --------------------------------------------------------------

VisitsDOY <- glmmTMB(n ~ DOY * Taxanomic_group,
                      family = nbinom2,
                      data = Visits24_DOY)

VisitsDOY2 <- glmmTMB(n ~ DOY * Taxanomic_group + (1 | Location),
                     family = nbinom2,
                     data = Visits24_DOY)

summary(VisitsDOY2)
check_model(VisitsDOY)


VisitsTime <- glmmTMB(n ~ Hour * Taxanomic_group + (1 | Location),
                      family = nbinom2,
                      data = Visits24_Time)

summary(VisitsTime)
check_model(VisitsTime)



VisitsTemp <- glmmTMB(n ~ Temperature * Taxanomic_group + (1 | Location),
                      family = nbinom2,
                      data = Visits24_Temp)



summary(VisitsTemp)
check_model(VisitsTemp)
