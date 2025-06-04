source("R/1_ImportData.R")


#Packages
library(glmmTMB)
library(performance)
library(emmeans)




# Q1: Do pollinators prefer dandelions over apple? ------------------------

#Difference in visits/flower during apple flowering

ManualObsModel1a <- glmmTMB(N_visits ~ Where + (1 | Location),
                           offset = log(NOpen),
                           family = nbinom2,
                           data = ManualVis_per_flower)

ManualObsModel1b <- glmmTMB(N_visits ~ Where + Location + (1 | Location),
                            offset = log(NOpen),
                            family = nbinom2,
                            data = ManualVis_per_flower)

ManualObsModel1c <- glmmTMB(N_visits ~ Where * Location + (1 | Location),
                            offset = log(NOpen),
                            family = nbinom2,
                            data = ManualVis_per_flower)

ManualObsModel1d <- glmmTMB(N_visits ~ Where + Location,
                            offset = log(NOpen),
                            family = nbinom2,
                            data = ManualVis_per_flower)

ManualObsModel1e <- glmmTMB(N_visits ~ Where * Location,
                            offset = log(NOpen),
                            family = nbinom2,
                            data = ManualVis_per_flower)

AIC(ManualObsModel1a, ManualObsModel1b, ManualObsModel1c, ManualObsModel1d, ManualObsModel1e) #Model1d lowest AIC, but less than 2 between 1b. Interaction not significant

summary(ManualObsModel1d)
emmeans(ManualObsModel1d, pairwise ~ Location)


ManualObsModel2b <- glmmTMB(N_visits ~ Where * Pollinator + (1 | Location),
                            offset = log(NOpen),
                            family = nbinom2,
                            data = ManualVis_per_flower)


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



## Split by pollinator
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


summary(ManualObsModel3c)
check_model(ManualObsModel1) #not great, but not bad

emmeans(ManualObsModel3b, pairwise ~ Location)




# Q2: Are some taxanomic groups only attracted to apple or dandelions --------

#Test for different groups of pollinators (bumblebees, honeybees and solitary bees), not enough data on species
ModelVisit2 <- glmmTMB(Count ~ Type * Pollinator + (1 | Location),
                       family = nbinom2,
                       data = Visits3)

summary(ModelVisit2)
check_model(ModelVisit2)

emmeans(ModelVisit2, pairwise ~ Type * Pollinator)
emmeans(ModelVisit2, pairwise ~ Pollinator)



# Q3: What effect does mowing have on pollinator visitations? -------------

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



# Additional --------------------------------------------------------------

VisitsDOY <- glmmTMB(N_visits ~ Where * DOY + (1 | Location),
                            family = nbinom2,
                            data = ManualVis_per_flower)
summary(VisitsDOY)

VisitsDOY <- glmmTMB(N_visits ~ Where * DOY + (1 | Location),
                     family = nbinom2,
                     data = ManualVis_per_flower)
summary(VisitsDOY)



