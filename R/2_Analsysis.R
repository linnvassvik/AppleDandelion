source("R/1_ImportData.R")


#Packages
library(glmmTMB)
library(performance)
library(emmeans)
library(DHARMa)


# One big integration model for all research questions --------------------

OneModel2 <- glmmTMB(N_visits ~ (Where * Pollinator) + (CompApple * Pollinator) + CompDandelion + Observation + (1|Location:Tree) + (1|DOY),
                    offset = log(NOpen), 
                    family = nbinom1,
                    data = IntegrationModel_2)


summary(OneModel2) 



OneModel3 <- aov(NOpen ~ Where,
                     data = IntegrationModel_2)

summary(OneModel3)
emm <- emmeans(OneModel3, ~ Where)
pairwise_comparisons <- contrast(emm, method = "pairwise", adjust = "tukey")
summary(pairwise_comparisons)

#Model check
# simres <- simulateResiduals(OneModel)
# testDispersion(simres)
# check_model(OneModel)
# 
# testZeroInflation(simres)
# plot(simres) #DHARMa supports nbinom1 for underdispersion
# 

