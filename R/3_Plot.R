source("R/2_Analysis.R")


#Packages
library(ggeffects)
library(ggridges)
library(ggpubr)



# Q1:Do pollinators prefer dandelions over apple? --------------------

#Plot with line, not filled in
# PhenologyDandelion %>%
#   ggplot(aes(x = DOY, y = NOpen, color = Where, fill = Where)) +
#   geom_smooth(alpha = 0.2, method = "gam", method.args = list(family = quasipoisson)) +
#   labs(x = "Day of the year (DOY)", y = "Average number of open flowers") +
#   scale_x_continuous(breaks = seq(min(PhenologyDandelion$DOY), max(PhenologyDandelion$DOY), by = 1)) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   ggtitle("Number of open apple flowers on two branches and dandelions in 1x1 m quadrat") +
#   scale_color_manual(name = "",
#                      values = c("Tree" = "#7F646C", "Ground" = "#CC9966"),
#                      labels = c("Tree" = "Apple flowers", "Ground" = "Dandelions")) +
#   scale_fill_manual(name = "",
#                     values = c("Tree" = "#7F646C", "Ground" = "#CC9966"),
#                     labels = c("Tree" = "Apple flowers", "Ground" = "Dandelions"))



#Phenology of apple and visits during growing season

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
  labs(x = "Day of the year (DOY)", y = "Number of open flowers") +
  scale_x_continuous(breaks = seq(min(PhenologyDandelion$DOY), max(PhenologyDandelion$DOY), by = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("") +
  scale_color_manual(name = "",
                     values = c("Tree" = "#7F646C", "Ground" = "#CC9966"),
                     labels = c("Tree" = "Apple flowers", "Ground" = "Dandelions")) +
  scale_fill_manual(name = "",
                    values = c("Tree" = "#7F646C", "Ground" = "#CC9966"),
                    labels = c("Tree" = "Apple flowers", "Ground" = "Dandelions"))




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
  labs(x = "Day of the year (DOY)", y = "Number of pollinator visits", color = "Observation", title = "") +
  theme_minimal() +
  scale_x_continuous(limits = c(min(PhenologyDandelion$DOY), max(PhenologyDandelion$DOY)),
                     breaks = sort(unique(c(seq(min(PhenologyDandelion$DOY), max(PhenologyDandelion$DOY), by = 1), 131, 142)))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(name = "",
                     values = c("Tree" = "#7F646C", "Ground" = "#CC9966"),
                     labels = c("Tree" = "Apple flowers", "Ground" = "Dandelions")) +
  scale_fill_manual(name = "",
                    values = c("Tree" = "#7F646C", "Ground" = "#CC9966"),
                    labels = c("Tree" = "Apple flowers", "Ground" = "Dandelions"))


ManualObs <- ggarrange(PhenoManObs, PolliManObs, nrow = 2, common.legend = TRUE, legend = "bottom", labels = c("a", "b"))


ggsave("Figures/ManualObs.png", plot = ManualObs, width = 10, height = 8, dpi = 300)





#Difference in visits before and during apple flowering
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




#Plot visitation against flowers

newdata <- expand.grid(
  NOpen = seq(1, max(ManualVis_per_flower$NOpen), length.out = 100),
  Where = c("Tree", "Ground"),
  Location = unique(ManualVis_per_flower$Location))

newdata$offset <- log(newdata$NOpen)

pred <- predict(ManualObsModel1a,
                newdata = newdata,
                type = "link",
                se.fit = TRUE,
                re.form = NA)

newdata$fit <- exp(pred$fit)
newdata$lower <- exp(pred$fit - 1.96 * pred$se.fit)
newdata$upper <- exp(pred$fit + 1.96 * pred$se.fit)

AppleDandelion <- ggplot() +
  geom_ribbon(data = newdata, 
              aes(x = NOpen, ymin = lower, ymax = upper, fill = Where), 
              alpha = 0.3) +
  geom_line(data = newdata, 
            aes(x = NOpen, y = fit, color = Where), 
            size = 1.2) +
  geom_point(data = ManualVis_per_flower, 
             aes(x = NOpen, y = N_visits, color = Where), 
             alpha = 0.5, position = position_jitter(width = 2, height = 0.2)) +
  scale_color_manual(values = c("Tree" = "#7F646C", "Ground" = "#CC9966"),
                     labels = c("Tree" = "Apple flowers", "Ground" = "Dandelions")) +
  scale_fill_manual(values = c("Tree" = "#7F646C", "Ground" = "#CC9966"),
                    labels = c("Tree" = "Apple flowers", "Ground" = "Dandelions")) +
  labs(x = "Number of open flowers",
       y = "Predicted number of pollinator visits",
       color = "",
       fill = "") +
  facet_wrap(~ Location) +
  theme_minimal(base_size = 14) +
  theme(strip.text = element_text(face = "bold"))

ggsave("Figures/AppleDandelion.png", plot = AppleDandelion, width = 10, height = 6, dpi = 300)




###### DOY
newdata <- expand.grid(
  DOY = seq(min(ManualVis_per_flower$DOY), max(ManualVis_per_flower$DOY), length.out = 100),
  Where = c("Tree", "Ground"),
  NOpen = mean(ManualVis_per_flower$NOpen)  # or median
)

# Add offset
newdata$offset <- log(newdata$NOpen)

# Predict on the link (log) scale, exclude random effects (re.form = NA)
pred <- predict(ManualObsModel3b,
                newdata = newdata,
                type = "link",
                se.fit = TRUE,
                re.form = NA)

# Add predictions to newdata and back-transform
newdata$fit <- exp(pred$fit)
newdata$lower <- exp(pred$fit - 1.96 * pred$se.fit)
newdata$upper <- exp(pred$fit + 1.96 * pred$se.fit)

# Plot
VisitsDOY <- ggplot(newdata, aes(x = DOY, y = fit, color = Where, fill = Where)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
  geom_line(size = 1.2) +
  geom_point(data = ManualVis_per_flower,
             aes(x = DOY, y = N_visits, color = Where),
             alpha = 0.5, position = position_jitter(width = 0.2, height = 0.1)) +
  labs(x = "Day of the year (DOY)", y = "Number of pollinator visits per observation",
       color = "", fill = "") +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("Tree" = "#7F646C", "Ground" = "#CC9966"),
                     labels = c("Tree" = "Apple flowers", "Ground" = "Dandelions")) +
  scale_fill_manual(values = c("Tree" = "#7F646C", "Ground" = "#CC9966"),
                    labels = c("Tree" = "Apple flowers", "Ground" = "Dandelions")) +
  scale_x_continuous(breaks = seq(min(newdata$DOY), max(newdata$DOY), by = 1)) +
  ylim(0,5)


ggsave("Figures/VisitsDOY.png", plot = VisitsDOY, width = 10, height = 6, dpi = 300)






# Q2: Are some taxanomic groups only attracted to apple or dandelions --------

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



# Plot from model output
emm <- emmeans(ManualObsModel2b, ~ Where * Pollinator)


emm_df <- summary(emm, type = "response")


GenusModel2 <- ggplot(emm_df, aes(x = Pollinator, y = response, fill = Where)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(width = 0.8), width = 0.2) +
  labs(
    y = "Predicted number of pollinator visits per flower",
    x = "",
    fill = "Pollinator"
  ) +
  scale_fill_manual(name = "",
                    values = c("Tree" = "#7F646C", "Ground" = "#CC9966"),
                    labels = c("Tree" = "Apple flowers", "Ground" = "Dandelions")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 1),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 16))


ggsave(GenusModel2, filename = "Figures/GenusModel2.jpeg", height = 8, width = 13)

# Q3: What effect does mowing have on pollinator visitations? -------------


newdata <- expand.grid(
  Total_Open = seq(1, 221, by = 5),
  Apple_variety = c("Discovery", "Summerred"),
  Taxonomic_group = "Honeybee")

newdata$log_Total_Open <- log(newdata$Total_Open)

pred <- predict(Model2b, 
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
       y = "Predicted number of pollinator visits",
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
  labs(x = "Day of the year (DOY)", y = "Number of open flowers", color = "", fill = "") +
  theme_minimal(base_size = 14)

ggsave("Figures/Pheno_mowing.png", plot = Pheno_mowing, width = 10, height = 8, dpi = 300)

MowingTreatment <- ggarrange(MowingProject, Pheno_mowing, common.legend = TRUE, legend = "bottom", labels = c("a", "b"))
ggsave("Figures/MowingTreatment.png", plot = MowingTreatment, width = 10, height = 6, dpi = 300)



# Extra -------------------------------------------------------------------


GenusModel2 <- ggplot(emm_df, aes(x = Pollinator, y = response, fill = Where)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(width = 0.8), width = 0.2) +
  labs(
    y = "Predicted number of pollinator visits per flower",
    x = "",
    fill = "Pollinator"
  ) +
  scale_fill_manual(name = "",
                    values = c("Tree" = "#7F646C", "Ground" = "#CC9966"),
                    labels = c("Tree" = "Apple flowers", "Ground" = "Dandelions")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 1),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 16))


emm <- emmeans(ModelFlower, ~ Where * Location)
emm_df <- summary(emm, type = "response")

FlowersLocation <- ggplot(emm_df, aes(x = Where, y = response, fill = Where)) +
  geom_col() +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(width = 0.8), width = 0.2) +
  scale_fill_manual(name = "",
                    values = c("Tree" = "#7F646C", "Ground" = "#CC9966"),
                    labels = c("Tree" = "Apple flowers", "Ground" = "Dandelions")) +
  theme_minimal() +
  labs(x = "", y = "Predicted number of open flowers per observation") +
  theme(axis.text.x = element_blank(),
        strip.text = element_text(face = "bold")) +
  facet_wrap(~ Location,
             strip.position = "bottom",
             switch = "x")


ggsave("Figures/FlowersLocation.png", plot = FlowersLocation, width = 10, height = 6, dpi = 300)


Visits_per_flower %>% 
  mutate(Apple_variety = fct_recode(Apple_variety,
                                    "Mowing" = "Discovery",
                                    "No mowing" = "Summerred")) %>%
  ggplot(aes(x = Apple_variety, y = Total_Open, color = Apple_variety, fill = Apple_variety)) +
  geom_col() +
  scale_fill_manual(values = c("Mowing" = "#669999", "No mowing" = "#7F646C")) +
  scale_color_manual(values = c("Mowing" = "#669999", "No mowing" = "#7F646C")) +
  theme_minimal() +
  labs(x = "", y = "Number of open flowers") +
  theme(axis.text.x = element_blank(),
        strip.text = element_text(face = "bold")) +
  facet_wrap(~ Location,
             strip.position = "bottom",
             switch = "x")





# Other -------------------------------------------------------------------
#NOT USED


# Visits3_other %>% 
#   ggplot(aes(x = ))
# 
# 
# #DOY
# 
# Visits24_DOY %>% 
#   ggplot(aes(x = DOY, y = n, fill = Taxanomic_group, color = Taxanomic_group)) +
#   geom_smooth() +
#   labs(x = "Day of year (DOY)", y = "Number of pollinators active", fill = "", color = "") +
#   scale_fill_manual(values = c("#FFAC81", "#B74F6F", "#FEC3A6")) +
#   scale_color_manual(values = c("#FFAC81","#B74F6F", "#FEC3A6")) +
#   scale_x_continuous(breaks = sort(unique(Visits24_DOY$DOY))) +
#   theme_minimal()
#   
#   
# # 1. Create new data frame for prediction: all DOY values and all Taxanomic_groups
# newdata <- expand.grid(
#   DOY = seq(min(Visits24_DOY$DOY), max(Visits24_DOY$DOY), by = 1),
#   Taxanomic_group = unique(Visits24_DOY$Taxanomic_group),
#   Location = NA   # random effect set to zero
# )
# 
# # 2. Predict on newdata (type = "response" to get counts), with standard errors
# preds <- predict(VisitsDOY2, newdata = newdata, type = "response", se.fit = TRUE)
# 
# newdata <- newdata %>%
#   mutate(
#     fit = preds$fit,
#     se = preds$se.fit,
#     lower = fit - 1.96 * se,
#     upper = fit + 1.96 * se
#   )
# 
# # 3. Plot model predictions with ribbons and lines, using your color scheme
# DOYActivity <- ggplot(newdata, aes(x = DOY, y = fit, color = Taxanomic_group, fill = Taxanomic_group)) +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
#   geom_line(size = 1) +
#   labs(x = "Day of year (DOY)", y = "", fill = "", color = "") +
#   scale_fill_manual(values = c("#B74F6F", "#FFAC81", "#FEC3A6")) +
#   scale_color_manual(values = c("#B74F6F", "#FFAC81", "#FEC3A6")) +
#   scale_x_continuous(breaks = sort(unique(Visits24_DOY$DOY))) +
#   theme_minimal()
# 
# ggsave("Figures/DOYActivity.png", plot = DOYActivity, width = 10, height = 8, dpi = 300)
# 
# ## Time
# 
#   
# Visits24_Time %>%
#   mutate(Hour = hour(hm(Time_rounded))) %>%
#   ggplot(aes(x = Hour, y = n, color = Taxanomic_group, fill = Taxanomic_group)) +
#   geom_smooth() +
#   scale_x_continuous(
#     breaks = seq(0, 23, by = 2),
#     labels = sprintf("%02d:00", seq(0, 23, by = 2))) +
#   labs(x = "Hour of day", y = "Number of pollinators active", fill = "", color = "") +
#   scale_fill_manual(values = c("#FFAC81", "#B74F6F", "#FEC3A6")) +
#   scale_color_manual(values = c("#FFAC81","#B74F6F", "#FEC3A6")) +
#   theme_minimal()
# 
# 
# 
# 
# 
# # 1. Create new data frame for prediction: all DOY values and all Taxanomic_groups
# newdata <- expand.grid(
#   Hour = seq(min(Visits24_Time$Hour), max(Visits24_Time$Hour), by = 1),
#   Taxanomic_group = unique(Visits24_Time$Taxanomic_group),
#   Location = NA   # random effect set to zero
# )
# 
# # 2. Predict on newdata (type = "response" to get counts), with standard errors
# preds <- predict(VisitsTime, newdata = newdata, type = "response", se.fit = TRUE)
# 
# newdata <- newdata %>%
#   mutate(
#     fit = preds$fit,
#     se = preds$se.fit,
#     lower = fit - 1.96 * se,
#     upper = fit + 1.96 * se
#   )
# 
# # 3. Plot model predictions with ribbons and lines, using your color scheme
# TimeActivity <- ggplot(newdata, aes(x = Hour, y = fit, color = Taxanomic_group, fill = Taxanomic_group)) +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
#   geom_line(size = 1) +
#   labs(x = "Time of day", y = "Predicted number of pollinators active", fill = "", color = "") +
#   scale_fill_manual(values = c("#FFAC81", "#B74F6F", "#FEC3A6")) +
#   scale_color_manual(values = c("#FFAC81","#B74F6F", "#FEC3A6")) +
#   scale_x_continuous(breaks = sort(unique(Visits24_Time$Hour))) +
#   theme_minimal()
# 
# 
# ggsave("Figures/TimeActivity.png", plot = TimeActivity, width = 10, height = 8, dpi = 300)
# 
# 
# 
# 
# 
# Visits24_Temp %>% 
#   ggplot(aes(x = Temperature, y = n, fill = Taxanomic_group, color = Taxanomic_group))+
#   geom_smooth() +
#   labs(x = "Temperature (Celsius)", y = "Number of pollinators active", fill = "", color = "") +
#   scale_fill_manual(values = c("#FFAC81", "#B74F6F", "#FEC3A6")) +
#   scale_color_manual(values = c("#FFAC81","#B74F6F", "#FEC3A6")) +
#   scale_x_continuous(breaks = seq(floor(min(Visits24_Temp$Temperature, na.rm = TRUE)),
#                  ceiling(max(Visits24_Temp$Temperature, na.rm = TRUE)), by = 3)) +
#   theme_minimal()
# 
# 
# 
# 
# # 1. Create new data frame for prediction: all DOY values and all Taxanomic_groups
# newdata <- expand.grid(
#   Temperature = seq(min(Visits24_Temp$Temperature), max(Visits24_Temp$Temperature), by = 1),
#   Taxanomic_group = unique(Visits24_Temp$Taxanomic_group),
#   Location = NA   # random effect set to zero
# )
# 
# # 2. Predict on newdata (type = "response" to get counts), with standard errors
# preds <- predict(VisitsTemp, newdata = newdata, type = "response", se.fit = TRUE)
# 
# newdata <- newdata %>%
#   mutate(
#     fit = preds$fit,
#     se = preds$se.fit,
#     lower = fit - 1.96 * se,
#     upper = fit + 1.96 * se
#   )
# 
# # 3. Plot model predictions with ribbons and lines, using your color scheme
# TempActivity <- ggplot(newdata, aes(x = Temperature, y = fit, color = Taxanomic_group, fill = Taxanomic_group)) +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
#   geom_line(size = 1) +
#   labs(x = "Temperature (Celsius)", y = "", fill = "", color = "") +
#   scale_fill_manual(values = c("#B74F6F", "#FFAC81", "#FEC3A6")) +
#   scale_color_manual(values = c("#B74F6F", "#FFAC81", "#FEC3A6")) +
#   scale_x_continuous(breaks = sort(unique(Visits24_Temp$Temperature))) +
#   theme_minimal()
# 
# ggsave("Figures/TempActivity.png", plot = TempActivity, width = 10, height = 8, dpi = 300)
# 
# 
# Activity_Camera <- ggarrange(DOYActivity, TimeActivity, TempActivity, nrow = 3, common.legend = TRUE, legend = "bottom", labels = c("a", "b", "C"), label.y = 1.07, label.x = 0.02)
# 
# ggsave("Figures/Activity_Camera.png", plot = Activity_Camera, width = 10, height = 10, dpi = 300)




