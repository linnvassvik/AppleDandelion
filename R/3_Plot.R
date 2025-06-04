source("R/2_Analysis.R")


#Packages
library(ggplot2)
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



# Q3: What effect does mowing have on pollinator visitations? -------------

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

