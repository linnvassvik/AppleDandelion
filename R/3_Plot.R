source("R/2_Analsysis.R")


#Packages
library(ggeffects)
library(ggridges)
library(ggpubr)
library(patchwork)
library(ggforce)


# Q1: Are there a difference in visitation rate between bees to different flowers -----------------------------------


################ Differences in visitation rate between pollinator groups


# PlotComp3 <- ggpredict(OneModel2, terms = c("Pollinator", "Where")) %>% 
#   rename(Where = group, Pollinator = x) %>% 
#   as.data.frame()
# 
# IntegrationDiscovery <- IntegrationModel_2 %>% filter(Where == "Discovery")
# PlotComp3Discovery <- PlotComp3 %>% filter(Where == "Discovery")  
# 
# MowedPlot <- ggplot() + 
#   geom_sina(data = IntegrationDiscovery,
#             aes(x = Pollinator, y = N_visits/NOpen, fill = Pollinator, color = Pollinator)) +
#   geom_violin(data = IntegrationDiscovery,
#               aes(x = Pollinator, y = N_visits/NOpen, fill = Pollinator, color = Pollinator),
#               alpha = 0.5, width = 1.1) +  
#   geom_pointrange(data = PlotComp3Discovery,
#                   aes(x = Pollinator, y = predicted,
#                       ymin = conf.low,
#                       ymax = conf.high),
#                   color = "black",
#                   size = 0.8) +
#   scale_fill_manual(values = c("Honeybee" = "#800026", "Bumblebee" = "#fd8d3c", "Solitary" = "#fed976"), 
#                     labels = c("Honeybee" = "Honeybee", "Bumblebee" = "Bumblebee", "Solitary" = "Solitary bee"),
#                     breaks = c("Honeybee", "Bumblebee", "Solitary")) +
#   scale_color_manual(values = c("Honeybee" = "#800026", "Bumblebee" = "#fd8d3c", "Solitary" = "#fed976"), 
#                      labels = c("Honeybee" = "Honeybee", "Bumblebee" = "Bumblebee", "Solitary" = "Solitary bee"),
#                      breaks = c("Honeybee", "Bumblebee", "Solitary")) +
#   labs(y = "", x = "", fill = "", color = "", title = "c) Mowed apple orchards") +
#   scale_x_discrete(limits = c("Honeybee", "Bumblebee", "Solitary")) + 
#   theme_minimal() +
#   theme(legend.position = "top", 
#         axis.text.x = element_blank(),
#         axis.text.y = element_text(size = 20),
#         axis.title = element_text(size = 17),
#         plot.title = element_text(face = "bold", size = 22),
#         legend.text = element_text(size = 16),   
#         legend.title = element_text(size = 18)) +
#   ylim(0,0.9)
# 
# 
# 
# IntegrationSummerred <- IntegrationModel_2 %>% filter(Where == "Summerred")
# PlotComp3Summerred <- PlotComp3 %>% filter(Where == "Summerred")  # x is Where in ggpredict output
# 
# 
# UnmowedPlot <- ggplot() + 
#   geom_sina(data = IntegrationSummerred,
#             aes(x = Pollinator, y = N_visits/NOpen, fill = Pollinator, color = Pollinator)) +
#   geom_violin(data = IntegrationSummerred,
#               aes(x = Pollinator, y = N_visits/NOpen, fill = Pollinator, color = Pollinator),
#               alpha = 0.5, width = 1.1) +  
#   geom_pointrange(data = PlotComp3Summerred,
#                   aes(x = Pollinator, y = predicted,
#                       ymin = conf.low,
#                       ymax = conf.high),
#                   color = "black",
#                   size = 0.8) +
#   scale_fill_manual(values = c("Honeybee" = "#bd0026", "Bumblebee" = "#fd8d3c", "Solitary" = "#fed976"), 
#                     labels = c("Honeybee" = "Honeybee", "Bumblebee" = "Bumblebee", "Solitary" = "Solitary bee"),
#                     breaks = c("Honeybee", "Bumblebee", "Solitary")) +
#   scale_color_manual(values = c("Honeybee" = "#bd0026", "Bumblebee" = "#fd8d3c", "Solitary" = "#fed976"), 
#                      labels = c("Honeybee" = "Honeybee", "Bumblebee" = "Bumblebee", "Solitary" = "Solitary bee"),
#                      breaks = c("Honeybee", "Bumblebee", "Solitary")) +
#   labs(y = "", x = "", fill = "", color = "", title = "b) Unmowed apple orchards") +
#   scale_x_discrete(limits = c("Honeybee", "Bumblebee", "Solitary")) + 
#   theme_minimal() +
#   theme(legend.position = "top", 
#         axis.text = element_text(size = 20),
#         axis.text.x = element_blank(),
#         axis.title = element_text(size = 17),
#         plot.title = element_text(face = "bold", size = 22),
#         legend.text = element_text(size = 16),   # increase legend labels
#         legend.title = element_text(size = 18)) +
#   ylim(0,0.9)
# 
# 
# 
# 
#   
# IntegrationDandelion <- IntegrationModel_2 %>% filter(Where == "Dandelion")
# PlotComp3Dandelion <- PlotComp3 %>% filter(Where == "Dandelion")  # x is Where in ggpredict output
# 
# 
# DandelionPlot <- ggplot() + 
#   geom_sina(data = IntegrationDandelion,
#             aes(x = Pollinator, y = N_visits/NOpen, fill = Pollinator, color = Pollinator)) +
#   geom_violin(data = IntegrationDandelion,
#               aes(x = Pollinator, y = N_visits/NOpen, fill = Pollinator, color = Pollinator),
#               alpha = 0.5, width = 1.1) +  
#   geom_pointrange(data = PlotComp3Dandelion,
#                   aes(x = Pollinator, y = predicted,
#                       ymin = conf.low,
#                       ymax = conf.high),
#                   color = "black",
#                   size = 0.8) +
#   scale_fill_manual(values = c("Honeybee" = "#bd0026", "Bumblebee" = "#fd8d3c", "Solitary" = "#fed976"), 
#                     labels = c("Honeybee" = "Honeybee", "Bumblebee" = "Bumblebee", "Solitary" = "Solitary bee"),
#                     breaks = c("Honeybee", "Bumblebee", "Solitary")) +
#   scale_color_manual(values = c("Honeybee" = "#bd0026", "Bumblebee" = "#fd8d3c", "Solitary" = "#fed976"), 
#                      labels = c("Honeybee" = "Honeybee", "Bumblebee" = "Bumblebee", "Solitary" = "Solitary bee"),
#                      breaks = c("Honeybee", "Bumblebee", "Solitary")) +
#   labs(y = "Visitation frequency", x = "", fill = "", color = "", title = "a) Dandelion understory") +
#   scale_x_discrete(limits = c("Honeybee", "Bumblebee", "Solitary")) + 
#   theme_minimal() +
#   theme(legend.position = "top", 
#         axis.text = element_text(size = 20),
#         axis.text.x = element_blank(),
#         axis.title = element_text(size = 17),
#         plot.title = element_text(face = "bold", size = 22),
#         legend.text = element_text(size = 16),   # increase legend labels
#         legend.title = element_text(size = 18)) +
#   ylim(0,0.9)
#   
#   
#   
#   
# VisitationDiff <- ggarrange(DandelionPlot, UnmowedPlot, MowedPlot, nrow = 1, common.legend = TRUE, legend = "bottom")
# ggsave(VisitationDiff, filename = "Figures/VisitationDiff.jpeg", height = 10, width = 14) 
# 
# 
# 
# 
# 


# Q2: Competition or facilitation between flowers in apple orchards -------

### KONKURRANSE FRA EPLEBLOMSTER PÅ LØVETANNBESØK
PlotComp1 <- ggpredict(OneModel2, terms = c("CompApple", "Pollinator"), 
                           condition = c(Where = "Dandelion")) %>% 
  rename(Pollinator = group) 


CometingApples <- ggplot(PlotComp1, aes(x = x, y = predicted, 
                                        color = Pollinator, fill = Pollinator)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  geom_point(data = IntegrationModel_2 %>%  filter(Where == "Dandelion"),
             aes(x = CompApple, y = N_visits/NOpen, color = Pollinator),
             alpha = 0.4, size = 2, position = position_jitter(width = 2, height = 0)) +
  scale_fill_manual(values = c("Honeybee" = "#660000", "Bumblebee" = "#CC3333", "Solitary" = "#FFAC81"), 
                    labels = c("Honeybee" = "Honeybee", "Bumblebee" = "Bumblebee", "Solitary" = "Solitary bee"),
                    breaks = c("Honeybee", "Bumblebee", "Solitary")) +
  scale_color_manual(values = c("Honeybee" = "#660000", "Bumblebee" = "#CC3333", "Solitary" = "#FFAC81"), 
                     labels = c("Honeybee" = "Honeybee", "Bumblebee" = "Bumblebee", "Solitary" = "Solitary bee"),
                     breaks = c("Honeybee", "Bumblebee", "Solitary")) +
  labs(x = "Number of competing apple flowers",
    y = "",
    color = "",
    fill = "",
    title = "b") +
  theme_minimal(base_size = 16) +
  theme(legend.text = element_text(size = 16))


### KONKURRANSE FRA LØVETANNBESØK PÅ EPLEBLOMSTER

PlotComp2 <- ggpredict(OneModel2, terms = c("CompDandelion", "Pollinator"), 
                       condition = c(Where = "Summerred")) %>% 
  rename(Pollinator = group)



CometingDandelions <- ggplot(PlotComp2, aes(x = x, y = predicted, 
                                            color = Pollinator, fill = Pollinator)) +
  geom_line(size = 1.2, linetype = "dashed") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  geom_point(data = IntegrationModel_2 %>%  filter(Where == "Summerred"),
             aes(x = CompDandelion, y = N_visits/NOpen, color = Pollinator),
             alpha = 0.4, size = 2, position = position_jitter(width = 1.5, height = 0)) +
  scale_fill_manual(values = c("Honeybee" = "#660000", "Bumblebee" = "#CC3333", "Solitary" = "#FFAC81"), 
                    labels = c("Honeybee" = "Honeybee", "Bumblebee" = "Bumblebee", "Solitary" = "Solitary bee"),
                    breaks = c("Honeybee", "Bumblebee", "Solitary")) +
  scale_color_manual(values = c("Honeybee" = "#660000", "Bumblebee" = "#CC3333", "Solitary" = "#FFAC81"), 
                     labels = c("Honeybee" = "Honeybee", "Bumblebee" = "Bumblebee", "Solitary" = "Solitary bee"),
                     breaks = c("Honeybee", "Bumblebee", "Solitary")) +
  labs(x = "Number of competing dandelion flowers",
    y = "Visitation rate",
    color = "",
    fill = "",
    title = "a") +
  theme_minimal(base_size = 16) +
  theme(legend.text = element_text(size = 16))

Q1Competition <- ggarrange(CometingDandelions, CometingApples, common.legend = TRUE, legend = "top")
ggsave(Q1Competition, filename = "Figures/Q1Competition.jpeg", height = 10, width = 14)









# Additional: plot over date ----------------------------------------------
IntegrationModel_plot <- IntegrationModel_2 %>% 
  mutate(DOY = as.numeric(as.character(DOY)))

VisitsDOY_plot <- IntegrationModel_plot %>% 
  ggplot(aes(x = DOY, y = pmax(N_visits, 0), color = Where, fill = Where)) +
  geom_point(position = position_jitter(width = 0.05, height = 2), alpha = 0.5) +
  geom_smooth(method = "gam",
              formula = y ~ s(x, k = 5),
              method.args = list(family = gaussian(link = "log")),
              fullrange = TRUE,
              alpha = 0.3, se = TRUE) +
  scale_color_manual(name = "",
                    values = c("Summerred" = "#7F646C", "Dandelion" = "#CC9966", "Discovery" = "#669999"),
                    labels = c("Summerred" = "Apple flowers unmowed", 
                               "Dandelion" = "Dandelions understory",
                               "Discovery" = "Apple flowers mowed"),
                    breaks = c("Dandelion", "Summerred", "Discovery")) +
  scale_fill_manual(name = "",
                    values = c("Summerred" = "#7F646C", "Dandelion" = "#CC9966", "Discovery" = "#669999"),
                    labels = c("Summerred" = "Apple flowers unmowed", 
                               "Dandelion" = "Dandelions understory",
                               "Discovery" = "Apple flowers mowed"),
                    breaks = c("Dandelion", "Summerred", "Discovery")) +
  labs(x = "", y = "Number of visits (count)", title = "a") +
  theme_minimal(base_size = 18) +
  coord_cartesian(xlim = c(135, 142), clip = "on") +
  scale_y_continuous(trans = "pseudo_log", limits = c(0, 30)) +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(face = "bold"))


#prevent lines from gooing below 0, and jitter points


#Phenology
Phenology24_plot <- Phenology24 %>% 
  rename(NOpen = Total_Open, NBuds = Total_Buds, NWithered = Total_Withered, Where = Apple_variety) %>% 
  mutate(NTot = NOpen + NBuds + NWithered) %>% 
  filter(Where != 'Summerred')


PhenologyCombined_plot <- PhenologyCombined %>% 
  select(-Percentage) %>% 
  mutate(Where = recode(Where, "Tree" = "Summerred", "Ground" = "Dandelion"))

PhenologyAll <- bind_rows(PhenologyCombined_plot, Phenology24_plot)


# PhenoDOY_plot <- PhenologyAll %>%
#   filter(DOY >=135 & DOY <= 142) %>% 
#   ggplot(aes(x = DOY, y = ((NOpen/NTot)*100), fill = Where, color = Where)) +
#   #geom_point(position = position_jitter(width = 0.2, height = 2), alpha = 0.5) +
#   geom_smooth(method = "gam",
#               formula = y ~ s(x, k = 5),
#               method.args = list(family = gaussian(link = "identity")),
#               fullrange = TRUE,
#               alpha = 0.3, se = FALSE) +
#   scale_color_manual(name = "",
#                    values = c("Summerred" = "#7F646C", "Dandelion" = "#CC9966", "Discovery" = "#669999"),
#                    labels = c("Summerred" = "Apple flowers unmowed", 
#                               "Dandelion" = "Dandelions understory",
#                               "Discovery" = "Apple flowers mowed"),
#                    breaks = c("Dandelion", "Summerred", "Discovery")) +
#   scale_fill_manual(name = "",
#                     values = c("Summerred" = "#7F646C", "Dandelion" = "#CC9966", "Discovery" = "#669999"),
#                     labels = c("Summerred" = "Apple flowers unmowed", 
#                                "Dandelion" = "Dandelions understory",
#                                "Discovery" = "Apple flowers mowed"),
#                     breaks = c("Dandelion", "Summerred", "Discovery")) +
#   labs(x = "Day of the year (DOY)", y = "Open flowers (%)") +
#   theme_minimal(base_size = 16) +
#   coord_cartesian(xlim = c(135, 142), clip = "on") +
#   scale_y_continuous(limits = c(0, 100)) +
#   theme(legend.position = "none")
  
  






PhenoDOY_plot <- PhenologyAll %>%
  filter(DOY >=135 & DOY <= 142) %>% 
  ggplot(aes(x = DOY, y = NOpen, fill = Where, color = Where)) +
  #geom_point(position = position_jitter(width = 0.2, height = 2), alpha = 0.5) +
  geom_smooth(method = "gam",
              formula = y ~ s(x, k = 5),
              method.args = list(family = gaussian(link = "identity")),
              fullrange = TRUE,
              alpha = 0.3, se = FALSE) +
  scale_color_manual(name = "",
                     values = c("Summerred" = "#7F646C", "Dandelion" = "#CC9966", "Discovery" = "#669999"),
                     labels = c("Summerred" = "Apple flowers unmowed", 
                                "Dandelion" = "Dandelions understory",
                                "Discovery" = "Apple flowers mowed"),
                     breaks = c("Dandelion", "Summerred", "Discovery")) +
  scale_fill_manual(name = "",
                    values = c("Summerred" = "#7F646C", "Dandelion" = "#CC9966", "Discovery" = "#669999"),
                    labels = c("Summerred" = "Apple flowers unmowed", 
                               "Dandelion" = "Dandelions understory",
                               "Discovery" = "Apple flowers mowed"),
                    breaks = c("Dandelion", "Summerred", "Discovery")) +
  labs(x = "", y = "Open flowers (count)", title = "b") +
  theme_minimal(base_size = 18) +
  coord_cartesian(xlim = c(135, 142), clip = "on") +
  #scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(breaks = c(135, 136, 137, 138, 139, 140, 141, 142),
                     labels = c("14 May", "15 May", "16 May", "17 May", "18 May", "19 May", "20 May", "21 May")) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))





  
PhenoVisitPlot <- ggarrange(VisitsDOY_plot, PhenoDOY_plot, nrow = 2, heights = c(3, 1))

ggsave(PhenoVisitPlot, filename = "Figures/PhenoVisitPlot.jpeg", height = 10, width = 14)




PhenologyAll %>% 
  filter(DOY >=135 & DOY <= 142) %>% 
  group_by(Where) %>% 
  summarise(n_flowers = sum(NOpen))

IntegrationModel_2 %>% 
  filter(DOY >=135 & DOY <= 142) %>% 
  group_by(Observation) %>% 
  summarise(n_visits = sum(N_visits))
  
  
#### OLD STUFF
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
  labs(x = "", y = "Open flowers (count)") +
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
  scale_x_continuous(breaks = c(131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143),
                     labels = c("11 May", "12 May", "13 May", "14 May", "15 May", "16 May", "17 May", "18 May", "19 May", "20 May", "21 May", "22 May", "23 May"))



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
  labs(x = "", y = "Pollinator visits (count)", color = "Observation", title = "") +
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
  scale_x_continuous(breaks = c(131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143),
                     labels = c("11 May", "12 May", "13 May", "14 May", "15 May", "16 May", "17 May", "18 May", "19 May", "20 May", "21 May", "22 May", "23 May"))


ManualObs <- ggarrange(PhenoManObs, PolliManObs, nrow = 2, common.legend = TRUE, legend = "bottom", labels = c("a", "b"))


ggsave("Figures/ManualObs.png", plot = ManualObs, width = 10, height = 8, dpi = 300)





# #Difference in visits before and during apple flowering
# Visits_combined <- Visits3_combined %>% 
#   ggplot(aes(y = Percent, x = Pollinator, fill = Type)) + 
#   geom_col(position = position_dodge()) +
#   scale_fill_manual(name = "",
#                     values = c("Tree" = "#7F646C", "Ground" = "#CC9966"),
#                     labels = c("Tree" = "Apple flowers", "Ground" = "Dandelions")) +
#   theme_minimal() +
#   geom_text(aes(label = paste0(round(Percent, 1), "%")),
#             position = position_dodge(width = 0.9),
#             vjust = -0.5,
#             size = 4,
#             color = "black") +
#   labs(title = NULL,
#        x = "",
#        y = "Percentage (%)") +
#   facet_wrap(~ Period, scales = "free_x") +
#   ggtitle("Percentage of visits") + 
#   theme(plot.title = element_text(hjust = 0.5)) +
#   facet_wrap(~ Period, scales = "free_x")
# 
# ggsave(Visits_combined, filename = "Figures/Visits_combined.jpeg", height = 8, width = 12)



# 
# #Plot visitation against flowers
# 
# newdata <- expand.grid(
#   NOpen = seq(1, max(ManualVis_per_flower$NOpen), length.out = 100),
#   Where = c("Tree", "Ground"))
# 
# newdata$DOY <- mean(ManualVis_per_flower$DOY, na.rm = TRUE)
# 
# newdata$offset <- log(newdata$NOpen)
# 
# pred <- predict(VisitsMan1,
#                 newdata = newdata,
#                 type = "link",
#                 se.fit = TRUE,
#                 re.form = NA)
# 
# 
# newdata$fit <- exp(pred$fit)
# newdata$lower <- exp(pred$fit - 1.96 * pred$se.fit)
# newdata$upper <- exp(pred$fit + 1.96 * pred$se.fit)
# 
# 
# 
# AppleDandelion <- ggplot() +
#   geom_ribbon(data = newdata, 
#               aes(x = NOpen, ymin = lower, ymax = upper, fill = Where, group = Where),
#               alpha = 0.3) +
#   geom_line(data = newdata, 
#             aes(x = NOpen, y = fit, color = Where, group = interaction(Where, Location)),
#             size = 1.2) +
#   geom_point(data = ManualVis_per_flower, 
#              aes(x = NOpen, y = N_visits, color = Where), 
#              alpha = 0.5, position = position_jitter(width = 2, height = 0.2)) +
#   scale_color_manual(values = c("Tree" = "#7F646C", "Ground" = "#CC9966"),
#                      labels = c("Tree" = "Apple flowers", "Ground" = "Dandelions")) +
#   scale_fill_manual(values = c("Tree" = "#7F646C", "Ground" = "#CC9966"),
#                     labels = c("Tree" = "Apple flowers", "Ground" = "Dandelions")) +
#   labs(x = "Open flowers (count)",
#        y = "Pollinator visits per day (count)",
#        color = "",
#        fill = "") +
#   theme_minimal(base_size = 14) +
#   theme(strip.text = element_text(face = "bold"))
# 
# ggsave("Figures/AppleDandelion.png", plot = AppleDandelion, width = 10, height = 6, dpi = 300)
# 








#



















###### DOY


ManualVis_per_flower2 <- ManualVis_per_flower %>%
  mutate(visit_freq = N_visits / NOpen,
         group = Where)


predictions <- ggpredict(model = VisitsMan1,
  terms = c("DOY [all]", "Where"),
  type = "fixed")


# Plot for model output
p2 <- ggplot(predictions, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3, color = NA) +
  geom_line(data = predictions %>% filter(x <= 140),
            aes(color = group, group = group),
            size = 1.2, linetype = "solid") +
  geom_line(data = predictions %>% filter(x >= 140),
            aes(color = group, group = group),
            size = 1.2, linetype = "dashed") +
  geom_point(data = ManualVis_per_flower2, 
             aes(x = DOY, y = visit_freq, color = group), 
             position = position_jitter(width = 0.2)) +
  scale_y_continuous(name = "Visitation frequency (bee visits/open flowers)") +
  scale_x_continuous(limits = c(135, 142), breaks = 135:142, 
                     labels = c("14 May", "15 May", "16 May", "17 May", "18 May", "19 May", "20 May", "21 May")) +
  labs(x = "", color = "", fill = "", title = "a") +
  theme_minimal(base_size = 16) +
  theme(legend.position = "top") +
  scale_color_manual(values = c("Tree" = "#7F646C", "Ground" = "#CC9966"), 
                     labels = c("Tree" = "Apple flowers", "Ground" = "Dandelions")) +
  scale_fill_manual(values = c("Tree" = "#7F646C", "Ground" = "#CC9966"), 
                    labels = c("Tree" = "Apple flowers", "Ground" = "Dandelions"))


#plot for apple phenology
phenology_avg <- ManualVis_per_flower2 %>%
  group_by(DOY, Where) %>%
  summarise(mean_NOpen = mean(NOpen), .groups = "drop")

phenology_avg2 <- PhenologyDandelion %>%
  group_by(DOY, Where) %>%
  summarise(mean_perc = mean(Percentage), .groups = "drop")

# Plot smooth area
p1 <- ggplot(phenology_avg2, aes(x = DOY, y = mean_perc, color = Where)) +
  geom_smooth(se = FALSE, size = 1.2) +
  scale_x_continuous(limits = c(135, 142), breaks = 135:142, labels = NULL) +
  scale_color_manual(values = c("Tree" = "#7F646C", "Ground" = "#CC9966"),
                     labels = c("Tree" = "Apple flowers", "Ground" = "Dandelions")) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "none") +
  labs(x = "", title = "b", color = "", y = "Open flowers (%)") +
  ylim(0,100)


# Stack the plots vertically
VisitsDOY <- p2 / p1 + plot_layout(heights = c(4, 1))


ggsave("Figures/VisitsDOY.png", plot = VisitsDOY, width = 12, height = 12, dpi = 300)







#Phenology mow no mow

Visits_per_flower2 <- Visits_per_flower %>%
  mutate(group = Apple_variety )


predictions_mowing <- ggpredict(model = Model2DOY,
                         terms = c("DOY [all]", "Apple_variety"),
                         type = "fixed")


# Plot for model output
a2 <- ggplot(predictions_mowing, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3, color = NA) +
  geom_line(size = 1.2, linetype = "dashed") +
  geom_point(data = Visits_per_flower2, 
             aes(x = DOY, y = Visits_per_flower, color = group), 
             position = position_jitter(width = 0.2, height = 0)) +
  scale_x_continuous(limits = c(136, 143), breaks = 136:143, 
                     labels = c("15 May", "16 May", "17 May", "18 May", "19 May", "20 May", "21 May", "22 May")) +
  labs(x = "", color = "", fill = "", title = "a", y = "Visitation frequency (bee visits/open flowers)") +
  theme_minimal(base_size = 16) +
  theme(legend.position = "top") +
  scale_color_manual(values = c("Summerred" = "#7F646C", "Discovery" = "#669999"), 
                     labels = c("Summerred" = "Unmowed", "Discovery" = "Mowed")) +
  scale_fill_manual(values = c("Summerred" = "#7F646C", "Discovery" = "#669999"), 
                    labels = c("Summerred" = "Unmowed", "Discovery" = "Mowed")) +
  ylim(0,1.1)



#plot for apple phenology
phenology_mowing <- Phenology25_perc %>%
  group_by(Apple_variety, DOY) %>%
  summarise(mean_perc = mean(Percentage), .groups = "drop")

# Plot smooth area
a1 <- ggplot(phenology_mowing, aes(x = DOY, y = mean_perc, color = Apple_variety)) +
  geom_smooth(se = FALSE, size = 1.2) +
  scale_x_continuous(limits = c(136, 143), breaks = 136:143, labels = NULL) +
  scale_color_manual(values = c("Summerred" = "#7F646C", "Discovery" = "#669999")) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "none") +
  labs(x = "", title = "b", color = "", y = "Open flowers (%)") +
  ylim(0,100)


VisitsDOY_mowing <- a2 / a1 + plot_layout(heights = c(4, 1))


ggsave("Figures/VisitsDOY_mowing.png", plot = VisitsDOY_mowing, width = 12, height = 12, dpi = 300)



# Q3: Are some taxanomic groups only attracted to apple or dandelions --------

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
    y = "Total number of pollinator visits (count)",
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
  labs(x = "Open apple flowers (count)",
       y = "Pollinator visits per day (count)",
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
  labs(x = "", y = "Open flowers per day (count)") +
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


### species across DOY

Species_vis_doy1 <- Visits_species_DOY %>% 
  ggplot(aes(x = DOY, y = Count, color = Pollinator, fill = Pollinator)) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 5), se = TRUE) +
  coord_cartesian(ylim = c(0, NA)) +
  scale_x_continuous(breaks = seq(min(Visits_species_DOY$DOY), max(Visits_species_DOY$DOY), by = 1)) +
  scale_color_brewer(palette = "Set3") +
  scale_fill_brewer(palette = "Set3") +
  #coord_cartesian(ylim = c(0, NA)) +
  theme_minimal()

ggsave("Figures/Species_vis_doy1.png", plot = Species_vis_doy1, width = 10, height = 6, dpi = 300)

Species_vis_doy2 <- Visits_species_DOY %>% 
  ggplot(aes(x = DOY, y = Count, color = Pollinator, fill = Pollinator)) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 5), se = TRUE) +
  coord_cartesian(ylim = c(0, NA)) +
  scale_x_continuous(breaks = seq(min(Visits_species_DOY$DOY), max(Visits_species_DOY$DOY), by = 1)) +
  scale_color_brewer(palette = "Set3") +
  scale_fill_brewer(palette = "Set3") +
  #coord_cartesian(ylim = c(0, NA)) +
  theme_minimal() +
  facet_wrap(~ Pollinator)

ggsave("Figures/Species_vis_doy2.png", plot = Species_vis_doy2, width = 10, height = 6, dpi = 300)
