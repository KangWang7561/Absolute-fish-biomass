
rm(list = ls())

library(tidyr)
library(plyr) 
library(dplyr)
library(nlstools)
library(reshape2) 
library(ggplot2)
library(ggnewscale)
library(ggrepel)
library(cowplot)
library(grid)
library(viridis)
library(splancs)
library(gridExtra)
library(rjags)
library(MixSIAR)
library(readxl)
library(writexl)
library(ggpmisc)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(giscoR)
library(readr)
library(ggpubr)
library(tidyverse)
library(factoextra)
library(corrplot)

# Figure 1 was made by Inkscape

# Figure 2 ####
Site_map = read_excel("Site map.xlsx")
Site_map = st_as_sf(Site_map, coords = c("Longitude", "Latitude"), crs = 4326)
europe = gisco_get_countries(resolution = "3", epsg = 4326)
finland = europe[europe$CNTR_NAME == "Finland", ]

# (a)
PC1 = ggplot() +
  geom_sf(data = europe, fill = "white", color = "black", size = 0.3) +
  geom_sf(data = Site_map, aes(fill = PC1), color = "black", size = 3.5,         
          shape = 21, stroke = 0.5) +
  geom_sf(data = finland, fill = NA, color = "black", size = 0.3) +
  scale_fill_gradient2(low = "#57B9FF", mid = "white", high = "red", midpoint = 0,
                       name = expression("Environmental gradient (PC1)"),
                       guide = guide_colorbar(frame.colour = "black", frame.linewidth = 0.1,
                                              barheight = unit(0.3, "cm"), barwidth = unit(6, "cm"),
                                              title.position = "bottom", title.hjust = 0.5, 
                                              direction = "horizontal")) +
  coord_sf(xlim = c(20, 31.4), ylim = c(59.8, 70.1), label_graticule = "SW") +
  scale_y_continuous(breaks = seq(60, 70, by = 1)) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "gray90", color = NA),
    panel.grid.major = element_line(color = "gray70", size = 0.3),
    axis.title = element_blank(),
    axis.text = element_text(size = 14, colour = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 16),
    legend.text  = element_text(size = 15))

# (b)
Allochthony = ggplot() +
  geom_sf(data = europe, fill = "white", color = "black", size = 0.3) +
  geom_sf(data = Site_map, aes(fill = Allochthony), color = "black", size = 3.5,         
          shape = 21, stroke = 0.5) +
  geom_sf(data = finland, fill = NA, color = "black", size = 0.3) +
  scale_fill_gradient2(low = "white", high = "#5C4033", 
                       name = expression("Proportional allochthony (%)"),
                       labels = scales::label_percent(accuracy = 1, suffix = ""),
                       guide = guide_colorbar(frame.colour = "black", frame.linewidth = 0.1,
                                              barheight = unit(0.3, "cm"), barwidth = unit(6, "cm"),
                                              title.position = "bottom", title.hjust = 0.5, 
                                              direction = "horizontal")) +
  coord_sf(xlim = c(20, 31.4), ylim = c(59.8, 70.1), label_graticule = "SW") +
  scale_y_continuous(breaks = seq(60, 70, by = 1)) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "gray90", color = NA),
    panel.grid.major = element_line(color = "gray70", size = 0.3),
    axis.title = element_blank(),
    axis.text = element_text(size = 14, colour = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 16),
    legend.text  = element_text(size = 15))

# (c)
CPUE = ggplot() +
  geom_sf(data = europe, fill = "white", color = "black", size = 0.3) +
  geom_sf(data = Site_map, aes(fill = CPUE), color = "black", size = 3.5,         
          shape = 21, stroke = 0.5) +
  geom_sf(data = finland, fill = NA, color = "black", size = 0.3) +
  scale_fill_gradient2(low = "white", high = "black",
                       name = expression("CPUE ("*g~net^{-1}*" "*12*h^{-1}*")"),
                       guide = guide_colorbar(frame.colour = "black", frame.linewidth = 0.1,
                                              barheight = unit(0.3, "cm"), barwidth = unit(6, "cm"),
                                              title.position = "bottom", title.hjust = 0.5, 
                                              direction = "horizontal")) +
  coord_sf(xlim = c(20, 31.4), ylim = c(59.8, 70.1), label_graticule = "SW") +
  scale_y_continuous(breaks = seq(60, 70, by = 1)) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "gray90", color = NA),
    panel.grid.major = element_line(color = "gray70", size = 0.3),
    axis.title = element_blank(),
    axis.text = element_text(size = 14, colour = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 16),
    legend.text  = element_text(size = 15))

comb_fig = plot_grid(PC1, Allochthony, CPUE, ncol = 3,
                     labels = c("(a)", "(b)", "(c)"))
ggsave("Figure 2.pdf", comb_fig, height = 8, width = 11)

rm(list = ls())

# Figure 3 ####
CPUE_Species = read_excel("Absolute biomass.xlsx", sheet = "Combined")
colnames(CPUE_Species) = gsub("\\*", ".", colnames(CPUE_Species))
colnames(CPUE_Species) = gsub("Perch_predatory", "Perch-predatory", colnames(CPUE_Species))
colnames(CPUE_Species) = gsub("White_bream", "White-bream", colnames(CPUE_Species))
colnames(CPUE_Species) = gsub("Blue_bream", "Blue-bream", colnames(CPUE_Species))
colnames(CPUE_Species) = gsub("Crucian_carp", "Crucian-carp", colnames(CPUE_Species))

CPUE_Species = CPUE_Species %>%
  pivot_longer(
    cols = matches("0.025Allo|0.5Allo|0.975Allo|CPUE|0.025Bio.Allo|0.5Bio.Allo|0.975Bio.Allo"),  
    names_to = c("Species", "Metric"),            
    names_pattern = "^(.*?)_(.+)$")%>%
  pivot_wider(
    names_from = Metric,                          
    values_from = value)

colnames(CPUE_Species)[c(4:6, 8:10)] = c("Allo_0.025", "Allo_0.5", "Allo_0.975", "Bio_Allo_0.025", "Bio_Allo_0.5", "Bio_Allo_0.975")
CPUE_Species$Species = gsub("Perch-predatory", "Perch_predatory", CPUE_Species$Species)
CPUE_Species$Species = gsub("White-bream", "White_bream",CPUE_Species$Species)
CPUE_Species$Species = gsub("Blue-bream", "Blue_bream", CPUE_Species$Species)
CPUE_Species$Species = gsub("Crucian-carp", "Crucian_carp", CPUE_Species$Species)
CPUE_Species$Species = gsub("Perch_predatory", "Predatory_perch", CPUE_Species$Species)
CPUE_Species_G = CPUE_Species[(CPUE_Species$Species %in% c("Global")), ]
CPUE_Species = CPUE_Species[(CPUE_Species$Species %in% c("Bleak", "Bream","Burbot", "LSRwhitefish",
                                                         "Perch","Predatory_perch","Pike", "Pikeperch",
                                                         "Roach","Ruffe","Smelt", "Vendace", "White_bream")), ]
CPUE_Species = na.omit(CPUE_Species)

species_list = sort(unique(CPUE_Species$Species))
species_colors = rep("grey", length(species_list))
species_colors = c("black", species_colors)
names(species_colors) = c("Global", species_list)

# (a)
Allochthony = ggplot()+
  geom_point(data = CPUE_Species,
             aes(x = PC1, y = Allo_0.5, color = factor(Species)),
             shape = 16, size = 3, alpha = 0.7) +
  stat_smooth(data = CPUE_Species_G,
              aes(x = PC1, y = Allo_0.5, color = factor(Species)),
              method = "nls", formula = y ~ a * exp(b * x), se = FALSE, size = 1,
              method.args = list(start = list(a = 0.1, b = 0.1))) +
  geom_point(data = CPUE_Species_G,
             aes(x = PC1, y = Allo_0.5, color = factor(Species)),
             shape = 16, size = 3, alpha = 1) +
  scale_color_manual(values = species_colors, name = "Species") +
  scale_y_continuous(name = "Proportional allochthony (%)",
                     limits = c(0, 1), expand = c(0, 0), labels = function(x) x * 100) +
  annotate("text", x = 2, y = 0.95,
           label = "italic(R)^2 == 0.71 ~~~ italic(P) < 0.001",
           parse = TRUE, size = 7, color = "black", fontface = "bold") +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 20, colour = "black"),
        axis.text = element_text(size = 18, colour = "black"), 
        plot.background = element_blank(),
        panel.background = element_rect(fill = "grey100", colour = "grey100"),
        panel.grid = element_blank())

# (b)
CPUE = ggplot()+
  geom_point(data = CPUE_Species,
             aes(x = PC1, y = CPUE, color = factor(Species)),
             shape = 16, size = 3, alpha = 0.7) +
  stat_smooth(data = CPUE_Species_G,
              aes(x = PC1, y = CPUE, color = factor(Species)),
              method = "lm", se = FALSE, size = 1) +
  geom_point(data = CPUE_Species_G,
             aes(x = PC1, y = CPUE, color = factor(Species)),
             shape = 16, size = 3, alpha = 1) +
  scale_color_manual(values = species_colors, name = "Species") +
  scale_y_continuous(name = expression("CPUE ("*g~net^{-1}*" "*12*h^{-1}*")"),
                     limits = c(0, 4600), expand = c(0, 0)) +
  annotate("text", x = 2, y = 4370,
           label = "italic(R)^2 == 0.52 ~~~ italic(P) < 0.001",
           parse = TRUE, size = 7, color = "black", fontface = "bold") +
  theme_classic() +
  theme(axis.title.x = element_blank(),  
        legend.position = "none",
        axis.title = element_text(size = 20, colour = "black"),
        axis.text = element_text(size = 18, colour = "black"), 
        plot.background = element_blank(),
        panel.background = element_rect(fill = "grey100", colour = "grey100"),
        panel.grid = element_blank())

# Simulate Bio_Allo_0.5 using Bootstrap
summary(lm(CPUE ~ PC1, data = CPUE_Species_G)) # CPUE = 1430.40 + 372.10 * PC1
summary(nls(Allo_0.5 ~ a * exp(b * PC1), data = CPUE_Species_G,
            start = list(a = 0.1, b = 0.1))) # Allo = 0.148 * e ^ -0.489 * PC1

set.seed(123)
PC1_seq = seq(min(CPUE_Species_G$PC1),
              max(CPUE_Species_G$PC1),
              length.out = 200)
B = 10000
pred_matrix = matrix(NA, nrow = B, ncol = length(PC1_seq))

for(i in 1:B){
  boot_data = CPUE_Species_G[sample(1:nrow(CPUE_Species_G), replace = TRUE), ]
  lm_cpue = lm(CPUE ~ PC1, data = boot_data)
  
  nls_allo = try(
    nls(Allo_0.5 ~ a * exp(b * PC1),
        data = boot_data,
        start = list(a = 0.1, b = -0.1)),
    silent = TRUE)
  if(class(nls_allo) == "try-error") next
  
  cpue_pred = predict(lm_cpue, newdata = data.frame(PC1 = PC1_seq))
  allo_pred = predict(nls_allo, newdata = data.frame(PC1 = PC1_seq))
  pred_matrix[i, ] = cpue_pred * allo_pred
}

Bio_fit = (1430.40 + 372.10 * PC1_seq) * (0.148 * exp(-0.489 * PC1_seq))
Bio_lower = apply(pred_matrix, 2, quantile, 0.025, na.rm = TRUE)
Bio_upper = apply(pred_matrix, 2, quantile, 0.975, na.rm = TRUE)

fit_df = data.frame(PC1 = PC1_seq, Bio = Bio_fit,
                    lower = Bio_lower, upper = Bio_upper)

# (c)
Absolute_allohthony = ggplot()+
  geom_point(data = CPUE_Species,
             aes(x = PC1, y = Bio_Allo_0.5, color = factor(Species)),
             shape = 16, size = 3, alpha = 0.7) +
  geom_point(data = CPUE_Species_G,
             aes(x = PC1, y = Bio_Allo_0.5, color = factor(Species)),
             shape = 16, size = 3, alpha = 1) +
  geom_line(data = fit_df, 
            aes(x = PC1, y = Bio),
            colour = "#808080", linewidth = 1) +
  stat_smooth(data = CPUE_Species_G,
              aes(x = PC1, y = Bio_Allo_0.5, color = factor(Species)),
              method = "lm", size = 1) +
  geom_ribbon(data = fit_df, 
              aes(x = PC1, ymin = lower, ymax = upper),
              fill = "#808080", alpha = 0.4) +
  scale_color_manual(values = species_colors, name = "Species") +
  scale_y_continuous(name = expression("Allochthonous CPUE ("*g~net^{-1}*" "*12*h^{-1}*")"),
                     limits = c(0, 1600), expand = c(0, 0)) +
  annotate("text", x = 3, y = 1520,
           label = "italic(R)^2 == 0.11 ~~~ italic(P)  == 0.028",
           parse = TRUE, size = 7, color = "black") +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 20, colour = "black"),
        axis.text = element_text(size = 18, colour = "black"), 
        plot.background = element_blank(),
        panel.background = element_rect(fill = "grey100", colour = "grey100"),
        panel.grid = element_blank())
Absolute_allohthony

comb_fig = plot_grid(Allochthony, CPUE, Absolute_allohthony, ncol = 3,
                     labels = c("(a)", "(b)", "(c)"), label_size = 18, label_x = -0.022,
                     align = "hv", axis = "lrtb", rel_heights = c(1, 0.15))
comb_fig = add_sub(comb_fig, size = 25,
                   "Forested catchment <---- Environmental gradient (PC1) ----> Agricultural catchment")
ggsave("Figure 3.pdf", comb_fig, width = 18, height = 6)

rm(list = ls())

# Supplementary Figure 1 ####
PCA_data = read_excel("Env_variables.xlsx", sheet = "PCA_Variables")
PCA_result = prcomp(PCA_data[,3:22], scale. = TRUE)
summary(PCA_result)

fviz_eig(PCA_result, addlabels = T)
fviz_pca_biplot(PCA_result)
var = get_pca_var(PCA_result)
corrplot(var$cos2, is.corr = F) 

PCA_scores = as.data.frame(PCA_result$x[,1:3])
PCA_scores = -(PCA_scores * 1)
PCA_scores$Lake = PCA_data$Abbr.
PCA_loadings = as.data.frame(PCA_result$rotation[,1:3])
PCA_loadings = -(PCA_loadings * 1)
PCA_loadings = cbind(Variable = rownames(PCA_loadings), PCA_loadings)
# write_xlsx(PCA_scores, "PCA_scores.xlsx")
# write_xlsx(PCA_loadings, "PCA_loadings.xlsx")
lim = ceiling(max(abs(PCA_scores[c("PC1", "PC2", "PC3")])))

xlab = paste0("PC1 (",round(summary(PCA_result)$importance[2,1]*100, digits=2), "%)")
ylab = paste0("PC2 (",round(summary(PCA_result)$importance[2,2]*100, digits=2), "%)")
Kang_theme = theme_bw()+
  theme(axis.text.x = element_text(face="bold", color="black", size=18),
        axis.text.y = element_text(face="bold", color="black", size=18),
        axis.title.x = element_text(face="bold", color="black", size=22),
        axis.title.y = element_text(face="bold", color="black", size=22),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())

# (a)
PC1_PC2 = ggplot(PCA_scores, aes(PC1, PC2))+
  geom_point(shape = 19, size = 3)+
  coord_cartesian(xlim=c(-lim, lim), ylim=c(-lim,lim))+
  geom_text_repel(aes(label = Lake), size = 4, seed = 2, max.overlaps = 100)+
  geom_text_repel(data = PCA_loadings, aes(x = PC1*12, y = PC2*12, label = rownames(PCA_loadings)), 
                  colour="#9A9A9A", size = 4, seed = 2, max.overlaps = 100)+
  geom_segment(data = PCA_loadings, aes(x = 0, y = 0,xend = PC1*12, yend = PC2*12),
               arrow = arrow(length = unit(0.1, "in")), colour = "#9A9A9A",
               size = 1, lineend = "round", linejoin="round")+
  scale_x_continuous(sec.axis = sec_axis( ~ . / (lim), name = "Component 1 loadings"))+
  scale_y_continuous(sec.axis = sec_axis( ~ . / (lim), name = "Component 2 loadings"))+
  labs(x = xlab, y = ylab)+
  Kang_theme

# (b)
ylab = paste0("PC3 (",round(summary(PCA_result)$importance[2,3]*100, digits=2), "%)")
PC1_PC3 = ggplot(PCA_scores, aes(PC1, PC3))+
  geom_point(shape = 19, size = 3)+
  coord_cartesian(xlim=c(-lim, lim), ylim=c(-lim,lim))+
  geom_text_repel(aes(label = Lake), size = 4, seed = 2, max.overlaps = 100)+
  geom_text_repel(data = PCA_loadings, aes(x = PC1*12, y = PC3*12, label = rownames(PCA_loadings)), 
                  colour="#9A9A9A", size = 4, seed = 2, max.overlaps = 100)+
  geom_segment(data = PCA_loadings, aes(x = 0, y = 0,xend = PC1*12, yend = PC3*12),
               arrow = arrow(length = unit(0.1, "in")), colour = "#9A9A9A",
               size = 1, lineend = "round", linejoin="round")+
  scale_x_continuous(sec.axis = sec_axis( ~ . / (lim), name = "Component 1 loadings"))+
  scale_y_continuous(sec.axis = sec_axis( ~ . / (lim), name = "Component 3 loadings"))+
  labs(x = xlab, y = ylab)+
  Kang_theme

# (c)
xlab = paste0("PC2 (",round(summary(PCA_result)$importance[2,2]*100, digits=2), "%)")
PC2_PC3 = ggplot(PCA_scores, aes(PC2, PC3))+
  geom_point(shape = 19, size = 3)+
  coord_cartesian(xlim=c(-lim, lim), ylim=c(-lim,lim))+
  geom_text_repel(aes(label = Lake), size = 4, seed = 2, max.overlaps = 100)+
  geom_text_repel(data = PCA_loadings, aes(x = PC2*12, y = PC3*12, label = rownames(PCA_loadings)), 
                  colour="#9A9A9A", size = 4, seed = 2, max.overlaps = 100)+
  geom_segment(data = PCA_loadings, aes(x = 0, y = 0,xend = PC2*12, yend = PC3*12),
               arrow = arrow(length = unit(0.1, "in")), colour = "#9A9A9A",
               size = 1, lineend = "round", linejoin="round")+
  scale_x_continuous(sec.axis = sec_axis( ~ . / (lim), name = "Component 2 loadings"))+
  scale_y_continuous(sec.axis = sec_axis( ~ . / (lim), name = "Component 3 loadings"))+
  labs(x = xlab, y = ylab)+
  Kang_theme

PCA =  ggarrange(PC1_PC2, PC1_PC3, PC2_PC3,
                 nrow = 2, ncol = 2, align = "hv",
                 labels = c("(a)", "(b)", "(c)"),
                 font.label = list(size = 20, face = "bold", color = "black"))
ggsave("Supplementary Figure 1.pdf", PCA, width = 16, height = 13)

rm(list = ls())

# Supplementary Figure 2 ####
Species_allochthony = read_excel("Absolute biomass.xlsx", sheet = "Lake+Species")
Species_allochthony = Species_allochthony[, 1:5]
Species_allochthony$CPUE = CPUE_Species_G$CPUE # Please run lines 130–161 to generate "CPUE_Species_G" and "species_colors"
colnames(Species_allochthony) = c("Lake", "PC1", "Allo_0.025", "Allo_0.5", "Allo_0.975", "CPUE")
Species_allochthony$Bio_Allo_0.025 = Species_allochthony$Allo_0.025*Species_allochthony$CPUE
Species_allochthony$Bio_Allo_0.5 = Species_allochthony$Allo_0.5*Species_allochthony$CPUE
Species_allochthony$Bio_Allo_0.975 = Species_allochthony$Allo_0.975*Species_allochthony$CPUE
Species_allochthony$Species = CPUE_Species_G$Species

# (a)
Allochthony_species = ggplot()+
  geom_point(data = CPUE_Species,
             aes(x = PC1, y = Allo_0.5, color = factor(Species)),
             shape = 16, size = 3, alpha = 0.7) +
  stat_smooth(data = Species_allochthony,
              aes(x = PC1, y = Allo_0.5, color = factor(Species)),
              method = "nls", formula = y ~ a * exp(b * x), se = FALSE, size = 0.5,
              method.args = list(start = list(a = 0.1, b = 0.1))) +
  geom_point(data = Species_allochthony,
             aes(x = PC1, y = Allo_0.5, color = factor(Species)),
             shape = 16, size = 3, alpha = 1) +
  scale_color_manual(values = species_colors, name = "Species") +
  scale_y_continuous(name = "Proportional allochthony (%)",
                     limits = c(0, 1), expand = c(0, 0), labels = function(x) x * 100) +
  annotate("text", x = 2, y = 0.95,
           label = "italic(R)^2 == 0.68 ~~~ italic(P) < 0.001",
           parse = TRUE, size = 7, color = "black", fontface = "bold") +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 20, colour = "black"),
        axis.text = element_text(size = 18, colour = "black"), 
        plot.background = element_blank(),
        panel.background = element_rect(fill = "grey100", colour = "grey100"),
        panel.grid = element_blank())

# (b)
Absolute_allohthony_species = ggplot()+
  geom_point(data = CPUE_Species,
             aes(x = PC1, y = Bio_Allo_0.5, color = factor(Species)),
             shape = 16, size = 3, alpha = 0.7) +
  stat_smooth(data = Species_allochthony,
              aes(x = PC1, y = Bio_Allo_0.5, color = factor(Species)),
              method = "lm", se = FALSE, size = 0.5) +
  geom_point(data = Species_allochthony,
             aes(x = PC1, y = Bio_Allo_0.5, color = factor(Species)),
             shape = 16, size = 3, alpha = 1) +
  scale_color_manual(values = species_colors, name = "Species") +
  scale_y_continuous(name = expression("allochthonous CPUE ("*g~net^{-1}*" "*12*h^{-1}*")"),
                     limits = c(0, 1600), expand = c(0, 0)) +
  annotate("text", x = 2, y = 1520,
           label = "italic(R)^2 == 0.10 ~~~ italic(P)  == 0.04",
           parse = TRUE, size = 7, color = "black") +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 20, colour = "black"),
        axis.text = element_text(size = 18, colour = "black"), 
        plot.background = element_blank(),
        panel.background = element_rect(fill = "grey100", colour = "grey100"),
        panel.grid = element_blank())

comb_fig = plot_grid(Allochthony_species, Absolute_allohthony_species, ncol = 2,
                     labels = c("(a)", "(b)"), label_size = 18, label_x = -0.022,
                     align = "hv", axis = "lrtb", rel_heights = c(1, 0.15))

comb_fig = add_sub(comb_fig, size = 22,
                   "Forested catchment <---- Environmental gradient (PC1) ----> Agricultural catchment")
ggsave("Supplementary Figure 2.pdf", comb_fig,
       width = 12, height = 6)

rm(list = ls())

# Supplementary Figure 3 ####
attach(CPUE_Species_G) # Please run lines 130–152 to generate "CPUE_Species_G"
model_linear = lm(Bio_Allo_0.5 ~ PC1)
model_quad = lm(Bio_Allo_0.5 ~ poly(PC1, 2, raw = TRUE))
model_exp = nls(Bio_Allo_0.5 ~ a * exp(b * PC1), 
                start = list(a = 1, b = 0.1))
model_gauss = nls(Bio_Allo_0.5 ~ a * exp(-((PC1 - mu)^2) / (2 * sigma^2)),
                  start = list(a = max(Bio_Allo_0.5), 
                               mu = PC1[which.max(Bio_Allo_0.5)], 
                               sigma = diff(range(PC1)) / 4))

AIC(model_linear, model_quad, model_exp, model_gauss)
coef_exp = coef(model_exp)
coef_gauss = coef(model_gauss)

p = ggplot(CPUE_Species_G, aes(x=PC1, y=Bio_Allo_0.5)) +
  geom_point(color="grey", size=3) +
  geom_smooth(aes(color="Linear"), method="lm", formula=y~x, se=FALSE) +
  geom_smooth(aes(color="Quadratic"), method="lm", formula=y~poly(x,2,raw=TRUE), se=FALSE) +
  stat_function(aes(color="Exponential"),
                fun=function(x) coef_exp["a"]*exp(coef_exp["b"]*x), size=1) +
  stat_function(aes(color="Gaussian"),
                fun=function(x) coef_gauss["a"]*exp(-((x-coef_gauss["mu"])^2)/(2*coef_gauss["sigma"]^2)), size=1) +
  scale_color_manual(values=c( "Linear"="black", "Quadratic"="#EEA236", 
                               "Exponential"="#5CB85C", "Gaussian"="#46B8DA")) +
  labs(x="Forested catchment <----  Environmental gradient (PC1)  ----> Agricultural catchment",
       color="Model") +
  scale_y_continuous(name = expression("Allochthonous CPUE ("*g~net^{-1}*" "*12*h^{-1}*")"))+
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 11),
        legend.position = c(0.8, 0.8), 
        axis.title = element_text(size = 13, colour = "black"),
        axis.text = element_text(size = 11, colour = "black"), 
        panel.background = element_rect(fill = "grey100", colour = "grey100"),
        panel.grid = element_blank())

ggsave("Supplementary Figure 3.pdf", plot = p, height = 6, width = 8)

rm(list = ls())

# Supplementary Figure 4 ####
Consumers = read.csv("Consumer_data_All.csv")
Sources = read.csv("Source_data_All.csv")
Water = read_excel("Lake_water.xlsx")
Lake_PCA = read_excel("Lake_PCA.xlsx")

Water$Source = "Water"
Water$Meand2H = Water$"d2H"
Water$SDd2H = 0 

Source_water = dplyr::bind_rows(Sources[c("Lake", "Source", "Meand2H", "SDd2H")], 
                                Water[c("Lake", "Source", "Meand2H", "SDd2H")])
Source_water = merge(x = Source_water, y = Lake_PCA, by = "Lake")

Labels_n = Consumers %>%
  group_by(Species) %>%
  summarise(n = length(d2H), n_lakes = length(unique(Lake)))
Labels_n = as.data.frame(Labels_n)

legend_data = data.frame(Source = factor(c("Water", "Terrestrial", "Aquatic", "Consumer"),
                                         levels = c("Water", "Terrestrial", "Aquatic", "Consumer")), x = 1, y = 1)

Fig_list = list()
for (i in unique(Consumers$Species)) {
  Fig_list[[i]] = 
    ggplot(Consumers %>% filter(Species == i), aes(x = PC1, y = d2H, colour = "Consumer")) +
    geom_point(data = Consumers %>% filter(Species == i), aes(x = PC1, y = d2H), size = 1.2, shape = 2)+
    geom_point(data = Source_water, aes(x = PC1, y = Meand2H, colour = Source), size = 1.2, shape = 16, inherit.aes = FALSE) +
    geom_linerange(data = Source_water, aes(x = PC1, ymin = Meand2H - SDd2H, ymax = Meand2H + SDd2H, group = Source, colour = Source), alpha = 0.3, inherit.aes = FALSE) +
    scale_colour_manual(values = c("#1017e8", "#665a26", "#27aad6", "black"),
                        limits = c("Water", "Terrestrial", "Aquatic", "Consumer"), 
                        labels = c("Water", "Allochtonous", "Autochtonous", "Consumer")) +
    labs(title = i)+
    new_scale_color() + 
    geom_text(data = Labels_n[which(Labels_n$Species == i), ], 
              x = -3, y = -248, hjust = 0, vjust = 0.5, size = 4.5, 
              aes(label = paste0("n=", n, ", lakes=", n_lakes))) +
    coord_cartesian(ylim = c(-250, -50)) +
    theme_bw() +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 12, colour = "black"), 
          axis.text.y = element_text(size = 12, colour = "black"),
          axis.title = element_blank(),
          plot.title = element_text(size = 14),
          plot.background = element_blank(),
          panel.background = element_rect(fill = "grey100", colour = "grey100"),
          panel.grid = element_blank())
}

legend_plot = ggplot(legend_data) +
  geom_point(aes(x = x, y = y, color = Source, shape = Source), size = 3) +   
  scale_colour_manual(
    values = c("Water" = "#1017e8", "Terrestrial" = "#665a26", "Aquatic" = "#27aad6", "Consumer" = "black"),
    limits = c("Water", "Terrestrial", "Aquatic", "Consumer"),
    labels = c("Lake water", "Terrestrial source", "Aquatic source", "Consumer"),
    name = NULL) +
  scale_shape_manual(
    values = c("Water" = 16, "Terrestrial" = 16, "Aquatic" = 16, "Consumer" = 2),
    limits = c("Water", "Terrestrial", "Aquatic", "Consumer"),
    labels = c("Lake water", "Terrestrial source", "Aquatic source", "Consumer"),
    name = NULL) +  theme_void()

legend = get_legend(legend_plot + theme(legend.position = "right",
                                        legend.direction = "horizontal",
                                        legend.box = "horizontal", 
                                        legend.background = element_rect(linetype = 2, linewidth = 0.5, color = "black"),
                                        legend.title = element_text(size = 14),
                                        legend.text = element_text(size = 14),
                                        legend.margin = margin(5, 5, 5, 5),
                                        plot.margin = unit(c(0, 0, 0, 0), units = "line")))

Fig_order = c("Alpinebullhead","Arcticcharr","Bleak","Browntrout",     
              "Burbot","DRwhitefish","Grayling", "LDRwhitefish",    
              "LSRwhitefish","Minnow","Perch", "Perch_predatory",      
              "Pike","Roach","Ruffe","Smelt", "SSRwhitefish", "Vendace")
New_figlist = list()
New_figlist = Fig_list[Fig_order]
New_figlist = cowplot::plot_grid(plotlist = New_figlist, ncol=4)
Comb_fig = plot_grid(legend, New_figlist,
                     ncol = 1, rel_heights = c(0.05, 1)) 

pdf(file = "Supplementary Figure 4.pdf", height = 14, width = 12)
grid.arrange(Comb_fig, 
             left = textGrob(expression(paste(delta^{2}, "H (\u2030)")), rot = 90, gp = gpar(fontsize = 20)), 
             bottom = textGrob("Forested catchment <----  Environmental gradient (PC1)  ----> Agricultural catchment", gp = gpar(fontsize = 20)))
dev.off()
rm(list = ls())

# Supplementary Figure 5 ####
CPUE_Species = read_excel("Absolute biomass.xlsx", sheet = "Combined")
colnames(CPUE_Species) = gsub("\\*", ".", colnames(CPUE_Species))
colnames(CPUE_Species) = gsub("Perch_predatory", "Perch-predatory", colnames(CPUE_Species))
colnames(CPUE_Species) = gsub("White_bream", "White-bream", colnames(CPUE_Species))
colnames(CPUE_Species) = gsub("Blue_bream", "Blue-bream", colnames(CPUE_Species))
colnames(CPUE_Species) = gsub("Crucian_carp", "Crucian-carp", colnames(CPUE_Species))

CPUE_Species = CPUE_Species %>%
  pivot_longer(
    cols = matches("0.025Allo|0.5Allo|0.975Allo|CPUE|0.025Bio.Allo|0.5Bio.Allo|0.975Bio.Allo"),  
    names_to = c("Species", "Metric"),            
    names_pattern = "^(.*?)_(.+)$")%>%
  pivot_wider(
    names_from = Metric,                          
    values_from = value)

colnames(CPUE_Species)[c(4:6, 8:10)] = c("Allo_0.025", "Allo_0.5", "Allo_0.975", "Bio_Allo_0.025", "Bio_Allo_0.5", "Bio_Allo_0.975")
CPUE_Species$Species = gsub("Perch-predatory", "Perch_predatory", CPUE_Species$Species)
CPUE_Species$Species = gsub("White-bream", "White_bream",CPUE_Species$Species)
CPUE_Species$Species = gsub("Blue-bream", "Blue_bream", CPUE_Species$Species)
CPUE_Species$Species = gsub("Crucian-carp", "Crucian_carp", CPUE_Species$Species)
CPUE_Species = CPUE_Species[!(CPUE_Species$Species %in% c("True", "Relative")), ]

fig_list = list()
for (i in unique(CPUE_Species$Species)){
  subset_data = CPUE_Species[CPUE_Species$Species == i, ]
  max_y = max(subset_data$Bio_Allo_0.5, na.rm = TRUE)
  y_breaks = pretty(c(0, max_y), n = 4)
  
  p = ggplot(data = subset_data) +
    geom_point(aes(x = PC1, y = Bio_Allo_0.5), size = 3, shape = 16) +
    lims(x = c(min(CPUE_Species$PC1), max(CPUE_Species$PC1))) +
    labs(title = i) +
    scale_y_continuous(breaks = y_breaks, limits = c(0, NA), expand = expansion(mult = c(0.05,0.05))) +
    theme_bw() +
    theme(legend.position = "none",
          axis.text = element_text(size = 16, colour = "black"),
          axis.title = element_blank(),
          plot.title = element_text(size = 20),
          plot.background = element_blank(),
          panel.background = element_rect(fill = "grey100", colour = "grey100"),
          panel.grid = element_blank())
  
  if (sum(!is.na(subset_data$Bio_Allo_0.5)) >= 7) {
    p = p + 
      geom_smooth(aes(x = PC1, y = Bio_Allo_0.5), method = "lm", color = "black", se = FALSE, linewidth = 1) +
      stat_poly_eq(aes(x = PC1, y = Bio_Allo_0.5, 
                       label = paste(after_stat(rr.label), after_stat(p.value.label), sep = "~~~")),
                   label.x = "right", label.y = "top", 
                   formula = y ~ x, parse = TRUE, size = 6, color = "black")
  }
  
  fig_list[[i]] = p
}

fig_order = c("Alpinebullhead", "Arcticcharr", "Bleak", "Bream", "Blue_bream", "White_bream",
              "Browntrout", "Burbot", "Crucian_carp", "Grayling", "Ide", "Minnow", "Perch", 
              "Perch_predatory", "Pike", "Pikeperch", "Roach", "Ruffe", "Rudd", "Smelt", 
              "Tench", "Vendace", "DRwhitefish", "LDRwhitefish", "LSRwhitefish", "SSRwhitefish")
new_figlist = list()
new_figlist = fig_list[fig_order]
comb_fig = plot_grid(plotlist = new_figlist, ncol=4, align="hv")

pdf(file="Supplementary Figure 5.pdf", width = 16, height = 20)
grid.arrange(comb_fig, left = textGrob(label = expression("Allochthonous CPUE ("*g~net^{-1}*" "*12*h^{-1}*")"),
                                       rot=90, gp = gpar(fontsize = 25)), 
             top= textGrob("" ,gp = gpar(fontsize = 12)),
             bottom = textGrob(paste0("Forested catchment <----  Environmental gradient (PC1)  ----> Agricultural catchment"),gp = gpar(fontsize = 25)))
dev.off()
rm(list = ls())
