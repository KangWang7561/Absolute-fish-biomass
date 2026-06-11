
rm(list = ls())
set.seed(756199513)

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
library(R2WinBUGS)
library(splancs)
library(gridExtra)
library(rjags)
library(MixSIAR)
library(readxl)
library(writexl)

# Trait + Lake model ####
mix = load_mix_data(filename = "Consumer_data_All.csv",
                    iso_names = "d2H",
                    factors = c("Lake", "Trait"),
                    fac_random = c(TRUE, TRUE),
                    fac_nested = c(FALSE, FALSE),
                    cont_effects = "PC1")

source = load_source_data(filename = "Source_data_All.csv", 
                          source_factors = "Lake",
                          conc_dep = FALSE,
                          data_type = "means", mix)

discr = load_discr_data(filename = "TEF.csv", mix)

output_options = list(summary_save = TRUE,                 
                      summary_name = "summary_Lake+Trait", 
                      sup_post = TRUE,        
                      plot_post_save_pdf = FALSE,           
                      plot_post_name = "posterior_density",
                      sup_pairs = TRUE,   
                      plot_pairs_save_pdf = TRUE,    
                      plot_pairs_name = "pairs_plot",
                      sup_xy = TRUE,       
                      plot_xy_save_pdf = FALSE,
                      plot_xy_name = "xy_plot",
                      gelman = TRUE,
                      heidel = FALSE,  
                      geweke = TRUE,   
                      diag_save = TRUE,
                      diag_name = "diagnostics_Lake+Trait",
                      indiv_effect = FALSE,       
                      plot_post_save_png = FALSE, 
                      plot_pairs_save_png = FALSE,
                      plot_xy_save_png = FALSE,
                      diag_save_ggmcmc = FALSE,
                      return_obj = TRUE)

model_filename = "MixSIAR_model.txt"
resid_err = TRUE
process_err = TRUE
write_JAGS_model(model_filename, process_err, resid_err, mix, source)
jags.mod = run_model(run = "very long", mix, source, discr, model_filename,
                     alpha.prior=1, process_err = TRUE, resid_err = TRUE)

options(max.print = 99999)
saveRDS(jags.mod, file = paste0("MixSIAR_Lake+Trait", ".rds"))
saveRDS(mix, file = paste0("mix_Lake+Trait",".rds"))
saveRDS(source, file = paste0("source_Lake+Trait",".rds"))
saveRDS(discr, file = paste0("discr_Lake+Trait",".rds"))

# Posterior ####
mix = readRDS(file = paste0("mix_Lake+Trait",".rds"))
source = readRDS(file = paste0("source_Lake+Trait",".rds"))
jags = readRDS(file = paste0("MixSIAR_Lake+Trait",".rds"))

Posterior = jags[["BUGSoutput"]][["sims.list"]][["p.ind"]]
Posterior = reshape2::melt(Posterior)
colnames(Posterior) = c("Draw", "Individual", "Source", "Proportion")
Posterior$Source2 = factor(Posterior$Source, labels = source[["source_names"]])
Posterior$Source2 = factor(Posterior$Source2, levels = source[["source_names"]])

number.sim = jags[["BUGSoutput"]][["n.sims"]]
number.sources = length(source[["source_names"]])
gg = cbind(rep(rep(mix[["data"]][["Lake"]], each = number.sim), times = number.sources),  
           rep(rep(mix[["data"]][["Trait"]], each = number.sim), times = number.sources))
colnames(gg) = c("Lake", "Trait")
Posterior = cbind(Posterior, gg)

Posterior$Lake = factor(Posterior$Lake, levels = unique(mix[["data"]][["Lake"]]))
Posterior$Species = factor(Posterior$Species, levels = unique(mix[["data"]][["Species"]])[order(unique(mix[["data"]][["Species"]]))])
Posterior$Source2 = factor(Posterior$Source2, levels = source[["source_names"]])

Posterior = subset(Posterior, select = -c(Individual, Source))
colnames(Posterior) = c("Draw", "Proportion", "Source", "Lake", "Trait")

Posterior.Lake = Posterior %>%
  group_by(Lake, Source) %>% 
  summarise(y0.025 = quantile(Proportion, 0.025, na.rm = TRUE),
            y0.1= quantile(Proportion, 0.1, na.rm = TRUE),
            y0.25= quantile(Proportion, 0.25, na.rm = TRUE),
            y0.375= quantile(Proportion, 0.375, na.rm = TRUE),
            y0.4= quantile(Proportion, 0.4, na.rm = TRUE),
            y0.5= quantile(Proportion, 0.5, na.rm = TRUE),
            y0.625= quantile(Proportion, 0.625, na.rm = TRUE),
            y0.75= quantile(Proportion, 0.75, na.rm = TRUE),
            y0.8= quantile(Proportion, 0.8, na.rm = TRUE),
            y0.9= quantile(Proportion, 0.9, na.rm = TRUE),
            y0.975= quantile(Proportion,0.975, na.rm = TRUE))

Posterior.Trait = Posterior %>%
  group_by(Lake, Trait, Source) %>%  
  summarise(y0.025 = quantile(Proportion, 0.025, na.rm = TRUE),
            y0.1= quantile(Proportion, 0.1, na.rm = TRUE),
            y0.25= quantile(Proportion, 0.25, na.rm = TRUE),
            y0.375= quantile(Proportion, 0.375, na.rm = TRUE),
            y0.4= quantile(Proportion, 0.4, na.rm = TRUE),
            y0.5= quantile(Proportion, 0.5, na.rm = TRUE),
            y0.625= quantile(Proportion, 0.625, na.rm = TRUE),
            y0.75= quantile(Proportion, 0.75, na.rm = TRUE),
            y0.8= quantile(Proportion, 0.8, na.rm = TRUE),
            y0.9= quantile(Proportion, 0.9, na.rm = TRUE),
            y0.975= quantile(Proportion,0.975, na.rm = TRUE))

# write_xlsx(Posterior.Lake, path = "Posterior.Lake.xlsx")
# write_xlsx(Posterior.Trait, path = "Posterior.Trait.xlsx")

# 
Posterior = read_excel("Posterior.xlsx")
colnames(Posterior) = c("Category", "2.5%", "50%", "97.5%")

Posterior.Lake = Posterior %>%
  filter(!grepl("Trait", Category, ignore.case = TRUE)) %>%
  separate(Category, into = c("prefix", "Lake", "Source"), sep = "\\.", remove = TRUE) %>%
  select(Lake, Source, `2.5%`, `50%`, `97.5%`)

Posterior.Trait = Posterior %>%
  filter(grepl("Trait", Category, ignore.case = TRUE)) %>%
  separate(Category, into = c("prefix", "Lake", "Trait", "Source"), sep = "\\.", remove = TRUE) %>%
  select(Lake, Trait, Source, `2.5%`, `50%`, `97.5%`)

# write_xlsx(Posterior.Lake, "Posterior.Lake.xlsx")
# write_xlsx(Posterior.Trait, "Posterior.Trait.xlsx")

# Plotting CPUE*Allo ~ PC1 for each species and global####
# Species
rm(list = ls())
CPUE = read_excel("Summary_CPUE_Allo.xlsx")
CPUE_Species = CPUE[, -c(3:11)]
colnames(CPUE_Species) = gsub("\\*", ".", colnames(CPUE_Species))
colnames(CPUE_Species) = gsub("Perch_predatory", "Perch-predatory", colnames(CPUE_Species))

CPUE_Species = CPUE_Species %>%
  pivot_longer(
    cols = matches("0.5Allo|0.975Allo|0.025Allo|CPUE|0.5Bio.Allo|0.975Bio.Allo|0.025Bio.Allo"),  
    names_to = c("Species", "Metric"),            
    names_pattern = "^(.*?)_(.+)$")%>%
  pivot_wider(
    names_from = Metric,                          
    values_from = value)

colnames(CPUE_Species)[c(4:6, 8:10)] = c("y0.5","y0.025","y0.975","Bio_Allo0.5","Bio_Allo0.025","Bio_Allo0.975")
CPUE_Species$Species <- gsub("Perch-predatory", "Perch_predatory", CPUE_Species$Species)

Allo = read_excel("Posterior.Fixed.Species.xlsx")
Allo = Allo[Allo$Source != "Aquatic", ]

CPUE_Species = merge(CPUE_Species, Allo, by = c("Lake", "Species"), all.x = TRUE)
CPUE_Species = subset(CPUE_Species, select = -c(y0.5.x, y0.025.x, y0.975.x, Source))

colnames(CPUE_Species)[colnames(CPUE_Species) == "y0.025.y"] <- "y0.025"
colnames(CPUE_Species)[colnames(CPUE_Species) == "y0.5.y"] <- "y0.5"
colnames(CPUE_Species)[colnames(CPUE_Species) == "y0.975.y"] <- "y0.975"
CPUE_Species$Bio_Allo0.025 = CPUE_Species$y0.025 * CPUE_Species$CPUE
CPUE_Species$Bio_Allo0.5 = CPUE_Species$y0.5 * CPUE_Species$CPUE
CPUE_Species$Bio_Allo0.975 = CPUE_Species$y0.975 * CPUE_Species$CPUE

fig_list = list()
label = "PC1"
for (i in unique(CPUE_Species$Species)){
  subset_data = CPUE_Species[CPUE_Species$Species == i, ]
  
  fig_list[[i]] = ggplot(data = subset_data) +
    geom_point(aes(x = PC1, y = Bio_Allo0.5), size = 1.2, shape = 16)+
    geom_linerange(aes(x = PC1, ymin = Bio_Allo0.025, ymax = Bio_Allo0.975), show.legend = FALSE, alpha = 0.35)+
    # geom_smooth(aes(y = Bio_Allo0.5, x = PC1), show.legend = FALSE, colour = "#ffb499", fill = "#ffb499", alpha = 0.35) +
    labs(x = label, y= "CPUE_Allo", title=i)+
    scale_y_continuous(expand = c(0, 0)) +
    lims(x = c(min(CPUE_Species$PC1), max(CPUE_Species$PC1)))+
    theme_bw() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), panel.background = element_rect(fill="grey100", colour = "grey100"), 
          axis.line = element_line(colour = "black"), 
          axis.text=element_text(size=14),legend.position="none",
          axis.text.x=element_blank(), #remove x axis labels
          #axis.ticks.x=element_blank(), #remove x axis ticks
          axis.text.y=element_blank(),  #remove y axis labels
          #axis.ticks.y=element_blank(),  #remove y axis ticks
          axis.title = element_blank(),
          plot.title=element_text(size=10),
          plot.background = element_blank())
}

# Setting figure order
fig_order = c("Alpinebullhead","Arcticcharr","Bleak","Browntrout",     
              "Burbot","DRwhitefish","Grayling","Hybrid",         
              "LDRwhitefish", "LSRwhitefish","Minnow","Perch",          
              "Perch_predatory","Pike","Roach","Ruffe",          
              "Smelt","SSRwhitefish", "Vendace")

new_figlist1 = list()
new_figlist1 = fig_list[fig_order]
comb_fig = cowplot::plot_grid(plotlist = new_figlist1, ncol=4)

pdf(file="CPUE_Allo for each species with PC1.pdf", 
    height = 7, width = 7) 
grid.arrange(comb_fig, left = textGrob("CPUE*Allo",rot=90, gp = gpar(fontsize = 11)), 
             top= textGrob("" ,gp = gpar(fontsize = 11)),
             bottom = textGrob(paste0("Agriculture <--  ", "PC1", " axis  --> Forestry"),gp = gpar(fontsize = 11)))
dev.off()

# Global
CPUE_Global = CPUE[, c(1:11)]
colnames(CPUE_Global) = gsub("\\*", ".", colnames(CPUE_Global))
CPUE_Global = CPUE_Global %>% select(-True_CPUE, -True_total_0.5Bio.Allo)
colnames(CPUE_Global) = gsub("Global_0.025Allo", "y0.025", colnames(CPUE_Global))
colnames(CPUE_Global) = gsub("Global_0.5Allo", "y0.5", colnames(CPUE_Global))
colnames(CPUE_Global) = gsub("Global_0.975Allo", "y0.975", colnames(CPUE_Global))
colnames(CPUE_Global) = gsub("Relative_Total_catch", "CPUE", colnames(CPUE_Global))
colnames(CPUE_Global) = gsub("Relative_total_0.025Bio.Allo", "Bio_Allo0.025", colnames(CPUE_Global))
colnames(CPUE_Global) = gsub("Relative_total_0.5Bio.Allo", "Bio_Allo0.5", colnames(CPUE_Global))
colnames(CPUE_Global) = gsub("Relative_total_0.975Bio.Allo", "Bio_Allo0.975", colnames(CPUE_Global))

global_fig = ggplot(data = CPUE_Global)+
  geom_point(aes(x = PC1, y = Bio_Allo0.5), shape = 16)+
  geom_linerange(aes(x = PC1, ymin = Bio_Allo0.025, ymax = Bio_Allo0.975), show.legend = FALSE, alpha = 0.35)+
  # geom_smooth(aes(y = Bio_Allo0.5, x = PC1), show.legend = FALSE, colour = "#ffb499", fill = "#ffb499", alpha = 0.35) +
  labs(x = label, y= "Bio_Allo", title = "Global")+
  scale_y_continuous(expand = c(0, 0)) +
  lims(x = c(min(CPUE_Global$PC1), max(CPUE_Global$PC1)))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_rect(fill="grey100", colour = "grey100"), 
        axis.line = element_line(colour = "black"), 
        axis.text=element_text(size=14),legend.position="none",
        axis.text.x=element_blank(), #remove x axis labels
        #axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        #axis.ticks.y=element_blank(),  #remove y axis ticks
        axis.title = element_blank(),
        plot.title=element_text(size=10),
        plot.background = element_blank())
global_fig

pdf(file="CPUE_Allo for global with PC1.pdf",
    height = 3.5, width = 3.5)
grid.arrange(global_fig, left = textGrob("CPUE*Allo",rot=90, gp = gpar(fontsize = 11)), 
             top = textGrob("" ,gp = gpar(fontsize = 11)),
             bottom = textGrob(paste0("Agriculture <--  ", "PC1", " axis  --> Forestry"),gp = gpar(fontsize = 11)))
dev.off()

# Combine them together
new_figlist2 = fig_list[fig_order]
new_figlist2 = append(new_figlist2, list(global_fig + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                                           panel.grid.minor = element_blank(), panel.background = element_rect(fill="grey100", colour = "grey100"), 
                                                           axis.line = element_line(colour = "black"), 
                                                           axis.text=element_text(size=14),legend.position="none",
                                                           axis.text.x=element_blank(), #remove x axis labels
                                                           #axis.ticks.x=element_blank(), #remove x axis ticks
                                                           axis.text.y=element_blank(),  #remove y axis labels
                                                           #axis.ticks.y=element_blank(),  #remove y axis ticks
                                                           axis.title = element_blank(),
                                                           plot.title=element_text(size=10),
                                                           plot.background = element_blank())), after = 0) 
comb_fig2 = cowplot::plot_grid(plotlist = new_figlist2, ncol=4)

pdf(file="CPUE_Allo for each species and global with PC1.pdf", 
    height = 9, width = 9) 
grid.arrange(comb_fig2, left = textGrob("CPUE_Allo",rot=90, gp = gpar(fontsize = 11)), 
             top= textGrob("" ,gp = gpar(fontsize = 11)),
             bottom = textGrob(paste0("Agriculture <--  ", "PC1", " axis  --> Forestry"),gp = gpar(fontsize = 11)))

dev.off()
#

# Lake and species specific posterior densities####
# For Fixed
my_colours1 = c("#27aad6", "#665a26")
plot_list_temp.Fixed = list()
scaleFUN = function(x) sprintf("%.2f", x)

for(i in levels(Posterior$Species)) {
  plot_list_temp.Fixed[[i]] = ggplot(Posterior[which(Posterior$Species == i), ]) +
    facet_wrap(~Lake) +
    geom_density(alpha = 0.4, aes(x = Proportion * 100, y = after_stat(scaled), colour = Source, fill = Source)) +
    scale_fill_manual(values = my_colours1) +
    scale_colour_manual(values = my_colours1) +
    scale_y_continuous(labels = scaleFUN) +
    labs(x = "Dietary Contribution (%)", 
         y = "Scaled posterior density",
         colour = "Source", fill = "Source",
         title = paste(i, "Diet sources", sep = "_")) +
    theme_classic() +
    theme(panel.border = element_rect(colour = "Black", fill = NA, linetype = 1),
          legend.direction = "horizontal", 
          legend.position = "bottom",
          legend.background = element_rect(colour = "black", fill = NA, linetype = 2),
          strip.placement = "outside",
          plot.title = element_text(size = 12))
}

pdf(file = "Dietary contribution by lake and species.Fixed.pdf")
print(plot_list_temp.Fixed)
dev.off()

#rm(i, my_colours1, scaleFUN())


