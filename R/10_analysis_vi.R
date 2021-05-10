# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(broom)

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
sanctuary_data = read_tsv(file = "data/02_clean_data.tsv")
immune_microbiota = read_tsv(file = "data/01_immune_microbiota.tsv")

# Wrangle data ------------------------------------------------------------

Immune_heatmap<- sanctuary_data %>% 
  filter(Subject != 208) %>%
  unite(Subject_treatment_time, c(Subject, Treatment, Timing,), remove=FALSE) %>% 
  select(Subject_treatment_time,
         contains(c("CD25",
                    "IL10_CD",
                    "IFNg_CD",
                    "TNFa_CD",
                    "IL13",
                    "IL17_CD",
                    "IL6_B7"))) %>%
  pivot_longer(cols = -Subject_treatment_time, 
               names_to = "Marker",
               values_to = "Frequency") %>% 


# Augment data ----------------------------------------------------------  
  mutate_if(is.numeric, log2)
  

# Visualize data ----------------------------------------------------------

Immune_heatmap_plot <-Immune_heatmap %>% 
  ggplot(aes(x = Subject_treatment_time, 
             y = Marker, 
             fill= Frequency)) + 
  geom_tile()+
  labs(title = "Cell frequency before and after treatment",
       x = "Subject, time, and treatment",
       y = "Immunce cell markers")+
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
Immune_heatmap_plot


# Write data --------------------------------------------------------------

write_tsv(x = Immune_heatmap,
          file = "data/05_Immune_heatmap.tsv")

ggsave(filename = "05_Immune_heatmap_plot.png",
       path = "results",
       plot = Immune_heatmap_plot,
       width = 12,
       height = 8)
