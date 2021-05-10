# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(broom)

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
sanctuary_data = read_tsv(file = "data/02_clean_data.tsv")

# Wrangle data ------------------------------------------------------------

Immune_heatmap<- sanctuary_data %>% 
  filter(Subject != 208) %>%
  unite(Subject_time, c(Subject, Timing), remove=FALSE) %>% 
  select(Subject_time,
         contains(c("CD25",
                    "IL10_CD",
                    "IFNg_CD",
                    "TNFa_CD",
                    "IL13_CD",
                    "IL17_CD"))) %>%
  pivot_longer(cols = -Subject_time, 
               names_to = "Marker",
               values_to = "Frequency") %>% 
  
# Augment data ----------------------------------------------------------  
  mutate_if(is.numeric, log2) 


# Visualize data ----------------------------------------------------------

Immune_heatmap_plot <-Immune_heatmap %>% 
  ggplot(aes(x = Subject_time, 
             y = Marker, 
             fill= Frequency, 
             width = 0.98,
             height = 0.98)) + 
  geom_tile()+
  labs(title = "Cell frequency before and after treatment",
       x = "Subjects pre or post treatment",
       y = "Markers")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
Immune_heatmap_plot


# Write data --------------------------------------------------------------

write_tsv(x = Immune_heatmap,
          file = "data/05_Immune_heatmap.tsv")

ggsave(filename = "05_Immune_heatmap_plot.png",
       path = "results",
       plot = Immune_heatmap_plot,
       width = 12,
       height = 8)




