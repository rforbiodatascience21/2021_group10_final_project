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
  unite(Subject_time, c(Subject, Timing), 
        remove=FALSE) %>% 
  select(Subject_time,
         Treatment,
         contains(c("CD25",
                    "IL10_CD",
                    "IFNg_CD",
                    "TNFa_CD",
                    "IL13",
                    "IL17_CD",
                    "IL6_B7"))) %>%
  pivot_longer(cols = c(-Subject_time, -Treatment),
               names_to = "Marker",
               values_to = "Frequency") %>% 


# Augment data ----------------------------------------------------------  
  mutate_if(is.numeric, log2)
  

# Visualize data ----------------------------------------------------------

Immune_heatmap_plot <-Immune_heatmap %>% 
  ggplot(aes(x = Subject_time, 
             y = Marker, 
             fill= Frequency)) + 
  geom_tile()+
  facet_grid(~Treatment) +
  labs(title = "Cell frequency before and after treatment",
       x = "Subject and time",
       y = "Immunce cell markers") +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))


# Write data --------------------------------------------------------------

write_tsv(x = Immune_heatmap,
          file = "data/10_Immune_heatmap.tsv")

ggsave(filename = "10_Immune_heatmap_plot.png",
       path = "results",
       plot = Immune_heatmap_plot,
       width = 12,
       height = 8)

