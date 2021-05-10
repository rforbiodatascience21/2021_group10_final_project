rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(patchwork)
library(RColorBrewer)


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
data <- read_tsv(file = "data/02_clean_data.tsv")


# Wrangle data ------------------------------------------------------------
# Subsetting the table for analysis and renaming the columns
ABC_taxa_data <- data %>% 
  select(Subject,
         Timing,
         Treatment,
         ABC_Total, 
         G_Bifidobacterium,
         G_Clostridium) %>% 
  rename("Abberant Behavior Score" = ABC_Total,
         "Bifidobacterium" = G_Bifidobacterium,
         "Clostridium" = G_Clostridium) %>% 
  mutate(Treatment = case_when(Treatment == "Pre" ~ "Prebiotic",
                               Treatment == "Syn" ~ "Synbiotic")) %>% 
  drop_na()


# Visualize data ----------------------------------------------------------
# Scatterplot visualization of correlation between
# aberrant behavior (total) score and Bifidobacterium amounts
# before and after the treatment
p1 <- ABC_taxa_data %>% 
  ggplot(mapping = aes(x = `Abberant Behavior Score`,
                       y = Bifidobacterium,
                       color = Timing,
                       label = Subject)) +
  geom_point(size = 4) +
  facet_wrap(~ Treatment) +
  theme_bw() +
  labs(x = element_blank()) +
  scale_color_manual(values=c("#E69F00", "#56B4E9")) +
  # Text displayed for patients after treatment
  geom_text(size = 3, 
            hjust=0, 
            vjust=0, 
            colour = "black",
            check_overlap = TRUE)

# Scatterplot visualization of correlation between
# aberrant behavior (total) score and Clostridium amounts
# before and after the treatment
p2 <- ABC_taxa_data %>% 
  ggplot(mapping = aes(x = `Abberant Behavior Score`,
                       y = Clostridium,
                       color = Timing,
                       label = Subject)) +
  geom_point(size = 4) +
  facet_wrap(~ Treatment) +
  theme_bw() +
  scale_color_manual(values=c("#E69F00", "#56B4E9")) +
  # Text displayed for patients after the Syn treatment
  geom_text(size = 3, 
            hjust=0, 
            vjust=0, 
            colour = "black",
            check_overlap = TRUE) 

ABC_vs_taxa_scatterplot <- (p1 / p2) + 
  plot_layout(guides = "collect")

# Write data --------------------------------------------------------------
write_tsv(x = ABC_taxa_data,
          file = "data/08_ABC_taxa_data.tsv")

ggsave(filename = "08_ABC_vs_taxa_scatterplot.png",
       path = "results",
       plot = ABC_vs_taxa_scatterplot,
       width = 12,
       height = 6)