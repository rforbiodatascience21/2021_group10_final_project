# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(RColorBrewer)
library(haven)


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
sanctuary_data <- read_tsv(file = "data/02_clean_data.tsv")


# Wrangle data ------------------------------------------------------------

# Subset data to pick out microbiome compositions and create
# long for plotting

microbiome_data <- sanctuary_data %>% 
  select(Sample,
         starts_with("k__")) %>% 
  pivot_longer(cols = -Sample,
               names_to = "Taxa",
               values_to = "Relative_abundance") %>% 
  mutate(Taxa = str_remove_all(Taxa,
                               "[kpcofg]__"),
         Taxa = str_remove_all(Taxa,
                               "\\["),
         Taxa = str_remove_all(Taxa,
                               "\\]"))%>% 
  separate(col = Taxa,
           into = c("Kingdom",
                    "Phylum",
                    "Class_taxa",
                    "Order_taxa",
                    "Family",
                    "Genus"),
           sep = ";") %>% 
  drop_na() %>% 
  mutate(Phylum = zap_empty(Phylum),
         Class_taxa = zap_empty(Class_taxa),
         Order_taxa = zap_empty(Order_taxa),
         Family = zap_empty(Family),
         Genus = zap_empty(Genus))


# Visualize data ----------------------------------------------------------

# Visualizing microbiome compositions in barplot

microbiome_plot <- microbiome_data %>% 
  filter(Relative_abundance > 0.01) %>% 
  ggplot(mapping = aes(x = Sample,
                       y = Relative_abundance,
                       fill = Order_taxa)) +
  geom_col() +
  scale_fill_manual(values = rep(brewer.pal(12,
                                            "Set3"),
                                 times = 2),
                    aesthetics = "fill") +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1)) +
  labs(title = "Microbiome composition of fecal samples") +
  ylim(0,1)


# Model data --------------------------------------------------------------
# PCA of microbiome composition

pca_microbiome <- sanctuary_data %>% 
  select(starts_with("k__")) %>% 
  drop_na() %>% 
  select(where(~ sd(.x) > 0)) %>% 
  scale() %>% 
  prcomp()

# Removing sample 208_v3, since the microbiome data 
# was lost for this sample
matching_sanctuary <- sanctuary_data %>% 
  filter(Sample != "208_v3")

pca_microbiome_plot <- pca_microbiome %>%
  augment(matching_sanctuary) %>% 
  ggplot(aes(.fittedPC1,
             .fittedPC2,
             color = Treatment)) + 
  geom_point(size = 2.5,
             aes(shape = Timing)) +
  theme_minimal() +
  labs(x = "PC1",
       y = "PC2",
       title = "PCA of microbiome composition") +
  scale_color_brewer(palette = "Set2")

# How much variance is described by each PC?
pca_microbiome_variance <- pca_microbiome %>% 
  tidy(matrix = "eigenvalues")

# Write data --------------------------------------------------------------

write_tsv(x = microbiome_data,
          file = "data/04_microbiome_data.tsv")
write_tsv(x = pca_microbiome_variance,
          file = "data/04_microbiome_pca_variance_explained.tsv")

ggsave(filename = "04_microbiome_composition_barplot.png",
       path = "results",
       plot = microbiome_plot,
       width = 12,
       height = 6)

ggsave(filename = "04_pca_microbiome_plot.png",
       path = "results",
       plot = pca_microbiome_plot,
       width = 12,
       height = 8)
