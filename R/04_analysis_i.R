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

# Subset data to pick out microbiome compositions and make
# data long for plotting

microbiome_data = sanctuary_data %>% 
  select(Sample, starts_with("k__")) %>% 
  pivot_longer(cols = -Sample,
               names_to = "Taxa",
               values_to = "Relative_abundance") %>% 
  mutate(Taxa = str_remove_all(Taxa,
                               "[kpcofg]__"),
         Taxa = str_remove_all(Taxa,
                               "\\["),
         Taxa = str_remove_all(Taxa,
                                "\\]")) %>% 
  separate(col = Taxa,
           into = c("Kingdom", "Phylum", "Class_taxa", "Order_taxa", "Family", "Genus"),
           sep = ";") %>% 
  drop_na() %>% 
  mutate(Phylum = zap_empty(Phylum),
         Class_taxa = zap_empty(Class_taxa),
         Order_taxa = zap_empty(Order_taxa),
         Family = zap_empty(Family),
         Genus = zap_empty(Genus))

# Subset Behavioral data for further plotting
# Aberrant behavior Data
ABC_data = sanctuary_data %>% 
  select(Subject,Treatment,Timing,starts_with("ABC"),-"ABC_Total") %>%
  rename(Irritability = ABC_SS1,
         Lethargy = ABC_SS2,
         Stereotypy = ABC_SS3,
         Hyperactivity = ABC_SS4) %>%
  pivot_longer(cols = -c(Subject,Timing,Treatment),
               names_to = "Abberant_Behavior",
               values_to = "Score") %>% 
  drop_na()

# Adaptative behavior data
ABAS_data = sanctuary_data %>% 
  select(Subject,Treatment,Timing,starts_with("ABAS")) %>%
  pivot_longer(cols = -c(Subject,Timing,Treatment),
               names_to = "Adaptative_Behavior",
               values_to = "Score") %>% 
  drop_na()

# Repetitive behavior data
RBS_data = sanctuary_data %>% 
  select(Subject,Treatment,Timing,starts_with("RBS")) %>%
  rename(Stereotype = RBS_SS1,
         Compulsive = RBS_SS2,
         Routine = RBS_SS3,
         Sameness = RBS_SS4,
         Restricted = RBS_SS5) %>%
  pivot_longer(cols = -c(Subject,Timing,Treatment),
               names_to = "Repetitive_Behavior",
               values_to = "Score") %>% 
  drop_na()


# Visualize data ----------------------------------------------------------

# Visualizing microbiome compositions in barplot

microbiome_plot = microbiome_data %>% 
  ggplot(mapping = aes(x = Sample,
                       y = Relative_abundance,
                       fill = Order_taxa))+
  geom_col()+
  scale_fill_manual(values = rep(brewer.pal(11,
                                            "Paired"),
                                 times = 5),
                    aesthetics = "fill")+
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1))


# Visualizing Abberant Behavior for all subject with boxplots
# before and after treatment

ABC_data %>% 
  ggplot(data = .,
         mapping = aes(x = factor(Subject),
                       y = Score,
                       color = factor(Timing))) +
  geom_boxplot() +
  facet_grid(.~Treatment)

# Visualizing Abberant Behavior for one subject before and after treatment

ABC_data %>%
  filter(Subject == 202) %>% 
  ggplot(data = .,
         mapping = aes(x = Abberant_Behavior,
                       y = Score,
                       fill = Treatment)) +
  geom_bar(stat="identity") +
  facet_grid(.~Timing)

# Write data --------------------------------------------------------------
write_tsv(x = microbiome_data,
          file = "data/04_microbiome_data.tsv")
ggsave(filename = "04_microbiome_composition_barplot.png",
       path = "results",
       plot = microbiome_plot,
       width = 12,
       height = 6)
