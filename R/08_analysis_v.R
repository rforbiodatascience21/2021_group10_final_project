rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(patchwork)


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
data <- read_tsv(file = "data/02_clean_data.tsv")


# Wrangle data ------------------------------------------------------------
# Subsetting the table for analysis and renaming the columns
ABCbehavior_actinobac_data <- data %>% 
  select(Subject,
         Sample, 
         Timing,
         Treatment,
         starts_with("ABC"), 
         P_Actinobacteria,
         G_Bifidobacterium,
         G_Clostridium,
         contains("g__Akkermansia")) %>% 
  rename("Irritability score" = ABC_SS1,
         "Lethargy score" = ABC_SS2,
         "Stereotypic_behavior score" = ABC_SS3,
         "Hyperactivity score" = ABC_SS4,
         "Abberant Behavior Score" = ABC_Total,
         "Actinobacteria" = P_Actinobacteria,
         "Bifidobacterium" = G_Bifidobacterium,
         "Clostridium" = G_Clostridium,
         "Akkermansia" = `k__Bacteria;p__Verrucomicrobia;c__Verrucomicrobiae;o__Verrucomicrobiales;f__Verrucomicrobiaceae;g__Akkermansia`) %>% 
  mutate(Treatment = case_when(Treatment == "Pre" ~ "Prebiotic",
                               Treatment == "Syn" ~ "Synbiotic"))


# Visualize data ----------------------------------------------------------
# Scatterplot visualization of correlation between
# aberrant behavior (total) score and Actionbacteria amounts
# before and after the treatment
p1 <- ABCbehavior_actinobac_data %>% 
  ggplot(mapping = aes(x = `Abberant Behavior Score`,
                       y = Actinobacteria,
                       color = Timing)) +
  geom_point(size = 4) +
  # Text displayed for patients after the Syn treatment
  geom_text(aes(label = ifelse(Timing == "Post" & (Treatment == "Synbiotic" | Treatment == "Prebiotic"),
                               as.character(Subject),
                               '')), 
            size = 3, 
            hjust=0, 
            vjust=0, 
            colour = "black") +
  facet_wrap(~ Treatment) +
  theme_bw() 


# Scatterplot visualization of correlation between
# aberrant behavior (total) score and Bifidobacterium amounts
# before and after the treatment
p2 <- ABCbehavior_actinobac_data %>% 
  ggplot(mapping = aes(x = `Abberant Behavior Score`,
                       y = Bifidobacterium,
                       color = Timing)) +
  geom_point(size = 4) +
  # Text displayed for patients after the Syn treatment
  geom_text(aes(label = ifelse(Timing == "Post" & (Treatment == "Synbiotic" | Treatment == "Prebiotic"),
                               as.character(Subject),
                               '')), 
            size = 3, 
            hjust=0, 
            vjust=0, 
            colour = "black") +
  facet_wrap(~ Treatment) +
  theme_bw() 

# Scatterplot visualization of correlation between
# aberrant behavior (total) score and Clostridium amounts
# before and after the treatment
p3 <- ABCbehavior_actinobac_data %>% 
  ggplot(mapping = aes(x = `Abberant Behavior Score`,
                       y = Clostridium,
                       color = Timing)) +
  geom_point(size = 4) +
  # Text displayed for patients after the Syn treatment
  geom_text(aes(label = ifelse(Timing == "Post" & (Treatment == "Synbiotic" | Treatment == "Prebiotic"),
                               as.character(Subject),
                               '')), 
            size = 3, 
            hjust=0, 
            vjust=0, 
            colour = "black") +
  facet_wrap(~ Treatment) +
  theme_bw() 

# Scatterplot visualization of correlation between
# aberrant behavior (total) score and Clostridium amounts
# before and after the treatment
p3 <- ABCbehavior_actinobac_data %>% 
  ggplot(mapping = aes(x = `Abberant Behavior Score`,
                       y = Clostridium,
                       color = Timing)) +
  geom_point(size = 4) +
  # Text displayed for patients after the Syn treatment
  geom_text(aes(label = ifelse(Timing == "Post" & (Treatment == "Synbiotic" | Treatment == "Prebiotic"),
                               as.character(Subject),
                               '')), 
            size = 3, 
            hjust=0, 
            vjust=0, 
            colour = "black") +
  facet_wrap(~ Treatment) +
  theme_bw() 

# Scatterplot visualization of correlation between
# aberrant behavior (total) score and Akkermansia amounts
# before and after the treatment
p4 <- ABCbehavior_actinobac_data %>% 
  ggplot(mapping = aes(x = `Abberant Behavior Score`,
                       y = Akkermansia,
                       color = Timing)) +
  geom_point(size = 4) +
  # Text displayed for patients after the Syn treatment
  geom_text(aes(label = ifelse(Timing == "Post" & (Treatment == "Synbiotic" | Treatment == "Prebiotic"),
                               as.character(Subject),
                               '')), 
            size = 3, 
            hjust=0, 
            vjust=0, 
            colour = "black") +
  facet_wrap(~ Treatment) +
  theme_bw() 
