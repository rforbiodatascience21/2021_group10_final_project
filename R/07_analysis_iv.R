# Clear workspace ---------------------------------------------------------
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
GIsymptoms_data <- data %>% 
  select(Sample, 
         Timing,
         Treatment,
         Pain,
         Gas,
         Dia,
         Con) %>%
  rename("Diarrhea" = Dia,
         "Constipation" = Con) %>% 
  mutate(Treatment = case_when(Treatment == "Pre" ~ "Prebiotic",
                               Treatment == "Syn" ~ "Synbiotic")) %>% 
  mutate(Pain, Gas, Diarrhea, Constipation = case_when(. == 0 ~ "never",
                                                       . == 1 ~ "rarely",
                                                       . == 2 ~ "sometimes",
                                                       . == 3 ~ "frequently",
                                                       . == 4 ~ "always")) %>% 
  drop_na()



# Visualize data ----------------------------------------------------------
# Barplot of pre and post treatment pain symptoms 
pain_plot <- GIsymptoms_data %>% 
  ggplot(mapping = aes(x = Timing,
                       fill = Pain)) +
  geom_bar(position='dodge') +
  facet_wrap(~ Treatment) +
  theme_bw() 

# or like this
GIsymptoms_data %>% 
  ggplot(mapping = aes(x = Pain,
                       fill = Timing)) +
  geom_bar(position='dodge') +
  facet_wrap(~ Treatment) +
  theme_bw()



# Barplot of pre and post treatment gas symptoms
gas_plot <- GIsymptoms_data %>% 
  ggplot(mapping = aes(x = Timing,
                       fill = Gas)) +
  geom_bar(position='dodge') +
  facet_wrap(~ Treatment) +
  theme_bw() 

# or like this
GIsymptoms_data %>% 
  ggplot(mapping = aes(x = Gas,
                       fill = Timing)) +
  geom_bar(position='dodge') +
  facet_wrap(~ Treatment) +
  theme_bw()


# Barplot of pre and post treatment diarrhea symptoms 
diarrhea_plot <- GIsymptoms_data %>% 
  ggplot(mapping = aes(x = Timing,
                       fill = Diarrhea)) +
  geom_bar(position='dodge') +
  facet_wrap(~ Treatment) +
  theme_bw()

# or like this
GIsymptoms_data %>% 
  ggplot(mapping = aes(x = Diarrhea,
                       fill = Timing)) +
  geom_bar(position='dodge') +
  facet_wrap(~ Treatment) +
  theme_bw()



# Barplot of pre and post treatment constipation symptoms 
constipation_plot <- GIsymptoms_data %>% 
  ggplot(mapping = aes(x = Timing,
                       fill = Constipation)) +
  geom_bar(position='dodge') +
  facet_wrap(~ Treatment) +
  theme_bw()

# or like this
GIsymptoms_data %>% 
  ggplot(mapping = aes(x = Constipation,
                       fill = Timing)) +
  geom_bar(position='dodge') +
  facet_wrap(~ Treatment) +
  theme_bw()