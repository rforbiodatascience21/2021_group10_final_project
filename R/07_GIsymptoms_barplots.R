# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(patchwork)
library(forcats)
library(RColorBrewer)


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
data <- read_tsv(file = "data/03_final_data_clean_aug.tsv")


# Wrangle data ------------------------------------------------------------
# Subsetting the table for analysis and renaming the columns
GIsymptoms_data <- data %>% 
  select(Timing,
         Treatment,
         Pain,
         Gas,
         Dia,
         Con) %>%
  rename("Diarrhea" = Dia,
         "Constipation" = Con) %>%
  mutate(Treatment = case_when(Treatment == "Pre" ~ "Prebiotic",
                               Treatment == "Syn" ~ "Synbiotic")) %>% 
  mutate(Constipation = case_when(Constipation == 0 ~ "never",
                                  Constipation == 1 ~ "rarely",
                                  Constipation == 2 ~ "sometimes",
                                  Constipation == 3 ~ "frequently",
                                  Constipation == 4 ~ "always"),
         Pain = case_when(Pain == 0 ~ "never",
                          Pain == 1 ~ "rarely",
                          Pain == 2 ~ "sometimes",
                          Pain == 3 ~ "frequently",
                          Pain == 4 ~ "always"),
         Diarrhea = case_when(Diarrhea == 0 ~ "never",
                              Diarrhea == 1 ~ "rarely",
                              Diarrhea == 2 ~ "sometimes",
                              Diarrhea == 3 ~ "frequently",
                              Diarrhea == 4 ~ "always"),
         Gas = case_when(Gas == 0 ~ "never",
                         Gas == 1 ~ "rarely",
                         Gas == 2 ~ "sometimes",
                         Gas == 3 ~ "frequently",
                         Gas == 4 ~ "always")) %>% 
drop_na()



# Visualize data ----------------------------------------------------------
# Barplot of pre and post treatment pain symptoms 
pain_plot <- GIsymptoms_data %>% 
  ggplot(mapping = aes(x = Timing,
                       fill = Pain)) +
  geom_bar(position='dodge') +
  facet_wrap(~ Treatment) +
  theme_bw() +
  aes(x = fct_inorder(Timing)) +
  labs(x = element_blank()) +
  scale_fill_manual(values=c("#000066", 
                             "#66CCFF", 
                             "#6666FF", 
                             "#0000CC")) +
  labs(title = "GI symptoms pre and post Prebiotic or Synbiotic treatment",
       subtitle = "Boxplots stratified on four symptom levels")


# Barplot of pre and post treatment gas symptoms
gas_plot <- GIsymptoms_data %>% 
  ggplot(mapping = aes(x = Timing,
                       fill = Gas)) +
  geom_bar(position='dodge') +
  facet_wrap(~ Treatment) +
  theme_bw() +
  aes(x = fct_inorder(Timing)) +
  labs(x = element_blank()) +
  scale_fill_manual(values=c("#000033",
                             "#000066", 
                             "#66CCFF", 
                             "#6666FF", 
                             "#0000CC")) 


# Barplot of pre and post treatment diarrhea symptoms 
diarrhea_plot <- GIsymptoms_data %>% 
  ggplot(mapping = aes(x = Timing,
                       fill = Diarrhea)) +
  geom_bar(position='dodge') +
  facet_wrap(~ Treatment) +
  theme_bw() +
  aes(x = fct_inorder(Timing)) +
  xlab("Timing") +
  scale_fill_manual(values=c("#000066", 
                             "#66CCFF", 
                             "#6666FF", 
                             "#0000CC")) 


# Barplot of pre and post treatment constipation symptoms 
constipation_plot <- GIsymptoms_data %>% 
  ggplot(mapping = aes(x = Timing,
                       fill = Constipation)) +
  geom_bar(position='dodge') +
  facet_wrap(~ Treatment) +
  theme_bw() +
  aes(x = fct_inorder(Timing)) +
  xlab("Timing") +
  scale_fill_manual(values=c("#000033",
                             "#000066", 
                             "#66CCFF", 
                             "#6666FF", 
                             "#0000CC")) 

GIsymptoms_plot <- (pain_plot | gas_plot) /
  (diarrhea_plot | constipation_plot) 

# Write data --------------------------------------------------------------
write_tsv(x = GIsymptoms_data,
          file = "data/07_Gastrointestinal_symptoms_data.tsv")

ggsave(filename = "07_Gastrointestinal_symptoms_bar_plot.png",
       path = "results",
       plot = GIsymptoms_plot,
       width = 12,
       height = 8)
