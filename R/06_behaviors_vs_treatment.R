# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggpubr)
library(haven)


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
sanctuary_data <- read_tsv(file = "data/03_final_data_clean_aug.tsv")


# Wrangle data ------------------------------------------------------------

# Subset Behavioral data for further plotting
# Aberrant behavior Data
ABC_data <- sanctuary_data %>% 
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
ABAS_data <- sanctuary_data %>% 
  select(Subject, Treatment, Timing, starts_with("ABAS")) %>%
  pivot_longer(cols = -c(Subject, Timing, Treatment),
               names_to = "Adaptative_Behavior",
               values_to = "Score") %>% 
  drop_na()

# Repetitive behavior data
RBS_data <- sanctuary_data %>% 
  select(Subject, Treatment, Timing, starts_with("RBS")) %>%
  rename(Stereotype = RBS_SS1,
         Compulsive = RBS_SS2,
         Routine = RBS_SS3,
         Sameness = RBS_SS4,
         Restricted = RBS_SS5) %>%
  pivot_longer(cols = -c(Subject, Timing, Treatment),
               names_to = "Repetitive_Behavior",
               values_to = "Score") %>% 
  drop_na()


# Visualize data ----------------------------------------------------------


# Visualizing Abberant Behavior for all subject with boxplots
# before and after treatment

ABC_plot_1 <- ABC_data %>% 
  ggplot(mapping = aes(x = factor(Subject),
                       y = Score,
                       color = Timing)) +
  geom_boxplot() +
  facet_grid(.~Treatment) +
  stat_compare_means(label = "p.signif",paired = TRUE) +
  xlab("Subject") +
  ylab("Behavior Score") +
  labs(title = "Subjects behavior score with Prebiotic or Synbiotic treatment",
       subtitle = "Boxplots stratified on before and after treatment")

# Visualizing Abberant Behavior for one subject before and after treatment

ABC_plot_2 <- ABC_data %>%
  filter(Subject == 202) %>% 
  ggplot(mapping = aes(x = Abberant_Behavior,
                       y = Score,
                       fill = Timing)) +
  geom_bar(stat="identity") +
  facet_grid(.~Treatment) +
  xlab("Abberant Behavior Type") +
  labs(title = "Subject \"202\" score with Prebiotic or Synbiotic treatment",
       subtitle = "Barplots stratified on pre and post treatment") +
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 0.5),
        panel.background = element_blank())

ABC_plot_3 <- ABC_data %>%
  filter(Subject == 212) %>% 
  ggplot(mapping = aes(x = Abberant_Behavior,
                       y = Score,
                       fill = Timing)) +
  geom_bar(stat="identity") +
  facet_grid(.~Treatment) +
  xlab("Abberant Behavior Type") +
  labs(title = "Subject \"212\" score with Prebiotic or Synbiotic treatment",
       subtitle = "Barplots stratified on pre and post treatment") +
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 0.5),
        panel.background = element_blank())

ABC_plot_significant_subjects <- ABC_plot_2+ABC_plot_3

# Write data --------------------------------------------------------------
write_tsv(x = ABC_data,
          file = "data/06_behaviors_vs_treatment.tsv")

ggsave(filename = "06_ABC_plot.png",
       path = "results",
       plot = ABC_plot_1,
       width = 12,
       height = 8)

ggsave(filename = "06_ABC_plot_subject_202.png",
       path = "results",
       plot = ABC_plot_2,
       width = 12,
       height = 8)

ggsave(filename = "06_ABC_plot_subject_202-212.png",
       path = "results",
       plot = ABC_plot_significant_subjects,
       width = 12,
       height = 8)

