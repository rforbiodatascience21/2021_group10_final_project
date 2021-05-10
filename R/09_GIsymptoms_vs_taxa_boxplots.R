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
GIsymptoms_taxa_data <- data %>% 
  select(Sample, 
         Timing,
         Treatment,
         Pain,
         Gas,
         Dia,
         Con,
         P_Actinobacteria,
         G_Bifidobacterium,
         contains("g__Akkermansia"),
         G_Clostridium) %>%
  rename("Diarrhea" = Dia,
         "Constipation" = Con,
         "Actinobacteria" = P_Actinobacteria,
         "Bifidobacterium" = G_Bifidobacterium,
         "Clostridium" = G_Clostridium,
         "Akkermansia" = `k__Bacteria;p__Verrucomicrobia;c__Verrucomicrobiae;o__Verrucomicrobiales;f__Verrucomicrobiaceae;g__Akkermansia`) %>% 
  mutate(Treatment = case_when(Treatment == "Pre" ~ "Prebiotic",
                               Treatment == "Syn" ~ "Synbiotic")) %>% 
  # mutate(Pain, Gas, Diarrhea, Constipation = case_when(. == 0 ~ "never",
  #                                                     . == 1 ~ "rarely",
  #                                                     . == 2 ~ "sometimes",
  #                                                     . == 3 ~ "frequently",
  #                                                     . == 4 ~ "always")) %>%
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
# Boxplots of pain symptoms against different taxa pre and post treatment
# Pain vs. bifidobacterium
p1 <- GIsymptoms_taxa_data %>% 
  ggplot(mapping = aes(x = Pain,
                       y = Bifidobacterium,
                       fill = Timing)) +
  geom_boxplot() +
  facet_wrap(~ Treatment) +
  theme_bw() 

# Pain vs clostridium
p2 <- GIsymptoms_taxa_data %>% 
  ggplot(mapping = aes(x = Pain,
                       y = Clostridium,
                       fill = Timing)) +
  geom_boxplot() +
  facet_wrap(~ Treatment) +
  ylim(0, 0.005) +
  theme_bw() 

# Pain vs. Actinobacteria
p3 <- GIsymptoms_taxa_data %>% 
  ggplot(mapping = aes(x = Pain,
                       y = Actinobacteria,
                       fill = Timing)) +
  geom_boxplot() +
  facet_wrap(~ Treatment) +
  theme_bw() 


# Pain vs. akkermansia
p4 <- GIsymptoms_taxa_data %>% 
  ggplot(mapping = aes(x = Pain,
                       y = Akkermansia,
                       fill = Timing)) +
  geom_boxplot() +
  facet_wrap(~ Treatment) +
  theme_bw() 


# Boxplots of gas symptoms against different taxa pre and post treatment 
# Gas vs. akkermansia
p5 <- GIsymptoms_taxa_data %>% 
  ggplot(mapping = aes(x = Gas,
                       y = Actinobacteria,
                       fill = Timing)) +
  geom_boxplot() +
  facet_wrap(~ Treatment) +
  theme_bw() 

# Gas vs. bifidobaterium
p6 <- GIsymptoms_taxa_data %>% 
  ggplot(mapping = aes(x = Gas,
                       y = Bifidobacterium,
                       fill = Timing)) +
  geom_boxplot() +
  facet_wrap(~ Treatment) +
  theme_bw()

# Gas vs. akkermansia
p7 <- GIsymptoms_taxa_data %>% 
  ggplot(mapping = aes(x = Gas,
                       y = Akkermansia,
                       fill = Timing)) +
  geom_boxplot() +
  facet_wrap(~ Treatment) +
  theme_bw()

# Gas vs. clostridium
p8 <- GIsymptoms_taxa_data %>% 
  ggplot(mapping = aes(x = Gas,
                       y = Clostridium,
                       fill = Timing)) +
  geom_boxplot() +
  facet_wrap(~ Treatment) +
  theme_bw()



# Boxplots of diarrhea symptoms against different taxa pre and post treatment 
# Diarrhea vs. akkermansia
p9 <- GIsymptoms_taxa_data %>% 
  ggplot(mapping = aes(x = Diarrhea,
                       y = Actinobacteria,
                       fill = Timing)) +
  geom_boxplot() +
  facet_wrap(~ Treatment) +
  theme_bw() 

# Diarrhea vs. bifidobaterium
p10 <- GIsymptoms_taxa_data %>% 
  ggplot(mapping = aes(x = Diarrhea,
                       y = Bifidobacterium,
                       fill = Timing)) +
  geom_boxplot() +
  facet_wrap(~ Treatment) +
  theme_bw()

# Diarrhea vs. akkermansia
p11 <- GIsymptoms_taxa_data %>% 
  ggplot(mapping = aes(x = Diarrhea,
                       y = Akkermansia,
                       fill = Timing)) +
  geom_boxplot() +
  facet_wrap(~ Treatment) +
  theme_bw()

# Diarrhea vs. clostridium
p12 <- GIsymptoms_taxa_data %>% 
  ggplot(mapping = aes(x = Diarrhea,
                       y = Clostridium,
                       fill = Timing)) +
  geom_boxplot() +
  facet_wrap(~ Treatment) +
  theme_bw()


# Boxplots of constipation symptoms against different taxa pre and post treatment 
# Constipation vs. akkermansia
p13 <- GIsymptoms_taxa_data %>% 
  ggplot(mapping = aes(x = Constipation,
                       y = Actinobacteria,
                       fill = Timing)) +
  geom_boxplot() +
  facet_wrap(~ Treatment) +
  theme_bw() 

# Constipation vs. bifidobaterium
p14 <- GIsymptoms_taxa_data %>% 
  ggplot(mapping = aes(x = Constipation,
                       y = Bifidobacterium,
                       fill = Timing)) +
  geom_boxplot() +
  facet_wrap(~ Treatment) +
  theme_bw()

# Constipation vs. akkermansia
p15 <- GIsymptoms_taxa_data %>% 
  ggplot(mapping = aes(x = Constipation,
                       y = Akkermansia,
                       fill = Timing)) +
  geom_boxplot() +
  facet_wrap(~ Treatment) +
  theme_bw()

# Constipation vs. clostridium
p16 <- GIsymptoms_taxa_data %>% 
  ggplot(mapping = aes(x = Constipation,
                       y = Clostridium,
                       fill = Timing)) +
  geom_boxplot() +
  facet_wrap(~ Treatment) +
  theme_bw()

pain_plot = (p1 + p2) / (p3 + p4)
gas_plot = (p5 + p6) / (p7 + p8)
diarrhea_plot = (p9 + p10) / (p11 + p12)
constipation_plot = (p13 + p14) / (p15 + p16)

# Write data --------------------------------------------------------------
write_tsv(x = GIsymptoms_taxa_data,
          file = "data/09_GIsymptoms_taxa_data.tsv")

ggsave(filename = "09_pain_plot.png",
       path = "results",
       plot = pain_plot,
       width = 12,
       height = 6)
ggsave(filename = "09_gas_plot.png",
       path = "results",
       plot = gas_plot,
       width = 12,
       height = 6)
ggsave(filename = "09_diarrhea_plot.png",
       path = "results",
       plot = diarrhea_plot,
       width = 12,
       height = 6)
ggsave(filename = "09_constipation_plot.png",
       path = "results",
       plot = constipation_plot,
       width = 12,
       height = 6)
