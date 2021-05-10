# Clear workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(corrr)

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
sanctuary_data <- read_tsv(file = "data/03_final_data_clean_aug.tsv")

# Wrangle data ------------------------------------------------------------

# 1 - Finding the interesting variables to visualize
# Subsetting microbiome and metabolites data
micro_meta_data <- sanctuary_data %>% 
  select(starts_with("k__"),
         ends_with("_fecal"),
         ends_with("_urine"),
         ends_with("_serum")) %>%
  drop_na()

# Creating the long correlation table
res.cor <- correlate(micro_meta_data) %>%
  shave() %>%
  stretch(na.rm = TRUE)

# Keeping only the correlations of interest (gut metabolites vs bacteria)

# all
res.cor <- res.cor %>%
  filter((str_detect(x,"_fecal") | str_detect(y,"_fecal")) |
           (str_detect(x,"_urine") | str_detect(y,"_urine")) |
           (str_detect(x,"_serum") | str_detect(y,"_serum")),
         !(str_detect(x,"_fecal") & str_detect(y,"_fecal")),
         !(str_detect(x,"_fecal") & str_detect(y,"_urine")),
         !(str_detect(x,"_fecal") & str_detect(y,"_serum")),
         !(str_detect(x,"_serum") & str_detect(y,"_serum")),
         !(str_detect(x,"_serum") & str_detect(y,"_fecal")),
         !(str_detect(x,"_serum") & str_detect(y,"_urine")),
         !(str_detect(x,"_urine") & str_detect(y,"_urine")),
         !(str_detect(x,"_urine") & str_detect(y,"_fecal")),
         !(str_detect(x,"_urine") & str_detect(y,"_serum")),
         !(startsWith(x,"k") & startsWith(y,"k"))
  )


# Correlations of gut vs bacteria superior to 0.8
high_corr <- res.cor %>%
  filter((r > 0.8 | r < -0.8))

# Finding highest fecal vs bacterias correlations
high_cor_fecal <- high_corr %>%
  filter((str_detect(x,"_fecal") | str_detect(y,"_fecal")),
         !(str_detect(x,"_fecal") & str_detect(y,"_fecal")))

# Finding highest urine vs bacterias correlations
high_cor_urine <- high_corr %>%
  filter((str_detect(x,"_urine") | str_detect(y,"_urine")),
         !(str_detect(x,"_urine") & str_detect(y,"_urine")))

# Finding highest serum vs bacterias correlations
high_cor_serum <- high_corr %>%
  filter((str_detect(x,"_serum") | str_detect(y,"_serum")),
         !(str_detect(x,"_serum") & str_detect(y,"_serum")))

# 2 - The data to visualize the findings

micro_meta_data_2 <- sanctuary_data %>% 
  select(Sample,
         Subject,
         starts_with("k__"),
         Arabinose_fecal,
         Serine_fecal,
         Mannitol_urine,
         Isoleucine_urine,
         Acetoacetate_serum,
         Glucose_serum) %>% 
  pivot_longer(cols = -c(Sample,Subject,Arabinose_fecal,Serine_fecal,
                         Mannitol_urine,Isoleucine_urine,Acetoacetate_serum,Glucose_serum),
               names_to = "Taxa",
               values_to = "Relative_abundance") %>% 
  mutate(Taxa = str_remove_all(Taxa,
                               "[kpcofg]__"),
         Taxa = str_remove_all(Taxa,
                               "\\["),
         Taxa = str_remove_all(Taxa,
                               "\\]")) %>% 
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

# Distribution of the correlation of the overall data
distribution <- res.cor %>%
  ggplot(aes(r)) +
  geom_histogram(bins = 10) +
  xlab("Correlation Coefficient") +
  ylab("Count") +
  labs(title = "Distribution of the correlation coefficients of Gut metabolites vs Bacterias \n")

# Examples of scatterplots based on the findings (from the pairs that had the 
# highest correlation)
# Arabinose_fecal against Gammaproteobacteria on a scatterplot
p1 <- micro_meta_data_2 %>% 
  mutate(Subject = as.factor(Subject),
         Class_taxa = as.factor(Class_taxa)) %>%
  filter(Class_taxa == "Gammaproteobacteria") %>%
  ggplot(mapping = aes(x = log10(Arabinose_fecal),
                       y = Relative_abundance,
                       colour = Subject)) +
  geom_point() +
  xlab("Arabinose fecal") +
  ylab("R. A. of Gammaproteobacteria") +
  theme(legend.position = "none")

# Serine_fecal against Clostridia on a scatterplot
p2 <- micro_meta_data_2 %>% 
  mutate(Subject = as.factor(Subject),
         Class_taxa = as.factor(Class_taxa)) %>%
  filter(Class_taxa == "Clostridia") %>%
  ggplot(mapping = aes(x = log10(Serine_fecal),
                       y = Relative_abundance,
                       colour = Subject)) +
  geom_point() +
  xlab("Serine fecal") +
  ylab("R. A. of Clostridia") +
  theme(legend.position = "none")

# Mannitol_urine against Methanobacteria on a scatterplot
p3 <- micro_meta_data_2 %>% 
  mutate(Subject = as.factor(Subject),
         Class_taxa = as.factor(Class_taxa)) %>%
  filter(Class_taxa == "Methanobacteria") %>%
  ggplot(mapping = aes(x = log10(Mannitol_urine),
                       y = Relative_abundance,
                       colour = Subject)) +
  geom_point() +
  xlab("Mannitol urine") +
  ylab("R. A. of Methanobacteria") +
  theme(legend.position = "none")

# Isoleucine_urine against Erysipelotrichi on a scatterplot
p4 <- micro_meta_data_2 %>% 
  mutate(Subject = as.factor(Subject),
         Class_taxa = as.factor(Class_taxa)) %>%
  filter(Class_taxa == "Erysipelotrichi") %>%
  ggplot(mapping = aes(x = log10(Isoleucine_urine),
                       y = Relative_abundance,
                       colour = Subject)) +
  geom_point() +
  xlab("Isoleucine urine") +
  ylab("R. A. of Erysipelotrichi") +
  theme(legend.position = "none")

# Acetoacetate_serum against Thermoplasmata on a scatterplot
p5 <- micro_meta_data_2 %>% 
  mutate(Subject = as.factor(Subject),
         Class_taxa = as.factor(Class_taxa)) %>%
  filter(Class_taxa == "Thermoplasmata") %>%
  ggplot(mapping = aes(x = log10(Acetoacetate_serum),
                       y = Relative_abundance,
                       colour = Subject)) +
  geom_point() +
  xlab("Acetoacetate serum") +
  ylab("R. A. of Thermoplasmata") +
  theme(legend.position = "none")

# Glucose_serum against Bacilli on a scatterplot
p6 <- micro_meta_data_2 %>% 
  mutate(Subject = as.factor(Subject),
         Class_taxa = as.factor(Class_taxa)) %>%
  filter(Class_taxa == "Bacilli") %>%
  ggplot(mapping = aes(x = log10(Glucose_serum),
                       y = Relative_abundance,
                       colour = Subject)) +
  geom_point() +
  xlab("Glucose serum") +
  ylab("R. A. of Bacilli") +
  theme(legend.position = "none")

# Merging the 6 examples in one figure
figure <- ggarrange(p1, p2, p3, p4, p5, p6,
                    labels = "AUTO",
                    ncol = 2, nrow = 3,
                    common.legend = TRUE,
                    legend = "right",
                    vjust = 0.5) %>%
  annotate_figure(top = text_grob("Relative abundance of some Bacterias versus some Metabolites \n",
                                  face = "bold", 
                                  size = 14))
figure



# Write data --------------------------------------------------------------
write_tsv(x = micro_meta_data,
          file = "data/11_metabolites_vs_bacterias.tsv")

write_tsv(x = micro_meta_data_2,
          file = "data/11_metabolites_vs_bacterias.tsv")

write_tsv(x = res.cor,
          file = "data/11_metabolites_vs_bacterias.tsv")

ggsave(filename = "11_distribution_plot.png",
       path = "results",
       plot = distribution,
       width = 12,
       height = 8)

ggsave(filename = "11_scatter_plots.png",
       path = "results",
       plot = figure,
       width = 12,
       height = 8)
